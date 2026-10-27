# What the old Manse interpreter got wrong

A post-mortem of the original implementation (`src/Lib.hs` as of commit `06a34cb`),
written while rewriting it. Unless marked otherwise, the behaviour below was checked by compiling the old
code with GHC 9.12 and running small programs against it. These are not guesses.

The order is roughly by how much each problem hurt.

---

## 1. The environment model was wrong, and everything else leaned on that

This is the root problem. Most of the strange behaviour below comes from it.

The interpreter ran in `StateT Environment (ExceptT RuntimeError Identity)`. The
environment was an immutable value: a `Map` of variables plus a `Maybe Environment`
parent. Entering a scope looked like this:

```haskell
runAsChildScopeOf par m = do
  (v, newState) <- tryExecuteAndReturnState m $ Environment{parent = Just par, variables = M.empty}
  SM.modify (\env' -> fromMaybe env' (parent newState))
  pure v
```

It runs `m` in a new child of `par`, then **replaces the current state with the
(possibly modified) `par`**. For a block, `par` is the current environment, so this
works. For a function call, `par` is the function's *closure*, so after every call
the caller's whole environment is replaced by the callee's closure environment.

Results:

```
roseduuri apu() { kylä lähtee 1; }
roseduuri pää() { seonnääs nii et x on ny 5; apu(); kylä lähtee x; }
```
→ `RuntimeError "Undefined variable\"x\""`. Calling any function deleted every
local variable of the caller. `apu() plus x` fails the same way.

```
roseduuri laskuri() { seonnääs nii et n on ny 0; roseduuri inc() { ... } kylä lähtee inc; }
roseduuri pää() { seonnääs nii et c on ny laskuri(); c(); ... }
```
→ `trying to call undefined function "c"`. The classic closure counter cannot work:
calling `laskuri()` deletes `c` before it is even assigned.

### Why the tests still passed

The existing tests happened to avoid this pattern. In every test, `pää` either used
no locals after a call, or recursed.

### Why recursion "needed" the mysterious hack

`callFn` contained this:

```haskell
-- not sure why I need this tbh, figure out later #somethingrotteninthestateofdenmark
SM.modify (\env -> ... M.insert n (VFunc (FunctionDeclaration n params block, env)) ...)
```

Here is the explanation. The hack re-inserts the function into its own call frame,
with that call frame as its closure. When the function then calls itself, the
"closure" that `runAsChildScopeOf` restores afterwards is the caller's own frame,
so the clobbering in the previous section happens to restore the correct
environment. The hack did not fix recursion. It hid the clobbering bug for one
special case.

### Closures were snapshots, not references

Because environments were immutable values, a closure captured a *copy* of its
environment at definition time. That has two consequences:

- **Forward references fail.** A function can't call a function defined after it:
  `roseduuri a() { kylä lähtee b(); } roseduuri b() {...}` →
  `trying to call undefined function "b"`. Mutual recursion was impossible.
- **Closures can't share state.** Two closures over the same variable would each
  have their own copy, so a write through one would be invisible to the other.
  (This follows from the design. It couldn't be shown directly, because the bug
  above breaks any program that tries.)

The function declaration tried to tie the knot with a lazy self-reference
(`newEnv` refers to itself). That's clever, but it only covers self-reference.

**The fix:** mutable environments (`IORef`s, or here, mutable vectors) in `IO`.
A closure holds a *pointer* to its defining frame. Calling a function creates a
new frame whose parent is that pointer. The caller's environment is never touched.
That's what Crafting Interpreters does, and Haskell can do it too.

### And it was slow

Each lookup of a variable `d` scopes up re-ran the whole monad stack `d` times
(`tryExecute (recursiveLookup str) upperCtx`). Every block entry ran a separate
`runExceptT`/`runStateT` and copied the result back. The rewrite resolves every
variable to a fixed `(hops, slot)` before running anything. It is about 2× faster
on `fib(30)` (0.78s → 0.38s, both built with `-O2`), and it does more work per
call, because it keeps stack traces and a depth limit.

---

## 2. Things that were plainly broken

| Program | Old result | Why |
|---|---|---|
| `kylä lähtee !tosi;` | `True` | `Not e -> evalExp e <&> VBoolean . isTruthy`: the `not` is missing. `!` was the identity function for truthiness. |
| `tulosta 123;` | (nothing) | `Print (PrintStatement p) -> pure Nothing -- todo handle prints`. The language had no output, and the monad stack had no `IO`, so it couldn't have output even with the code written. |
| `1 jaettuna 0` | a 309-digit integer | Division by zero gives `Infinity`. `show` checks "is it an integer?" with `x == fromInteger (round x)`, which is true for `Infinity`, then prints `round Infinity`. |
| `1 on sama ku "a"` vs `1 eijoo "a"` | `False` vs a crash | `==` returned `False` for mixed types, but `!=` raised a runtime error. |
| `"a" plus "b"` | `Cannot add a non-number` | No string concatenation. Strings could be created and returned, but nothing could be done with them. |
| `return "pää"` | `"p\228\228"` | Output went through `show`, which escapes non-ASCII. Every word with ä or ö came out garbled, in a language based on a Finnish dialect. |
| `f()()` | parse error | `parseExp` parsed at most one call suffix, and evaluation of `Call` only accepted a plain identifier as the callee. |
| `pää(x)` from the CLI | `wrong num of args` | `Main` always passed `[]`. `runProgram`'s `args` parameter was ignored. |
| Two `for` loops with `i` in one function | `Redeclaration of variable` | The loop variable was declared in the enclosing scope, because the desugaring didn't wrap the loop in a block. |
| `elikkä jos (init; ; step)` | parse error | The `for` parser's semicolon handling was ad hoc. The `(Nothing, Just e2)` case in the desugaring would have used the *step expression as the loop condition*, but it was unreachable. |
| `else if` | parse error | Only `mut jos ei ni { }` existed. |
| `1 plus` + newline + `2` | parse error | Operators were lexed as `string "plus "` with a literal trailing space, so a newline or tab after an operator failed. |
| `// comment` at end of file | parse error | `singleLineComment` required `eol` and didn't accept end of input. |
| `/* a */ // b` | parse error | Comments were skipped as `skipMany line >> skipMany block`, so a line comment after a block comment wasn't skipped. |

---

## 3. Error messages

- **Almost every parse error said the same thing**:
  `"Error at offset 0" "Unexpected token: Tokens (TFun :| [])" "Expected one of: fromList [EndOfInput]" "error lol"`.
  The statement parser wrapped every alternative in `try`, so any failure anywhere
  backtracked to the start of the file. The offset was also an index into the
  *token list*, not the source text, so it could not have pointed at a line anyway.
- `parseFile` returned `Either () Program`, which threw away the error. The caller
  then printed `"error lol"`.
- Runtime errors had no position: `RuntimeError "Undefined variable\"x\""`. Note the
  missing space, and that it was printed with `print`, which adds a second layer of quotes.
- No stack traces.

The rewrite parses straight from `Text` (no separate token list) and keeps a
source offset on every node that can fail. Errors print the file, line, column,
the source line with a caret under the error, and a call stack, in the dialect.

---

## 4. Code-level problems

- **Type-level machinery used to filter a list.** The `Constrable` class, the
  `OVERLAPPABLE`/`OVERLAPPING` instances, `constr f = constr (f undefined)`, and the
  hand-written `Data ManseOperator` instance with `mkConstr`/`mkDataType` all existed
  to do `filter (=|= PrefixOp) manseOperators`. Two separate lists would have done it.
  The rewrite also uses heavy types, but each use buys something (see the README).
- **The `Ord Precedence` instance** had four nearly identical cases, each with an
  unreachable `otherwise`, and used `RAssoc x == RAssoc x = GT` as a hack to make
  assignment right associative.
- **Partial patterns everywhere:** `\(Binary tok' _ _) -> ...` on a list that was
  only filtered by constructor, `\(TIdentifier i) -> i`, `TIdentifier v <- satisfy ...`,
  `fromJust`, `tail`.
- **A hand-rolled Pratt parser**, when `parser-combinators` already provides
  `makeExprParser`. It also had to handle prefix operators and call suffixes, and
  it got the call suffixes wrong (see `f()()`).
- **`Eq` derived on `RuntimeValue`,** which contained `Environment`, which contained
  functions, which contained environments. Function equality compared whole
  environment trees structurally. The knot-tied self-reference could make that loop
  forever, if anything ever compared two functions. (Nothing did, because `on sama ku`
  had a catch-all `False`.)
- **Defined but never used:** tokens `TClass`, `TSuper`, `TThis`, `TDot`; the `Print`
  statement; `runProgramWithArgs`; the `args` parameter of `runProgram`.
- **Imports:** `Debug.Trace`, `dbg`, `Data.Set`, `VisualStream`,
  `Alternative`; `Typeable` imported twice and `errorBundlePretty` imported twice.
  There was no `-Wall` to catch any of it.
- **`package.yaml`** was the stack template: package `loxinterpreter`, executable
  `loxinterpreter-exe`, `author: "Author name here"`, `github: "githubuser/loxinterpreter"`,
  and a list of 21 default extensions, including `OverloadedStrings` and
  `KindSignatures` twice each and `GADTs`/`PolyKinds`/`TypeFamilies`, none of which were used.
- **The generated `.cabal` file was committed** next to `package.yaml`.
- **One 800-line `Lib.hs`** held the lexer, parser, AST, evaluator, and file I/O.
- `interpretProgram` was a hand-written loop with `safeHead` + `tail`, and called
  `interpretStatement` without using its result.
- `shortCircuitReturn` used `foldM` over the statement list, so after a `return`
  it still visited every remaining statement (doing nothing for each).

---

## 5. Tests

- `unsafeFromRight = either undefined id`: if any test file failed to parse, the
  whole suite crashed with `Prelude.undefined` and gave no hint which file.
- Tests checked only `pää`'s return value, because there was no output.
- Because of that, the tests could not see bug #1: none of them used a local
  variable after calling a non-recursive function.

The rewrite uses golden tests. Each `test/golden/*.manse` file is run, and
everything it prints, including errors, is compared against a `.out` file.
`GOLDEN_ACCEPT=1 stack test` regenerates them.

---

## 6. Things it got right

- The dialect. `kylä lähtee` for `return` and `roseduuri` for function are good choices.
- Megaparsec was the right tool. It just shouldn't have been run twice
  (text → tokens → AST), which lost the source positions.
- `isTruthy` matched Lox exactly.
- The `README` was honest about recursion being broken. The `"Fix recursion"` commit fixed
  the symptom, and the comment in the code admitted nobody knew why it worked. That
  made the real bug easy to find.
