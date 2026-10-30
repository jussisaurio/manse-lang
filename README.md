# The Manse Programming Language

We read [Crafting Interpreters](https://craftinginterpreters.com) at the Reaktor Tampere book club.
Manse is heavily influenced by that book, except written in a good programming language.

Manse is a dynamically typed, interpreted, C-family language whose syntax resembles
the regional dialect of Tampere, Finland. Its error messages are in the dialect too.

```
luakka Elukka {
  alota(nimi) {
    itse.nimi on ny nimi;
  }
  ääntele() eli "{itse.nimi} sanoo {itse.ääni()}";
  ääni() eli "...";
}

luakka Koira niinku Elukka {
  ääni() eli "hau";
}

roseduuri pää() {
  tulosta Koira("Musti").ääntele();     // Musti sanoo hau

  väli(1, 11)
    sitte suodata(roseduuri(x) eli x jakojäännös 2 on sama ku 0)
    sitte kartota(roseduuri(x) eli x potenssiin 2)
    sitte summa
    sitte kato;                          // 220
}
```

## Running

You need [Stack](https://docs.haskellstack.org). It installs the right GHC (9.12) by itself.

```
stack build
stack exec manse                       # REPL
stack exec manse -- tiedosto.manse a b # run a file; pää(argumentit) gets ["a", "b"]
stack exec manse -- -e 'tulosta 1 plus 2;'
stack test                             # golden tests
GOLDEN_ACCEPT=1 stack test             # accept new golden output
```

A program runs its top-level statements from top to bottom. Then, if it defined
`pää`, that gets called. A `pää` with one parameter receives the command line
arguments as a list.

## The language

### Values

| Manse | |
|---|---|
| `1`, `3.14` | numbers (doubles) |
| `"teksti"`, `"moro {nimi}!"` | strings, with `{interpolation}`; escape with `\{` |
| `tosi`, `epätosi` | booleans |
| `tyhjä` | nil |
| `[1, "kaks", [3]]` | lists (mutable, shared by reference) |

`tyhjä` and `epätosi` are falsy. Everything else is truthy, including `0` and `""`.

### Variables and functions

```
seonnääs nii et x on ny 5;       // declare
x on ny x plus 1;                // assign
xs[0] on ny 1;                   // assign to an index
olio.kenttä on ny 2;             // assign to a field

roseduuri tuplaa(x) {
  kylä lähtee x kertaa 2;        // return
}
roseduuri tuplaa(x) eli x kertaa 2;          // same thing, shorter
seonnääs nii et f on ny roseduuri(x) eli x kertaa 2;   // lambda
```

Functions are first-class closures. Closures capture variables by reference, so
counters and shared state work as you'd expect.

### Operators

From loosest to tightest:

| | |
|---|---|
| `on ny` | assignment (right associative) |
| `x sitte f(y)` | pipeline: means `f(x, y)`; `x sitte f` means `f(x)` |
| `tai` | or (returns an operand, short circuits) |
| `ja` | and (returns an operand, short circuits) |
| `on sama ku`, `ei oo sama ku` / `eijoo` | equality (structural for lists) |
| `on isompi ku`, `on isompi tai sama ku`, `om piänempi ku`, `om piänempi tai sama ku` | comparison (numbers or strings) |
| `plus`, `miinus` | `plus` also joins strings and lists |
| `kertaa`, `jaettuna`, `jakojäännös` | `"ha" kertaa 3` is `"hahaha"` |
| `potenssiin` | exponent (right associative) |
| `miinus x`, `ei x`, `!x` | negation, not |
| `f(x)`, `xs[i]`, `olio.kenttä` | call, index (negative counts from the end), field |

`x.f(a)` on anything that isn't an object means `f(x, a)`, so `[3, 1, 2].järjestä().käännä()` works.

### Control flow

```
jos x on isompi ku 0 ni {
  ...
} mut jos x on sama ku 0 ni {
  ...
} mut jos ei ni {
  ...
}

kuha ehto ni { ... }                                       // while
elikkä jos (seonnääs nii et i on ny 0; i om piänempi ku 10; i on ny i plus 1) ni { ... }
jokaselle alkio tästä lista ni { ... }                     // for each (lists and strings)

ny riittää;                                                // break
jatketaa;                                                  // continue
```

### Classes

```
luakka Koira niinku Elukka {      // niinku = inherits from
  alota(nimi) {                   // constructor
    kanta.alota(nimi);            // kanta = super
    itse.temppuja on ny [];       // itse = this
  }
  opeta(temppu) {
    lisää(itse.temppuja, temppu);
    kylä lähtee itse;
  }
}
Koira("Musti").opeta("istu").opeta("tassu");
```

### Exceptions

```
yritä {
  paiskaa "jotain meni pieleen";       // throw any value
} jos käy kalpaten (virhe) ni {
  tulosta "kävi kalpaten:", virhe;
}
```

Runtime errors (division by zero, calling a non-function, etc.) can be caught the
same way. If nothing catches them, you get this:

```
Hei kato ny! jako.manse:2:17: nollalla ei voi jakaa, hölömö
  |
2 |   kylä lähtee a jaettuna b;
  |                 ^

  roseduurissa jaa, jota kutsuttiin jako.manse:6:18
  roseduurissa pää
```

### Built-ins

`ohje(f)` prints the signature of any function:

```
manse> ohje(osa)
"osa(teksti tai lista, kokonaisluku, kokonaisluku) -> mikä vaan"
```

- **Output and input:** `tulosta a, b;` (statement), `kato(x)` (prints `x` and returns it, handy in pipelines), `lue()`
- **Lists:** `pituus`, `lisää`, `poista`, `kopio`, `osa`, `väli`, `kartota`, `suodata`, `taita`, `summa`, `tulo`, `jokanen`, `joku`, `sisältää`, `käännä`, `numeroi`, `järjestä`, `järjestä_millä`, `suurin`, `pienin`
- **Strings:** `pituus`, `osa`, `pilko`, `yhdistä`, `isoks`, `pieneks`, `siistiks`, `numeroks`, `tekstiks`
- **Numbers:** `pyöristä`, `alaspäin`, `ylöspäin`, `itseisarvo`, `juuri`, `satunnainen`
- **Other:** `tyyppi`, `kello`, `varmista(ehto, viesti)`, `ohje`

Half of the standard library is written in Manse itself: see [`prelude/prelude.manse`](prelude/prelude.manse).

## How it works

```
src/
  Manse.hs               public API: run files, REPL input, render errors
  Manse/Syntax.hs        the AST, indexed by compiler phase
  Manse/Parser.hs        megaparsec, straight from Text to AST
  Manse/Resolver.hs      static scope resolution, Parsed -> Resolved
  Manse/Interpreter.hs   tree-walking evaluator
  Manse/Value.hs         runtime values, environments, the Eval monad
  Manse/Native.hs        Haskell functions -> Manse built-ins, via types
  Manse/Builtins.hs      the built-ins
prelude/prelude.manse    the standard library, in Manse
test/golden/             test programs and their expected output
```

Some type-level features that each do a specific job:

- **Trees that grow.** `Expr (p :: Phase)` uses type families for the parts that
  differ between phases. A `Parsed` variable is just a name. A `Resolved` one
  carries its `Loc`: how many frames up, and which slot. The interpreter only
  accepts `Program Resolved`, so skipping the resolver is a type error.
- **Resolved slots.** The resolver gives every local variable a fixed slot in a
  fixed-size mutable frame. A variable lookup at runtime walks `n` parent pointers
  and indexes a vector, with no name lookups. Blocks that declare nothing get no
  frame at all.
- **Built-ins derived from Haskell types.** A built-in is a plain Haskell function:
  `native "osa" $ \(x :: Either Text ManseList) (from :: Int) (to :: Int) -> ...`.
  From the type alone, a type family computes the arity as a type-level `Nat`.
  The `NativeSig` class decodes each argument, with error messages that name the
  expected type (`pituus: 1. argumentin pitäis olla teksti tai lista, mutta se on luku`),
  and generates the signature `ohje` shows. If a built-in forgets to return
  `Eval`, a custom `TypeError` tells you so, in Finnish.
- **`DerivingVia`.** `Eval` is a newtype over `Ctx -> IO a`, which derives its
  `Monad`/`MonadReader`/`MonadIO` instances from `ReaderT Ctx IO`.

For the story of what was wrong with the first version, see
[docs/what-sucked.md](docs/what-sucked.md).
