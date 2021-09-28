# The Manse Programming Language

We are reading [Crafting Interpreters](https://craftinginterpreters.com) at Reaktor Tampere book club.
This language is heavily influenced by that book, except written in a good programming language.

Manse is a C-family style dynamically typed, interpreted language whose syntax resembles the regional dialect of Tampere, Finland.

## Current features:

- Variable declaration: `seonnääs nii et VARIABLE_NAME on ny VARIABLE_VALUE;`
- Variable reassignment: `VARIABLE_NAME on ny VARIABLE_VALUE;` 
- If statements: `jos (CONDITION) ni {} mut jos ei ni {}`)
- While loops: `kuha (CONDITION) ni {}`
- For loops: `elikkä jos (EXPRESSION_OR_DECLARATION; CONDITION; EXPRESSION) {}`
- Functions: `roseduuri FUNCTION_NAME(arg1, arg2) { kylä lähtee arg1 plus arg2; }`
- Block scoped variables
- First class functions and closures
- Recursion

## TODO:

- Better docs
- Better, non-horrible error messages
- Fix a million edge cases, probably
- Moar tests
- Moar features
- REPL

## Example Manse file:


```
roseduuri getClosure() {
    seonnääs nii et x on ny 3;

    roseduuri double() {
        kylä lähtee x kertaa 2;
    }

    kylä lähtee double;
}

roseduuri pää() {
    seonnääs nii et funcWithClosure on ny getClosure();

    kylä lähtee funcWithClosure();
}

```

### Requirements:

[Stack build tool for Haskell](https://docs.haskellstack.org/en/stable/README/)

### To run tests:

`stack test`

### To run a Manse file:

`stack run <path-to-file>`
