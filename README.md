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

## TODO:

- Better docs
- Fix a million edge cases, probably
- Moar tests
- Moar features

## Example Manse file:


```
roseduuri hölömö_viponassi(n) {
  jos (n om piänempi tai sama ku 1) ni {
    kylä lähtee n;
  } mut jos ei ni {
    kylä lähtee hölömö_viponassi(n miinus 1) plus hölömö_viponassi(n miinus 2);
  }
}

// pää is like 'main' in regular boring languages
roseduuri pää(arg1, arg2) {
  seonnääs nii et a on ny 1;
  kuha (a om piänempi ku 10) ni {
    a on ny a plus 1;
  }

  elikkä jos (seonnääs nii et b on ny 0; b om piänempi ku a; b on ny b plus 3) ni {
    a on ny a plus 2;
  }

  kylä lähtee a kertaa hölömö_viponassi(arg1 plus arg2);
}

```

