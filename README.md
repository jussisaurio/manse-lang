# The Manse Programming Language

We are reading [Crafting Interpreters](https://craftinginterpreters.com) at Reaktor Tampere book club.
This language is heavily influenced by that book, except written in a good programming language.

Better docs coming later.

Better implementation coming later.

Better language features coming later.


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

