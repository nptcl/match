Test code for Prolog.  
It runs in Common Lisp.

# Usage

Run Common Lisp.

```
$ sbcl
...
*
```

Load `match.lisp`.

```lisp
* (load #p"match.lisp")
```

Use the package `MATCH`.

```lisp
* (defpackage work (:use cl match))
* (in-package work)
```

Initialize match.  
Run `init-match` or `with-match`.

```lisp
* (init-match)
```

or,

```lisp
* (with-match
    form
    ...)
```

Here is an example of execution.  
The following Prolog rules are used.


```prolog
aaa.
bbb(ccc, ddd).
eee(X) :- bbb(X, ddd), aaa.
```

In Common Lisp, it is defined as follows

```lisp
(defrule aaa)
(defrule (bbb ccc ddd))
(defrule (eee ?x) (bbb ?x ddd) aaa)
```

Here are some examples.

```lisp
* (match (bbb ccc ddd))
NIL
T

* (match (bbb ccc eee))
NIL
NIL

* (match (bbb ?x ?y))
((?X . CCC) (?Y . DDD))
T

* (match (bbb _ ?y))
((?Y . DDD))
T
```

This is an example of using a query.

```lisp
* (defrule (append () ?x ?x))
* (defrule (append (?u . ?x) ?y (?u . ?z))
    (append ?x ?y ?z))
* (query (append ?x ?y (a b c d e f)))
?X = NIL
?Y = (A B C D E F)
(y or n) n  ;; [input]
?X = (A)
?Y = (B C D E F)
(y or n) n  ;; [input]
?X = (A B)
?Y = (C D E F)
(y or n) y  ;; [input]
((?X A B) (?Y C D E F))
T
```

The `match` macro will return the first match.  
The `query` macro queries for matches with the `y-or-n-p` function.

The `match-lisp` function can be used to select the matching condition.

```lisp
* (match-lisp
    '(append ?x ?y (a b c d e f))
    (lambda (list)
      (= 3 (length (cdr (assoc '?x list))))))
((?X A B C) (?Y D E F))
T
```

The following commands can be used in Prolog.

- `true`
- `fail`
- `!`  (cut)
- `and`
- `or`
- `is`
- `progn`

`is` argument is passed directly to `eval` in Common Lisp.  

```lisp
* (with-match
    (defrule (aaa 100))
    (query (aaa ?x) (is ?y (+ 200 ?x))))
?X = 100
?Y = 300
(y or n)
```

`progn` returns the result of `eval` in Common Lisp.  
For example, use the following.

```lisp
* (defrule (< ?x ?y) (progn (< ?x ?y)))
* (match (< 10 20))
NIL
T

```
# License

[The Unlicense](LICENSE)


# Distribution

https://github.com/nptcl/match
