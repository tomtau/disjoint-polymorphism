# Disjoint Polymorphism

Implementation of [Disjoint Polymorphism](http://i.cs.hku.hk/~bruno/papers/ESOP2017.pdf).

## Build and Run

This project can be built with `cabal` or `stack`.

* cabal
```
cabal sandbox init
cabal install --only-dependencies
cabal build
cabal exec disjoint-intersection-exe
```

* stack
```
stack build
stack exec disjoint-intersection-exe
```

## REPL

The REPL prompt is `>`, type `:q` to quit or input any expression in the source language to check its result.

```
> 2
Source typing result
int

Target expression
2

Evaluation result
2
```

## Syntax of the source langauge

* Primitive type: `int`, `bool`
* Top type/value: `T : T`
* Type annotation: `2 : int`
* Lambda: `(\x . x+1) : int->int`
* Merge: `true ,, (\x.x) : int->int`
* Intersection type: `bool & (int->int)`
* If: `if x == 0 then true else false`
* Disjoint (universal) quantification: `\/A*int. A -> A`
* Type-level lambda: `/\A * int . (\x . x) : A -> A`

## Examples

```
> (/\A . /\B * A . (\ x . x) : A & B -> B) @ int @ bool (1,,true)

Source typing result
bool

Target expression
((((ΛA . (ΛB . (λx . ((λx1 . ((λx2 . x2) x1.2)) x))))@int)@bool) ((λx . (((λx1 . ((λx2 . x2) x1.1)) x),  ((λx1 . ((λx2 . x2) x1.2)) x))) (1,  True)))

Evaluation result
True
```
