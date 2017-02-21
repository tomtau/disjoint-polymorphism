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
Typing result
: int

Evaluation result
=> 2
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
* Term declaration: `def id A (x : A) : A = x; id @ int 3`
* Type declaration: `type Point = {isZero : bool, axis : int}; def point : Point = {isZero = true, axis = 3};`

## Examples

```
$ less examples/fst.txt
def fst A [B*A] (x : A&B) : A = x;

def snd A [B*A] (x : A&B) : B = x;

snd @ int @ bool (1,,true)
```

```
> :load examples/fst.txt
Typing result
: bool

Evaluation result
=> True
```
