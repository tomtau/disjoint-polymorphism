# Disjoint Polymorphism

[![wercker status](https://app.wercker.com/status/f8efc6b9552b883a408799e75a4b87f3/s/master "wercker status")](https://app.wercker.com/project/byKey/f8efc6b9552b883a408799e75a4b87f3)

A delegation-based, statically typed language that supports dynamic composable
traits, disjoint polymorphism, intersection types and a merge operator. For the
underlying theory, please refer
to [Disjoint Polymorphism](http://i.cs.hku.hk/~bruno/papers/ESOP2017.pdf).

## Build and Run

This project can be built with `cabal` or `stack`.

* cabal
```
cabal sandbox init
cabal install --only-dependencies
cabal build
cabal exec disjoint-polymorphism-exe
```

* stack
```
stack build
stack exec disjoint-polymorphism-exe
```

## REPL

The REPL prompt is `>`:
- type `:q` to quit or input any expression in the source language to check its
result
- type `:load` to load a file

```
> 2
Typing result
: Double

Evaluation result
=> 2.0
```

## Syntax of the source langauge

* Primitive type: `Double`, `Bool`, `String`
* Top type/value: `() : T`
* Type annotation: `2 : Double`
* Merge: `true ,, 3`
* Intersection type: `Bool & (Double -> Double)`
* If: `if x == 0 then true else false`
* λ term: `(\x -> x+1) : Double -> Double`
* Λ term: `/\ (A * Double) . (\x -> x) : A -> A`
* Disjoint (universal) quantification: `forall (A*Int) . A -> A`
* Term declaration: `def id A (x : A) = x;`
* Type declaration: `type Person = {name : String, male : Bool};`

## Examples

See [examples/](./examples/)
