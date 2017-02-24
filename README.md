# Disjoint Polymorphism

[![wercker status](https://app.wercker.com/status/16b783eb6c0bc3585645ffba0be57999/s/master "wercker status")](https://app.wercker.com/project/byKey/16b783eb6c0bc3585645ffba0be57999)

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

The REPL prompt is `>`:
- type `:q` to quit or input any expression in the source language to check its
result
- type `:load` to load a file

```
> 2
Typing result
: Int

Evaluation result
=> 2
```

## Syntax of the source langauge

* Primitive type: `Int`, `Bool`, `String`
* Top type/value: `() : T`
* Type annotation: `2 : Int`
* Merge: `true ,, 3`
* Intersection type: `Bool & (Int -> Int)`
* If: `if x == 0 then true else false`
* λ term: `(\x -> x+1) : Int -> Int`
* Λ term: `/\A * Int . (\x -> x) : A -> A`
* Disjoint (universal) quantification: `forall A*Int. A -> A`
* Term declaration: `def id A (x : A) : A = x; id Int 3;`
* Type declaration: `type Person = {name : String, male : Bool};`

## Examples

See [examples/](./examples/)
