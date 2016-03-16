# Disjoint Intersection Types

## Run

```
stack build
stack exec disjoint-intersection-exe
```

## Syntax

* Primitive type: `int`, `bool`
* Top type/value: `T : T`
* Type annotation: `2 : int`
* Lambda: `(\x . x+1) : int->int`
* Pair: `(1, true)`
* Projection: `(1, true)._1`, `(1, true)._2`
* Merge: `true ,, (\x.x) : int->int`
* Intersection type: `bool & (int->int)`
* Let: `let x = 3 in (x, x + 1)`
* If: `if x == 0 then true else false`

## Examples

```
let x = (3,,true) in let succ = (\x.x+1):int->int in let not = (\x.if x then false else true):bool->bool in (succ x, not x)
# (4, False)

let succ = (\x.x+1):int->int in ((1,,true) ,, (2,,false))
# (int & bool) and (int & bool) are not disjoint

let succ = (\x.x+1):int->int in let not = (\x.if x then false else true):bool->bool in ((succ,,not):int->int) (3,,true)
# 4

((\f . f (3,,true)) : ((int & bool) -> T) -> T) ((\x.x) : int->int ,, (\x.x) : bool -> bool)
# ()
```
