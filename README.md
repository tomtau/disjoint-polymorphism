# Disjoint Intersection Types

## Run

```
stack build && stack exec disjoint-intersection-exe
```

## Examples

```
let x = (3,,true) in let succ = (\x.x+1):int->int in let not = (\x.if x then false else true):bool->bool in (succ x ,, not x)
# (4, False)

let succ = (\x.x+1):int->int in ((1,,true) ,, (2,,false))
# (int & bool) and (int & bool) are not disjoint


let succ = (\x.x+1):int->int in let not = (\x.if x then false else true):bool->bool in ((succ,,not):int->int) (3,,true)
# 4
```
