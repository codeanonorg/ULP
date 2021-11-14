# ULP

**U**n **L**angage de **P**rogrammation (ULP) est un
langage jouet inspiré de [APL](https://fr.wikipedia.org/wiki/APL_(langage)).

## Objectifs

+ Un langage à tableau style APL
+ Fortement *point free*
+ Statiquement et fortement typé
+ Compilation native
+ Syntaxe simple (sans unicode !)

## Syntaxe

### Combinateurs de base

```
F ` flip (λf.λx.λy.f y x)
D ` generalized substitution (λf.λg.λh.λx. f (g x) (h x))
K ` constant function λx.λy.x
I ` identity function λx.x
S ` substitution (λf.λg.λh.λx. f (g x) (h x)) = {D w I}
```

### Lambdas

```
{f w} ` anonymous function of parameter w
{g w1 w2} ` anonymous function of parameters w1 w2
```

### Opérateurs de base

```
+ ` addition
- ` soustraction
* ` multiplication
: ` division
% ` modulo
```

### Tableaux

```
# ` longueurs
$ ` map
/ ` reduce
\ ` filter
M ` max
m ` min
```

### Booléens

```
= ` égalité
! ` négation
& ` et
| ` ou
< ` Plus petit
> ` Plus grand
```

```
LE <- D | < =
LE 0 [1 2 3] = [True True True]
```


```
+/#$[[1 2 3 5] [1 2 3 4]] ` calculer le nombre d'éléments dans une matrice
```

