

- `=` (unificación)
```
X = 3.        % X queda unificado con 3
f(X) = f(5).  % X queda unificado con 5
3 = 1+2.      % false (1+2 NO se evalúa)
```


- `==` (igualdad estricta)
```
X = 3, X == 3.     % true
X = 3, X == Y.     % false, porque X está instanciado y Y no
1+2 == 1+2.        % true (mismos términos sintácticos)
```


- `=:=` (igualdad aritmética evaluada)
```
3 =:= 1+2.     % true
X =:= 3.       % ERROR si X no está instanciado
```


- `\=` (no unificables)
```
3 \= 4.     % true
X \= 3.    % puede ser true o false según bindings
```


- `=\=` (desigualdad aritmética evaluada)
```
3 =\= 1+2.   % false
5 =\= 2+3.   % false
```


- `is` Evalúa la expresión de la derecha y la unifica con la izquierda.
```
X is 3+2.        % X = 5
3 is 1+2.        % false
```


- `sort(List, Sorted)` Ordena y elimina duplicados.
```
sort([3,1,2,1], X).   % X = [1,2,3]
```


- `msort(List, Sorted)` Ordena sin eliminar duplicados.
```
msort([3,1,2,1], X).  % X = [1,1,2,3]
```


- `between(Low, High, X)` Genera o verifica valores entre Low y High.
```
between(1,5,X).  % X = 1 ; 2 ; 3 ; 4 ; 5
```


- `list_to_set(List, Set)` Elimina duplicados preservando orden.
```
list_to_set([1,2,1,3], X).  % X = [1,2,3]
```


- `union(Set1, Set2, Union)` Asume que ambas son conjuntos (sin duplicados).
```
union([1,2],[2,3], U).  % U = [1,2,3]
```


- `intersection(Set1, Set2, Intersection)`
```
intersection([1,2,3],[2,3,4], X).  % X = [2,3]
```


- `subset(Sub, Set)` True si todos los elementos de Sub están en Set.


- `subtract(Set, Remove, Result)` Quita de la primera lista todos los elementos presentes en la segunda.
```
subtract([1,2,3,4],[2,4], X).  % X = [1,3]
```


- `select(Elem, List, Rest)` Extrae un elemento, devolviendo el resto.
```
select(X, [1,2,3], R).  
% X=1,R=[2,3] ; X=2,R=[1,3] ; X=3,R=[1,2]
```


- `delete(List, Elem, Result)` Elimina todas las ocurrencias.
```
delete([1,2,1,3], 1, X).   % X = [2,3]
```


- `numlist(Low, High, List)` Genera lista consecutiva.
```
numlist(3,7,L).   % L = [3,4,5,6,7]
```