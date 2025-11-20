# Ejercicio 12

- Realizarse de manera lineal (utilizando en cada paso el resolvente obtenido en el paso anterior).
- Utilizar únicamente cláusulas de Horn.
- ~~Utilizar cada cláusula a lo sumo una vez.~~
- Empezar por una cláusula objetivo (sin literales positivos)
- Empezar por una cláusula que provenga de la negación de lo que se quiere demostrar.
- ~~Recorrer el espacio de búsqueda de arriba hacia abajo y de izquierda a derecha.~~
- Utilizar la regla de resolución binaria en lugar de la general.

# Ejercicio 13

Partimos de la base que para ser inteligente hay que ser un robot capaz de resolver un problema logico.
Los problemas logicos son los Y que cumplen que PL(Y) o que Pr(Y), ya que los problemas de la practica son logicos.
Por otro lado, alan es un robot japones.

```
R(alan)
J(alan)
Pr(ejercicio13) # Como existe al menos un ejercicio en esta practica, decimos que por ejemplo, el ejercicio 13 es de esta practica
Pr(Y) ⟹ PL(Y)
(R(X) ∧ J(X) ∧ Pr(Y)) ⟹ (Res(X, Y))
(R(X) ∧ PL(Y) ∧ Res(X, Y)) ⟹ I(X)
```

Quiero encontrar un X tal que I(X). Utilizamos resolucion SLD para esto.
Entonces, dadas las hipotesis

```
R(alan)
J(alan)
Pr(ejercicio13)
Pr(Y) ⟹ PL(Y)
(R(X) ∧ J(X) ∧ Pr(Y)) ⟹ (Res(X, Y))
(R(X) ∧ PL(Y) ∧ Res(X, Y)) ⟹ I(X)
```

Planteamos las clausulas:

- {R(alan)}
- {J(alan)}
- {Pr(ejercicio13)}
- {¬Pr(Y), PL(Y)}
- {¬R(X), ¬J(X), ¬Pr(Y), Res(X, Y)}
- {¬R(X), ¬PL(Y), ¬Res(X, Y), I(X)}

Luego, como quiero encontrar un X tal que I(X), introduzco ¬I(X) (nuestra clausula objetivo).

Resuelvo mediante SLD. Para esto elijo una clausula definicion y una objetivo.

1) {R(alan)}                            # Definicion
2) {J(alan)}                            # Definicion
3) {Pr(ejercicio13)}                    # Definicion
4) {¬Pr(Y), PL(Y)}                      # Definicion
5) {¬R(X), ¬J(X), ¬Pr(Y), Res(X, Y)}    # Definicion
6) {¬R(X), ¬PL(Y), ¬Res(X, Y), I(X)}    # Definicion
7) {¬I(X)}                              # Objetivo

- 7) {¬I(X)}

  - (7) y (6) = mgu(¬I(X) $=^?$ I(X)) =
    S_8 = {}
    8 = {¬R(X), ¬PL(Y), ¬Res(X, Y)} (Nuevo objetivo)
  - (8) y (5) = mgu({¬Res(X, Y) $=^?$ ¬Res(X, Y)}) =
    S_9 = {}
    9 = {¬R(X), ¬PL(Y), ¬R(X), ¬J(X), ¬Pr(Y)} = {¬R(X), ¬PL(Y), ¬J(X), ¬Pr(Y)}  (Nuevo objetivo)
  - (9) y (4) = mgu({PL(Y) $=^?$ PL(Y)}) =
    S_10 = {}
    10 = {¬R(X), ¬J(X), ¬Pr(Y)}
  - (10) y (3) = mgu(Pr(Y) $=^?$ Pr(ejercicio13)) ?
    S_11 = {Y := ejercicio13}
    11 = {¬R(X), ¬J(X)}
  - (11) y (1) = mgu(R(X) $=^?$ R(alan)) =
    S_12 = {X := alan}
    12 = {¬J(alan)}
  - (12) y (2) = mgu(J(alan) $=^?$ J(alan)) =
    S_13 = {}
    13 = {} 
    Llegamos a {}, por lo tanto encontramos una refutacion a ¬I(X). 
    En particular, X := alan.

# Ejercicio 14

Nuestra base de conocimiento es:
{¬suma(X, Y, Z), suma(X, suc(Y), suc(Z))}
{suma(X, cero, X)}
{¬suma(X, X, Y), par(Y)}

Quiero probar par(suc(suc(cero))). Por lo tanto, lo refuto: ¬par(suc(suc(cero)))

Planteo mis clausulas:
{
    (1) {¬suma(X, Y, Z), suma(X, suc(Y), suc(Z))},
    (2) {suma(X, cero, X)},
    (3) {¬suma(X, X, Y), par(Y)},
    (4) {¬par(suc(suc(cero)))}
}

Podemos usar SLD, ya que todas las clausulas son de Horn.

- De (4) y (3) tengo : MGU({par(suc(suc(cero))), par(Y)}) =

  - S_5 = {Y := suc(suc(cero))}
  - (5) {¬suma(X, X, suc(suc(cero)))}
- De (5) y (1) tengo : MGU({suma(X, X, suc(suc(cero))), suma(X, suc(Y), suc(Z))}) =

  - S_6 = {X := suc(Y), Z := suc(cero)}
  - (6) = {¬suma(suc(Y), Y, suc(cero))}
- De (6) y (2) tengo : MGU({
  suma(X, cero, X),
  suma(suc(Y), Y, suc(cero))
  }) =

  - S_7 = {X := suc(Y), Y := cero, X := suc(cero)} := {Y := cero, X := suc(cero)}
  - (7) = {}

# Ejercicio 16

Teorema del bebedor: (∃X.enBar(X)) ⟹ ∃Y.(enBar(Y) ∧ (bebe(Y) ⟹ ∀Z.(enBar(Z) ⟹ bebe(Z))))

Quiero demostrarlo, entonces voy a refutarlo:
¬[(∃X.enBar(X)) ⟹ ∃Y.(enBar(Y) ∧ (bebe(Y) ⟹ ∀Z.(enBar(Z) ⟹ bebe(Z))))]

1) Nos deshacemos del ⟹
   ¬[(¬∃X.enBar(X)) ∨ ∃Y.(enBar(Y) ∧ (¬bebe(Y) ∨ ∀Z.(¬enBar(Z) ∨ bebe(Z))))]
2) Empujamos el ¬
   [(¬¬∃X.enBar(X)) ∧ ¬∃Y.(enBar(Y) ∧ (¬bebe(Y) ∨ ∀Z.(¬enBar(Z) ∨ bebe(Z))))]
   [(∃X.enBar(X)) ∧ ∀Y.¬(enBar(Y) ∧ (¬bebe(Y) ∨ ∀Z.(¬enBar(Z) ∨ bebe(Z))))]
   [(∃X.enBar(X)) ∧ ∀Y.(¬enBar(Y) ∨ ¬(¬bebe(Y) ∨ ∀Z.(¬enBar(Z) ∨ bebe(Z))))]
   [(∃X.enBar(X)) ∧ ∀Y.(¬enBar(Y) ∨ (¬¬bebe(Y) ∧ ¬∀Z.(¬enBar(Z) ∨ bebe(Z))))]
   [(∃X.enBar(X)) ∧ ∀Y.(¬enBar(Y) ∨ (¬¬bebe(Y) ∧ ∃Z.¬(¬enBar(Z) ∨ bebe(Z))))]
   [(∃X.enBar(X)) ∧ ∀Y.(¬enBar(Y) ∨ (¬¬bebe(Y) ∧ ∃Z.(¬¬enBar(Z) ∧ ¬bebe(Z))))]
   [(∃X.enBar(X)) ∧ ∀Y.(¬enBar(Y) ∨ (bebe(Y) ∧ ∃Z.(enBar(Z) ∧ ¬bebe(Z))))]
3) Extramos ∀/∃
   ∃X.∀Y.∃Z.[(enBar(X)) ∧ (¬enBar(Y) ∨ (bebe(Y) ∧ (enBar(Z) ∧ ¬bebe(Z))))]
4) Nos deshacemos de ∃
   ∀Y.∃Z.[(enBar(c_X)) ∧ (¬enBar(Y) ∨ (bebe(Y) ∧ (enBar(Z) ∧ ¬bebe(Z))))]
   ∀Y.[(enBar(c_X)) ∧ (¬enBar(Y) ∨ (bebe(Y) ∧ (enBar($f_Z(Y)$) ∧ ¬bebe($f_Z(Y)$))))]
5) Pasamos a CNF
   ∀Y.[(enBar(c_X)) ∧ (¬enBar(Y) ∨ (bebe(Y) ∧ (enBar($f_Z(Y)$) ∧ ¬bebe($f_Z(Y)$))))]
   A = enBar(c_X)
   B = ¬enBar(Y)
   C = bebe(Y)
   D = enBar($f_Z(Y)$)
   E = ¬bebe($f_Z(Y)$)
   ∀Y.[(A) ∧ (B ∨ (C ∧ (D ∧ E)))]
   ∀Y.[A ∧ (B ∨ (C ∧ D ∧ E))]
   ∀Y.[A ∧ ((B ∨ C) ∧ (B ∨ D) ∧ (B ∨ E))]
   ∀Y.[A ∧ (B ∨ C) ∧ (B ∨ D) ∧ (B ∨ E)]
   ∀Y.[enBar(c_X) ∧ (¬enBar(Y) ∨ bebe(Y)) ∧ (¬enBar(Y) ∨ enBar($f_Z(Y)$)) ∧ (¬enBar(Y) ∨ ¬bebe($f_Z(Y)$))]

Por lo tanto nos quedan las siguientes clausulas:
{
   (1) {enBar(c_X)},
   (2) {¬enBar(Y), bebe(Y)},
   (3) {¬enBar(Y), enBar($f_Z(Y)$)},
   (4) {¬enBar(Y), ¬bebe($f_Z(Y)$)}
}

(1) Hay una persona c_X que está en el bar
(2) Para todo Y, si Y está en el bar entonces está bebiendo
(3) Para todo Y, si Y está en el bar entonces hay un $f_Z(Y)$ que tambien está en el bar
(4) Para todo Y, si Y está en el bar, entonces hay un $f_Z(Y)$ que no bebe

Intentemos refutar nuestro conjunto de clausulas.

2 y 4 -> 5
5 y 3 -> 6
6 y 1 -> {}

# Ejercicio 18

{¬Progenitor(X, Y), Descendiente(Y, X)}
{¬Descendiente(X, Y), ¬Descendiente(Y, Z), Descendiente(X, Z)}
{¬Abuelo(X, Y), Progenitor(X, medio(X, Y))}
{¬Abuelo(X, Y), Progenitor(medio(X, Y), Y)}

Queremos demostrar que ∀X.∀Y.(Abuelo(X, Y) ⇒ Descendiente(Y, X)). Como queremos demostrarlo, refutémoslo.

¬∀X.∀Y.(Abuelo(X, Y) ⇒ Descendiente(Y, X)) =
¬∀X.∀Y.(¬Abuelo(X, Y) ∨ Descendiente(Y, X)) =
∃X.¬∀Y.(¬Abuelo(X, Y) ∨ Descendiente(Y, X)) =
∃X.∃Y.¬(¬Abuelo(X, Y) ∨ Descendiente(Y, X)) =
∃X.∃Y.(¬¬Abuelo(X, Y) ∧ ¬Descendiente(Y, X)) =
∃X.∃Y.(Abuelo(X, Y) ∧ ¬Descendiente(Y, X)) =
Abuelo(c_X, c_Y) ∧ ¬Descendiente(c_Y, c_X)

Es decir, ahora nuestras clausulas son:
(1) {¬Progenitor(X, Y), Descendiente(Y, X)}
(2) {¬Descendiente(X, Y), ¬Descendiente(Y, Z), Descendiente(X, Z)}
(3) {¬Abuelo(X, Y), Progenitor(X, medio(X, Y))}
(4) {¬Abuelo(X, Y), Progenitor(medio(X, Y), Y)}
(5) {Abuelo(c_X, c_Y)}
(6) {¬Descendiente(c_Y, c_X)}

Pensemos los significados de las clausulas, y armemos un plan para encontrar una refutación.

(1) Es una implicación. Si Y es progenitor de X entonces X es descendiente de Y.
(2) Es una implicación. Si Y es descendiente de X y Z es descendiente de Y, entonces Z es descendiente de X.
(3) Es una implicación. Si Y es el abuelo de X, entonces el que está en el medio de X e Y es el progenitor de X. Es decir, si Y es tu abuelo, entonces el que está en el medio de vos y tu abuelo (tu padre), es tu progenitor.
(4) Es una implicación. Si Y es abuelo de X, entonces Y es progenitor del que está en el medio de X e Y. Es decir, si Y es tu abuelo, entonces tu abuelo es progenitor del que está en el medio tuyo y tu abuelo (tu padre).
Luego en cuanto a (5) y (6) podemos decir que están definidas con constantes, es decir, c_Y es el abuelo de c_X, y c_X no es descendiente de c_Y. Notemos inmediatamente que esto no tiene sentido. Claramente si c_Y es abuelo de c_X, entonces c_X es descendiente de c_Y.

Utilizando resolución, encontremos una refutación, es decir, en algún paso queremos llegar a una clausula vacía. Por (1), si Y es progenitor de X, entonces X es descendiente de Y, utilizando (5), digamos que c_Y es abuelo de c_X, o sea, que c_X es descendiente de c_Y, y ahí usamos (6).

De (5) con (3) tenemos : MGU({Abuelo(c_X, c_Y), Abuelo(X, Y)})
    - S_7 = {X := c_X, Y := c_Y}
    - (7) {Progenitor(c_X, medio(c_X, c_Y))} (Mi progenitor es mi padre)

De (7) y (1) tenemos : MGU({Progenitor(c_X, medio(c_X, c_Y)), Progenitor(X, Y)})
    - S_8 = {X := c_X, Y := medio(c_X, c_Y)}
    - (8) = {Descendiente(medio(c_X, c_Y), c_X)} (Vos sos descendiente de tu padre)

De (5) y (4) tenemos : MGU({Abuelo(c_X, c_Y), Abuelo(X, Y)})
    - S_9 = {X := c_X, Y := c_Y}
    - (9) {Progenitor(medio(c_X, c_Y), c_Y)} (Tu abuelo es progenitor de tu padre)

De (9) y (1) tenemos : MGU({Progenitor(medio(c_X, c_Y), c_Y), Progenitor(X, Y)})
    - S_10 = {X := medio(c_X, c_Y), Y := c_Y}
    - (10) {Descendiente(c_Y, medio(c_X, c_Y))} (Tu padre es descendiente de tu abuelo)

De (10) y (2) tenemos : MGU({Descendiente(c_Y, medio(c_X, c_Y)), Descendiente(X, Y)})
    - S_11 = {X := c_Y, Y := medio(c_X, c_Y)}
    - (11) {¬Descendiente(medio(c_X, c_Y), Z), Descendiente(c_Y, Z)} (Si alguien es descendiente de tu padre entonces ese alguien es descendiente de su abuelo)

De (11) con (8) tenemos MGU({Descendiente(medio(c_X, c_Y), Z), Descendiente(medio(c_X, c_Y), c_X)})
    - S_12 = {Z := c_X}
    - (12) {Descendiente(c_Y, c_X)}

De (12) con (6) tenemos MGU({Descendiente(c_Y, c_X), Descendiente(c_Y, c_X)})
    - S_13 = {}
    - (13) {}

# Ejercicio 19

# Ejercicio 20

## Inciso A

```prolog
natural(cero). 
natural(suc(X)) :- 
    natural(X). 

mayorOIgual(suc(X), Y) :- 
    mayorOIgual(X, Y).
mayorOIgual(X, X) :- 
    natural(X).
```

Veamos que pasa al realizar la consulta `?- mayorOIgual(suc(suc(N)), suc(cero))`.
Entramos por la primera definición de mayorOIgual, y en particular la unificación es X := suc(N), Y := suc(cero). Realizamos la llamada recursiva mayorOIgual(suc(N), suc(cero)). Volvemos a entrar por la primera definición de mayorOIgual y la unificación es X := N e Y := suc(cero). Realizamos la llamada recursiva mayorOIgual(N, suc(cero)). Pero como N no está instanciada, nuevamente unifica con la primera definición. Y así sucede indefinidamente. En conclusión, "se cuelga".

## Inciso B

Primero pasamos la base de conocimientos a forma clausal.

Escribamos nuestra base de conocimientos como fórmulas lógicas.

(1) natural(cero)
(2) ∀X.(natural(X) ⟹ natural(suc(X)))
(3) ∀X.∀Y.(mayorOIgual(X, Y) ⟹ mayorOIgual(suc(X), Y))
(4) ∀X.(natural(X) ⟹ mayorOIgual(X, X))

Si nos deshacemos de las implicaciones, nos queda

(1) natural(cero)
(2) ∀X.(¬natural(X) ∨ natural(suc(X)))
(3) ∀X.∀Y.(¬mayorOIgual(X, Y) ∨ mayorOIgual(suc(X), Y))
(4) ∀X.(¬natural(X) ∨ mayorOIgual(X, X))

No tenemos ¬ por empujar ni ∃ para eliminar. Por lo tanto nuestras cláusulas son:

(1) {natural(cero)}
(2) {¬natural(X), natural(suc(X))}
(3) {¬mayorOIgual(X, Y), mayorOIgual(suc(X), Y)}
(4) {¬natural(X), mayorOIgual(X, X)}

Como queremos probar la validez de la consulta `mayorOIgual(suc(suc(N)), suc(cero))`, enconces lo refutamos. En particular, la fórmula lógica de `mayorOIgual(suc(suc(N)), suc(cero))` es:

- ∃N.(mayorOIgual(suc(suc(N)), suc(cero)))
  Si lo refutamos obtenemos:
- ¬∃N.(mayorOIgual(suc(suc(N)), suc(cero))) = ∀N.¬(mayorOIgual(suc(suc(N)), suc(cero)))

Por tanto el conjunto de cláusulas nos queda:

(1) {natural(cero)}
(2) {¬natural(X), natural(suc(X))}
(3) {¬mayorOIgual(X, Y), mayorOIgual(suc(X), Y)}
(4) {¬natural(X), mayorOIgual(X, X)}
(5) {¬mayorOIgual(suc(suc(N)), suc(cero))}

Pensemos la consulta de forma logica. ¿Quién es mayor o igual a suc(cero)? Bueno, suc(cero) en adelante. Por ejemplo, si N fuese cero, entonces encontraríamos una solución. Entendiendo esto, resolvamos.

De (5) y (3) tengo : MGU({mayorOIgual(suc(suc(N)), suc(cero)), mayorOIgual(suc(X), Y)})
    - S_6 = {X := suc(N), Y := suc(cero)}
    - (6) {¬mayorOIgual(suc(N), suc(cero))}

De (6) y (4) tengo : MGU({mayorOIgual(suc(N), suc(cero)), mayorOIgual(X, X)})
    - S_7 = {X := suc(cero)}
    - (7) = {¬natural(suc(cero))}

De (7) y (2) tengo : MGU({natural(suc(cero)), natural(suc(X))})
    - S_8 = {X := cero}
    - (8) {¬natural(cero)}

De (8) y (1) tengo : MGU({natural(cero), natural(cero)})
    - S_9 = {}
    - (9) = {}

## Inciso C

Notemos que la resolución fue SLD, la razón por la cual podemos afirmar esto, es porque:

- Se realizó de manera lineal.
- Se utilizaron únicamente cláusulas de Horn.
- Empezamos por una cláusula objetivo que además provenía de la negación de lo que queríamos probar.
- Utilizamos la regla de resolución binaria.

# Ejercicios de Parcial

## [2024 2C (Recuperatorio)](https://www.cubawiki.com.ar/images/4/4b/PLP-2C2024-2r.pdf)

```prolog
member(X, [X | Y]).
member(X, [Y | T]) :- 
    member(X, T)
.

seFormaCon([], A).
seFormaCon([A | AS], B) :- 
    member(A, B), 
    seFormaCon(AS, B)
.
```

### Inciso A

Por empezar, seFormaCon, pide que todos los elementos de la lista del primer parámetro, estén presentes en la lista del segundo parámetro.

Veamos qué sucede al realizar la consulta `seFormaCon(A, [a, b]), member(a, A), member(b, A)`. A fines lógicos, lo que estamos pidiendo son las listas A que se forman con los elementos de la lista [a, b], tal que además a pertenece a la lista A encontrada, y b tambén. En particular (nuevamente a fines lógicos), estos dos últimos predicados son redundantes.

Prolog comienza unificando con la primera definición de seFormaCon, por lo tanto tenemos una primera solución, la lista vacía.
Luego unifica con la segunda definición de seFormaConen 3 con {X := a} es lo mismo poner. B := [a, b]. Prolog busca un A usando member. Veamos qué pasa. Member unifica en la primera definición, e instancia en X el primer elemento de [a, b], por lo tanto A := a. Luego Prolog realiza la recursión, en particular seFormaCon(AS, [a, b]).
Nuevamente Prolog tendrá el mismo comportamiento que tuvo anteriormente, AS := [A' | AS'], y nuevamente llamará a member e instanciará en A', a. Hará la recursión nuevamente, y así indefinidamente. En resumen, nunca sale del primer predicado, y en particular se queda generando la lista infinita de a's.

### Inciso B

Expresemos la base de conocimientos y la consulta anterior, como fórmulas lógicas.

∀X.∀Y.member(X, [X | Y])
∀X.∀T.∀Y.(member(X, T) ⟹ member(X, [Y | T]))
∀A.seFormaCon([], A)
∀A.∀B.∀AS.(member(A, B) ∧ seFormaCon(AS, B)) ⟹ seFormaCon([A | AS], B)

∃A.(seFormaCon(A, [a, b]) ∧ member(a, A) ∧ member(b, A))

### Inciso C

Queremos encontrar una solución a la consulta utilizando resolución. Trabajemos primero con la base de conocimientos.

- Nos deshacemos del ⟹
  ∀X.∀Y.member(X, [X | Y])
  ∀X.∀T.∀Y.(¬member(X, T) ∨ member(X, [Y | T]))
  ∀A.seFormaCon([], A)
  ∀A.∀B.∀AS.(¬(member(A, B) ∧ seFormaCon(AS, B)) ∨ seFormaCon([A | AS], B))

∀X.∀Y.member(X, [X | Y])
∀X.∀T.∀Y.(¬member(X, T) ∨ member(X, [Y | T]))
∀A.seFormaCon([], A)
∀A.∀B.∀AS.((¬member(A, B) ∨ ¬seFormaCon(AS, B)) ∨ seFormaCon([A | AS], B))

Ahora sí, pasamos a forma clausal. Tenemos:

(1) {member(X, [X | Y])}
(2) {¬member(X, T), member(X, [Y | T])}
(3) {seFormaCon([], A)}
(4) {¬member(A, B), ¬seFormaCon(AS, B), seFormaCon([A | AS], B)}

Ahora, queremos encontrar una solución a la consulta. Refutamos la misma y la agregamos a el conjunto de cláusulas.

- Lo refutamos
  ¬(∃A.(seFormaCon(A, [a, b]) ∧ member(a, A) ∧ member(b, A)))
- Empujamos el ¬
  (¬∃A.(seFormaCon(A, [a, b]) ∧ member(a, A) ∧ member(b, A)))
  (∀A.¬(seFormaCon(A, [a, b]) ∧ member(a, A) ∧ member(b, A)))
  ∀A.(¬seFormaCon(A, [a, b]) ∨ ¬member(a, A) ∨ ¬member(b, A))

En particular, obtenemos la siguiente cláusula:
(5) {¬seFormaCon(A, [a, b]), ¬member(a, A), ¬member(b, A)}

Agregamos la misma a nuestro conjunto de cláusulas. Nos queda lo siguiente:

(1) {member(X, [X | Y])}
(2) {¬member(X, T), member(X, [Y | T])}
(3) {seFormaCon([], A)}
(4) {¬member(A, B), ¬seFormaCon(AS, B), seFormaCon([A | AS], B)}
(5) {¬seFormaCon(A, [a, b]), ¬member(a, A), ¬member(b, A)}

Notemos lo siguiente: son todas cláusulas de Horn.

Sabemos también que la lista [a, b] o la [b, a] cumple lógicamente con la base de conocimientos. Intentemos construir dicha lista.

De (5) y (1) tengo : MGU({member(X, [X | Y]), member(a, A)})
    - S_6 = {X := a, A := [a | Y]}
    - (6) = {¬seFormaCon([a | Y], [a, b]), ¬member(b, [a | Y])}

De (6) y (4) tengo : MGU({seFormaCon([a | Y], [a, b]), seFormaCon([A | AS], B)})
    - S_7 = {A := a, Y := AS, B := [a, b]}
    - (7) = {¬member(b, [a | AS]), ¬member(a, [a, b]), ¬seFormaCon(AS, [a, b])}

De (7) y (1) tengo : MGU({member(a, [a, b]), member(X, [X | Y])})
    - S_8 = {X := a, Y := [b]}
    - (8) = {¬member(b, [a | AS]), ¬seFormaCon(AS, [a, b])}

De (8) y (2) tengo : MGU({member(b, [a | AS]), member(X, [Y | T])})
    - S_9 = {X := b, Y := a, AS := T}
    - (9) = {¬seFormaCon(T, [a, b]), ¬member(b, T)}

De (9) y (1) tengo : MGU({member(b, T), member(X, [X | Y])})
    - S_10 = {X := b, T := [b | Y]}
    - (10) = {¬seFormaCon([b | Y], [a, b])}

De (10) y (4) tengo : MGU({seFormaCon([b | Y], [a, b]), seFormaCon([A | AS], B)})
    - S_11 = {A := b, AS := Y, B := [a, b]}
    - (11) = {¬member(b, [a, b]), ¬seFormaCon(Y, [a, b])}

De (11) y (3) tengo : MGU({seFormaCon(Y, [a, b]), seFormaCon([], A)})
    - S_12 = {Y := [], A := [a, b]}
    - (12) = {¬member(b, [a, b])}

De (12) y (2) tengo : MGU({member(b, [a, b]), member(X, [Y | T])})
    - S_13 = {X := b, Y := a, T := [b]}
    - (13) = {¬member(b, [b])}

De (13) y (1) tengo : MGU({member(X, [X | Y]), member(b, [b])})
    - S_14 = {X := b, Y := []}
    - (14) = {}

## 2025 1C

### Inciso A

```prolog
member(X, [X | _]).
member(X, [Y | XS]) :- member(X, XS).

esSublista(_, []).
esSublista(L, [X | XS]) :- member(X, L), esSublista(L, XS).
```

Se pide explicar qué sucede al realizar la consulta `-? esSublista([a, b], R), member(b, R)`. Primero Prolog usa la primera definición de esSublista e instancia la lista vacía en R, pero no se cumple que member(b, []), entonces no es imprime dicha solución candidata. Luego entramos en la segunda definición de esSublista. Tenemos `member(X, [a, b])`, la primera solución que encuentra member es a, luego hacemos la recursión con `esSublista([a, b], XS)`. Vemos que pasará exactamente lo mismo que recién, se encontrara un X miembro de la lista [a, b], en particular a, y se hará nuevamente la recursión. Por ende estaremos generando de manera infinita, la lista que contiene elementos a.

### Inciso B

Expresemos la base de conocimientos y la consulta como fórmulas lógicas.

∀X.∀T.member(X, [X | T])
∀X.∀XS.∀Y.(member(X, XS) ⟹ member(X, [Y | XS]))
∀L.esSublista(L, [])
∀X.∀L.∀XS.((member(X, L) ∧ esSublista(L, XS)) ⟹ esSublista(L, [X | XS]))
∃R.(esSublista([a, b], R) ∧ member(b, R))

### Inciso C

Pasemos todo a forma clausal. En particular, como queremos encontrar una solución a la consulta. La refutamos. Es decir, queremos pasar a forma clausal las siguientes fórmulas.

∀X.∀T.member(X, [X | T])
∀X.∀XS.∀Y.(member(X, XS) ⟹ member(X, [Y | XS]))
∀L.esSublista(L, [])
∀X.∀L.∀XS.((member(X, L) ∧ esSublista(L, XS)) ⟹ esSublista(L, [X | XS]))
∀R.(¬esSublista([a, b], R) ∨ ¬member(b, R))

1) Nos deshacemos de las implicaciones
∀X.∀T.member(X, [X | T])
∀X.∀XS.∀Y.(¬member(X, XS) ∨ member(X, [Y | XS]))
∀L.esSublista(L, [])
∀X.∀L.∀XS.(¬(member(X, L) ∧ esSublista(L, XS)) ∨ esSublista(L, [X | XS]))
∀R.(¬esSublista([a, b], R) ∨ ¬member(b, R))

2) Empujamos el ¬
∀X.∀T.member(X, [X | T])
∀X.∀XS.∀Y.(¬member(X, XS) ∨ member(X, [Y | XS]))
∀L.esSublista(L, [])
∀X.∀L.∀XS.(¬member(X, L) ∨ ¬esSublista(L, XS) ∨ esSublista(L, [X | XS]))
∀R.(¬esSublista([a, b], R) ∨ ¬member(b, R))

De estas fórmulas, obtenemos la siguiente forma clausal:

(1) {member(X, [X | T])}
(2) {¬member(X, XS), member(X, [Y | XS])}
(3) {esSublista(L, [])}
(4) {¬member(X, L), ¬esSublista(L, XS), esSublista(L, [X | XS])}
(5) {¬esSublista([a, b], R), ¬member(b, R)}

Utilizando resolución, encontremos una solución.


## [2025 1C (Recuperatorio)](https://www.cubawiki.com.ar/images/0/03/PLP-1C2025-2r.pdf)

### Inciso A

Pasamos a forma clausal:

- Definición de cota superior (I)
∀F.∀X.(cota(F, X) ⟹ ∀N.ev(F, N) ≤ X)
∀F.∀X.(¬cota(F, X) ∨ ∀N.ev(F, N) ≤ X)
∀F.∀X.∀N.(¬cota(F, X) ∨ ev(F, N) ≤ X)

Por lo tanto la cláusula queda: {¬cota(F, X), ev(F, N) ≤ X} 


- Definición de cota superior (II)
∀F.∀X.(∀N.ev(F, N) ≤ X ⟹ cota(F, X))
∀F.∀X.(¬∀N.ev(F, N) ≤ X ∨ cota(F, X))
∀F.∀X.(∃N.¬ev(F, N) ≤ X ∨ cota(F, X))
∀F.∀X.(¬ev(F, n(F, X)) ≤ X ∨ cota(F, X))

Por lo tanto la cláusula queda: {¬ev(F, n(F, X)) ≤ X, cota(F, X)}


- El supremo de una función es una cota superior, y es la más chica de las cotas superiores.
∀F.(cota(F, sup(F)) ∧ ∀Y.(cota(F, Y) ⟹ sup(F) ≤ Y))
∀F.(cota(F, sup(F)) ∧ ∀Y.(¬cota(F, Y) ∨ sup(F) ≤ Y))
∀F.∀Y.(cota(F, sup(F)) ∧ (¬cota(F, Y) ∨ sup(F) ≤ Y))
∀F.∀Y.(cota(F, sup(F)) ∧ (¬cota(F, Y) ∨ sup(F) ≤ Y))

Por lo tanto extraemos dos cláusulas: {cota(F, sup(F))} y {¬cota(F, Y), sup(F) ≤ Y}


- Transitividad de la relación menor o igual
∀X.∀Y.∀Z.((X ≤ Y ∧ Y ≤ Z) ⟹ X ≤ Z)
∀X.∀Y.∀Z.(¬(X ≤ Y ∧ Y ≤ Z) ∨ X ≤ Z)
∀X.∀Y.∀Z.((¬(X ≤ Y) ∨ ¬(Y ≤ Z)) ∨ X ≤ Z)
∀X.∀Y.∀Z.(¬(X ≤ Y) ∨ ¬(Y ≤ Z) ∨ X ≤ Z)

Por lo tato la cláusula queda: {¬(X ≤ Y), ¬(Y ≤ Z), X ≤ Z}


### Inciso B

Queremos demostrar, usanto resolución, que si una función es más chica que otra en todo punto, el supremo también es más chico

∀F.∀G.((∀N.ev(F, N) ≤ ev(G, N)) ⟹ sup(F) ≤ sup(G))

Como queremos demostrarlo, entonces lo refutamos, si llegamos a que (con nuestra base de conocimientos) la refutación es insatisfactible, luego queda probado que lo que queremos demostrar es válido.

¬(∀F.∀G.((∀N.ev(F, N) ≤ ev(G, N)) ⟹ sup(F) ≤ sup(G)))
¬∀F.∀G.((∀N.ev(F, N) ≤ ev(G, N)) ⟹ sup(F) ≤ sup(G))
∃F.¬∀G.((∀N.ev(F, N) ≤ ev(G, N)) ⟹ sup(F) ≤ sup(G))
∃F.∃G.¬((∀N.ev(F, N) ≤ ev(G, N)) ⟹ sup(F) ≤ sup(G))
∃F.∃G.¬(¬(∀N.ev(F, N) ≤ ev(G, N)) ∨ (sup(F) ≤ sup(G)))
∃F.∃G.(¬¬(∀N.ev(F, N) ≤ ev(G, N)) ∧ ¬(sup(F) ≤ sup(G)))
∃F.∃G.∀N.((ev(F, N) ≤ ev(G, N)) ∧ ¬(sup(F) ≤ sup(G)))
∃F.∃G.∀N.((ev(f, N) ≤ ev(g, N)) ∧ ¬(sup(f) ≤ sup(g)))
∀N.((ev(f, N) ≤ ev(g, N)) ∧ ¬(sup(f) ≤ sup(g)))

Por lo tanto nos quedan las siguientes cláusulas:


(1) {¬cota(F, X), ev(F, N) ≤ X} 
(2) {¬ev(F, n(F, X)) ≤ X, cota(F, X)}
(3) {cota(F, sup(F))}
(4) {¬cota(F, Y), sup(F) ≤ Y}
(5) {¬(X ≤ Y), ¬(Y ≤ Z), X ≤ Z}
(6) {ev(f, N) ≤ ev(g, N)}
(7) {¬(sup(f) ≤ sup(g))}


De (7) y (4) tengo : MGU({sup(f) ≤ sup(g), sup(F) ≤ Y})
    - S_8 = {F := f, Y := sup(g)}
    - (8) = {¬cota(f, sup(g))}

De (8) y (2) tengo : MGU({cota(f, sup(g)), cota(F, X)})
    - S_9 = {F := f, X := sup(g)}
    - (9) = {¬ev(f, n(f, sup(g))) ≤ sup(g)}

De (9) y (5) tengo : MGU({ev(f, n(f, sup(g))) ≤ sup(g), X ≤ Z})
    - S_10 = {X := ev(f, n(f, sup(g))), Z := sup(g)}
    - (10) = {¬ev(f, n(f, sup(g))) ≤ Y, ¬(Y ≤ sup(g))}

De (10) y (6) tengo : MGU({ev(f, n(f, sup(g))) ≤ Y, ev(f, N) ≤ ev(g, N)})
    - S_11 = {N := n(f, sup(g)), Y := ev(g, n(f, sup(g)))}
    - (11) = {¬(ev(g, n(f, sup(g))) ≤ sup(g))}

De (11) y (1) tengo : MGU({ev(g, n(f, sup(g))) ≤ sup(g), ev(F, N) ≤ X})
    - S_12 = {F := g, N := n(f, sup(g)), X := sup(g)}
    - (12) = {¬cota(g, sup(g))}

De (12) y (3) tengo : MGU({cota(g, sup(g)), cota(F, sup(F))})
    - S_13 = {F := g}
    - (13) = {}