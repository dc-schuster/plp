print_matrix([]) :- !.
print_matrix([Fila|Resto]) :-
    write('  '),      % dos espacios de sangría
    writeln(Fila),
    print_matrix(Resto).

show_matrix(Matriz) :-
    writeln('['),
    print_matrix(Matriz),
    writeln(']').
% -----------------------------------------------------
% -------------------- Ejercicio 3 --------------------
% -----------------------------------------------------

natural(0).
natural(suc(X)) :- natural(X).

menorOIgual(X, suc(Y)) :- menorOIgual(X, Y).
menorOIgual(X, X) :- natural(X).

% Se pide analizar qué sucede al realizar la consulta menorOIgual(0, X').
% Primero matchea con la primera regla de menorOIgual, por lo que tenemos: X := 0 y el X' que queríamos encontrar, 
% es una variable libre, en particular X' := suc(Y). Por lo que por la definición de la primera regla de menorOIgual,
% tenemos ahora menorOIgual(0, Y'), Y' debe ser, tal que "matchee" con suc(Y). Luego tenemos nuevamente una consulta
% de la misma pinta que la inicial, volveremos a matchear con la primera regla, y así indefinidamente.

% Lo que pasa acá, es que no se debe usar como primera definición de un predicado, uno recursivo. En predicados recursivos
% vamos a querer tener primero los posibles casos base, y luego los casos recursivos si los hay.


% -----------------------------------------------------
% -------------------- Ejercicio 4 --------------------
% -----------------------------------------------------

% juntar(?Lista1,?Lista2,?Lista3)
% Basicamente si nuestra primera lista es vacía, entonces no queda otra que L2 sea L3 (primera definición).
% En el otro caso, queremos que la cabeza de L1 sea también la cabeza de L3, y vamos haciendo recursión sin tocar L2,
% hasta que llegamos a la definición base.
juntar([], L2S, L2S).
juntar([L1 | L1S], L2S, [L1 | L3S]) :-
    juntar(L1S, L2S, L3S).


% -----------------------------------------------------
% -------------------- Ejercicio 5 --------------------
% -----------------------------------------------------

% last(?L, ?U)
% Usando append (juntar), vamos a pedir que si concatenamos una lista XS' con una [U], dé XS.
% Entonces justamente ese U va a ser el último de XS.
lastP(XS, U) :- append(_, [U], XS).

% reverse(+L, ?R), donde R contiene los mismos elementos que L, pero en orden inverso.
reverseP([X], [X]).
reverseP(XS, [R | RS]) :-
    lastP(XS, R), 
    append(XSSinUltimo, [R], XS),
    reverseP(XSSinUltimo, RS)
.

% prefijo(?P, +L), donde P es prefijo de la lista L.
prefijo(P, L) :- append(P, _, L).

% sufijo(?S, +L), donde S es sufijo de la lista L.
sufijo(S, L) :- append(_, S, L).

% pertenece(?X, +L), que es verdadero sii el elemento X se encuentra en la lista L.
pertenece(X, [L | LS]) :- X = L.
pertenece(X, [L | LS]) :- pertenece(X, LS).


% -----------------------------------------------------
% -------------------- Ejercicio 6 --------------------
% -----------------------------------------------------

% aplanar(+Xs, -Ys)
aplanar([], []).
aplanar([[] | Tail], L) :-
    aplanar(Tail, L).
aplanar([Head | Tail], L) :-
    Head = [_ | _],
    aplanar(Head, AplHead),
    aplanar(Tail, AplTail),
    juntar(AplHead, AplTail, L).
aplanar([X | XS], [L | LS]) :- 
    X = L, 
    aplanar(XS, LS).


% -----------------------------------------------------
% -------------------- Ejercicio 7 --------------------
% -----------------------------------------------------

% intersección(+L1, +L2, -L3)
% El plan va a ser el siguiente, si la cabeza de L1 pertenece a L2, entonces será la cabeza de L3.
% Luego para que no se repita, la eliminamos de la cola de L2 y de L3. Luego hacemos la recursión.
% Cuando L1 sea la lista vacía, paramos, ya que la interseccion de una lista vacía con otra, es la vacía.

% Cremos un predicado llamado eliminarTodos(+Elem, +Lista, ?Result) que eliminar Elem de Lista.
eliminarTodos(_, [], []).
eliminarTodos(E, [L | LS], [L | LSSinE]) :-
    E \= L,
    eliminarTodos(E, LS, LSSinE).
eliminarTodos(E, [E | LS], LSSinE) :-
    eliminarTodos(E, LS, LSSinE).

interseccion([], _, []).
interseccion(_, [], []).
interseccion([L1 | L1S], L2S, [L1 | L3S]) :-
    member(L1, L2S),
    eliminarTodos(L1, L1S, L1SSinL1),
    eliminarTodos(L1, L2S, L2SSinL1),
    interseccion(L1SSinL1, L2SSinL1, L3S)   
.


% partir(N, L, L1, L2), donde L1 tiene los N primeros elementos de L, y L2 el resto
partir(N, L, L1, L2) :-
    length(L1, N),
    append(L1, L2, L)
.


% Borrar es eliminarTodos (lo hicimos arriba).
sacarDuplicados([], []).
sacarDuplicados([X | XS], [X | L2]) :-
    eliminarTodos(X, XS, XSSinX),
    sacarDuplicados(XSSinX, L2)
.


% permutación(+L1, ?L2)
% Creamos un predicado auxiliar
eliminarUno(_, [], []).
eliminarUno(E, [X | XS], [X | RS]) :-
    E \= X,
    eliminarUno(E, XS, RS)
.
eliminarUno(E, [X | XS], YS) :-
    E = X,
    XS = YS
.


% reparto(+L, +N, -LListas) que tenga éxito si LListas es una lista de N listas
reparto(L, 1, [L]).
reparto(L, N, [L | Rec]) :-
    N > 0,
    append(L, XS, L),
    Nm1 is N - 1,
    reparto(XS, Nm1, Rec)
.


% repartoSinVacías(+L, -LListas)
% La idea de la solución es la siguiente. Si nos dan una lista, bueno, esta misma podría tranquilamente ser un reparto.
% la segunda definición dice que si tenemos una lista, la podemos escribir como Lista = L ++ Rest.
% Por un lado nos aseguramos que ninguna sea vacía, ya que no tenemos una definición que se haga cargo de ese caso.
% Por otro lado hacemos la recursión.
repartoSinVacias(L, [L]).
repartoSinVacias(Lista, [L | Rec]) :-
    append(L, Rest, Lista),
    L \= [],
    Rest \= [],
    repartoSinVacias(Rest, Rec)
.


% -----------------------------------------------------
% -------------------- Ejercicio 8 --------------------
% -----------------------------------------------------

% parteQueSuma(+L, +S, -P)
parteQueSuma(L, 0, []).
parteQueSuma(L, S, [P | PS]) :-
    S > 0,
    member(P, L),
    append(_, [P | Sufijo], L),
    SmP is S - P,
    SmP >= 0,
    parteQueSuma(Sufijo, SmP, PS)
.

% -----------------------------------------------------
% -------------------- Ejercicio 9 --------------------
% -----------------------------------------------------

desde(X, X).
desde(X, Y) :- 
    N is X + 1, 
    desde(N, Y)
.
% Se pregunta cómo deben instanciarse los parámetros para que el predicado funcione. La primera regla unifica si ambos
% parámetros unifican, por otro lado, la segunda definición toma dos parámetros que no necesariamente unifican, y hace
% recursión incrementando el primero en uno. 
% Si damos ambos sin instanciar, primero obtendremos una solución gracias a la primera definición, luego obtendremos un error
% ya que nuestros argumentos no están lo suficientemente instanciados. Notemos que en la segunda definición se dice que N is X + 1,
% pero el motor aritmético de Prolog trabaja con numeros ya instanciados.
% Si el primer parámetro está instanciado y el segundo no, una solución será la que es igual que el instanciado (por la primera definición).
% Luego la segunda definición nos incrementará el primer parámetro en uno, y hara la recursión. Vemos entonces que obtendremos todos Y mayores
% e iguales a ese primer parámetro instanciado.
% Finalmente, si ambos están instanciados, si fuesen iguales obtendríamos true por la primera definición, si no fuesen iguales, y en particular
% el segundo parámetro fuese menor a el primero, nos quedaríamos sin memoria ya que el primer parámetro se incrementaría indefinidamente sin 
% unificar con el primero, mientras que si el segundo es mayor o igual al primero, tarde o temprano obtendríamos true.

% Para que el predicado funcione, simplemente hagamonos cargo de "arreglar" todas las falencias que mencionamos recién.
% desdeReversible(+X, ?Y)
desdeReversible(X, Y) :-
    nonvar(Y),
    X =< Y.
desdeReversible(X, X).
desdeReversible(X, Y) :-
    var(Y),
    Xm1 is X + 1,
    desdeReversible(Xm1, Y)
.
% Lo que hicimos entonces fue:
% 1 - Si Y está instanciada, entonces debe ser mayor o igual a X.
% 2 - Si ambos parámetros son iguales, devolvemos true.
% 3 - Si Y es una variable libre, entonces podemos hacer la recursión para generar todos los Y desde X.


% -----------------------------------------------------
% -------------------- Ejercicio 10 -------------------
% -----------------------------------------------------

% intercalar(L1, L2, L3), donde L3 es el resultado de intercalar uno a uno los elementos de las listas L1 y L2
intercalar([], L2, L2).
intercalar(L1, [], L1).
intercalar([L1 | L1S], [L2 | L2S], [L1 | [L2 | L3S]]) :-
    intercalar(L1S, L2S, L3S)
.

% Analicemos reversibilidad. Primero veamos el caso intercalar(+L1, +L2, -L3). Si tenemos tanto L1 como L2 instanciadas,
% entonces vamos a tomar elementos de L1 y L2 para ir construyendo L3. Como se van reduciendo ambas listas, en algún momento
% llegaremos a alguno de los casos base, por lo que intercalar funciona para esta instanciación.

% Para el caso intercalar(-L1, -L2, +L3). En el caso que tengamos L3 instanciada, queremos encontrar las listas L1 y L2 que 
% cumplen que al intercalarlas, dan L3. Una primera solución la obtendremos de la primera definición, es decir, en L1 se instanciará
% la lista [] y en L2 se instanciará L3. Lo mismo para la segunda definición.

% Luego queremos ver qué pasa con la tercera definición. Vemos que también funcionará, ya que los primeros dos elementos de L3 van a 
% instanciar los primeros elementos de las listas L1 y L2. Luego al hacer recursión, lo hacemos con una lista L3 de menor longitud. 
% En algún momento llegaremos a algun caso base.


% -----------------------------------------------------
% -------------------- Ejercicio 11 -------------------
% -----------------------------------------------------

vacio(nil).

raiz(bin(_, R, _), R).

altura(nil, 0).
altura(bin(I, _, D), AlturaRec) :-
    altura(I, AltI),
    altura(D, AltD),
    AlturaRec is 1 + max(AltI, AltD)
.

cantidadDeNodos(nil, 0).
cantidadDeNodos(bin(I, _, D), Nodos) :-
    cantidadDeNodos(I, NodosI),
    cantidadDeNodos(D, NodosD),
    Nodos is 1 + NodosI + NodosD
.


% -----------------------------------------------------
% -------------------- Ejercicio 12 -------------------
% -----------------------------------------------------

% inorder(+AB,-Lista)
inorder(nil, []).
inorder(bin(I, R, D), L) :-
    inorder(I, RecI),
    inorder(D, RecD),
    append(RecI, [R | RecD], L)
.

% arbolConInorder(+Lista, -AB)
arbolConInorder([], nil).
arbolConInorder(L, AB) :-
    append(LI, [R | LD], L),
    arbolConInorder(LD, ABD),
    arbolConInorder(LI, ABI),
    AB = bin(ABI, R, ABD)
.


% aBB(+T), que será verdadero si T es un árbol binario de búsqueda
% La estrategia que vamos a usar va a ser la siguiente. Que la raíz sea mayor o igual a el máximo elemento de la subrama izquierda.
%                                                       Que la raíz sea menor que el mínimo elemento de la subrama izquierda.
% Para esto, hacemos dos predicados, minList y maxList.
minList([E], E).
minList([H1 | Tail], Min) :-
    minList(Tail, MinRec),
    Min is min(MinRec, H1)
.

maxList([E], E).
maxList([H1 | Tail], Max) :-
    maxList(Tail, MaxRec),
    Max is max(MaxRec, H1)
.

aBB(nil).
aBB(bin(nil, _, nil)).
aBB(bin(I, R, D)) :-
    inorder(I, ElemsI), inorder(D, ElemsD),
    maxList(ElemsI, MaxI), minList(ElemsD, MinD),
    R >= MaxI, R < MinD,
    aBB(I), aBB(D)
.

% -----------------------------------------------------
% ------------------- Generate & Test -----------------
% -----------------------------------------------------

% Antes de empezar con esta sección, es importante entender tres conceptos claves sobre la generación infinita.
% 1 - Se debe usar ni mas ni menos que un generador infinito.
% 2 - El generador infinito va siempre antes que cualquier otro generador.
% 3 - Los generadores infinitos deben usarse únicamente para generar infinitas soluciones.
% Finalmente, es importante que las soluciones generadas en cada paso sean finitas.

nat(0).
nat(N) :-
    nat(M),
    N is M + 1
.

% -----------------------------------------------------
% -------------------- Ejercicio 13 -------------------
% -----------------------------------------------------

% coprimos(-X,-Y)
% Respetando las reglas de la generación infinita, la idea que vamos a usar es para cada N, buscamos los pares de numeros
% tales que suman N y son coprimos. 
coprimos((X, Y)) :-
    nat(N),
    between(1, N, X),
    Y is N - X,    
    X =< Y,
    1 is gcd(X, Y),
    N is X + Y    
.

% Como vemos, usamos un solo generador infinito, en particular nat. Luego dicho generador está antes que cualquier otro generador.


% -----------------------------------------------------
% -------------------- Ejercicio 14 -------------------
% -----------------------------------------------------


generadorListaQueSuma(1, Suma, [Suma]).
generadorListaQueSuma(Longitud, Suma, [Head | Tail]) :-
    Suma >= 0,
    Longitud > 0,
    between(0, Suma, Head),
    SumaNueva is Suma - Head,
    LongitudNueva is Longitud - 1,
    generadorListaQueSuma(LongitudNueva, SumaNueva, Tail)
.   

generadorSemiMagico(0, _, _, []).
generadorSemiMagico(Filas, Columnas, Suma, [Fila | RestoFilas]) :-
    Filas > 0,
    Columnas > 0,
    Suma >= 0,
    generadorListaQueSuma(Columnas, Suma, Fila),
    FilasRestantes is Filas - 1,
    generadorSemiMagico(FilasRestantes, Columnas, Suma, RestoFilas)
.

cuadradoSemiMagico(N, Cuadrado) :-
    nat(Suma),
    generadorSemiMagico(N, N, Suma, Cuadrado)
.

% Ahora queremos los cuadrados mágicos. No solo queremos que las filas sumen un numero dado, sino que las columnas también.
% Vamos a usar los predicados que ya creamos, solo que ahora vamos a chequear que las columnas sumen lo mismo que suman las filas.

iEsimaColumna(_, [], []).
iEsimaColumna(I, [Fila1 | RestoFilas], [C | Olumna]) :-
    length(Fila1, Longitud),
    I < Longitud,
    nth0(I, Fila1, C),
    iEsimaColumna(I, RestoFilas, Olumna)
.
    
iEsimaColumnaSuma(I, Matriz, Suma) :-
    iEsimaColumna(I, Matriz, Columna),
    sum_list(Columna, Suma)    
.

existenColumnasConDistintasSumas([M | Atriz]) :-
    length(M, CantColumnas),
    between(0, CantColumnas, Indice1), between(0, CantColumnas, Indice2),

    Indice1 \= Indice2, 

    iEsimaColumnaSuma(Indice1, [M | Atriz], Suma1),
    iEsimaColumnaSuma(Indice2, [M | Atriz], Suma2),

    Suma1 \= Suma2
.

cuadradoMagico(N, Cuadrado) :-
    cuadradoSemiMagico(N, Cuadrado),
    % Quiero que para toda columna, sumen lo mismo. En otras palabras, no existen dos columnas diferentes C1, C2,
    % tales que sus sumatorias son diferentes.
    not(existenColumnasConDistintasSumas(Cuadrado))
.

% -----------------------------------------------------
% -------------------- Ejercicio 15 -------------------
% -----------------------------------------------------

esTriangulo(tri(A, B, C)) :-
    A < B + C, A > B - C,
    B < A + C, B > A - C,
    C < A + B, C > A - B,
    A > 0, B > 0, C > 0
.


perimetro(tri(A, B, C), P) :-
    nonvar(P),
    between(1, P, A),
    between(1, P, B),
    between(1, P, C), 
    P is A + B + C,   
    esTriangulo(tri(A, B, C))
.
perimetro(tri(A, B, C), P) :-
    var(P),
    nat(P),
    between(1, P, A),
    between(1, P, B),
    between(1, P, C), 
    P is A + B + C,   
    esTriangulo(tri(A, B, C))
.


triangulo(tri(A, B, C)) :-
    nat(P),
    between(1, P, A),
    between(1, P, B),
    between(1, P, C), 
    P is A + B + C,   
    esTriangulo(tri(A, B, C))
.

% -----------------------------------------------------
% -------------------- Ejercicio 16 -------------------
% -----------------------------------------------------


frutal(frutilla).
frutal(banana).
frutal(manzana).
cremoso(banana).
cremoso(americana).
cremoso(frutilla).
cremoso(dulceDeLeche).

% Ana desea comprar un cucurucho con sabores que le gustan. 
% El cucurucho admite hasta 2 sabores. 
% Los siguientes predicados definen las posibles maneras de armar el cucurucho.

leGusta(X) :- frutal(X), cremoso(X).
cucurucho(X,Y) :- leGusta(X), leGusta(Y).

% i. Escribir el árbol de búsqueda para la consulta ?- cucurucho(X,Y).

% ?- cucurucho(X,Y).
%     |
%     |_ ?- leGusta(X), leGusta(Y).
%        |
%        |_ -? frutal(X), cremoso(X), leGusta(Y).
%            |
%            |_ -? cremoso(X), leGusta(Y).            MGU {X := frutilla}
%            |   |_ -? leGusta(Y)                     MGU {X := frutilla}
%            |       |_ -? frutal(Y), cremoso(Y)
%            |           |_ -? cremoso(Y).            MGU {X := frutilla, Y := frutilla}
%            |           |   |_ ✓ ................... MGU {X := frutilla, Y := frutilla}
%            |           |
%            |           |_ -? cremoso(Y).            MGU {X := frutilla, Y := banana}
%            |           |   |_ ✓ ................... MGU {X := frutilla, Y := banana}
%            |           |
%            |           |_ -? cremoso(Y).            MGU {X := frutilla, Y := manzana}
%            |               |_ ✗
%            |
%            |_ -? cremoso(X), leGusta(Y).            MGU {X := banana}
%            |   |_ -? leGusta(Y)                     MGU {X := banana}
%            |       |_ -? frutal(Y), cremoso(Y)
%            |           |_ -? cremoso(Y)             MGU {X := banana, Y:= frutilla}
%            |           |   |_ ✓ ................... MGU {X := banana, Y:= frutilla}
%            |           |
%            |           |_ -? cremoso(Y)             MGU {X := banana, Y:= banana}
%            |           |   |_ ✓ ................... MGU {X := banana, Y:= banana}
%            |           |
%            |           |_ -? cremoso(Y)             MGU {X := banana, Y:= manzana}
%            |               |_  ✗
%            |
%            |_ -? cremoso(X), leGusta(Y).            MGU {X := manzana}
%                |_ ✗


% -----------------------------------------------------
% -------------------- Ejercicio 17 -------------------
% -----------------------------------------------------

% Escribamos la definición de not
% not(P) :- P, !, fail
% not(P).

% Tenemos los predicados P(?X) y Q(?X), queremos analizar que sucede al realizar la consulta P(Y), not(Q(Y)).
% Primero al hacer P(Y), obtenemos un Y' tal que P(Y') es true. Luego tenemos not(Q(Y)). Si usamos la primera definición
% de not tenemos Q(Y'), !, fail. Es decir, si Q(Y') fuese true, se cortaría la busqueda, y por fail obtendríamos false.
% Luego volveríamos a encontrar un Y' tal que P(Y') sea true, hasta que se dé que not(Q(Y)) es verdadero. Vemos entonces
% que estamos buscando todos los Y' que cumplen que P(Y') es true Q(Y') es false.

% Veamos que pasa si en vez de hacer P(Y), not(Q(Y)), hacemos not(Q(Y)), P(Y). 
% Podemos pensar que tenemos lo siguiente: Q(Y), !, fail, P(Y). Es decir, una vez que encontremos un Y tal que Q(Y) es
% true, inmediatamente se hara cut y fail, y nunca se va a verificar que suceda P(Y). Entonces podemos obtener respuestas
% que no cumplan la formula logica de P(Y) ∧ ¬Q(Y), ya que nunca se llegaría a verificar que suceda P(Y) cuando suceda
% que Q(Y) es true.

% Tenemos un predicado P(X), queremos determinar cuántas soluciones de P(Y) hay. En particular si hay una, queremos dar true,
% en caso contrario queremos dar false.
% Supongamos que tenemos dos soluciones, podemos decir lo siguiente:
% P(X), P(Y), X \= Y. Como queremos evitar esto, entonces podemos hacer lo siguiente: not(P(X), P(Y), X \= Y).


% -----------------------------------------------------
% -------------------- Ejercicio 18 -------------------
% -----------------------------------------------------

hayCorteMasParejo(L, BestDif) :-
    append(L1, L2, L),
    sum_list(L1, Sum1), sum_list(L2, Sum2),
    Dif_ is Sum1 - Sum2, Dif is abs(Dif_),
    Dif < BestDif
.

% corteMásParejo(+L,-L1,-L2)
corteMasParejo(L, L1, L2) :-
    append(L1, L2, L),
    sum_list(L1, Sum1), sum_list(L2, Sum2),
    Dif_ is Sum1 - Sum2, Dif is abs(Dif_),
    not(hayCorteMasParejo(L, Dif))
.


% -----------------------------------------------------
% -------------------- Ejercicio 20 -------------------
% -----------------------------------------------------

noEsPrimo(N) :-
    N > 1,
    Nm1 is N - 1,
    between(2, Nm1, Divisor),
    0 =:= N mod Divisor
.

esPrimo(N) :-
    N > 1,
    not(noEsPrimo(N))
.

noTodosLosPrimosDividen(Numero) :-
    between(2, Numero, Primo),
    esPrimo(Primo),

    0 =:= Numero mod Primo,
    0 =\= Numero mod (Primo * Primo)
.
    
% próximoNumPoderoso(+X,-Y)
proximoNumPoderoso(APartir, SiguientePoderoso) :-
    nat(SiguientePoderoso),
    SiguientePoderoso > APartir,
    not(noTodosLosPrimosDividen(SiguientePoderoso))
.



% -----------------------------------------------------
% ---------------- Ejercicios de Parcial --------------
% -----------------------------------------------------

% https://www.cubawiki.com.ar/images/4/4b/PLP-2C2024-2r.pdf

% caminoDesde(+P, -C) 
% Recordemos las reglas de la generación infinita.
% 1 - Se debe usar ni mas ni menos que un generador infinito.
% 2 - El generador infinito va siempre antes que cualquier otro generador.
% 3 - Los generadores infinitos deben usarse únicamente para generar infinitas soluciones.
% Finalmente, es importante que las soluciones generadas en cada paso sean finitas.

% Tenemos que encontrar una manera para limitar la cantidad de veces que nos movemos para un lado o para otro. 
% Esto es así ya que podría pasar que vayamos siempre para la derecha/izquierda/arriba/abajo. 
% Si queremos "expander" los caminos de forma equitativa tanto para derecha/izquierda/arriba/abajo, podríamos 
% pedir que la sumatoria de los X/Y de cada coordenada de la lista, 

nat(0).
nat(N) :-
    nat(M),
    N is M + 1
.

moverUno((X, Y), (X, Yp1)) :- Yp1 is Y + 1.
moverUno((X, Y), (X, Ym1)) :- Ym1 is Y - 1.
moverUno((X, Y), (Xp1, Y)) :- Xp1 is X + 1.
moverUno((X, Y), (Xm1, Y)) :- Xm1 is X - 1.

caminosConSumatoria(0, CoordenadaInicial, []). % No me puedo mover
caminosConSumatoria(Sumatoria, (XInicial, YInicial), [(X, Y) | Camino]) :-
    Sumatoria > 0,
    
    % Para que sea un camino válido, tenemos que movernos nada más y nada menos que 1 paso respecto al anterior.
    moverUno((XInicial, YInicial), (X, Y)),
    SumatoriaN is Sumatoria - 1,
    caminosConSumatoria(SumatoriaN, (X, Y), Camino)
.

caminoDesde((X, Y), [(X, Y) | Camino]) :-
    nat(Expansion),
    caminosConSumatoria(Expansion, (X, Y), Camino)
.


% objeto(?Id,?P,?V)
objeto(1, 50, 10).
objeto(2, 75, 15).
objeto(3, 60, 5).
objeto(4, 10, 1).

% mochila(+C,-L)
% Verdadero cuando L es una lista de identificadores de objetos que al guardarlos en la mochila no supera su capacidad C.

mochila(Capacidad, IDs) :-
    mochila(Capacidad, 0, IDs)
.

mochila(_, _, []).
mochila(Capacidad, MinID, IDs) :-
    objeto(ID, Peso, _),
    Capacidad >= Peso,
    
    ID >= MinID,

    NuevaCapacidad is Capacidad - Peso,
    NextMinID is ID + 1,
    mochila(NuevaCapacidad, NextMinID, RestoIDs),
    list_to_set([ID | RestoIDs], IDs)
.

% Queremos analizar la reversibilidad de el predicado mochila(+C, -L) en C y L.
% Empecemos analizando la reversibilidad en C. Supongamos que C no está instanciado, es decir, no sabemos la capacidad de la mochila. 
% Sabemos que el motor aritmético trabaja con variables instanciadas, por ende tendremos un error. Concluímos rápidamente que no es reversible en C.

% Analizamos la reversibilidad en la lista de los IDs. Tenemos la capacidad de una mochila y una lista de identificadores. Vamos a querer devolver,
% un valor de verdad, ver si se cumplen todos los predicados que conforman mochila. Hasta que se vuelve a llamar a mochila/3, no habrá problema.
% Veamos qué pasa cuando se realiza la recursión. Como tenemos los IDs instanciados, primero Prolog buscará un ID que "matchee" con el primer elemento
% de IDs. Luego, en la recursión pasará lo mismo. En particular intentará matchear RestoIDs con la tail de IDs. Vemos que es reversible.

% mejorMochila(+C, -L)
valorMochila([], 0).
valorMochila([ID | RestoIDs], Valor) :-
    objeto(ID, _, ValorElemento),
    valorMochila(RestoIDs, ValorRec),
    Valor is ValorElemento + ValorRec
.

hayMochilaConMejorValor(Capacidad, Mochila) :-
    mochila(Capacidad, MejorMochila),
    valorMochila(MejorMochila, ValorMejorMochila),
    valorMochila(Mochila, ValorMochilaOriginal),
    ValorMejorMochila > ValorMochilaOriginal
.

mejorMochila(Capacidad, Mochila) :-
    mochila(Capacidad, Mochila),
    not(hayMochilaConMejorValor(Capacidad, Mochila))
.


% https://www.cubawiki.com.ar/images/f/f8/PLP-1C2024-2r.pdf

% generarCapicuas(-L)
natC(1).
natC(N) :-
    natC(M),
    N is M + 1
.

listaQueSuma(0, []).
listaQueSuma(Suma, [Head | Tail]) :-
    Suma > 0,
    between(1, Suma, Head),
    NuevaSuma is Suma - Head,
    listaQueSuma(NuevaSuma, Tail)
.

particionIgual(L, Parte1, Parte2) :-
    length(L, Longitud),
    1 =:= Longitud mod 2,
    append(Parte1, [_ | Parte2], L),
    length(Parte1, MismaLongitud),
    length(Parte2, MismaLongitud)
.
particionIgual(L, Parte1, Parte2) :-
    length(L, Longitud),
    0 =:= Longitud mod 2,
    append(Parte1, Parte2, L),
    length(Parte1, MismaLongitud),
    length(Parte2, MismaLongitud)
.

% Generamos las listas capicúas que suman un numero
generarCapicuas(Lista) :-
    natC(Sumatoria), 
    % Notemos que la maxima longitud que puede tomar una lista capicúa es |Sumatoria|, ya que cada elemento suma 1.

    listaQueSuma(Sumatoria, Lista),
    
    % Ahora quiero chequear que la lista sea capicúa.
    particionIgual(Lista, Parte1, Parte2),
    reverse(Parte1, Parte2)    
.


% tokenizar(+D, +F, -T)
tokenizar(_, [], []).
tokenizar(Diccionario, Frase, [T1 | RestoT]) :-
    
    append(T1, RestoFrase, Frase),

    member(T1, Diccionario),
    
    tokenizar(Diccionario, RestoFrase, RestoT)
.



% https://www.cubawiki.com.ar/images/0/0a/PLP-2C2024-2P.pdf

% La idea es primero conseguir todas las posibles subsecuencias, luego pedir que sean crecientes
subsecuencia([], []).
subsecuencia([_ | Tail], Subsecuencia) :-
    subsecuencia(Tail, Subsecuencia)
.
subsecuencia([Head | Tail], [Head | Rec]) :-
    subsecuencia(Tail, Rec)
.


creciente([_]).
creciente([Fst | [Snd | Tail]]) :-
    Fst < Snd,
    creciente([Snd | Tail])
.

subsecuenciaCreciente(_, []).
subsecuenciaCreciente(Lista, Subsecuencia) :-
    subsecuencia(Lista, Subsecuencia),
    creciente(Subsecuencia)
.


haySubsecuenciaMasLarga(Lista, Subsecuencia) :-
    subsecuenciaCreciente(Lista, Candidata),
    length(Candidata, LongitudCandidata),
    length(Subsecuencia, LongitudSubsecuencia),
    LongitudCandidata > LongitudSubsecuencia
.


subsecuenciaCrecienteMasLarga(Lista, Subsecuencia) :-
    subsecuenciaCreciente(Lista, Subsecuencia),
    not(haySubsecuenciaMasLarga(Lista, Subsecuencia))
.


% fibonacci(-X)
fibonacci(1, 1).
fibonacci(2, 1).
fibonacci(N, Res) :-
    N > 2,
    Nm1 is N - 1,
    Nm2 is N - 2,
    fibonacci(Nm1, ResNm1), fibonacci(Nm2, ResNm2),
    Res is ResNm1 + ResNm2
.


fibonacci(X) :-
    nat(N),
    fibonacci(N, X)
.



% https://www.cubawiki.com.ar/images/0/03/PLP-1C2025-2r.pdf

unico(L, Elem) :-
    select(Elem, L, LSinElem),
    not(member(Elem, LSinElem))
.


noTodosSonUnicos([_ | XS]) :- noTodosSonUnicos(XS).
noTodosSonUnicos([X | XS]) :- member(X, XS).


sinRepetidos(L) :- not(noTodosSonUnicos(L)).


formula(VS, F, 0) :- member(F, VS).
formula(VS, F, CantidadLogicas) :-
    CantidadLogicas > 0,
    CantidadLogicasMenos1 is CantidadLogicas - 1,
    formula(VS, F1, CantidadLogicasMenos1),
    F = neg(F1)
.
formula(VS, F, CantidadLogicas) :-
    CantidadLogicas > 1,
    CantidadLogicasMenos2 is CantidadLogicas - 2,
    formula(VS, F1, CantidadLogicasMenos2),
    formula(VS, F2, CantidadLogicasMenos2),
    F = imp(F1, F2)
.
formula(VS, F) :-
    desde(0, CantidadLogicas),
    formula(VS, F, CantidadLogicas)
.
