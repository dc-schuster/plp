
import Data.List

-----------------------------------------------------
-------------------- Ejercicio 1 --------------------
-----------------------------------------------------

-- (Int, Int) -> Int
max2 :: (Int, Int) -> Int
max2 (x, y) | x >= y = x
            | otherwise = y


-- Floating a => (a, a) -> a
-- Si corremos :type sqrt, vemos que sqrt tiene el siguiente tipado: sqrt :: Floating a => a -> a
normaVectorial ::  Floating a => (a, a) -> a
normaVectorial (x, y) = sqrt (x^2 + y^2)


-- Num c => c -> c -> c
subtract :: Num c => c -> c -> c
subtract = flip (-)


-- Num c => c -> c
predecesor :: Num c => c -> c
predecesor = Main.subtract 1


-- (Int -> a) -> b
-- Toma una lambda que dado un Int, devuelve un a, luego como se 
-- ejecuta la lambda, termina devolviendo dicho valor de tipo a
evaluarEnCero :: (Int -> a) -> a
evaluarEnCero = \f -> f 0


-- Toma una funcion, y luego devuelve la composicion evaluada
-- Entonces, la funcion es de pinta (a -> a).
-- Luego tiene que recibir un a para evaluar en la funcion.
-- Luego devuelve un a.
dosVeces :: (a -> a) -> a -> a
dosVeces = \f -> f . f


-- Si vemos :type map, es del tipo (a -> b) -> [a] -> [b]
-- Vemos que la funcion a aplicar a cada elemento de la lista recibida, es flip.
-- Flip tiene el tipo (a -> b -> c) -> b -> a -> c
-- Entonces como a cada elemento de la lista le tenemos que aplicar flip, y flip recibe una funcion
-- de tipo (a -> b -> c), entonces la lista debe tener elementos de dicho tipo.
-- Luego flip va a "flipear" el orden de recibida de argumentos.
flipAll :: [(a -> b -> c)] -> [(b -> a -> c)]
flipAll = map flip


-- Pensemos esto de la siguiente manera.
-- Tenemos un flip: (λf.λx.λy) = f y x al que le aplicamos un flip, entonces justamente la f que recibe flip,
-- es flip. Es decir, tenemos (λx.λy) = flip y x. Nuevamente, como flip recibe una funcion, entonces sabemos que el tipo de y
-- es una funcion de dos argumentos, decimos que es del tipo (a -> b -> c). Sin embargo, como vemos, tenemos (λx.λy) = flip y x.
-- Es decir, recibimos primero el x. Pensemos de que tipo será. Como la funcion y sera del tipo (a -> b -> c), y se van a flipear
-- los argumentos, tenemos que va a terminar siendo del tipo (b -> a -> c), por lo que x tiene que ser de tipo b.
flipRaro :: b -> (a -> b -> c) -> (a -> c)
flipRaro = flip flip


-- Una funcion currificada, es aquella que toma los parametros uno a la vez, mientras que una no currificada, toma los
-- parametros cdomo tupla. Entonces las funciones no currificadas, es decir, las que toman parametros como tuplas, son:
-- max2 y normaVectorial.


-----------------------------------------------------
-------------------- Ejercicio 2 --------------------
-----------------------------------------------------


-- Como dijimos, una funcion currificada es aquella que toma los parametros uno a la vez. Entonces la funcion curry seria recibir 
-- una funcion no currificada, es decir, que recibe los parametros en forma de tupla, y "permitir la aplicacion parcial", es decir, 
-- recibir los argumentos uno a la vez.
curry :: ((a, b) -> c) -> (a -> b -> c)
curry f x y = f (x, y)


-- Ahora si queremos recibir una funcion currificada, es decir, una que recibe los parametros de a uno, y devolver una no currificada,
-- es decir, una que recibe los parametros como forma de tupla, tenemos:
uncurry :: (a -> b -> c) -> ((a, b) -> c)
uncurry f (a, b) = f a b


-----------------------------------------------------
-------------------- Ejercicio 3 --------------------
-----------------------------------------------------


-- Sumatoria de elementos de una lista.
sumFoldr :: Num a => [a] -> a 
sumFoldr lista = foldr (+) 0 lista


-- Dado un elemento, queremos ver si está en la lista.
elemFoldr :: Eq a => [a] -> a -> Bool
elemFoldr lista elem = 
    foldr (\x acum -> elem == x || acum) False lista


-- Dado un predicado y una lista, nos quedamos con los elementos que cumplan dicho predicado.
filterFoldr :: (a -> Bool) -> [a] -> [a]
filterFoldr p lista = 
    foldr (\elem cum -> if p elem then elem : cum else cum) [] lista


-- Dada una lista, le aplicamos una funcion a cada elemento de la imsma
mapFoldr :: (a -> b) -> [a] -> [b]
mapFoldr f lista = foldr (\elem acum -> [f elem] ++ acum) [] lista


-- foldr1 es exactamente foldr solo que no toma caso base
mejorSegun :: (a -> a -> Bool) -> [a] -> a
mejorSegun f lista = foldr1 (\elem rec -> if f elem rec then elem else rec) lista 


-- sumasParciales :: Num a => [a] -> [a]
sumasParciales :: Num a => [a] -> [a]
sumasParciales lista = 
    foldl (\rec elem -> 
        if null rec 
            then [elem] 
        else 
            rec ++ [elem + last rec]) 
    [] lista


-- La idea es hace el primer elemento, menos el segundo, mas el tercero, menos el cuarto, etc.
-- Si planteamos elem - rec, estamos haciendo el primer elemento menos la rec. Luego - con - es +,
-- por lo que estariamos sumando el tercero, y así...
sumaAlt :: Num a => [a] -> a
sumaAlt lista = foldr (\elem rec -> elem - rec) 0 lista


sumaAltInversa :: Num a => [a] -> a
sumaAltInversa lista = foldl1 (\rec elem -> elem - rec) revLista
    where revLista = reverse lista


-----------------------------------------------------
-------------------- Ejercicio 4 --------------------
-----------------------------------------------------

-- Queremos las permutaciones de una lista. Se recomienda usar concatMap. Lo que hace concatMap, es:
-- dado una lista, y una funcion que convierte un elemento en una lista de elementos, va aplicando esta
-- funcion a cada elemento de la lista original, por lo que tenemos una lista de listas. Luego concatena las listas.
-- Por otro lado se recomienda usar take y drop.

-- La idea es la siguiente: usando foldr, tomamos un elemento y el resultado parcial de todas las permutaciones.
-- Luego, con concatMap, tomamos una permutacion parcial, y usando map, para cada elemento del acumulador, es decir,
-- para cada permutacion, vamos a insertar el elemento en la posicion i de la lista original, en una posicion diferente
-- en cada una de las listas del acum de foldr.
permutaciones :: [a] -> [[a]]
permutaciones lista = 
    foldr (\elem acum ->
        concatMap (\permParcial -> 
            map (\indice -> insert permParcial elem indice) [0 .. length permParcial]
        ) acum
    ) [[]] lista
    where insert lista elem indice = take indice lista ++ [elem] ++ drop indice lista


-- Dada una lista L y devuelve la lista de todas las listas formadas por los mismos elementos de L, 
-- en su mismo orden de aparición. Ej: [5, 1, 2] → [[], [5], [1], [2], [5, 1], [5, 2], [1, 2], [5, 1, 2]]
-- ! No logré mantener el orden.         [5, 1, 2] → [[], [2], [1], [1, 2], [5], [5, 2], [5, 1], [5, 1, 2]]
partes :: [a] -> [[a]]
partes lista =
    foldr (\elem acum ->
        acum ++ map (\parteParcial -> elem : parteParcial) acum
    ) [[]] lista


-- prefijos, que dada una lista, devuelve todos sus prefijos.
-- prefijos [5, 1, 2] → [[], [5], [5, 1], [5, 1, 2]]
-- La idea es la siguiente: suponemos que el ultimo elemento de acum, es el ultimo prefijo generado.
-- Entonces queremos conservar esos prefijos, pero ademas sumarle el prefijo que tiene al final, el elem.
-- Entonces seria algo como acum ++ [ultimo prefijo ++ [elem]]
prefijos :: [a] -> [[a]]
prefijos lista = 
    foldl (\acum elem ->
          acum ++ [last acum ++ [elem]]
    ) [[]] lista


-- dada una lista, devuelve todas sus sublistas: listas de elementos que aparecen consecutivos en la lista original.
-- sublistas [5, 1, 2] → [[], [5], [1], [2], [5, 1], [1, 2], [5, 1, 2]]
-- sublistas (x:xs) = drop 1 (prefijos (x:xs)) ++ sublistas xs 



-----------------------------------------------------
-------------------- Ejercicio 5 --------------------
-----------------------------------------------------

-- Recordemos las condiciones necesarias para que una funcion
-- utilice recursion estructural. Por un lado su caso base debe devolver un valor fijo. 
-- Por otro lado, tenemos que hacer un solo llamado recursivo sobre la cola. Por otro lado, no se debe usar otras funciones
-- recursivas dentro de la misma. 

-- elementosEnPosicionesPares tiene un caso base fijo ([]), por otro lado, en el caso recursivo, hace recursion sobre la cola
-- una sola vez y no utiliza otras funciones recursivas. En conclusion, utiliza recursion estructural.

-- entrelazar no utiliza recursion estructural. Si bien se hace una llamada recursiva, la recursion está dentro de una lambda,
-- y no sobre entrelazar. Además la recursion debe ocurrir en un "area", no en multiples. Vemos que entrelazar ocurre tanto en la 
-- rama then como en la rama else. Ademas vamos intercalando entre x, ys, xs, y tail de ys.
-- Reescribamos entrelazar utilizando recursion estructural.

entrelazarEstructural :: [a] -> [a] -> [a]
entrelazarEstructural [] ys = ys
entrelazarEstructural xs [] = xs
entrelazarEstructural (x:xs) (y:ys) = x : y : entrelazarEstructural xs ys

-- Tenemos multiples casos base, pero eso está bien, ademas nuestros casos base devuelven valores fijos. 
-- Por otro lado, nuestro caso recursivo hace recursión una sola vez, y utiliza los parametros como fueron dados, sin modificarlos.



-----------------------------------------------------
-------------------- Ejercicio 6 --------------------
-----------------------------------------------------

recr :: (a -> [a] -> b -> b) -> b -> [a] -> b 
recr f z [] = z
recr f z (x : xs) = f x xs (recr f z xs)


-- Dado un elemento y una lista devuelve el resultado de eliminar de la lista 
-- la primera aparición del elemento (si está presente).
sacarUna :: Eq a => a -> [a] -> [a]
sacarUna elem lista = recr (\x xs rec ->
        if x == elem then
            xs
        else
            x : rec
    ) [] lista

-- O la version con case
sacarUnaCase :: Eq a => a -> [a] -> [a]
sacarUnaCase elem lista = recr (\x xs rec ->
        case x == elem of
            True -> xs
            False -> x : rec
    ) [] lista


-- ? Explicar por qué el esquema de recursión estructural (foldr) no es adecuado para implementar la función sacarUna del punto anterior.
-- La razon por la cual el esquema de recursion estructural no es adecuado para implementar dicha funcion, es que necesitamos ver
-- el valor de distintos elementos (en este caso la cabeza), y en base al valor del mismo, hacer, o no recursion.
-- Si usasemos foldr, por empezar no podriamos frenar la recursion, por lo que terminaríamos sacando todos los elem de la lista.
-- Y por otro lado, no tendriamos acceso a la cola de la lista sin la recursion aplicada, por lo que no podriamos devolver xs en 
-- el caso de que x == elem.


insertarOrdenado :: Ord a => a -> [a] -> [a]
insertarOrdenado elem lista =
    recr (\x xs rec ->
        case elem > x of
            True -> x : rec
            False -> elem : x : xs
    ) [elem] lista

-- Veamos por qué funciona el codigo. Por empezar, utilizando recr, recibimos en la lambda el primer elemento de la lista no procesada,
-- luego xs y finalmente el resultado de la recursion. Primero nos fijamos si el elememto a insertar es mayor que x, si esto es así,
-- tenemos la rama true que lo que hace es concatenar x con el resultado recursivo. Queremos que esto sea así ya que si insertamos el 
-- elemento ahora, perdemos la caracteristica de lista ordenada.
-- En el caso de que elem sea menor o igual a x, entonces queremos insertarlo, por lo que hacemos elem : x : xs. Notemos que no usamos
-- rec, ya que queremos terminar la recursion. 
-- Finalmente, en el caso de que el elemento a insertar sea el mas grande de la lista, vemos que no hay una clausula del case que lo contemple
-- por lo que tenemos que agregarlo al caso base.



-----------------------------------------------------
-------------------- Ejercicio 7 --------------------
-----------------------------------------------------

-- mapPares, una versión de map que toma una función currificada (es decir, toma los parametros de a uno) de dos argumentos 
-- y una lista de pares de valores, y devuelve la lista de aplicaciones de la función a cada par. Pista: recordar curry y uncurry.

mapParesFoldr :: (a -> a -> b) -> [(a, a)] -> [b]
mapParesFoldr f lista = 
    foldr (\par acum ->
        [Main.uncurry f par] ++ acum
    ) [] lista

-- Ahora con foldl
mapParesFoldl :: (a -> a -> b) -> [(a, a)] -> [b]
mapParesFoldl f lista = 
    foldl (\acum par ->
        acum ++ [Main.uncurry f par]
    ) [] lista



armarPares :: [a] -> [b] -> [(a, b)]
armarPares [] _ = []
armarPares _ [] = []
armarPares (x:xs) (y:ys) = [(x, y)] ++ armarPares xs ys

-- Veamos por qué esto utiliza recursion estructural. 
-- Por empezar tiene casos base que devuelven valores fijos.
-- Por otro lado, el caso recursivo utiliza la informacion tal como la recibe, y recursa una sola vez.


-- Toma una funcion currificada (toma los parametros de a uno) de dos argumentos y dos listas de igual longitud.
-- Devuelve una lista de aplicaciones de la funcion a cada elemento de las dos listas. 

mapDoble :: (a -> b -> c) -> [a] -> [b] -> [c]
mapDoble _ [] [] = []
mapDoble f (x:xs) (y:ys) = [f x y] ++ mapDoble f xs ys


-----------------------------------------------------
-------------------- Ejercicio 8 --------------------
-----------------------------------------------------

-- Skip


-----------------------------------------------------
-------------------- Ejercicio 9 --------------------
-----------------------------------------------------

-- foldr :: (a -> b -> b) -> b -> [a] -> b
-- foldr f z [] = z
-- foldr f z (x:xs) = f x (foldr f z xs)

-- Se pide dar foldNat para los naturales. Veamos como es foldr, para entender cómo debe ser foldNat.
-- foldr recibe una funcion de dos parametros, un caso base y la lista a foldear.
-- Entonces podemos pensar foldNat como: foldNat :: funcion -> CasoBase -> Int a foldear -> Resultado

foldNat :: (Integer -> b -> b) -> b -> Integer -> b
foldNat _ z 0 = z
foldNat f z n = f n (foldNat f z (n - 1)) 


potencia :: Integer -> Integer -> Integer
potencia n pot = foldNat (\_ acum -> acum * n) 1 pot


-------------------------------------------------------
--------------------- Ejercicio 10 --------------------
-------------------------------------------------------

-- Skip


-------------------------------------------------------
--------------------- Ejercicio 11 --------------------
-------------------------------------------------------

data Polinomio a = X
                 | Cte a
                 | Suma (Polinomio a) (Polinomio a)
                 | Prod (Polinomio a) (Polinomio a)
                 deriving (Eq, Show)

-- Tenemos que tomar una funcion para cada caso, podemos pensar algo como foldPolinomio x fCte fSuma fProd polinomio
-- Pensemos el tipado. Empecemos por el x, digamos que va a ser de un tipo b. Notar que cada vez que se termine una recursion,
-- recibimos algo de tipo b.
-- Suponiendo que tenemos un Polinomio de tipo a, entonces las constantes del polinomio tambien van a ser del tipo a. Luego como
-- vamos a devolver algo de tipo b, entonces la funcion constante debe ser del tipo (a -> b).
-- En cuanto a funcion suma, esto lo haremos entre dos polinomios, en particular, entre la recursion de dos polinomios. Como foldear
-- un polinomio nos devuelve algo de tipo b, entonces fSuma debe ser del tipo (b -> b -> b).
-- Finalmente para fProd, es la misma justificacion que fSuma.

foldPolinomio :: b -> (a -> b) -> (b -> b -> b) -> (b -> b -> b) -> Polinomio a -> b
foldPolinomio x fCte fSuma fProd polinomio =
    case polinomio of
        X                -> x
        Cte  cte         -> fCte cte
        Suma p1 p2 -> fSuma (foldPoliRec p1) (foldPoliRec p2)
        Prod p1 p2 -> fProd (foldPoliRec p1) (foldPoliRec p2)
    where foldPoliRec = foldPolinomio x fCte fSuma fProd


evaluar :: Num a => a -> Polinomio a -> a
evaluar x polinomio = foldPolinomio x id (+) (*) polinomio


-------------------------------------------------------
--------------------- Ejercicio 12 --------------------
-------------------------------------------------------

data AB a = Nil 
          | Bin (AB a) a (AB a)
          deriving (Eq, Show)
        
-- Para pensar por qué obtenemos este tipado, pensar lo mismo que pensamos en foldPolinomio
foldAB :: b -> (b -> a -> b -> b) -> AB a -> b
foldAB cNil fBin ab =
    case ab of
        Nil       -> cNil
        Bin i r d -> fBin (foldABRec i) r (foldABRec d) 
    where foldABRec = foldAB cNil fBin

-- Recordemos recr
-- recr :: (a -> [a] -> b -> b) -> b -> [a] -> b 
-- recr f z [] = z
-- recr f z (x : xs) = f x xs (recr f z xs)

-- Vemos que recr tiene un tipado de la pinta funcion -> casoBase -> elementoAProcesar -> Resultado
-- En particular, si analizamos la funcion, sabemos que tiene la cabeza de la lista x, xs, y luego rec xs.
-- Por lo que la funcion tiene el tipo (a -> [a] -> b -> b)
-- Para recAB queremos algo similar. recAB :: funcion -> casoBase -> AB a -> Resultado
--                                   recAB :: funcion -> b        -> AB a -> b
-- Pensemos el tipo de la funcion. Queremos tener i, r y d, pero ademas el resultado recursivo de i y d.
-- Por lo tanto tenemos que la funcion es del tipo: (AB a -> a -> AB a -> b -> b -> b)
--                                                   i -> r -> d -> reci -> recb -> Resultado
recAB :: (AB a -> a -> AB a -> b -> b -> b) -> b -> AB a -> b
recAB _ cNil Nil = cNil
recAB f cNil (Bin i r d) = f i r d (recAB f cNil i) (recAB f cNil d)

esNil :: AB a -> Bool
esNil ab = case ab of
    Nil -> True
    _   -> False

altura :: AB a -> Integer
altura ab = foldAB 0 (\alturaI _ alturaD -> 1 + max alturaI alturaD) ab

cantNodos :: AB a -> Integer
cantNodos ab = foldAB 0 (\nodosI _ nodosD -> 1 + nodosI + nodosD) ab

-- La función devuelve el mejor elemento del arbol según una función de comparación.
mejorSegunAB :: (a -> a -> a) -> AB a -> a
mejorSegunAB f (Bin i r d) = foldAB r (\mejorRamaI r mejorRamaD -> f (f mejorRamaI r) mejorRamaD) (Bin i r d)

-- Un árbol binario es de búsqueda si el valor de un nodo es mayor o igual que los valores que aparecen en el 
-- subárbol izquierdo y es estrictamente menor que los valores que aparecen en el subárbol derecho.
esABB :: Ord a => AB a -> Bool
esABB Nil = True
esABB ab = recAB (\i r d recI recD -> recI && recD && nivelValido i r d) True ab
    where 
        maxAB ab = mejorSegunAB (\x y -> if x > y then x else y) ab
        minAB ab = mejorSegunAB (\x y -> if x > y then y else x) ab
        nivelValido i r d = case (esNil i, esNil d) of
            -- Si llegamos hasta acá, recI && recD == True
            (True, True)   -> True -- como i y d son Nil, entonces debemos devolver True
            (False, True)  -> maxAB i <= r -- Como la subrama derecha es Nil la ignoramos, comparamos con la raiz izquierda
            (True, False)  -> minAB d > r -- Como la subrama izquierda es Nil, la ignoramos, comparamos con la raiz derecha
            (False, False) -> (maxAB i <= r) && (minAB d > r) -- Ninguna subrama es Nil, comparamos con ambas subramas.


-------------------------------------------------------
--------------------- Ejercicio 13 --------------------
-------------------------------------------------------

-- Queremos definir la funcion ramas, que da los distintos caminos desde la raíz hasta las hojas
ramas :: Eq a => AB a -> [[a]]
ramas t = foldAB [[]] (\recI r recD ->
        if (recI == [[]]) then      -- i es Nil
            if (recD == [[]]) then  -- i y d son Nil
                [[r]]
            else                 -- i es Nil, d es Bin
                map (r:) recD
        else                     -- i es Bin
            if (recD == [[]]) then  -- i es Bin, d es Null
                map (r:) recI
            else
                map (r:) recI ++ -- i y d son Bin
                map (r:) recD   
    ) t


cantHojas :: Eq a => AB a -> Integer
cantHojas t = foldAB 0 (\recI _ recD -> if (recI + recD) == 0 then 1 else (recI + recD)) t


espejo :: AB a -> AB a
espejo t = foldAB Nil (\recI r recD -> 
            case (esNil recI, esNil recD) of
            (True, True)   -> (Bin Nil r Nil)
            (False, True)  -> (Bin Nil r recI)
            (True, False)  -> (Bin recD r Nil)
            (False, False) -> (Bin recD r recI)
        ) t


mismaEstructura :: AB a -> AB b -> Bool
mismaEstructura Nil Nil = True
mismaEstructura Nil (Bin i r d) = False
mismaEstructura (Bin i r d) Nil = False
mismaEstructura (Bin i1 r1 d1) (Bin i2 r2 d2) = (mismaEstructura i1 i2) && (mismaEstructura d1 d2)

-------------------------------------------------------
--------------------- Ejercicio 14 --------------------
-------------------------------------------------------

data AIH a = Hoja a 
           | Bin' (AIH a) (AIH a)

foldAIH :: (b -> b -> b) -> (a -> b) -> AIH a -> b
foldAIH fb fh t = 
    case t of
        Hoja a    -> fh a 
        (Bin' i d) -> fb (foldAIHRec i) (foldAIHRec d)
    where foldAIHRec = foldAIH fb fh


alturaAIH :: AIH a -> Integer
alturaAIH t = foldAIH (\recI recD -> 1 + max recI recD) (\_ -> 1) t

-------------------------------------------------------
--------------------- Ejercicio 15 --------------------
-------------------------------------------------------

-- Los RoseTree son árboles no vacíos (No utilizaremos Nil), y con cantidad indeterminada de hijos, 
-- por lo que usaremos una lista de RoseTree para denotar los hijos.

data RoseTree a = RoseTree a [RoseTree a]

-- Se pide escribir el esquema de recursión estructural para RoseTree, es decir, foldRoseTree.
-- Pensemos cómo va a funcionar para luego escribirla. Fold en general, toma una función, un caso base, y el dato a foldear.
-- Definamos primero b como el tipo de dato a devolver. Por lo tanto, el caso base también debe ser del tipo b. 
-- Vamos a foldear un RoseTree a. Entonces, de qué tipo debe ser la función? 
-- Bueno, sabemos que va a recibir el dato de la raíz, y la recursion en sus ramas. Como la cantidad de ramas son indefinidas,
-- y queremos aplicar f a todos sus hijos, si la funcion devuelve algo de tipo b, entonces el resultado de la recursion sera [b].

-- La idea va a ser aplicarle f a la raíz, y luego aplicarle f a todas las ramas que salen de dicha raíz.

foldRoseTree :: (a -> [b] -> b) -> RoseTree a -> b
foldRoseTree f (RoseTree raiz ramas) = f raiz (map (foldRoseTree f) ramas)

-- dado un RoseTree, devuelve una lista con sus hojas ordenadas
-- Pensemos: recibimos un RoseTree de tipo a, queremos devolver una lista de elementos de tipo a.
-- Pensemos cómo armar la lambda que usará el fold. Recibimos primero un a, y luego queremos recibir la recursión en sus ramas.
-- Si al aplicarle a un nodo la funcion, nos devuelve una lista, entonces si le aplicamos el fold a toda la lista, vamos a tener una lista de listas.
hojas :: RoseTree a -> [a]
hojas t = 
    foldRoseTree (\raiz rec -> 
        if null rec then -- es hoja
            [raiz]
        else
            concat rec
    ) t

-- La idea va a ser que cada vez que se haga la recursión, se guarde la distancia "parcial" desde un nodo x hasta alguna hoja. 
distancias :: RoseTree a -> [Integer]
distancias t = 
    foldRoseTree (\raiz rec ->
        if null rec then
            [0]
        else
            map (+1) (concat rec)        
    ) t

-- Devuelve la altura de un RoseTree (la cantidad de nodos de la rama más larga). Si el
-- Si la rec es null, significa que estamos en una hoja, y en particular la altura si vemos esa misma hoja, es 0.
-- En cambio si no estamos en una hoja, la altura es 1 mas la maxima altura de sus ramas.
alturaRoseTree :: RoseTree a -> Integer
alturaRoseTree t = 
    foldRoseTree (\raiz rec ->
        if null rec
            then 1
        else
            1 + maximum rec
    ) t


-------------------------------------------------------
----------------- Ejercicios de Parcial ---------------
-------------------------------------------------------

-- https://www.cubawiki.com.ar/images/5/5d/PLP-2024-1ºC-Primer_Parcial-MOA.pdf

data AT a = NilT
          | Tri a (AT a) (AT a) (AT a)
          deriving (Eq, Show)


-- Vamos a escribir foldAT, vamos a recibir una función, un caso base, y el árbol.
-- Nos decidimos por b como el tipo del resultado, por ende, el caso base tambien será de tipo b.
-- Por otro lado, la función recibira el dato, la recursión en la rama izquierda, medio y derecha.
-- Como la recursión ya es un resultado, será del tipo b.
foldAT :: (a -> b -> b -> b -> b) -> b -> AT a -> b
foldAT f z t = 
    case t of
        NilT        -> z
        Tri r i m d -> f r (foldATRec i) (foldATRec m) (foldATRec d)
    where foldATRec = foldAT f z


preorder :: AT a -> [a]
preorder t = foldAT (\r recI recM recD -> [r] ++ recI ++ recM ++ recD) [] t


mapAT :: (a -> b) -> AT a -> AT b
mapAT f t = foldAT (\r recI recM recD -> Tri (f r) recI recM recD) NilT t


nivel :: AT a -> Int -> [a]
nivel t = 
    foldAT (\r recI recM recD n -> 
        if n == 0 then
            [r] 
        else 
            (recI (n - 1)) ++ (recM (n - 1)) ++ (recD (n - 1))) 
    (\_ -> []) t

    
-- https://www.cubawiki.com.ar/images/4/48/PLP_1recu_16_07_24_VF.pdf

data Prop = Var String
          | No Prop
          | Y Prop Prop
          | O Prop Prop
          | Imp Prop Prop

type Valuacion = String -> Bool


foldProp :: (String -> b) -> (b -> b) -> (b -> b -> b) -> (b -> b -> b) -> (b -> b -> b) -> Prop -> b
foldProp fStr fNo fY fO fImp prop =
    case prop of
        Var str   -> fStr str
        No p      -> fNo  (foldPropRec p)
        Y p1 p2   -> fY   (foldPropRec p1) (foldPropRec p2)
        O p1 p2   -> fO   (foldPropRec p1) (foldPropRec p2)
        Imp p1 p2 -> fImp (foldPropRec p1) (foldPropRec p2)
    where foldPropRec = foldProp fStr fNo fY fO fImp


recProp :: (String -> b) -> (Prop -> b -> b) -> (Prop -> Prop -> b -> b -> b) -> 
                                                        (Prop -> Prop -> b -> b -> b) -> 
                                                        (Prop -> Prop -> b -> b -> b) ->
                                                        Prop -> b
recProp fStr fNo fY fO fImp prop =
    case prop of
        Var str   -> fStr str
        No p      -> fNo p (recPropRec p)
        Y p1 p2   -> fY p1 p2 (recPropRec p1) (recPropRec p2)
        O p1 p2   -> fO p1 p2 (recPropRec p1) (recPropRec p2)
        Imp p1 p2 -> fImp p1 p2 (recPropRec p1) (recPropRec p2)
    where recPropRec = recProp fStr fNo fY fO fImp


variables :: Prop -> [String]
variables prop = 
    foldProp (\var -> [var]) 
             (\vars -> nub vars) 
             (\vars1 vars2 -> nub (vars1 ++ vars2)) 
             (\vars1 vars2 -> nub (vars1 ++ vars2)) 
             (\vars1 vars2 -> nub (vars1 ++ vars2))
             prop



evaluarProp :: Valuacion -> Prop -> Bool
evaluarProp val prop = 
    foldProp (\var -> val var)
             (\p -> not p)
             (\p1 p2 -> p1 && p2)
             (\p1 p2 -> p1 || p2)
             (\p1 p2 -> (not p1) || p2)
             prop


estaEnFNN :: Prop -> Bool
estaEnFNN prop = 
    recProp 
        (\_ -> True) 
        (\p rec -> rec && (case p of
            Var _ -> True
            _     -> False
        )) 
        (\_ _ rec1 rec2 -> rec1 && rec2)
        (\_ _ rec1 rec2 -> rec1 && rec2)
        (\_ _ _ _ -> False)
        prop



-- https://www.cubawiki.com.ar/images/0/06/PLP-1C-2025-1P-EI.pdf

data ABNV a = Hoja a
            | Uni a (ABNV a) 
            | Bi (ABNV a) a (ABNV a)


foldABNV :: (a -> b) -> (a -> b -> b) -> (b -> a -> b -> b) -> ABNV a -> b
foldABNV fHoja fUni fBi t = case t of
        Hoja r  -> f r
        Uni r uni -> fUni r (foldABNVrec uni)
        Bi bil r bir -> fBi (foldABNVrec bil) r (foldABNVrec bir)
    where foldABNVrec = foldABNV fHoja fUni fBi


recABNV :: (a -> b) -> (ABNV a -> a -> b -> b) -> (ABNV a -> a -> ABNV a -> b -> b -> b) -> ABNV a -> b
recABNV fHoja fUni fBi t = case t of
        Hoja r -> f r
        Uni r uni -> fUni uni r (recABNVrec uni)
        Bi bil r bir -> fBi bil r bir (recABNVrec bil) (recABNVrec bir)
    where recABNVrec = recABNV fHoja fUni fBi


elemABNV :: Eq a => a -> ABNV a -> Bool
foldABNV e t = foldABNV (\r -> r == e) (\r rec -> rec || r == e) (\recI r recD -> recI || recD || r == e) t


reemplazarUno :: Eq a -> a -> a -> ABNV a -> ABNV a
reemplazarUno x y t = 
    recABNV 
        (\r -> if r == x then Hoja y else Hoja r)
        (\uni r rec -> if r == x then Uni y uni else Uni r rec)
        (\bil r bir recI recD -> if r == x then Bi bil y bir else (if elemABNV x bil) then Bi recI r bir else Bi bil r recD)
        t


nivel :: ABNV a -> Int -> [a]
nivel t = foldABNV (\r n -> if n == 0 then [r] else [])
                   (\r recUni n -> if n == 0 then [r] else recUni (n - 1))
                   (\recI r recD n -> if n == 0 then [r] else (recI (n - 1)) ++ (recD (n - 1)))
                   t
