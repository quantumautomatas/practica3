import Data.Char
import Data.List

--1. Implementar una máquina de Turing definiendo el tipo de dato algebraico
--MaqT con base en la definición formal de una máquina de Turing estándar.

-- Tipo para los estados de la máquina.
data State = Q Int deriving (Show, Eq, Ord)

--  Sinónimo para los símbolos
type Symbol = Char

-- Tipo para las direcciones
data Dir = Izq | Der | Est deriving (Eq)

-- Sinónimos para los alfabetos
type Alf = [Symbol]

-- Sinónimo para las transiciones
type Trans = ((State, Symbol), (State, Symbol, Dir))

-- Sinónimo para la función de transición
type Delta = (State, Symbol) -> Maybe (State, Symbol, Dir)

-- Tipo de máquina de Turing
data MaqT = MT {q::[State], s::Alf, g::Alf, d::Delta, b::Symbol, q0::State, f::[State]}
-- q: Conjunto de estados
-- s: Alfabeto del lenguaje
-- g: Alfabeto de la cinta
-- d: Función de transición
-- q0: Estado inicial
-- b: Símbolo en blanco
-- f: Conjunto de estados finales

--2. Definir la función compute que recibe una MaqT, una cadena e imprime
--el procesamiento formal de la cadena con configuraciones.

type Config = (State , String , Int)

compute :: MaqT -> String -> [Config]
compute mt str
    | not (valida mt) = error "máquina inválida"
    | elem (b mt) str || not (subset str (s mt)) = error "cadena inválida"
    | otherwise = deltaGen mt [(q0 mt, str, 0)]

-- Generando la lista de configuraciones
deltaGen :: MaqT -> [Config] -> [Config]
deltaGen _ [] = error "Configuración inválida"
deltaGen mt conf@((qn, t, n):_) =
    let (rs, nt, ni) = getSymbol t n (b mt); r = (d mt) (qn, rs) in -- evaluando la delta
        case r of
            Nothing -> conf -- Si no hay transición, acaba y devuelve la lista acumulada
            Just (qn', ns, dir) -> deltaGen mt ((qn', nnnt, nnnni):conf) -- Si
                where nni = case dir of -- hay transición, se agrega la
                            Izq -> ni - 1 -- configuración asociada a la lista y
                            Der -> ni + 1 -- se sigue computando
                            Est -> ni
                      ; (nnt, _) = (writeSymbol nt ni ns (b mt)) -- Escribiendo símbolo en la cinta
                      ; (_, nnnt, nnnni) = getSymbol  nnt nni (b mt) -- Moviendo la cabeza de la cinta


--3. Definir la función accept que recibe una MaqT, una cadena y dice
--si la cadena es aceptada por la máquina de Turing.

accept :: MaqT -> String -> Bool
accept mt str = elem qk (f mt) where (qk, _, _):_ = compute mt str


--4. Definir la función encode que recibe una MaqT y la codifica para
--pasarla como entrada de la Máquina Universal.

encode :: MaqT -> String
encode mt = foldr (\x xs -> x ++ xs) "" (map (encodeTrans (q mt) (g mt)) (enumTrans (d mt) (q mt) (g mt)))

-- Codifica una transición. Para esto, requiere de la lista de estados y la
-- lista de símbolos, para saber su posición y en base a eso darles un valor.
encodeTrans :: [State] -> Alf -> Trans -> String
encodeTrans qs gs ((p, str), (q', ns, dir)) =
    "0"++(enc p qs)++"0"++ (enc str gs)++"0"++ (enc q' qs)++"0"++ (enc ns gs) ++ "0" ++ k ++"0"
    where k =
            case dir of
                Izq -> "11"
                Der -> "1"
                Est -> "111"

-- Codifica un elemento de una lista en base a su posición en la lista.
enc :: (Eq a) => a -> [a] -> String
enc a as =
    let r = elemIndex a as
    in case r of
        Nothing -> ""
        Just n -> replicate (n+1) '1'


--5. Utilizando el tipo de dato algebraico MaqT definir la máquina de Turing
--que acepte el lenguaje L = {a^n b^n c^n } y mostrar la formalización de la cadena aabbcc

-- Idea
-- q0: busca la primera 'a' y la marca
-- q1: busca la primera 'b' y la marca
-- q2: busca la primera 'c' y la marca
-- q3: se regresa en la cadena hasta la primera 'a' que encuentre
-- q4: recorre la cadena revisando que todos los símbolo estén marcados

{--
La máquina de Turing se ve graficamente de la siguiente forma

              X/X,->
              a/a,->
              _____
              \   /
               q1  \
      a/X,->  />     \
            /          \ b/X,->
X/X,->__   /             \     __
    /   \ /               \>  /  \ X/X,->
    \__ >|q0|                q2   \ b/b,->
         \>    \>  a/a,-      /\__/
          \      \          /
   _/_,-   \        \      / c,X/-
            \    _/_,->      \</
            q4 <------- q3
           /  \        /  \
          /____\      /____\
          X,X/->      X/X,<-
                      b/b,<-
--}



delta :: Delta
delta (Q 0, 'X') = Just (Q 0, 'X', Der)
delta (Q 0, 'a') = Just (Q 1, 'X', Der)
delta (Q 1, 'a') = Just (Q 1, 'a', Der)
delta (Q 1, 'X') = Just (Q 1, 'X', Der)
delta (Q 1, 'b') = Just (Q 2, 'X', Der)
delta (Q 2, 'b') = Just (Q 2, 'b', Der)
delta (Q 2, 'X') = Just (Q 2, 'X', Der)
delta (Q 2, 'c') = Just (Q 3, 'X', Est)
delta (Q 3, 'X') = Just (Q 3, 'X', Izq)
delta (Q 3, 'b') = Just (Q 3, 'b', Izq)
delta (Q 3, 'a') = Just (Q 0, 'a', Est)
delta (Q 3, '_') = Just (Q 4, '_', Der)
delta (Q 4, 'X') = Just (Q 4, 'X', Der)
delta (Q 4, '_') = Just (Q 0, '_', Est)
delta _ = Nothing


m1 :: MaqT
m1 = MT {
    q = [Q 0, Q 1, Q 2, Q 3, Q 4],
    s = ['a', 'b', 'c'],
    g = ['a', 'b', 'c', '_', 'X'],
    d = delta,
    b = '_',
    q0 = Q 0,
    f = [Q 0]
}

-- Formalización
fm'aabbcc' :: [Config]
fm'aabbcc' = compute m1 "aabbcc"

--}
--Extra
--1. Definir la función decode que recibe una String representando una máquina
--codificada y regresa la MaqT que representa.

-- se supone que la máquina de Turing estaba en forma estándar antes de ser
-- codificada, esto es que su estado incial era q0, los finales eran [q1]
-- y que el símbolo en blanco era el primero en la lista del alfabeto de la cinta
decode :: String -> MaqT
decode str = let enDel = map decodeTrans (getCodedTrans str)
            in MT {
                    q = union (extractStates enDel) [Q 0, Q 1], -- q0 y q1 siempre presentes
                    s = extractSigma enDel,
                    g = union (extractGamma enDel) ['0'], -- siempre el blanco
                    d = evalFunc enDel,
                    b = '0',
                    q0 = Q 0,
                    f = [Q 1]
                }

-- Sacando los estado desde la función de transición
extractStates :: [Trans] -> [State]
extractStates del = foldr (\((st, _), (nst, _, _)) xs -> union (union [st] [nst]) xs) [] del

-- Sacando el alfabeto de la cinta la función de transición
-- Se toma a '0' como blanco
extractGamma :: [Trans] -> Alf
extractGamma del = foldr (\((_, str), (_, ns, _)) xs -> union (union [str] [ns]) xs) [] del

-- Al ser '0' el primer caracter usado en la decodifcación, '0' es el blanco
extractSigma :: [Trans] -> Alf
extractSigma del = [sy | sy <- (extractGamma del), sy /= '0']

-- Toma una codificación de una máquina de Turing y devuelve las cadenas
-- que representan la codificación de las transiciones de la máquina.
-- Hace un 'split' donde encuentre dos '0'.
getCodedTrans :: String -> [String]
getCodedTrans [] = []
getCodedTrans ss = let (x1, x2) = getCodedTransAux [] ss
                    in x1:(getCodedTrans x2)

-- Toma una codificación de una máquina de Turing y devuelve dos cadenas.
-- La primera representa la siguiente transición codificada, y la otra es todo
-- lo que sobra de la codificación.
getCodedTransAux :: String -> String -> (String, String)
getCodedTransAux sx []= (sx, "")
getCodedTransAux sx ('0':'0':xs) = (sx++['0'], '0':xs)
getCodedTransAux sx (x:xs) = getCodedTransAux (sx++[x]) xs

-- Dada la codificación de una transición, obtiene la transición
decodeTrans :: String -> Trans
decodeTrans str =
    let (xs1, xs2, xs3, xs4, xs5) = tuple5 (getParts str)
    in ((decodeState xs1, decodeSymbol xs2), (decodeState xs3, decodeSymbol xs4, decodeDir xs5))

-- Toma una lista de exactamente 5 elementos y devuelve una tupla con esos cinco
-- elementos
tuple5 :: [a] -> (a, a, a, a, a)
tuple5 [a1, a2, a3, a4, a5] = (a1, a2, a3, a4, a5)
tuple5 _ = error "lista no 5-tuplificable"

-- Toma una codifiación de una cadena y devuelve una lista con las partes de la
-- transición en orden [estado, símbolo, estado, símbolo, dirección]
getParts :: String -> [String]
getParts [] = []
getParts ss = let (x1, x2) = getPartsAux [] ss
                    in [str | str <- x1:(getParts x2), str /= ""]

-- Toma la codificación de una transición (o parte de ella) y devuelve dos cadenas.
-- La primera es la siguiente parte de la transición (estados, símbolo o direcciones)
-- y la segunda es el resto de lo que queda de la codificación.
getPartsAux :: String -> String -> (String, String)
getPartsAux sx "" = (sx, "")
getPartsAux sx ('0':xs) = (sx,xs)
getPartsAux sx (x:xs) = getPartsAux (sx++[x]) xs

-- Obtener un estado dada su codificación
decodeState :: String -> State
decodeState st = Q ((length st) - 1)

-- caracteres en ASCII a partir de '0'
-- '0' siempre es el blanco
decodeSymbol :: String -> Symbol
decodeSymbol str = chr ((length str) + 47)

-- Obtener una dirección dada su codificación
decodeDir :: String -> Dir
decodeDir "1" = Der
decodeDir "11" = Izq
decodeDir "111" = Est
decodeDir _ = error "estado invalido"

-- Auxiliares
-- Si es subconjunto
subset :: (Eq a) => [a] -> [a] -> Bool
subset [] _ = True
subset _ [] = False
subset (x:xs) ys
    | elem x ys = subset xs ys
    | otherwise = False

-- Obtner el símbolo en la n-ésima posición.
getSymbol :: String -> Int -> Symbol -> (Symbol, String, Int)
getSymbol t n blank
    | n < 0 =  getSymbol (addToTape t n blank) 0 blank
    | n >= l = getSymbol (addToTape t (n - l + 1) blank) n blank
    | otherwise = (t !! n, t, n)
    where l = length t

-- Escribir el símbolo en la n-ésima posición.
writeSymbol :: String -> Int -> Symbol -> Symbol -> (String, Int)
writeSymbol t n sym blank
    | n < 0 = writeSymbol (addToTape t n blank) 0 sym blank
    | n >= l = writeSymbol (addToTape t (n - l + 1) blank) n sym blank
    | otherwise = ((take n t) ++ [sym] ++ (drop (n+1) t), n)
    where l = length t

-- Alarga una cadena para simular que es infinita
addToTape :: String -> Int -> Symbol -> String
addToTape t n sy
    | n < 0 = replicate (-n) sy ++ t
    | otherwise = t ++ (replicate n sy)

-- Función para pasar una función Delta a la lista de transiciones que representa
enumTrans :: Delta -> [State] -> Alf -> [Trans]
enumTrans del qs alf = foldr (\x xs -> enumTransAux del x xs) [] [(p, sy) | p <- qs, sy <- alf]

-- Intenta evaluar una posible entrada en una función parcial.
-- Si existen, la agrega a la enumeración.
-- Si no, no hace nada.
enumTransAux :: Delta -> (State, Symbol) -> [Trans] -> [Trans]
enumTransAux del p'sy ts =
    let r = del p'sy
        in case r of
            Nothing -> ts
            Just t -> ((p'sy, t):ts)

-- Simula la evaluación de una función parcial cuya representación es una lista
-- de tuplas
evalFunc :: (Eq a, Eq b) => [(a, b)] -> a -> Maybe b
evalFunc [] _ = Nothing
evalFunc ((xi, xf):xs) x
    | xi == x = Just xf
    | otherwise =  evalFunc xs xi

-- Restricciones entre los diferentes componentes de MaqT que no se puede
-- restringir solo con el tipo
valida :: MaqT -> Bool
valida mt  =
    elem (b mt) (g mt) && not (elem (b mt) (s mt)) && elem (q0 mt) (q mt)
    && subset (s mt) (g mt) && subset (f mt) (q mt)
    && subset (extractGamma (enumTrans (d mt) (q mt) (g mt))) (g mt)
    && subset (extractStates (enumTrans(d mt) (q mt) (g mt))) (q mt)
