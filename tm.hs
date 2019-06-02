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

instance Show Dir where
    show Izq = "<-"
    show Der = "->"
    show Est = "--"

-- Sinónimos para los alfabetos
type Alf = [Symbol]

-- Sinónimo para las transiciones
type Trans = ((State, Symbol), (State, Symbol, Dir)) 

-- Sinónimo para la función de transición
-- Al ser una función parcial, se define sólo como el conjunto de transiciones
-- que la representan.
type Delta = [Trans]

-- Tipo de máquina de Turing
data MaqT = MT {q::[State], s::Alf, g::Alf, d::Delta, b::Symbol, q0::State, f::[State]} deriving (Show, Eq)
-- q: conjunto de estados
-- s: alfabeto del lenguaje
-- g: alfabeto de la cinta
-- d: función de transición
-- q0: estado inicial
-- b: símbolo en blanco
-- f: conjunto de estados finales

-- Restricciones entre los diferentes componentes de MaqT que no se puede
-- restringir solo con el tipo
valida :: MaqT -> Bool
valida mt  =
    elem (b mt) (g mt) && not (elem (b mt) (s mt)) && elem (q0 mt) (q mt) 
    && subset (s mt) (g mt) && subset (f mt) (q mt) && isFunc (d mt)

--2. Definir la función compute que recibe una MaqT, una cadena e imprime
--el procesamiento formal de la cadena con configuraciones.

data Tape = T {tp::String, z::Int} deriving (Eq)

instance Show Tape where
    show t = "(" ++ (show (tp t)) ++ ", " ++ (show ( z t)) ++ ")"

type Config = (State , Tape , Int)

compute :: MaqT -> String -> [Config]
compute mt str
    | not (valida mt) = error "máquina inválida"
    | elem (b mt) str || not (subset str (s mt)) = error "cadena inválida"
    | otherwise = deltaGen mt [(q0 mt, T {tp = str, z = 0}, 0)]

-- Generación de la lista de configuraciones
deltaGen :: MaqT -> [Config] -> [Config]
deltaGen _ [] = error "Configuración inválida"
deltaGen mt conf@((qn, t, n):_) =
    let (rs, nt) = getSymbol t n (b mt); r = applyDelta (d mt) (qn, rs) in 
        case r of
            [] -> conf
            (qn', ns, dir):_ -> deltaGen mt ((qn', nnt, nn):conf)
                where nn = case dir of
                            Izq -> n - 1
                            Der -> n + 1
                            Est -> n
                      ; (_, nnt) = getSymbol (writeSymbol nt n ns (b mt)) nn (b mt)


applyDelta :: Delta -> (State, Symbol) -> [(State, Symbol, Dir)]
applyDelta del (st, symbol) = [(nst, ns, nd) | ((pst, ps),(nst, ns, nd)) <- del, pst == st, symbol == ps]

--3. Definir la función accept que recibe una MaqT, una cadena y dice
--si la cadena es aceptada por la máquina de Turing.

accept :: MaqT -> String -> Bool
accept mt str = elem qk (f mt) where (qk, _, _):_ = compute mt str


--4. Definir la función encode que recibe una MaqT y la codifica para
--pasarla como entrada de la Máquina Universal.

encode :: MaqT -> String
encode mt = foldr (\x xs -> x ++ xs) "" (map (encodeTrans (q mt) (g mt)) (d mt))

encodeTrans :: [State] -> Alf -> Trans -> String
encodeTrans qs gs ((p, str), (q', ns, dir)) = 
    "0"++(enc p qs)++"0"++ (enc str gs)++"0"++ (enc q' qs)++"0"++ (enc ns gs) ++ "0" ++ k ++"0"
    where k = 
            case dir of
                Izq -> "11"
                Der -> "1"
                Est -> "111"

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

delta :: Delta
delta = [
    ((Q 0, 'X'), (Q 0, 'X', Der)),
    ((Q 0, 'a'), (Q 1, 'X', Der)),
    ((Q 1, 'a'), (Q 1, 'a', Der)),
    ((Q 1, 'X'), (Q 1, 'X', Der)),
    ((Q 1, 'b'), (Q 2, 'X', Der)),
    ((Q 2, 'b'), (Q 2, 'b', Der)),
    ((Q 2, 'X'), (Q 2, 'X', Der)),
    ((Q 2, 'c'), (Q 3, 'X', Est)),
    ((Q 3, 'X'), (Q 3, 'X', Izq)),
    ((Q 3, 'b'), (Q 3, 'b', Izq)),
    ((Q 3, 'a'), (Q 0, 'a', Est)),
    ((Q 3, '_'), (Q 4, '_', Der)),
    ((Q 4, 'X'), (Q 4, 'X', Der)),
    ((Q 4, '_'), (Q 0, '_', Est))
    ]

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
process :: [Config]
process = compute m1 "aabbcc"

--}
--Extra
--1. Definir la función decode que recibe una String representando una máquina
--codificada y regresa la MaqT que representa.

decode :: String -> MaqT
decode str = let del = map decodeTrans (getCodedTrans str)
            in MT {
                    q = extractStates del, 
                    s = extractSigma del, 
                    g = extractGamma del, 
                    d = del,
                    b = '0',
                    q0 = Q 0, 
                    f = [Q 1]
                }                           

-- Sacando los estado desde la función de transición
-- Se supone que q0 es el inicial y q1 el final
extractStates :: Delta -> [State]
extractStates del = foldr (\((st, _), (nst, _, _)) xs -> union (union [st] [nst]) xs) [Q 0, Q 1] del

-- Sacando el alfabeto de la cinta la función de transición
-- Se toma a '0' como blanco
extractGamma :: Delta -> Alf
extractGamma del = foldr (\((_, str), (_, ns, _)) xs -> union (union [str] [ns]) xs) ['0'] del

-- Al ser '1' el primer caracter 
-- usado en la decodifcación, '0' es el blanco
extractSigma :: Delta -> Alf
extractSigma del = [sy | sy <- (extractGamma del), sy /= '0']

getCodedTrans :: String -> [String]
getCodedTrans [] = []
getCodedTrans ss = let (x1, x2) = getCodedTransAux [] ss
                    in x1:(getCodedTrans x2)

getCodedTransAux :: String -> String -> (String, String)
getCodedTransAux sx []= (sx, "")
getCodedTransAux sx ('0':'0':xs) = (sx++['0'], '0':xs) 
getCodedTransAux sx (x:xs) = getCodedTransAux (sx++[x]) xs

decodeTrans :: String -> Trans
decodeTrans str = 
    let (xs1, xs2, xs3, xs4, xs5) = splitTrans str
    in ((decodeState xs1, decodeSymbol xs2), (decodeState xs3, decodeSymbol xs4, decodeDir xs5))

-- Parte una cadena que codifica a una transición en sus diferentes partes
splitTrans :: String -> (String, String, String, String, String)
splitTrans str = tuple5 (getParts str)

tuple5 :: [a] -> (a, a, a, a, a)
tuple5 [a1, a2, a3, a4, a5] = (a1, a2, a3, a4, a5)
tuple5 _ = error "lista no 5-tuplificable"

getParts :: String -> [String]
getParts [] = []
getParts ss = let (x1, x2) = getPartsAux [] ss
                    in [str | str <- x1:(getParts x2), str /= ""]

getPartsAux :: String -> String -> (String, String)
getPartsAux sx "" = (sx, "")
getPartsAux sx ('0':xs) = (sx,xs) 
getPartsAux sx (x:xs) = getPartsAux (sx++[x]) xs

decodeState :: String -> State
decodeState st = Q ((length st) - 1)

-- caracteres en ASCII a partir de '1'
-- '0' está reservado para el blanco
decodeSymbol :: String -> Symbol
decodeSymbol str = chr ((length str) + 48) 

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
getSymbol :: Tape -> Int -> Symbol -> (Symbol, Tape)
getSymbol t n blank
    | indx < 0 =  getSymbol (addToTape t indx blank) n blank 
    | indx >= l = getSymbol (addToTape t (indx - l + 1) blank) n blank
    | otherwise = ((tp t) !! indx, t)
    where indx = n + (z t); l = length(tp t)

-- Escribir el símbolo en la n-ésima posición.
writeSymbol :: Tape -> Int -> Symbol -> Symbol-> Tape
writeSymbol t n sym blank
    | indx < 0 = writeSymbol (addToTape t indx blank) n sym blank 
    | indx >= l = writeSymbol (addToTape t (indx - l) blank) n sym blank
    | otherwise = T {tp = ns, z = (z t)}
    where indx = n + (z t); l = length (tp t); 
            ns = (take indx (tp t)) ++ [sym] ++ (drop (indx+1) (tp t))

addToTape :: Tape -> Int -> Symbol -> Tape
addToTape t n sy
    | n < 0 = let nchunk = replicate (-n) sy in T {tp = nchunk ++ (tp t), z = (z t) - n}
    | otherwise = let nchunk = replicate n sy in T {tp = (tp t) ++ nchunk, z = (z t)}


{--enumTrans :: Delta -> [State] -> Alf -> [Trans]
enumTrans del qs alf = foldr (\x xs -> enumTransAux del x xs) [] [(p, sy) | p <- qs, sy <- alf]

enumTransAux :: Delta -> (State, Symbol) -> [Trans] -> [Trans]
enumTransAux del (p, sy) ts = 
    let r = del p sy
        in case r of
            Nothing -> ts
            Just t -> (t:ts)--}
-- Si una lista de tuplas representa una función
isFunc :: (Eq a, Eq b) => [(a, b)] -> Bool
isFunc [] = True
isFunc  ((xi, _):xs) = 
    let r = evalFunc xs xi in
        case r of
            Nothing -> True
            Just _ -> False

evalFunc :: (Eq a, Eq b) => [(a, b)] -> a -> Maybe b
evalFunc [] _ = Nothing
evalFunc ((xi, xf):xs) x
    | xi == x = Just xf
    | otherwise =  evalFunc xs xi