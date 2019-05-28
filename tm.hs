import Data.Char
import Data.List

--1. Implementar una máquina de Turing definiendo el tipo de dato algebraico
--MaqT con base en la definición formal de una máquina de Turing estándar.

-- Tipo para los estados de la máquina.
type State = Int

--  Tipo para los símbolos
type Symb = Char

-- Tipo para los alfabetos
type Alf = [Symb]

-- Tipo para las direcciones
data Dir = Izq | Der | Est

-- Tipo para las transiciones
type Trans = ((State, Symb), (State, Symb, Dir))

-- Tipo para la función de transición
type Delta = [Trans]

-- Tipo de máquina de Turing
type MaqT = ([State], Alf, Symb, Alf, State, [State], Delta)

--2. Definir la función compute que recibe una MaqT, una cadena e imprime
--el procesamiento formal de la cadena con configuraciones.

type Config = (State , String , Int)

compute :: MaqT -> String -> [ Config ]
compute m s
    | not (valida m) = error "máquina inválida"
    | elem (getBlank m) s = error "cadena inválida"
    | otherwise = deltaGen (getDelta m) [(getInit m, s, 0)] (getBlank m)

deltaGen :: Delta -> [Config] -> Symb -> [Config]
deltaGen _ [] _ = error "Configuración inválida"
deltaGen d conf@((st, s, n):_) b =
    let r = applyDelta d (st, getSymb s n b) in 
        case r of
            [] -> conf
            (nst, ns, dir):_ -> deltaGen d ((nst, writeSymb ns s n, nn):conf) b
                where nn = case dir of
                            Izq -> n - 1
                            Der -> n + 1
                            Est -> n                          


applyDelta :: Delta -> (State, Symb) -> [(State, Symb, Dir)]
applyDelta d (st, symb) = [(nst, ns, nd) | ((pst, ps),(nst, ns, nd)) <- d, pst == st, symb == ps]

--3. Definir la función accept que recibe una MaqT, una cadena y dice
--si la cadena es aceptada por la máquina de Turing.

accept :: MaqT -> String -> Bool
accept m s = elem f (getFinal m) where (f, _, _):_ = compute m s


--4. Definir la función encode que recibe una MaqT y la codifica para
--pasarla como entrada de la Máquina Universal.

encode :: MaqT -> String
encode m = let sts = getStates m; g = getGamma m; tr = getDelta m in
    foldr (\x xs -> x++"0"++xs) "" (map (encodeTrans sts g) tr)

encodeTrans :: [State] -> Alf -> Trans -> String
encodeTrans sts g ((st, s), (nst, ns, d)) = 
    "0"++(enc st sts)++"0"++ (enc s g)++"0"++ (enc nst sts)++"0"++ (enc ns g) ++ "0" ++ k++"0"
    where k = 
            case d of
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
gamma :: Alf
gamma = ['a', 'b', 'c', '_', 'X']

blank :: Symb
blank = '_'

sigma :: Alf
sigma = ['a', 'b', 'c']

i :: State
i = 0

--Extra
--1. Definir la función decode que recibe una String representando una máquina
--codificada y regresa la MaqT que representa.

decode :: String -> MaqT
decode s = let d = decodeTransAll s 
            in (extractStates d, extractGamma d, 'a', extractSigma d, 0, extractFinal d, d)
                

extractStates :: Delta -> [State]
extractStates d = foldr (\((st, _), (nst, _, _)) xs -> union [st, nst] xs) [] d

extractGamma :: Delta -> Alf
extractGamma d = foldr (\((_, s), (_, ns, _)) xs -> union [s, ns] xs) [] d

extractSigma :: Delta -> Alf
extractSigma d = (extractGamma d) \\ ['a'] -- Al ser 'a' el caracter con valor ASCII, es el blanco

extractFinal :: Delta -> [State]
extractFinal d = [foldr (\((st, _), (nst, _, _)) xs -> max (max st nst) xs) (-1) d]

decodeTransAll :: String -> Delta
decodeTransAll s = map decodeTrans (getCodedTrans s)

getCodedTrans :: String -> [String]
getCodedTrans [] = [[]]
getCodedTrans ss = (take n ss) : (getCodedTrans (drop n ss)) where n = getNextInd ss 0

getNextInd :: String -> Int -> Int
getNextInd [] _ = 0
getNextInd ('0':'0':_) n = n + 1
getNextInd (_:xs) n = getNextInd xs (n + 1)

decodeTrans :: String -> Trans
decodeTrans s = 
    let (xs1, xs2, xs3, xs4, xs5) = splitState s
    in ((decodeState xs1, decodeSymb xs2), (decodeState xs3, decodeSymb xs4, decodeDir xs5))


splitState :: String -> (String, String, String, String, String)
splitState s = 
    let (_, ini) = getNext s 
    in let (s1, xs1) = getNext ini 
        in let (s2, xs2) = getNext xs1 
            in let (s3, xs3) = getNext xs2 
                in let (s4, xs4) = getNext xs3 
                    in let (s5, _) = getNext xs4 
                        in (s1, s2, s3, s4, s5)

decodeState :: String -> State
decodeState st = (length st) - 1

decodeSymb :: String -> Symb
decodeSymb s = chr ((length s) + 47) -- caracteres en ASCII a partir de '0'

decodeDir :: String -> Dir
decodeDir "1" = Der
decodeDir "11" = Izq
decodeDir "111" = Est
decodeDir _ = error "estado invalido"

getNext :: String -> (String, String)
getNext s = getNextTail s []

getNextTail :: String -> String -> (String, String)
getNextTail _ []= error "no hay siguiente"
getNextTail sx ('0':xs) = (sx, xs) 
getNextTail sx (x:xs) = getNextTail (sx++[x]) xs

-- Auxiliares
-- Si es subconjunto
subset :: (Eq a) => [a] -> [a] -> Bool
subset [] _ = True
subset _ [] = False
subset (x:xs) ys
    | elem x ys = subset xs ys
    | otherwise = False

-- Sacar componentes de la máquina
getStates :: MaqT -> [State]
getStates (st, _, _, _, _, _, _) = st

getGamma :: MaqT -> Alf
getGamma (_, g, _, _, _, _, _) = g

getBlank :: MaqT -> Symb
getBlank (_, _, b, _, _, _, _) = b

getSigma :: MaqT -> Alf
getSigma (_, _, _, s, _, _, _) = s

getInit :: MaqT -> State
getInit (_, _, _, _, ini, _, _) = ini

getFinal :: MaqT -> [State]
getFinal (_, _, _, _, _, f, _) = f

getDelta :: MaqT -> Delta
getDelta (_, _, _, _, _, _, d) = d


-- Restricciones entre los diferentes componentes de MaqT que no se puede
-- restringir solo con el tipo
valida :: MaqT -> Bool
valida (st, gam, b, eps, ini, fin, _) =
    elem b gam && not (elem b eps) && elem ini st && subset eps gam && subset fin st

-- Obtner el símbolo en la n-ésima posición.
getSymb :: String -> Int -> Symb -> Symb
getSymb s n b
    | n < 0 || n > length s = b
    | otherwise = s !! n

-- Escirbir el símbolo en la n-ésima posición.
writeSymb :: Symb -> String -> Int -> String
writeSymb s ss n
    | n < 0 || n > length ss = error "posición fuera de rango"
    | otherwise = (take n ss) ++ [s] ++ (drop (n+1) ss)
