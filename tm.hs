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

type Config = ( State , String , Int )

compute :: MaqT -> String -> [ Config ]
compute m s
    | not (valida m) = error "máquina inválida"
    | elem (getBlank m) s = error "cadena inválida"
    | otherwise = deltaGen (getDelta m) [(getInit m, s, 0)] 

deltaGen :: Delta -> [Config] -> [Config]
deltaGen d conf@((st, s, n):cs) =
    let r = d st (getSymb s n) in
        case r of
            [] -> c
            (nst, ns, d):_ -> deltaGen ((nst, nc, nn):conf)
                where nc = writeSymb ns s n; nn = case d of
                                                Izq -> n - 1
                                                Der -> n + 1
                                                Est -> n

applyDelta :: Delta -> (State, Symb) -> [(State, Symb, Dir)]
applyDelta d (st, symb) = [(nst, ns, nd) | pst == st && symb == ps ,((pst, ps),(nst, ns, nd)) <- d]

--3. Definir la función accept que recibe una MaqT, una cadena y dice
--si la cadena es aceptada por la máquina de Turing.

accept :: MaqT -> String -> Bool
accept m s = elem f (getFinal m) where (f, _, _):_ = compute m s


--4. Definir la función encode que recibe una MaqT y la codifica para
--pasarla como entrada de la Máquina Universal.

encode :: MaqT -> String
encode m = let sts = getStates m; g = getGamma m; tr = getDelta m in
    foldr (\x xs -> x++"0"++xs) "" (map (encondeTrans sts g) tr)

encodeTrans :: [State] -> Alf -> Trans -> String
encodeTrans sts g ((st, s), (nst, ns, d)) = 
    "0"++(enc st sts)++"0"++ (enc s g)++"0"++ (enc nst sts)++"0"++ k++"0"
    where k = 
            case d of
                Izq -> "11"
                Der -> "1"
                Est -> "111"

enc :: a -> [a] -> String
enc a as = 
    let r = elemIndex a as
    in case r of
        Nothing -> ""
        Just n -> replicate (n+1) '1' 


--5. Utilizando el tipo de dato algebraico MaqT definir la máquina de Turing
--que acepte el lenguaje L = {a^n b^n c^n } y mostrar la formalización de la cadena aabbcc



--Extra
--1. Definir la función decode que recibe una String representando una máquina
--codificada y regresa la MaqT que representa.
decode :: String -> MaqT


-- Auxiliares
-- Si es subconjunto
subset :: [a] -> [a] -> Bool
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

getSigma :: MaqT -> Symb
getSigma (_, _, _, s, _, _, _) = s

getInit :: MaqT -> State
getInit (_, _, _, _, i, _, _) = i

getFinal :: MaqT -> Symb
getFinal (_, _, _, _, _, f, _) = f

getDelta :: MaqT -> Delta
getDelta (_, _, _, _, _, _, d) = d


-- Restricciones entre los diferentes componentes de MaqT que no se puede
-- restringir solo con el tipo
valida :: MaqT -> Bool
valida (st, gamma, b, eps, init, fin, d) =
    elem b gamma && not (elem b eps) && elem init st && subset eps gamma && subset fin st

-- Obtner el símbolo en la n-ésima posición.
getSymb :: String -> Int -> Symb
getSymb s n = s !! n

-- Escirbir el símbolo en la n-ésima posición.
writeSymb :: Symb -> String -> Int -> String
writeSymb s n = s !! n
