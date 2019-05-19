--1. Implementar una máquina de Turing definiendo el tipo de dato algebraico
--MaqT con base en la definición formal de una máquina de Turing estándar.

-- Tipo para los estados de la máquina.
type State = Int

--  Tipo para los símbolos
type Symb = Char

-- Tipo para los afabetos
type Alf = [Symb]

-- Tipo para las direcciones
data Dir = Izq | Der | Est

-- Tipo para la función de transición
type Delta = (State, Symb) -> Maybe (State, Symb, Dir)

type MaqT = ([State], Alf, Symb, Alf, State, [State], Delta)

--2. Definir la función compute que recibe una MaqT, una cadena e imprime
--el procesamiento formal de la cadena con configuraciones.

type Config = ( State , String , Int )

compute :: MaqT -> String -> [ Config ]
compute m s
    | not (valida m) = error "máquina inválida"
    | elem (getBlank m) s = error "cadena inválida"
    | otherwise = applyDelta (getDelta m) [(getInit m, s, 0)] 

applyDelta :: Delta -> [Config] -> [Config]
applyDelta d c@((st, s, n):cs) =
    let r = d st (getSymb s n) in
        case r of
            Nothing -> c
            Just (nst, ns, d) -> applyDelta ((nst, nc, nn):c)
                where nc = setSymb ns s n; nn = case d of
                                            Izq -> n - 1
                                            Der -> n + 1
                                            Est -> n



applyDeltaOnce :: Delta -> Config -> Config


    
--3. Definir la función accept que recibe una MaqT, una cadena y dice
--si la cadena es aceptada por la máquina de Turing.

accept :: MaqT -> String -> Bool


--4. Definir la función encode que recibe una MaqT y la codifica para
--pasarla como entrada de la Máquina Universal.

encode :: MaqT -> String


--5. Utilizando el tipo de dato algebraico MaqT definir la máquina de Turing
--que acepte el lenguaje L = {a^n b^n c^n } y mostrar la formalización de la cadena aabbcc



--Extra
--1. Definir la función decode que recibe una String representando una máquina
--codificada y regresa la MaqT que representa.
decode :: String -> MaqT


-- Auxiliares
-- Si es subconjunto
subset :: [A] -> [A] -> Bool
subset [] _ = True
subset _ [] = False
subset (x:xs) ys
    | elem x ys = subset xs ys
    | otherwise = False

-- Sacar componentes de la máquina
getDelta :: MaqT -> Delta
getDelta (_, _, _, _, _, _, d) = d

getInit :: MaqT -> State
getDelta (_, _, _, _, i, _, _) = i

getBlank :: MaqT -> Symb
getDelta (_, _, b, _, _, _, _) = b

-- Restricciones entre los diferentes componentes de MaqT que no se puede
-- restringir solo con el tipo
valida :: MaqT -> Bool
valida (st, gamma, b, eps, init, fin, d) =
    elem b gamma && not (elem b eps) && elem init st && subset eps gamma && subset fin st

-- Obtner el símbolo en la n-ésima posición.
getSymb :: String -> Int -> Symb
getSymb s n = s !! n

-- Escirbir el símbolo en la n-ésima posición.
setSymb :: Symb -> String -> Int -> String
setSymb s n = s !! n
