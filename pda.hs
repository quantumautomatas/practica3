module PDA where
    import Data.List

    -- Tipo de dato algebraico para simular los estados de un autómata
    data State = Q Int deriving(Show, Eq)
    
    -- Sinónimos para símbolos y alfabetos
    type Symbol = Char
    type Alphabet = [Symbol]
    data Sigma = S Symbol | E
    
    -- Sinónimo para la función de transición
    type Delta = (State, Sigma, Symbol) -> [(State, [Symbol])]
    
    -- Autómata de pila: PDA = <Q, S, G, d, q0, Z0, F>
    data Automata = PDA {
                            q :: [State], -- Cjto. de edos.
                            s :: Alphabet, -- Alfabeto de entrada
                            g :: Alphabet, -- Alfabeto de la pila
                            d :: Delta,    -- Func. de transición
                            q0:: State,   -- Edo. inicial
                            z0:: Symbol,  -- Símbolo al fondo de la pila
                            f :: [State]  -- Cjto. de edos. finales
                        }
    
    -- Sinónimo para la pila
    type Stack = [Symbol]
    
    -- Comportamientos para la pila
    -- Sacar el símbolo al tope
    pop :: Stack -> Stack
    pop [] = []
    pop xs = tail xs
    
    -- Agregar un símbolo al tope
    push :: Stack -> Symbol -> Stack
    push [] a   = [a]
    push stck a = [a]++stck
    
    -- Agregar múltiples símbolos a la pila
    multipush :: Stack -> [Symbol] -> Stack
    multipush stk ns = ns ++ stk
    
    -- Sinónimo para la Máquina
    type Machine = (Automata, Stack)
    
    -- Sinónimo para las configuraciones
    type Config = (State, String, Stack)
    
    -- Función para procesar cadenas
    compute :: Machine -> String -> [[Config]]
    compute (au, stk) str = computeAux [[(q0 au, str, push stk (z0 au))]] au

    -- Función para procesar cadenas, con recursión de cola
    computeAux:: [[Config]] -> Automata -> [[Config]]
    computeAux cs a = 
        let (tc, ht) = decideCompute cs a
            in case tc of
                [] -> ht
                _ -> ht ++ (computeAux tc a)

    -- Toma una lista de procesamientos de cadenas [[Config]], y las divide en 
    -- dos grupos:
    -- -Aquellas donde ya no hay caminos posibles para continuar la ejecución
    -- -Aquellas donde aún hay caminos posibles para continuar
    -- Luego, en el caso de las lista donde aún es posible continuar, genera dichos
    -- posibles caminos.
    -- Y devuelve dos listas:
    -- - Las nuevas listas de configuración
    -- - Las antiguas listas de configuración que no pueden ser continuadas.
    decideCompute :: [[Config]] -> Automata -> ([[Config]], [[Config]])
    decideCompute cs a = 
        let fo = filter (\x -> let st = (step x (d a)) in [x] == st || st == []) cs
            in (foldr (union) [] (map (\x -> step x (d a)) (cs \\ fo)), fo)
    
    -- stepEdo :: Config -> Delta -> State
    -- stepEdo (edo, str, c_stck) delta = head (delta edo (head str) (head c_stck))
    
    -- Función que realiza un solo paso de computo
    -- Esto es, que toma un procesamiento de una cadena, y obtiene todos las
    -- posibles opciones para continuar ese procesamiento
    step :: [Config] -> Delta -> [[Config]]
    step [] _ = error "configuración vacía"
    step (c@(_, _, ""):cs) _ = [c:cs]
    step (c@(p, "", (sk:sks)):cs) del = 
        [(buildConfigEps (p, "", (sk:sks)) rd):c:cs | rd <- (del (p, E, sk))]
    step (c@(p, (sy:ss), (sk:sks)):cs) del = 
        [(buildConfigEps (p, (sy:ss), (sk:sks)) rd):c:cs | rd <- (del (p, E, sk))]
        ++[(buildConfig (p, (sy:ss), (sk:sks)) rd):c:cs | rd <- (del (p, S sy, sk))] 

    
    -- Función que obtiene la configuración dado el resultado de la evaluación de
    -- una función de transición cuando sí se consumen símbolos de la cadena.
    buildConfig :: Config -> (State, [Symbol]) -> Config
    buildConfig (_, _, []) (_, []) = error "configuración inválida"
    buildConfig (_, "", _) _ = error "configuración inválida"
    buildConfig (_, (_:ss), stk) (p, []) = (p, ss, pop stk)
    buildConfig (_, (_:ss), stk) (p, nstk) = (p, ss, multipush (pop stk) nstk)

    -- Función que obtiene la configuración dado el resultado de la evaluación de
    -- una función de transición cuando no se consumen símbolos de la cadena
    -- (epsilon-transiciones).
    buildConfigEps :: Config -> (State, [Symbol]) -> Config
    buildConfigEps (_, _, "") _ = error "configuración inválida"
    buildConfigEps (_, str, stk) (p, []) = (p, str, pop stk)
    buildConfigEps (_, str, stk) (p, nstk) = (p, str, multipush (pop stk) nstk)


    -- Funcion que indica si un autómata de pila acepta por pila vacía
    acceptByStack :: Machine -> String -> Bool
    acceptByStack m str = elem "" [stk | (_, _, stk):_ <- compute m str]

    -- Funcion que indica si un autómata de pila acepta por estado final
    acceptByState :: Machine -> String -> Bool
    acceptByState m@(a, _) str = 
        intersect (f a) fc /= []
        where fc = [p | (p, _, _):_ <- compute m str]
    
    -- Ejemplo
    -- Autómata que acepta a L = {a^nb^m^ck | n = m o m = k}
    delta1 :: Delta
    delta1 (Q 0, S 'a', 'Z') = [(Q 1, "AZ"), (Q 4, "Z")]
    delta1 (Q 1, S 'a', 'A') = [(Q 1, "AA")]
    delta1 (Q 1, S 'b', 'A') = [(Q 2, [])]
    delta1 (Q 2, S 'b', 'A') = [(Q 2, [])]
    delta1 (Q 2, S 'c', 'Z') = [(Q 3, "Z")]
    delta1 (Q 3, S 'c', 'Z') = [(Q 3, "Z")]
    delta1 (Q 4, S 'a', 'Z') = [(Q 4, "Z")]
    delta1 (Q 4, S 'b', 'Z') = [(Q 5, "BZ")]
    delta1 (Q 5, S 'b', 'B') = [(Q 5, "BB")]
    delta1 (Q 5, S 'c', 'B') = [(Q 6, [])]
    delta1 (Q 6, S 'c', 'B') = [(Q 6, [])]
    delta1 (Q 6, E , 'Z') = [(Q 7, "Z")]
    delta1 _ = []
    
    pda1 :: Automata
    pda1 = PDA {
        q = [(Q 0), (Q 1), (Q 2), (Q 3), (Q 4), (Q 5), (Q 6), (Q 7)],
        s = ['a', 'b', 'c'],
        g = ['Z','A','B'],
        d = delta1,
        q0 = (Q 0),
        z0 = 'Z',
        f = [(Q 0), (Q 3), (Q 7)]
    }

    -- Procesamiento formal de la cadena "aabbcc"
    fm'aabbcc':: [[Config]]
    fm'aabbcc' = compute (pda1, []) "aabbcc"
    