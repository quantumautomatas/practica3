module PDA where

    -- Tipo de dato algebraico para simular los estados de un autómata
    data State = Q Int deriving(Show, Eq)
    
    -- Sinónimos para símbolos y alfabetos
    type Symbol = Char
    type Alphabet = [Symbol]
    
    -- Sinónimo para la función de transición
    type Delta = State -> Symbol -> Symbol -> [(State, [Symbol])]
    
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
    pop :: Stack -> Stack
    pop xs = tail xs
    
    push :: Stack -> Symbol -> Stack
    push [] a   = [a]
    push stck a = [a]++stck
    
    multipush :: Stack -> [Symbol] -> Stack
    multipush stk [] = stk
    multipush stk (x:xs) = multipush (push stk x) xs
    
    -- Sinónimo para la Máquina
    type Machine = (Automata, Stack)
    
    -- Sinónimo para las configuraciones
    type Config = (State, String, Stack)
    
    -- Función para procesar cadenas
    -- compute :: Machine -> String -> [[Config]]
    -- compute (_,_) "" = []
    -- compute (atm, stck) str = 
    
    -- stepEdo :: Config -> Delta -> State
    -- stepEdo (edo, str, c_stck) delta = head (delta edo (head str) (head c_stck))
    
    -- Función que realiza un solo paso de computo
    -- step :: Config -> Delta -> Config
    -- step (edo, str, c_stck) d = 
    
    -- Ejemplo
    delta1 :: Delta
    delta1 (Q 0) 'a' 'Z' = [((Q 1), "AZ"), ((Q 4), "Z")]
    delta1 (Q 1) 'a' 'A' = [((Q 1), "AA")]
    delta1 (Q 1) 'b' 'A' = [((Q 2), "ε")]
    delta1 (Q 2) 'b' 'A' = [((Q 2), "ε")]
    delta1 (Q 2) 'c' 'Z' = [((Q 3), "Z")]
    delta1 (Q 3) 'c' 'Z' = [((Q 3), "Z")]
    delta1 (Q 4) 'a' 'Z' = [((Q 4), "Z")]
    delta1 (Q 4) 'b' 'Z' = [((Q 5), "BZ")]
    delta1 (Q 5) 'b' 'B' = [((Q 5), "BB")]
    delta1 (Q 5) 'c' 'B' = [((Q 6), "ε")]
    delta1 (Q 6) 'c' 'B' = [((Q 6), "ε")]
    delta1 (Q 6) 'ε' 'Z' = [((Q 7), "Z")]
    
    pda1 = PDA {
        q = [(Q 0), (Q 1), (Q 2), (Q 3), (Q 4), (Q 5), (Q 6), (Q 7)],
        s = ['a', 'b', 'c'],
        g = ['Z','A','B'],
        d = delta1,
        q0 = (Q 0),
        z0 = 'Z',
        f = [(Q 0), (Q 3), (Q 7)]
    }
    