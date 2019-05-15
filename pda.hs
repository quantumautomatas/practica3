-- 1. Implementar un autómata de pila definiendo el tipo de dato
---algebraico Automata con base en su definición formal.



--2.Definir el tipo de dato algebraico Stack y las funciones
--necesarias para que su comportamiento sea el de una pila

type Machine = ( Automata , Stack )



--3. Definir la función copute que recibe una Machine, una cadena
--e imprime el procesamiento formal de la cadena con configuraciones.
type Config = ( State , String , Stack )


compute :: Machine -> String -> [[ Config ]]


-- 4. Definir la función acceptByStack que recibe una Machine, una cadena
--y dice si la cadena es aceptada por el autómata de pila.

acceptByStack :: Machine -> String -> Bool



--5. Definir la función acceptByEmptyState que recibe una Machine, una cadena
--y dice si la cadena es aceptada por el autómata de pila.

acceptByState :: Machine -> String -> Bool


--6. Utilizando el tipo de dato algebraico Machine definir el autómata de
--pila que acepte el lenguaje L = {a n b m c k | m = n o m = k}
--y mostrar la formalización de la cadena aabbcc.
