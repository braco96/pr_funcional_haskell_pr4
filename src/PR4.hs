

data Juego = Carta Int| Mano [Juego]deriving (Eq,Show)

data Arbol a = Nodo a [Arbol a] deriving (Eq,Show)


----------- Estas funciones transforma Dato en un elemento del tipo maybe
sacarTiros man = map partida man
  
  

partida (Carta val) =  Just val 

partida (Mano game) =  puntuacion((sacarTiros game))
--------------------------------------------

------------------------------------------------------------Esta funcion nos da el valor de nuestra mano , si es mayor que 21 retornará nothing
puntuacion ::[Maybe Int]->Maybe Int
puntuacion lista =if (er(lista)||sum1>21) then Nothing
                  else Just sum1
                  where sum1=suma(lista)
        
------------------------------------------------------------------------------------------------------

-------------------------------- Si el conjunto cumple la condicion la tirada no será valida , esta funcion detecta si lo es
er :: [Maybe Int]->Bool
er [] =False
er ((Just a) :xx) = (0==a)|| a>12||er xx
 
 
------------------------------------------------------------------------------------------------------------------------------

------------------------------------------ Si es valida esta funcion suma todos los dados
suma:: [Maybe Int]->Int
suma []=0
suma((Just a):resto)=a+suma resto

---------------------------------------------------------------------------------------------

 
------------------------------------------LAB1 SEÑORA

maximaaSuma :: Arbol Int -> Int
maximaaSuma a = maximum [sum xs | xs <- ramas a]


------------------------------------------------------CONJUNTO A ARBOL

--------------------------------------------------- Me da un arbol por cada rama
aArbol :: [a]->Arbol a
aArbol [x] = (Nodo x [])
aArbol (x:xs) = (Nodo x [aArbol xs])


------------------------------------------------------ uno la raiz con el conjunto de ramas
generarArbol ::a->[[a]]->Arbol a
generarArbol raiz lista = (Nodo raiz) (map   aArbol lista )

----------------------------------------------------
----------------------------------------------------EJERCICIO ALTURA 
-----------------------------------------Longitud de la altura maxima
alturamas ::Arbol Int   ->[ Int]
alturamas array =  [length(x)|x<-(ramas array)]
 
alturah arbol h = filter (==h) (alturamas arbol)
------------------------------------------Funcion auxiliar para convertir un arbol a un conjunto SEÑORA
ramas :: Arbol a -> [[a]]
ramas (Nodo x []) = [[x]]
ramas (Nodo x as) = [x : xs | a <- as, xs <- ramas a]

 

numeroAristas (Nodo raiz hijos) =  ((foldr (+) 1 (map numeroAristas hijos)))


-----------let a=generarArbol 3 [[6,4,3,2,3],[4,5],[3,2,,24]] 