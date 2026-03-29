module Practica03 where

--Sintaxis de la logica proposicional
data Prop = Var String | Cons Bool | Not Prop
            | And Prop Prop | Or Prop Prop
            | Impl Prop Prop | Syss Prop Prop
            deriving (Eq)

instance Show Prop where 
                    show (Cons True) = "⊤"
                    show (Cons False) = "⊥"
                    show (Var p) = p
                    show (Not p) = "¬" ++ show p
                    show (Or p q) = "(" ++ show p ++ " ∨ " ++ show q ++ ")"
                    show (And p q) = "(" ++ show p ++ " ∧ " ++ show q ++ ")"
                    show (Impl p q) = "(" ++ show p ++ " → " ++ show q ++ ")"
                    show (Syss p q) = "(" ++ show p ++ " ↔ " ++ show q ++ ")"

p, q, r, s, t, u :: Prop
p = Var "p"
q = Var "q"
r = Var "r"
s = Var "s"
t = Var "t"
u = Var "u"
w = Var "w"
v = Var "v"

{-
FORMAS NORMALES
-}

--Ejercicio 1
fnn :: Prop -> Prop
fnn (Var p)           = Var p
fnn (Cons True)       = Cons True
fnn (Cons False)      = Cons False
fnn (And f1 f2)       = And (fnn f1) (fnn f2)
fnn (Or f1 f2)        = Or  (fnn f1) (fnn f2)
fnn (Impl f1 f2)      = Or  (negar (fnn f1)) (fnn f2)--- equivalencia logica
fnn (Syss f1 f2)      = And (Or (negar (fnn f1)) (fnn f2)) --- equivalencia logica
                            (Or (negar (fnn f2)) (fnn f1))
fnn (Not (Not f))     = fnn f
fnn (Not (And f1 f2)) = Or  (negar (fnn f1)) (negar (fnn f2))--- de Morgan
fnn (Not (Or  f1 f2)) = And (negar (fnn f1)) (negar (fnn f2))-- de Morgan 
fnn (Not f)           = negar (fnn f) 



--Ejercicio 2
fnc :: Prop -> Prop
fnc f = distribuir (fnn f)--- distribuye fnn 

{-
RESOLUCION BINARIA
-}

--Sinonimos a usar
type Literal = Prop
type Clausula = [Literal]

--Ejercicio 1
-- Convierte FNC a lista de cláusulas
clausulas :: Prop -> [Clausula]
clausulas (And p q) = clausulas p ++ clausulas q
clausulas (Or p q)  = [literales (Or p q)]
clausulas p         = [[p]]

--Ejercicio 2 resolución binaria
resolucion :: Clausula -> Clausula -> Clausula
resolucion c1 c2
  | hayComplemento c1 c2 =
      let l = primerComplemento c1 c2
      in sinDuplicados (eliminar1vez l c1 ++ eliminar1vez (complemento l) c2)
  | otherwise = sinDuplicados (c1 ++ c2)
-- funciones auxiliriares de resolucion binaria:

-- funcion auxiliar de clausulas -> se obtienen las literales de las clausulas

literales :: Prop -> Clausula
literales (Or p q) = sinDuplicados (literales p ++ literales q)
literales p        = [p]


--- funcion auxiliar -> quita duplicados de clausulas

sinDuplicados :: Clausula -> Clausula
sinDuplicados [] = []
sinDuplicados (x:xs)
  | estaEn x xs = sinDuplicados xs
  | otherwise   = x : sinDuplicados xs


-- funcion auxiliar -> devuelve el opuesto de un literal
complemento :: Literal -> Literal
complemento (Not p) = p
complemento p       = Not p

  -- funcion auxiliar -> encuentra la primer literal de c1 cuyo complemento está en c2
primerComplemento :: Clausula -> Clausula -> Literal
primerComplemento (x:xs) c2
  | estaEn (complemento x) c2 = x
  | otherwise                  = primerComplemento xs c2


-- > Verifica si una literal está en una cláusula
estaEn :: Literal -> Clausula -> Bool
estaEn _ []     = False
estaEn l (x:xs) = l == x || estaEn l xs

-- > elimina una la primera aparicion de una literal 
eliminar1vez :: Literal -> Clausula -> Clausula
eliminar1vez _ []     = []
eliminar1vez l (x:xs)
  | l == x    = xs
  | otherwise = x : eliminar1vez l xs


  -- > Verifica si c1 tiene alguna literal cuyo complemento está en c2
hayComplemento :: Clausula -> Clausula -> Bool
hayComplemento [] _     = False
hayComplemento (x:xs) c2
  | estaEn (complemento x) c2 = True
  | otherwise                  = hayComplemento xs c2

--- hasta aqui termina resolucion

{-
ALGORITMO DE SATURACION
-}

--Ejercicio 1
hayResolvente :: Clausula -> Clausula -> Bool
hayResolvente c1 c2 = hayComplemento c1 c2

--Ejercicio 2
--Funcion principal que pasa la formula proposicional a fnc e invoca a res con las clausulas de la formula. 
--este ya no lo logramos :(
saturacion :: Prop -> Bool
saturacion = undefined



---funciones auxiliares
--Funcion que devuelve la negacion de la formula ingresada
negar :: Prop -> Prop
negar (Var p) = Not (Var p)
negar (Cons True) = (Cons False)
negar (Cons False) = (Cons True)
negar (Not f) = f
negar (And f1 f2) = (Or (negar f1) (negar f2))
negar (Or f1 f2) = (And (negar f1) (negar f2))
negar (Impl f1 f2) = (And f1 (negar f2))
negar (Syss f1 f2)= negar (And (Impl f1 f2) (Impl f2 f1)) 


--Funcion que aplica la distribucion 
distribuir :: Prop -> Prop
distribuir (Or p (And q r)) = And (distribuir (Or p q)) (distribuir (Or p r))
distribuir (Or (And q r) p) = And (distribuir (Or q p)) (distribuir (Or r p))
distribuir (Or p q) =
  let p' = distribuir p
      q' = distribuir q
  in if esAnd p' || esAnd q'
       then distribuir (Or p' q')
       else Or p' q'
distribuir (And p q) = And (distribuir p) (distribuir q)
distribuir p         = p

--funcion auxiliar de distribuir para ver si es un and
esAnd :: Prop -> Bool
esAnd (And _ _) = True
esAnd _         = False

