module PPON where

import Documento

data PPON
  = TextoPP String
  | IntPP Int
  | ObjetoPP [(String, PPON)]
  deriving (Eq, Show)

-- Ejercicio 5
pponAtomico :: PPON -> Bool
pponAtomico ppon = case ppon of
  ObjetoPP _ -> False
  _          -> True

-- Ejercicio 6
pponObjetoSimple :: PPON -> Bool
pponObjetoSimple ppon = case ppon of
  ObjetoPP xs -> all (pponAtomico . snd) xs 
  _           -> False

-- Ejercicio 7
intercalar :: Doc -> [Doc] -> Doc
intercalar _ [] = vacio    -- > Como foldr1 no maneja el caso de la lista vacia , lo incluimos.
intercalar s ds = foldr1 (\t rec -> t <+> s <+> rec) ds

entreLlaves :: [Doc] -> Doc
entreLlaves [] = texto "{ }"
entreLlaves ds =
  texto "{"
    <+> indentar
      2
      ( linea
          <+> intercalar (texto "," <+> linea) ds
      )
    <+> linea
    <+> texto "}"

-- Ejercicio 8
aplanar :: Doc -> Doc
aplanar = foldDoc vacio fTexto fLinea 
  where
    fTexto s acc = texto s <+> acc
    fLinea _ acc = texto " " <+> acc

-- Ejercicio 9

pponADoc :: PPON -> Doc
pponADoc ppon = case ppon of
  TextoPP s      -> texto (show s)
  IntPP i        -> texto (show i)
  ObjetoPP lista -> if pponObjetoSimple ppon then aplanar docs else docs
    where
      docs = entreLlaves (foldr parAdoc [] lista)
      parAdoc (s, ppon') rec = (texto (show s) <+> texto ": " <+> pponADoc ppon') : rec

{-

Es recursión primitiva por que:

    - Cada caso base se escribe combinando los parámetros que no son del tipo PPON.
    
    - El caso recursivo se escribe combinando los valores que no son del tipo PPON
      (en este caso los string s), y el llamado recursivo sobre los parámetros 
      que sí son del tipo PPON.

    - Sin embargo, la función accede a las sub-estructuras en la llamada de la función
      auxiliar `pponObjetoSimple` (si se accede a las sub-estructuras entonces la recursión 
      no es estructural) por ende la recursión es primitiva.
    
    - la recursion NO es global porque no se acceden a los resultados de recursiones anteriores

-}
