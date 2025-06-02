module Documento
  ( Doc,
    vacio,
    linea,
    texto,
    foldDoc,
    (<+>),
    indentar,
    mostrar,
    imprimir,
  )
where

data Doc
  = Vacio
  | Texto String Doc
  | Linea Int Doc
  deriving (Eq, Show)

vacio :: Doc
vacio = Vacio

linea :: Doc
linea = Linea 0 Vacio

texto :: String -> Doc
texto t | '\n' `elem` t = error "El texto no debe contener saltos de línea"
texto [] = Vacio
texto t = Texto t Vacio

-- Ejercicio 1:
foldDoc :: b -> (String -> b -> b) -> (Int -> b -> b) -> Doc -> b
foldDoc fVacio _ _ Vacio                 = fVacio
foldDoc fVacio fTexto fLinea (Texto s x) = fTexto s (foldDoc fVacio fTexto fLinea x)
foldDoc fVacio fTexto fLinea (Linea i x) = fLinea i (foldDoc fVacio fTexto fLinea x)

   
-- Ejercicio 2:
infixr 6 <+>
(<+>) :: Doc -> Doc -> Doc
d1 <+> d2 = foldDoc d2 fTexto Linea d1 
  where
    fTexto s docAcumulado = case docAcumulado of
      Texto s' d  -> Texto (s ++ s') d 
      _           -> Texto s docAcumulado

{-
  - Sean d1 y d2 documentos que cumplen el invariante.
  - Usamos foldDoc, que recorre el documento d1 de manera estructural. 
    Por lo tanto, al aplicar fTexto, docAcumulado representa el resultado 
    de procesar subdocumentos de d1, y al d1 cumplir el invariante, cada
    subdocumento también lo cumplirá.
  - Cuando en fTexto docAcumulado es un Texto s' d:
    Sabemos que s proviene de d1, por lo tanto no es vacío ni contiene saltos de línea.
    s' y d provienen de docAcumulado y cumplen el invariante por lo dicho anteriormente.
  - Luego, si s y s' no son vacíos y no contienen saltos de línea, entonces
    (++) devolverá un string no vacío y sin saltos de línea ya que solo une
    los caracteres.
  - Por lo tanto, procesar Texto (s ++ s') d mantiene el invariante.
  - Cuando docAcumulado es Vacio o Linea i d, idem al paso anterior: sabemos que s cumple
    el invariante por provenir de d1, y como docAcumulado no es un Texto s' d, 
    procesar Texto s docAcumulado mantiene el invariante.
-}


-- Ejercicio 3:
indentar :: Int -> Doc -> Doc
indentar i = foldDoc Vacio Texto (\j docAcumulado -> Linea (j+i) docAcumulado) 

{-
  - Por precondición del problema sabemos que i >= 0.
  - Cuando la función procese (\j docAcumulado -> Linea (j+i) docAcumulado) sabemos que:
    - j proviene del documento original que asumimos cumple el invariante, por lo tanto,
      este también lo deberá cumplir y será un entero mayor o igual a cero.
    - Luego al usar foldDoc sabemos que docAcumulado representa el resultado de procesar
      subdocumentos del documento original, por lo tanto docAcumulado también cumple
      el invariante.
    - Como i >= 0 y j >= 0 , sabemos que j+i >= 0, por propiedad de enteros.
    - Entonces procesar Linea (j+i) docAcumulado mantiene el invariante.

-}

-- Ejercicio 4:
mostrar :: Doc -> String
mostrar = foldDoc "" fTexto fLinea 
  where
    fTexto s rec = s ++ rec
    fLinea i rec = "\n" ++ replicate i ' ' ++ rec

-- | Función dada que imprime un documento en pantalla

-- ghci> imprimir (Texto "abc" (Linea 2 (Texto "def" Vacio)))
-- abc
--   def

imprimir :: Doc -> IO ()
imprimir d = putStrLn (mostrar d)
