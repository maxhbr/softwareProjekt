Wir beginnen mit einer Körpererweiterung $\F_{q^n} \mid \F_q$ und stellen uns 
die Frage nach einer Enumeration aller primitiven und normalen Elemente dieser 
Erweiterung.
Wie bereits in \thref{bem:primnorm} erläutert, sind die Nullstellen des
Pi-Polynoms zu $X^n-1$ gerade die normalen Elemente der Körpererweiterung und die 
Nullstellen des Kreisteilungspolynoms $\Phi_{n-1}$ gerade die primitiven 
Elemente. Folglich ist der $\ggT$ beider gerade das Produkt der Minimalpolynome
aller primitiven \emph{und} normalen Elemente!

\begin{code}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
module Main
  where
\end{code}

Imports zum Messen der Ausführungszeit und zum Verarbeiten von Input-Parametern.

\begin{code}
import System.CPUTime
import System.Environment
\end{code}

Ferner benötigen wir die Bibliothek ħGalFldħ und 
ħGalFld.More.SpecialPolysħ.

\begin{code}
import GalFld.GalFld
import GalFld.More.SpecialPolys
\end{code}

Wir erzeugen einen Primkörper der Charakteristik 2 mit dem Namen ħPFħ.

\begin{code}
$(genPrimeField 2 "PF")

pf = 1::PF
p = charakteristik pf
\end{code}

Anschließend erstellen wir eine neue Datenstruktur, genannt ħTħ, die die 
gesammelten Informationen speichern soll.

\begin{code}
data T = T { ext :: Int -- Grad des Grundkörpers über dem Primkörper
           , deg :: Int -- Grad der Erweiterung
           , countP :: Int -- Anzahl primitiver Elemente
           , countN :: Int -- Anzahl normaler Elemente
           , countPN :: Int } -- Anzahl primitiv-normaler Elemente
\end{code}


Nach diesen Schritten der Vorbereitung können wir nun den zentralen Teil 
des Beispiels formulieren: Die Berechnung der primitiv-normalen Elemente durch 
Faktorisierung des $\ggT$ des Kreisteilungspolynoms und des passenden 
Pi-Polynoms.

\begin{code}
genPrimNorm :: Int -> Int -> (T, [(Int, Polynom (FFElem PF))])
genPrimNorm m n = (record, fac)
  where one     = extendFFBy m pf
        cyP     = cyclotomicPoly (p^(n*m)-1) one
        piP     = piPoly $ pTupUnsave [(n,one),(0,-1)]
        ggT     = ggTP cyP piP
        fac     = factorP ggT
        record  = T m n (uDegP cyP) (uDegP piP) (uDegP ggT)
\end{code}

Bleibt nur noch ħif'ħ als kleines Hilfsmittel zu formulieren

\begin{code}
if' :: Bool -> a -> a -> a
if' True  x _ = x
if' False _ y = y
\end{code}

und in einer ħmainħ-Funktion die Ein- und Ausgaben zusammenzufügen.

\begin{code}
main = do
  args <- getArgs
  let indxs = if' (length args == 2)
                  [(read $ head args)..(read $ head $ tail args)]
                  ( if' (length args == 1)
                        [2..(read $ head args)]
                        [2..] )
  mapM_ (\m -> (mapM_ (\n -> do
    st <- getCPUTime
    let gpn = genPrimNorm m n
    putInfo $ fst gpn
    --putPolys $ snd gpn
    putTime st ) indxs)) [2..5]
      where putInfo (T m n cP cN cPN) = do
              putStrLn $ "In F" ++ show (p^m) ++ "^" ++ show n ++ " über F" ++ show (p^m)
                ++ " gibt es:"
              putStrLn $ "\t\t" ++ show cP ++ " primitive Elemente"
              putStrLn $ "\t\t" ++ show cN ++ " normale Elemente"
              putStrLn $ "\t\t" ++ show cPN ++ " primitive und normale Elemente"
            putPolys fs = do
              putStrLn "Mit Minimalpolynomen:"
              mapM_ (\(_,f) -> putStrLn $ "\t" ++ show f) fs
            putTime st = do
              ft <- getCPUTime
              putStrLn $ "("
                ++ show (fromIntegral (ft - st) / 1000000000000) ++ "s)\n"
\end{code}
