
\begin{code}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
module Main
  where
\end{code}

Imports zum messen der Ausführzeit und zum verarbeiten von Input Parametern.

\begin{code}
import System.CPUTime
import System.Environment
\end{code}

Importiere auch die nötige Bibliothek \texttt{GalFld} und
\texttt{GalFld.More.SpecialPolys} welches standartmäßig nicht enthalten ist, da
es eine sehr spezielle Funktion enthält.

\begin{code}
import GalFld.GalFld
import GalFld.More.SpecialPolys
\end{code}

Wir Erzeugen uns einen Primkörper mit charakteristik \texttt{p} mit dem Namen
\texttt{PF}.

\begin{code}
$(genPrimeField 2 "PF")

pf = 1::PF
p = charakteristik pf
\end{code}

Wir machen uns eine Datenstruktur \texttt{T} in der wir später Information
speichern wollen.

\begin{code}
data T = T { deg :: Int -- Grad der Erweiterung
           , countP :: Int -- Anzahl primitiver Elemente
           , countN :: Int -- Anzahl normaler Elemente
           , countPN :: Int } -- Anzahl primitivNormaler Elemente
\end{code}

Hier nun die Funktion \texttt{genPrimNorm}, die zu einem gegebenem \texttt{Int}
als Grad die ganze Arbeit erledigt und die notwendigen Polynome generiert und
den ggT dieser faktorisiert.

\begin{code}
genPrimNorm :: Int -> (T, [(Int, Polynom PF)])
genPrimNorm n = (record, fac)
  where cyP    = cyclotomicPoly (p^n-1) pf
        piP    = piPoly $ pTupUnsave [(n,pf),(0,-1)]
        ggT    = ggTP cyP piP
        fac    = factorP ggT
        record = T n (uDegP cyP) (uDegP piP) (uDegP ggT)
\end{code}

Nun definieren wir noch eine praktische Funktion \texttt{if'}, die leider in
Prelude fehlt.

\begin{code}
if' :: Bool -> a -> a -> a
if' True  x _ = x
if' False _ y = y
\end{code}

In der \texttt{main} wird alles zusammengefügt und schön formatiert ausgegeben. 

\begin{code}
main = do
  args <- getArgs
  let indxs = if' (length args == 2)
                  [(read $ head args)..(read $ head $ tail args)]
                  ( if' (length args == 1)
                        [2..(read $ head args)]
                        [2..] )
  mapM_ (\n -> do
    st <- getCPUTime
    let gpn = genPrimNorm n
    putInfo $ fst gpn
    putPolys $ snd gpn
    putTime st ) indxs
      where putInfo (T n cP cN cPN) = do
              putStrLn $ "In F" ++ show p ++ "^" ++ show n ++ " über F" ++ show p
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
