Zunächst brauchen wir zwei Haskell Erweiterungen:
\begin{code}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
\end{code}
Nun können wir die nötigen Module einbinden
\begin{code}
module Main
  where

import Prelude hiding (writeFile, readFile, appendFile)
import qualified Prelude as P
import System.Environment
import System.Directory
import Data.List
import Control.Monad

import qualified Data.Binary as B
import Data.ByteString.Lazy (writeFile, readFile, appendFile)
import Control.Parallel
import Control.Parallel.Strategies

\end{code}
Zuletzt laden wir noch unsere Bibliothek.
\begin{code}
import GalFld.GalFld
\end{code}

\iffalse
\begin{code}
import Debug.Trace
\end{code}
\fi

\section{Erzeugen der Körper}

Nun erzeugen wir uns einen Primkörper \texttt{FP} mit der hier gewählen
Charakteristik $c=3$. Das hier vorgestellte Programm ist generisch in dieser
Charakteristik.
\begin{code}
$(genPrimeField 3 "PF")
\end{code}

Über diesem Körper können wir nun Erweiterungskörper generieren. Zunächst holen
wir uns ein (beliebiges) Element aus unserem Primkörper.
\begin{code}
pf = 1::PF
\end{code}

Eine Grad $2$ Erweiterung über dem Primkörper erzeugen wir wie folgt:
\begin{code}
e2fpMipo = $([|findIrred (getAllMonicPs (elems pf) [2])|])
e2pf = FFElem (pList[0,pf]) e2fpMipo
\end{code}
Wobei \texttt{e2fpMipo} $\in\texttt{FP}[x]$ ein Irreduzibles Polynom vom Grad
$2$. \texttt{e2pf} ist damit ein erzeugendes Element im gesuchtem Körper, enthält
also die ganze Information.

Analog eine Grad $2$ Erweiterung über diese neue Erweiterung sowie eine Grad
$4$ Erweiterung über unserem Grundkörper \texttt{FP}.
\begin{code}
e2e2pfMipo = $([|findIrred (getAllMonicPs (elems e2pf) [2])|])
e2e2pf = FFElem (pList[0,e2pf]) e2e2pfMipo

e4pfMipo = $([|findIrred (getAllMonicPs (elems pf) [4])|])
e4pf = FFElem (pList[0,pf]) e4pfMipo
\end{code}

Hier noch ein paar größere Erweiterungen, für die die Minimalpolynome aber erst
zur Laufzeit (wenn benötigt) erzeugt werden.
\begin{code}
e5e2pfMiPo = findIrred $ getAllMonicPs (elems e2pf) [5]
e5e2pf = FFElem (pList[0,e2pf]) e5e2pfMiPo

e5e4pfMiPo = findIrred $ getAllMonicPs (elems e4pf) [5]
e5e4pf = FFElem (pList[0,e4pf]) e5e4pfMiPo

e99fpMipo = findIrred (getAllMonicPs (elems pf) [99])
e99pf = FFElem (pList[0,pf]) e99fpMipo
\end{code}

\section{Main Funktion}

\begin{code}
main = do
  mapM_ (\d -> putStrLn $ show d ++ "  " ++ show (irred d)) [2..]
    where es      = elems pf
          irred d = findIrredRabin $ getAllMonicPs es [d]

\end{code}

\section{Weiteres}
\begin{code}
{-

degree = 4

#ifdef linux_HOST_OS
outputPath = "/tmp/"
#else
outputPath = ""
#endif
outputFile = outputPath ++ "irreds" ++ show (modulus pf) ++ "_" ++ show degree

step1 = (doesFileExist outputFile) >>= \case
  True -> print "Die irreduziblen Polynome wurden bereits generiert"
  False -> do
    print "Finde alle irreduziblen Polynome und speichere diese in eine Datei"
    -- Leere die Datei
    P.writeFile outputFile ""
    -- Schreibe die gefundenen Polynome in die Datei
    writeFile outputFile
      $ B.encode
      $ findIrreds
      $ getAllMonicPs (elems e2pf) [degree]

-}
\end{code}

Als nächstes lesen wir die Daten wieder aus der Datei aus.

\begin{code}
{-

step2 = do
  -- Lese die Binärdatei
  raw <- readFile outputFile
  let ls = B.decode raw :: [Polynom (FFElem PF)]
  return ls

-}
\end{code}

Nun begeben wir uns in den gefundenen Polynomen auf die Suche nach möglichst
dünnen Polynomen.

\begin{code}
{-

step3 ls = do
  print $ "Die dünnsten Polynome enthalten " ++ show minC ++ " Monome"
  print $ "Es gibt " ++ show (length minCPs) ++ " von ihnen"
    where ls'    = map (\f -> ((length . p2Tup) f,f)) ls
          cs     = map fst ls'
          minC   = minimum cs
          minCPs = [ f | (c,f) <- ls' , c == minC ]

-}
\end{code}

\subsection{Alles zusammenfügen und ausführen}
\begin{code}
{-

main :: IO ()
main = do
  step1
  ls <- step2
  step3 ls

-}
\end{code}
