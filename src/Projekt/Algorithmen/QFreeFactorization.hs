
module Projekt.Algorithmen.QFreeFactorization
  (qFreeFact
  )where
import Projekt.Core

-- !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
-- Funktioniert nur über Charakteristik 0, also NICHT HIER                   !!
-- (unsere implementierung über Z geht auch nicht, weil Fractional nötig ist)!!
-- !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
-- Außerdem: Nicht getestet! potentiel noch nicht mal fertig.


--------------------------------------------------------------------------------
--  Implementation
--
-- Im Script Algorithmus (5.16) auf Seite ~49
-- 
-- Das f₁ aus dem Script ist nicht nötig
-- Das f₂ aus dem Script ist hier: g
-- Das f₃ aus dem Script ist hier: h
-- Das c₂ aus dem Script ist hier: c
qFreeFact :: (Eq a, Fractional a) => Polynom a -> [Polynom a]
qFreeFact f = qFreeFact' g h c ++ [fst $ divP (fst $ divP f g) c]
  where g = ggTP f $ deriveP f
        h = ggTP g $ deriveP f
        c = fst $ divP g h

qFreeFact' :: (Eq a, Fractional a) => Polynom a -> Polynom a -> Polynom a -> [Polynom a]
qFreeFact' g h c | fromInteger 1 == c = []
                 | otherwise         = qFreeFact' h newH newC ++ [fst $ divP c newC]
  where newH = ggTP g $ deriveP g
        newC = fst $ divP h newH



--------------------------------------------------------------------------------
--  Beispiel

f = P [(7,1.0), (6,1), (5,-1), (3,-1), (2,-1), (1,1), (0,1)]

-- hat die Zerlegung:
-- L = [ x²+1 , x-1 , x+1 ]
