--------------------------------------------------------------------------------
-- | 
-- Module      : GalFld.Algorithmen.SFreeFactorization
-- Note        : Implementiert eine Quadratfreie Faktorisierung
-- 
-- 
-- 
--------------------------------------------------------------------------------
module GalFld.Algorithmen.SFreeFactorization
  (sff, appSff, findTrivialsSff
  , sffFactor
  ) where
import Control.Parallel
import Control.Parallel.Strategies

import GalFld.Core
--------------------------------------------------------------------------------
--  Wrapper

sff :: (Show a, FiniteField a, Num a, Fractional a) => Polynom a -> [(Int,Polynom a)]
sff = appFact sffFactor . obviousFactor

appSff :: (Show a, FiniteField a, Num a, Fractional a) => 
                                        [(Int,Polynom a)] -> [(Int,Polynom a)]
appSff = appFact sff

-- |Gibt alle Faktorisierungen zurück, welche nach SFF noch trivial sind
-- Wendet zuvor die offensichtliche Faktorisierung an
findTrivialsSff :: (Show a, Fractional a, Num a, FiniteField a) => 
                                            [Polynom a] -> [[(Int,Polynom a)]]
findTrivialsSff ps = [fs | fs <- parMap rpar appSff
                         --(findTrivialsOb ps) -- Suche nur 0 als Ns
                         (findTrivialsNs ps) -- Suche alle Ns
                       , isTrivialFact fs]

--------------------------------------------------------------------------------
--  Algorithmus

{-
 - from: http://en.wikipedia.org/wiki/Factorization_of_polynomials_over_finite_fields

  Algorithm: SFF (Square-Free Factorization)
  Input: A monic polynomial f in Fq[x]
  Output: Square-free factorization of f

  i←1; R ← 1; g ← f′;
  if g ≠ 0 then {
     c ← gcd(f, g);
     w ← f/c;
     while w ≠ 1 do {
         y ← gcd(w, c); z ← w/y;
         R ← R·z^i; i ← i+1;
         w ← y; c ← c/y }
     if c ≠ 1 then {
         c ← c^1/p;
         Output(R·SFF(c)^p) }
     else Output(R)
  else {
     f ← f^1/p;
     Output(SFF(f)^p) }
  end

  Oder siehe:
      Computer Algebra and Symbolic Computation: Mathematical Methods Volume 2
      Kapitel 9
  Oder (gleich wie Wiki):
       K. O. Geddes,  S. R. Czapor,  G. Labahn: Algorithms for Computer Algebra
       Seite 345(355)
 -}

sffFactor :: (Show a, FiniteField a, Num a, Fractional a) => 
                                                Polynom a -> [(Int,Polynom a)]
sffFactor f 
  | isNullP f       = [(1,nullP)]
  | uDegP f <= 1     = [(1,f)]
  | df /= 0 && c /= 1 = r ++ map (\(n,x) -> (n*p,x)) (sffFactor $ charRootP c)
  | df /= 0 && c == 1 = r
  | otherwise       = map (\(n,x) -> (n*p,x)) $ sffFactor $ charRootP f
  where df         = deriveP f
        c'         = ggTP f df
        p          = charOfP f
        (r,c)      = sffFactor' 1 (f @/ c') c'
        sffFactor' i w c | w == 1      = ([], c)
                         | z /= 1      = ((i,z) : r, c')
                         | otherwise  = (r,c')
          where y      = ggTP w c
                z      = w @/ y
                (r,c') = sffFactor' (i+1) y (c @/ y)

--------------------------------------------------------------------------------
--  Beispiele

{-
 - Beispiel in F3[x]:
 -      f = X¹¹+2x⁹+2x⁸+x⁶+x⁵+2x³+2x²+1
 -        = (x+1)(x²+1)³(x+2)⁴
 -}
f=pList [1::F3,0,2,2,0,1,1,0,2,2,0,1]
sqf :: [(Int,Polynom F3)]
sqf=[(1,pList [1::F3,1])
    ,(2,pList [1])
    ,(3,pList [1::F3,0,1])
    ,(4,pList [2::F3,1])]
