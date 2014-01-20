module Project.Core.Polynomials
    ( addPoly
    , derivePoly
    ) where

{-
 - Terme (als Bausteine von Polynomen)
 -}
{-type Term = (Integer,Integer) -- (degree, coeff)-}
{-data Term a = Term a deriving (Show)-}
data Term a = NullTerm
            |  Term { degree :: Integer
                   , coeff :: a
                   } deriving (Eq,Show)

{-
 - Polynome
 -}
{-type Poly = [Term]            -- [(degree, coefficient)] and degree should be descending-}
{-data Poly a = NullPoly-}
            {-| Poly [Term a] deriving (Show)-}
data Poly a = NullPoly
            | Term a :-: Poly a deriving (Eq,Show)


{-
 - Funktionen auf Polynomen
 -}

addPoly :: Num t => Poly t -> Poly t -> Poly t
addPoly NullPoly g = g
addPoly (t :-: f) g = addPoly f (t :-: g)

{-derivePoly :: Num t =>  Poly t -> Poly t-}
derivePoly :: Poly Integer -> Poly Integer
derivePoly NullPoly   = NullPoly
derivePoly (t :-: f)  = deriveTerm t :-: derivePoly f
  where deriveTerm (Term {degree = d, coeff = c})
            | d == 0     = NullTerm
            | otherwise = Term { degree = (d - 1) , coeff = (c * d)}
