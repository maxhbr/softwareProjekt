module Projekt.Algorithmen.SFreeFactorization
  (sff
  ) where
import Projekt.Core.FiniteFields
import Projekt.Core.Polynomials
import Projekt.Core.Factorization

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
 -}

sff :: (FiniteField a, Num a, Fractional a) => Polynom a -> [(Int,Polynom a)]
sff f | df /= 0 && c /= 1 = r ++ map (\(n,x) -> (n*p,x)) (sff $ charRootP c)
      | df /= 0 && c == 1 = r
      | otherwise       = map (\(n,x) -> (n*p,x)) $ sff $ charRootP f
  where df         = deriveP f
        c'         = ggTP f df
        p          = charakteristik $ prodOfCoeffsP f
        (r,c)      = sff' 1 (f @/ c') c'
        sff' i w c | w == 1      = ([], c)
                   | z /= P[one] = ((i,z) : r, c')
                   | otherwise  = (r,c')
          where y      = ggTP w c
                z      = w @/ y
                (r,c') = sff' (i+1) y (c @/ y)

--------------------------------------------------------------------------------
--  Beispiele

{-
 - Beispiel in F3[x]:
 -      f = X¹¹+2x⁹+2x⁸+x⁶+x⁵+2x³+2x²+1
 -        = (x+1)(x²+1)³(x+2)⁴
 -}
f=P[1::F3,0,2,2,0,1,1,0,2,2,0,1]
sqf :: [(Int,Polynom F3)]
sqf=[(1,P[1::F3,1])
    ,(2,P[1])
    ,(3,P[1::F3,0,1])
    ,(4,P[2::F3,1])]

