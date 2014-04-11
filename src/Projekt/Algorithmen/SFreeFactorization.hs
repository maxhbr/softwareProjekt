module Projakt.Algorithmen.QFreeFactorization where
import Projekt.Core.FiniteFields
import Projekt.Core.Polynomials

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
           R ← R·zi; i ← i+1; 
           w ← y; c ← c/y }
     if c ≠ 1 then {
           c ← c1/p;
           Output(R·SFF(c)p) }
     else  Output(R)
  else {
           f ← f1/p;
           Output(SFF(f)p) }
  end.
 -}
sff :: FiniteField a => Polynom a -> [Polynom a]
sff f = undefined

{-
 - Example:
 -      f= X¹¹+2x⁹+2x⁸+x⁶+x⁵+2x³+2x²+1 \in F3[x]
 - Liefert:
 -      f= (x+1)(x²+1)³(x+2)⁴
 -}
f=P[1::F3,0,2,2,0,1,1,0,2,2,0,1]
