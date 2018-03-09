module Control.Structure where

open import Prelude
open import Control.RawMonad

open RawFunctor {{...}} public

instance
  ApplicativeToFunctor : ∀ {AF}{{_ : RawApplicative AF}} → RawFunctor AF
  fmap {{ApplicativeToFunctor}}                    = (_<*>_) ∘ pure -- pure f <*> x
  functor-identity-law {{ApplicativeToFunctor}}    = applicative-identity-law
  functor-composition-law {{ApplicativeToFunctor}} {f = f}{g = g}{x = x} =
    trans (trans (cong (λ y → y <*> x) (sym applicative-homomorphism-law)) (sym (cong (λ y → y <*> pure g <*> x) applicative-homomorphism-law))) applicative-composition-law

open RawApplicative {{...}} public

instance
  MonadToApplicative : ∀ {M}{{_ : RawMonad M}} → RawApplicative M
  pure {{MonadToApplicative}}                         = return
  _<*>_ {{MonadToApplicative}}                  mf ma = mf >>= λ f → ma >>= λ a → return (f a)
  applicative-identity-law {{MonadToApplicative}} = trans monad-left-identity-law  monad-right-identity-law
  applicative-composition-law {{MonadToApplicative}} {u}{v}{w}  =  {!!}
  applicative-homomorphism-law {{MonadToApplicative}} = trans monad-left-identity-law monad-left-identity-law
  applicative-interchange {{MonadToApplicative}} {u = u}{x = x} =
    trans
      ?
      (sym monad-left-identity-law)
