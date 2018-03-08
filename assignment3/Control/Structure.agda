module Control.Structure where

open import Prelude
open import Control.RawMonad

open RawFunctor {{...}} public

test-lemma : {A B C : Set} {f : B → C}{g : A → B}{x : A} → (f ∘ g) x ≡ f (g x)
test-lemma = refl

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
  applicative-identity-law {{MonadToApplicative}}     = {!!}
  applicative-composition-law {{MonadToApplicative}}  = {!!}
  applicative-homomorphism-law {{MonadToApplicative}} = {!!}
  applicative-interchange {{MonadToApplicative}}      = {!!}
