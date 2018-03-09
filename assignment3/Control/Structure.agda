module Control.Structure where

open import Prelude
open import Control.RawMonad

open RawFunctor

instance
  ApplicativeToFunctor : ∀ {AF}{{_ : RawApplicative AF}} → RawFunctor AF
  fmap                    ApplicativeToFunctor                       = (_<*>_) ∘ pure
  functor-identity-law    ApplicativeToFunctor                       = applicative-identity-law
  functor-composition-law ApplicativeToFunctor {f = f}{g = g}{x = x} =
    trans (trans (cong (λ y → y <*> x) (sym applicative-homomorphism-law)) (sym (cong (λ y → y <*> pure g <*> x) applicative-homomorphism-law))) applicative-composition-law

instance
  MonadToApplicative : ∀ {M}{{_ : RawMonad M}} → RawApplicative M
  MonadToApplicative {M} = let pure : {A : Set} -> A -> M A
                               pure                            = return
                               
                               _<*>_ : {A B : Set} -> M (A -> B) -> M A -> M B
                               _<*>_                           = λ mf ma ->  mf >>= λ f → ma >>= λ a → return (f a)
                        in record
                             { pure                         = pure
                             ; _<*>_                        = _<*>_
                             ; applicative-identity-law     = trans monad-left-identity-law monad-right-identity-law
                             ; applicative-composition-law  = {!!}
                             ; applicative-homomorphism-law = trans monad-left-identity-law monad-left-identity-law
                             ; applicative-interchange      =  λ { {u = u} {x = y} ->
                                                   u <*> pure y
                                                         ≡⟨⟩
                                                   u >>= (λ f → return y >>= (λ y → return (f y)))
                                                         ≡⟨ cong (_>>=_ u) (funExt λ f → monad-left-identity-law) ⟩
                                                   u >>= (λ f -> return (f y))
                                                          ≡⟨ sym monad-left-identity-law ⟩
                                                   return (λ f → f y) >>= (λ g → u >>= λ f → return (g f))
                                                          ≡⟨⟩
                                                   pure (λ f → f y) <*> u ∎
                                                   } 
                             } 
