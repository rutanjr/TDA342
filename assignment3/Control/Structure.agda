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
  MonadToApplicative {M} =
    let pure : {A : Set} -> A -> M A
        pure                            = return
        infix 4 _<*>_
        _<*>_ : {A B : Set} -> M (A -> B) -> M A -> M B
        _<*>_ = λ mf ma ->  mf >>= λ f → ma >>= λ a → return (f a)
    in record
       { pure                         = pure
         ; _<*>_                        = _<*>_
         ; applicative-identity-law     = trans monad-left-identity-law monad-right-identity-law
         ; applicative-composition-law   = λ { {A = A}{B = B}{C = C}{u = mf}{v = mg}{w = ma} →
           let
             m : M ((A → B) → A → C)
             m = return (_∘_) >>= λ h → mf >>= λ f → return (h f)
             k : ∀ {D} → ((A → B) → D) → M D 
             k = λ h' → mg >>= λ g → return (h' g)
             --h : ∀ {D} → (A → D) → M D 
             h = λ h'' → ma >>= λ a → return (h'' a)
           in
             ((pure (_∘_) <*> mf) <*> mg) <*> ma
               ≡⟨ sym (trans monad-associativity-law (trans (trans (sym monad-left-identity-law) monad-associativity-law ) monad-associativity-law)) ⟩
             mf >>= (λ f → (return ((_∘_) f) >>= λ h' → mg >>= (λ g → return (h' g)) >>= h))
               ≡⟨ cong ((_>>=_) mf) (funExt λ f →
                 (return ((_∘_) f) >>= λ h' → mg >>= (λ g → return (h' g)) >>= h)
                   ≡⟨ trans monad-left-identity-law (sym monad-associativity-law) ⟩
                 mg >>= (λ g → return (f ∘ g) >>= h)
                   ≡⟨ cong ((_>>=_) mg) (funExt λ g → monad-left-identity-law) ⟩
                 mg >>= (λ g → ma >>= λ a → return ((f ∘ g) a))
                   ∎ )⟩
             mf >>= (λ f → mg >>= λ g → ma >>= λ a → return ((f ∘ g) a))
               ≡⟨ {!!} ⟩
             {!!} ∎
         }
         ; applicative-homomorphism-law = trans monad-left-identity-law monad-left-identity-law
         ; applicative-interchange      =  λ { {u = u} →
           trans (cong ((_>>=_) u) (funExt λ x → monad-left-identity-law)) (sym monad-left-identity-law)
           } 
         }
                          
