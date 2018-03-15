module Control.Monad.Trans.StateT where

open import Prelude
open import Control.Monad
open import Control.Monad.Trans.Class
open import Control.Structure

StateT : ∀ (S : Set) → (M : Set → Set) → Set → Set
StateT s m a = s → m (a × s)

runStateT : {M : Set → Set}{{_ : Monad M}}{A S : Set} → StateT S M A → S → M (A × S)
runStateT ts s = ts s

execStateT : {M : Set → Set}{{_ : Monad M}}{A S : Set} → StateT S M A → S → M A
execStateT ts s = ts s >>= return ∘ fst

evalStateT : {M : Set → Set}{{_ : Monad M}}{A S : Set} → StateT S M A → S → M S
evalStateT ts s = ts s >>= return ∘ snd

return' : ∀ {M : Set → Set}{{_ : Monad M}} → {A S : Set} → A → StateT S M A
return' = λ a s → return (a , s)
_>>='_ : ∀ {M : Set → Set}{{_ : Monad M}} → ∀ {A B S : Set} →
          StateT S M A → (A → StateT S M B) → StateT S M B
ma >>=' mf = λ s → ma s >>= λ { (a , s') → (mf a) s' }

instance
  StateTMonad : {M : Set → Set}{{_ : Monad M}} → {S : Set} → Monad (StateT S M)
  StateTMonad =
    let
      return : ∀ {M : Set → Set}{{_ : Monad M}} → {A S : Set} → A → StateT S M A
      return = λ a s → return (a , s)
      _>>=_ : ∀ {M : Set → Set}{{_ : Monad M}} → ∀ {A B S : Set} →
              StateT S M A → (A → StateT S M B) → StateT S M B
      ma >>= mf = λ s → ma s >>= λ { (a , s') → (mf a) s' }
    in record
      { return = return
      ; _>>=_ = _>>=_
      ; monad-left-identity-law  = funExt λ s → monad-left-identity-law
      ; monad-right-identity-law = funExt λ s → monad-right-identity-law
      ; monad-associativity-law  = funExt λ s → monad-associativity-law
      }


  StateTApplicative : {M : Set → Set}{{_ : Monad M}} → {S : Set} → Applicative (StateT S M)
  StateTApplicative =
    record
      { pure = return
      ; _<*>_ = λ mf ma → mf >>= λ f → ma >>= λ a → return (f a)
      ; applicative-identity-law     = applicative-identity-law
      ; applicative-composition-law  = applicative-composition-law
      ; applicative-homomorphism-law = applicative-homomorphism-law
      ; applicative-interchange-law  = applicative-interchange-law
      }

  StateTFunctor : {M : Set → Set}{{_ : Monad M}} → {S : Set} → Functor (StateT S M)
  StateTFunctor =
    record
      { fmap = _<*>_ ∘ pure
      ; functor-identity-law    = functor-identity-law
      ; functor-composition-law = functor-composition-law
      }

  StateTMonadTrans : ∀ {S} → MonadTrans (StateT S)
  StateTMonadTrans =
    record
      { lift = λ ma → (λ s → ma >>= λ a → return (a , s))
      ; trans-identity-law = funExt λ a → funExt λ s → monad-left-identity-law
      ; trans-composition-law = λ { {m = m}{f = f} →
        trans (funExt λ s → sym monad-associativity-law )
        (sym (funExt λ s → trans (sym monad-associativity-law) (
        (cong (_>>=_ m) (funExt λ a → monad-left-identity-law)))))}
      }
  
get : {M : Set → Set}{{_ : Monad M}}{A S : Set} → StateT S M S
get = λ s → return (s , s)

put : {M : Set → Set}{{_ : Monad M}}{A S : Set} → S → StateT S M Unit
put s = λ _ → return (⟨⟩ , s)

state : {M : Set → Set}{{_ : Monad M}}{A S : Set} → (S → A × S) → StateT S M A
state f = λ s → return (f s)

gets : {M : Set → Set}{{_ : Monad M}}{A S : Set} → (S → A) → StateT S M A
gets f = λ s → return ((f s) , s)

modify : {M : Set → Set}{{_ : Monad M}}{A S : Set} → (S → S) → StateT S M Unit
modify f = state (λ s → ⟨⟩ , f s)
