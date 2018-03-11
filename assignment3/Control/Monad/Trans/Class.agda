module Control.Monad.Trans.Class where

open import Prelude
open import Control.Monad

--open Monad {{...}} public

-- https://hackage.haskell.org/package/transformers/docs/Control-Monad-Trans-Class.html#t:MonadTrans
record MonadTrans (T : (Set → Set) → Set → Set) : Set₁ where
  field
    {{monadTransM}}       : ∀ {M}{{_ : Monad M}} → Monad (T M)
    lift                  : ∀ {M}{{_ : Monad M}} → ∀ {A} → M A → T M A
    trans-identity-law    : ∀ {M}{{_ : Monad M}} → ∀ {A} → (lift ∘ return) ≡ return --return x
    trans-composition-law : ∀ {M}{{_ : Monad M}} → ∀ {A B} → {m : M A}{f : A → M B} → lift (m >>= f) ≡ (lift m) >>= (lift ∘ f)
    
open MonadTrans {{...}} public
