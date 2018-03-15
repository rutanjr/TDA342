module Control.Monad where

open import Prelude

-- https://hackage.haskell.org/package/base/docs/Control-Monad.html#t:Functor
record Functor (F : Set → Set) : Set₁ where
  field
    fmap                    : ∀ {A B} → (A → B) → F A → F B
    functor-identity-law    : ∀ {A}{x : F A}
                              → (fmap id) x ≡ id x 
    functor-composition-law : ∀ {A B C}{f : B → C}{g : A → B} --{x : F A}
                              → fmap (f ∘ g) ≡ (fmap f ∘ fmap g)

open Functor {{...}} public

infixl 5 _<$>_

_<$>_ : ∀ {F} {{_ : Functor F}} {A B} → (A → B) → F A → F B
ff <$> fb = fmap ff fb

record Applicative (AF : Set → Set) : Set₁ where
  infixl 4 _<*>_
  field
    --{{functorA}}                 : Functor AF
    pure                         : ∀ {A} → A → AF A
    _<*>_                        : ∀ {A B} → AF (A → B) → AF A → AF B
    applicative-identity-law     : ∀ {A}{v : AF A} → pure id <*> v ≡ v
    applicative-composition-law  : ∀ {A B C}{u : AF (B → C)}{v : AF (A → B)}{w : AF A}
                                   → pure _∘_ <*> u <*> v <*> w ≡ u <*> (v <*> w)
    applicative-homomorphism-law : ∀ {A B}{f : A → B}{x : A}
                                   → pure f <*> pure x ≡ pure (f x)
    applicative-interchange      : ∀ {A B}{u : AF (A -> B)}{x : A}
                                   → u <*> pure x ≡ pure (λ f → f x) <*> u

open Applicative {{...}} public

-- https://hackage.haskell.org/package/base/docs/Control-Monad.html#t:Monad
record Monad (M : Set → Set) : Set₁ where
  infixl 3 _>>=_
  field
    --{{applicativeM}}         : Applicative M
    return                   : ∀ {A} → A → M A
    _>>=_                    : ∀ {A B} → M A → (A → M B) → M B
    monad-left-identity-law  : ∀ {A B}{x : A}{k : A → M B}
                               → return x >>= k ≡ k x
    monad-right-identity-law : ∀ {A}{m : M A}
                               → m >>= return ≡ m
    monad-associativity-law  : ∀ {A B C}{m : M A}{k : A → M B }{h : B → M C}
                               → m >>= (λ x → k x >>= h)  ≡  (m >>= k) >>= h

open Monad {{...}} public

_>>_ : ∀ {M} {{_ : Monad M}} {A B} → M A → M B → M B
ma >> mb = ma >>= λ _ → mb

fail : ∀ {A} → Maybe A
fail = Nothing
