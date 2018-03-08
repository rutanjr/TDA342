module Control.RawMonad where

open import Prelude

record RawFunctor (F : Set → Set) : Set₁ where
  field
    fmap                    : ∀ {A B} → (A → B) → F A → F B
    functor-identity-law    : ∀ {A}{x : F A}
                              → (fmap id) x ≡ id x 
    functor-composition-law : ∀ {A B C}{f : B → C}{g : A → B}{x : F A}
                              → fmap (f ∘ g) x ≡ (fmap f ∘ fmap g ) x

record RawApplicative (AF : Set → Set) : Set₁ where
  infixl 4 _<*>_
  field
    pure                         : ∀ {A} → A → AF A
    _<*>_                        : ∀ {A B} → AF (A → B) → AF A → AF B
    applicative-identity-law     : ∀ {A}{v : AF A} → pure id <*> v ≡ v
    applicative-composition-law  : {A B C : Set}{u : AF (B → C)}{v : AF (A → B)}{w : AF A}
                                   → pure _∘_ <*> u <*> v <*> w ≡ u <*> (v <*> w)
    applicative-homomorphism-law : ∀ {A B}{f : A → B}{x : A}
                                   → pure f <*> pure x ≡ pure (f x)
    applicative-interchange      : ∀ {A B}{u : AF (A -> B)}{x : A}
                                   → u <*> pure x ≡ pure (λ f → f x) <*> u

open RawApplicative {{...}} public

record RawMonad (M : Set → Set) : Set₁ where
  infixl 3 _>>=_

  field
    return                   : ∀ {A} → A → M A
    _>>=_                    : ∀ {A B} → M A → (A → M B) → M B
    monad-left-identity-law  : ∀ {A B}{x : A}{k : A → M B}
                               → return x >>= k ≡ k x
    monad-right-identity-law : ∀ {A}{m : M A}
                               → m >>= return ≡ m
    monad-associativity-law  : ∀ {A B C}{m : M A}{k : A → M B }{h : B → M C}
                               → m >>= (λ x → k x >>= h)  ≡  (m >>= k) >>= h

open RawMonad {{...}} public

