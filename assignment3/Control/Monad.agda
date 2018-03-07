module Control.Monad where

open import Prelude

-- https://hackage.haskell.org/package/base/docs/Control-Monad.html#t:Functor
record Functor (F : Set → Set) : Set₁ where
  -- field
open Functor {{...}} public

record Applicative (A : Set → Set) : Set₁ where  
  field
    {{functorA}} : Functor A

open Applicative {{...}} public

-- https://hackage.haskell.org/package/base/docs/Control-Monad.html#t:Monad
record Monad (M : Set → Set) : Set₁ where
  infixl 3 _>>=_

  field
    {{functorM}}             : Functor M
    {{applicativeM}}         : Applicative M
    return                   : ∀ {A} → A → M A
    _>>=_                    : ∀ {A B} → M A → (A → M B) → M B
    monad-left-identity-law  : ∀ {A B}{x : A}{k : A -> M B}
                               -> return x >>= k ≡ k x
    monad-right-identity-law : ∀ {A}{m : M A}
                               -> m >>= return ≡ m
    monad-associativity-law  : ∀ {A B C}{m : M A}{k : A -> M B }{h : B -> M C}
                               -> m >>= (λ x -> k x >>= h)  ≡  (m >>= k) >>= h

open Monad {{...}} public

_>>_ : ∀ {M} {{_ : Monad M}} {A B} → M A → M B → M B
ma >> mb = ma >>= λ _ → mb

instance
  ListFunctor : Functor List
  ListFunctor = {! !}

  ListMonad : Monad List
  ListMonad = {!!}

  MaybeFunctor : Functor Maybe
  MaybeFunctor = {!!}

  MaybeMonad : Monad Maybe
  MaybeMonad = {!!}

fail : ∀ {A} → Maybe A
fail = {!!}
