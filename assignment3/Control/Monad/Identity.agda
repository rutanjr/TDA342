module Control.Monad.Identity where

open import Prelude
open import Control.Monad
open import Control.Structure

Identity : Set → Set
Identity = id

instance
  IdentityFunctor : Functor Identity
  IdentityFunctor =
    record
      { fmap                    = id
      ; functor-identity-law    = refl
      ; functor-composition-law = refl
      }

  IdentityApplicative : Applicative Identity
  IdentityApplicative =
    record
      { pure                         = id
      ; _<*>_                        = λ mf ma → mf ma
      ; applicative-identity-law     = refl
      ; applicative-composition-law  = refl
      ; applicative-homomorphism-law = refl
      ; applicative-interchange-law  = refl
      }

  IdentityMonad : Monad Identity
  IdentityMonad = 
    record
      { return                   = id
      ; _>>=_                    = λ ma mf → mf ma
      ; monad-left-identity-law  = refl
      ; monad-right-identity-law = refl
      ; monad-associativity-law  = refl
      }
      
