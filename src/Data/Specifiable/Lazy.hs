{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}

#ifdef DEFINE_AESON_INSTANCES
{-# LANGUAGE ScopedTypeVariables #-}
#endif

module Data.Specifiable.Lazy
  where

import Control.Applicative (Applicative((<*>), pure), Alternative((<|>), empty))
import Control.Monad (Monad((>>=), return), MonadPlus(mzero, mplus))
import Data.Data (Data, Typeable, typeRep)
import Data.Either (Either(Left, Right))
import Data.Eq (Eq)
import Data.Function (($), (.))
import Data.Functor (Functor(fmap), (<$>), (<$))
import Data.Maybe (Maybe(Just, Nothing))
import Data.Monoid (Monoid(mappend, mempty))
import Data.Ord (Ord)
import Data.Proxy (Proxy(Proxy))
import Data.Semigroup (Semigroup((<>)))
import GHC.Generics (Generic, Generic1)
import Text.Read (Read)
import Text.Show (Show, showString, showsPrec)

#ifdef DEFINE_AESON_INSTANCES
import Data.Aeson (FromJSON(parseJSON), ToJSON(toJSON))
import qualified Data.Aeson as Aeson (Value(Null))
import qualified Data.Aeson.Types as Aeson (typeMismatch)
#endif

import Data.Default.Class (Default(def))
import Data.Profunctor (Profunctor, dimap)


-- | Data type which is isomorphic to @'Maybe' ('Either' a b)@:
--
-- @
-- 'Unspecified' ~ 'Nothing'
-- 'Imprecise' ~ 'Just' '.' 'Left'
-- 'Specific' ~ 'Just' '.' 'Right'
-- @
--
-- Usually the type @b@ ('Specific' value) is an enum (simple sum type) or
-- other well known value. The type @a@ ('Imprecise' value) is a fallback to a
-- more generic type, e.g. 'Data.String.String', and is used when the value
-- doesn't match the more specific type. If the value can not fit even the @a@
-- type ('Imprecise') then 'Unspecified' should be used.
data Specifiable a b
    = Unspecified
    -- ^ Value is not specified. This constructor behaves as neutral element
    -- for 'Semigroup' and 'Monoid' instances.

    | Imprecise a
    | Specific b
  deriving (Data, Eq, Generic, Generic1, Ord, Show, Read, Typeable)

instance Functor (Specifiable a) where
    fmap f = \case
        Unspecified -> Unspecified
        Imprecise a -> Imprecise a
        Specific  b -> Specific (f b)

instance Applicative (Specifiable a) where
    pure = Specific

    f <*> x = case f of
        Unspecified -> Unspecified
        Imprecise a -> Imprecise a
        Specific  g -> fmap g x

instance Monad (Specifiable a) where
    return = pure

    x >>= f = case x of
        Unspecified -> Unspecified
        Imprecise a -> Imprecise a
        Specific  b -> f b

instance Alternative (Specifiable a) where
    empty = Unspecified

    x <|> y = case x of
        Unspecified -> y
        Imprecise _ -> y
        Specific  b -> Specific b

instance MonadPlus (Specifiable a) where
    mzero = empty
    mplus = (<|>)

-- | 'Semigroup' instance treats only 'Unspecified' as a neutral element, in
-- which it differs from 'Alternative' and 'MonadPlus' instances.
--
-- @
-- 'Unspecified' '<>' x = x
-- x '<>' 'Unspecified' = x
-- x '<>' _           = x
-- @
instance Semigroup (Specifiable a b) where
    Unspecified <> x = x
    x <> Unspecified = x
    x <> _           = x

-- |
-- @
-- 'mempty' = 'Unspecified'
--
-- 'Unspecified' ``mappend`` x = x
-- x ``mappend`` 'Unspecified' = x
-- x ``mappend`` _           = x
-- @
instance Monoid (Specifiable a b) where
    mempty = Unspecified
    mappend = (<>)

-- | @'def' = 'Unspecified'@
instance Default (Specifiable a b) where
    def = Unspecified

-- {{{ Aeson Instances --------------------------------------------------------

#ifdef DEFINE_AESON_INSTANCES

-- | When types @a ~ b@, i.e. are equal, then the result will never be
-- 'Imprecise'. Dependency on 'Typeable' is for better error messages.
instance
    ( Typeable a
    , Typeable b
    , FromJSON a
    , FromJSON b
    ) => FromJSON (Specifiable a b)
  where
    parseJSON v = (Unspecified <$ null v)
        <|> (Specific <$> parseJSON v)
        <|> (Imprecise <$> parseJSON v)
        <|> Aeson.typeMismatch expectedType v
      where
        expectedType = showString "one of: null, "
            . showsPrec 10 (typeRep (Proxy :: Proxy a))
            . showString ", or "
            $ showsPrec 10 (typeRep (Proxy :: Proxy b)) "\""

        null Aeson.Null = pure ()
        null _          = mzero

instance (ToJSON a, ToJSON b) => ToJSON (Specifiable a b) where
    toJSON = \case
        Unspecified -> Aeson.Null
        Imprecise b -> toJSON b
        Specific a -> toJSON a

#endif
    -- DEFINE_AESON_INSTANCES

-- }}} Aeson Instances --------------------------------------------------------

-- | Catamorphism (destructor) for 'Specifiable' type. Its behaviour is similar
-- to 'Data.Maybe.maybe' and 'Data.Either.either'.
specifiable :: c -> (a -> c) -> (b -> c) -> Specifiable a b -> c
specifiable c f g = \case
    Unspecified -> c
    Imprecise a -> f a
    Specific  b -> g b

toMaybeEither :: Specifiable a b -> Maybe (Either a b)
toMaybeEither = specifiable Nothing (Just . Left) (Just . Right)

fromMaybeEither :: Maybe (Either a b) -> Specifiable a b
fromMaybeEither = \case
    Nothing        -> Unspecified
    Just (Left  a) -> Imprecise a
    Just (Right b) -> Specific b

maybeEither
    :: (Profunctor p, Functor f)
    => p (Maybe (Either a b)) (f (Maybe (Either a1 b1)))
    -> p (Specifiable a b) (f (Specifiable a1 b1))
maybeEither = dimap toMaybeEither (fmap fromMaybeEither)
