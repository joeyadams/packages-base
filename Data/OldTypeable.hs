{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE CPP
           , NoImplicitPrelude
           , OverlappingInstances
           , ScopedTypeVariables
           , ForeignFunctionInterface
           , FlexibleInstances
  #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

-- The -XOverlappingInstances flag allows the user to over-ride
-- the instances for Typeable given here.  In particular, we provide an instance
--      instance ... => Typeable (s a) 
-- But a user might want to say
--      instance ... => Typeable (MyType a b)

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Typeable
-- Copyright   :  (c) The University of Glasgow, CWI 2001--2004
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  portable
--
-- The 'Typeable' class reifies types to some extent by associating type
-- representations to types. These type representations can be compared,
-- and one can in turn define a type-safe cast operation. To this end,
-- an unsafe cast is guarded by a test for type (representation)
-- equivalence. The module "Data.Dynamic" uses Typeable for an
-- implementation of dynamics. The module "Data.Data" uses Typeable
-- and type-safe cast (but not dynamics) to support the \"Scrap your
-- boilerplate\" style of generic programming.
--
-----------------------------------------------------------------------------

module Data.OldTypeable {-# DEPRECATED "Use Data.Typeable instead" #-}
  (

        -- * The Typeable class
        Typeable( typeOf ),     -- :: a -> TypeRep

        -- * Type-safe cast
        cast,                   -- :: (Typeable a, Typeable b) => a -> Maybe b
        gcast,                  -- a generalisation of cast

        -- * Type representations
        TypeRep,        -- abstract, instance of: Eq, Show, Typeable
        showsTypeRep,

        TyCon,          -- abstract, instance of: Eq, Show, Typeable
        tyConString,    -- :: TyCon   -> String
        tyConPackage,   -- :: TyCon   -> String
        tyConModule,    -- :: TyCon   -> String
        tyConName,      -- :: TyCon   -> String

        -- * Construction of type representations
        mkTyCon,        -- :: String  -> TyCon
        mkTyCon3,       -- :: String  -> String -> String -> TyCon
        mkTyConApp,     -- :: TyCon   -> [TypeRep] -> TypeRep
        mkAppTy,        -- :: TypeRep -> TypeRep   -> TypeRep
        mkFunTy,        -- :: TypeRep -> TypeRep   -> TypeRep

        -- * Observation of type representations
        splitTyConApp,  -- :: TypeRep -> (TyCon, [TypeRep])
        funResultTy,    -- :: TypeRep -> TypeRep   -> Maybe TypeRep
        typeRepTyCon,   -- :: TypeRep -> TyCon
        typeRepArgs,    -- :: TypeRep -> [TypeRep]
        typeRepKey,     -- :: TypeRep -> IO TypeRepKey
        TypeRepKey,     -- abstract, instance of Eq, Ord

        -- * The other Typeable classes
        -- | /Note:/ The general instances are provided for GHC only.
        Typeable1( typeOf1 ),   -- :: t a -> TypeRep
        Typeable2( typeOf2 ),   -- :: t a b -> TypeRep
        Typeable3( typeOf3 ),   -- :: t a b c -> TypeRep
        Typeable4( typeOf4 ),   -- :: t a b c d -> TypeRep
        Typeable5( typeOf5 ),   -- :: t a b c d e -> TypeRep
        Typeable6( typeOf6 ),   -- :: t a b c d e f -> TypeRep
        Typeable7( typeOf7 ),   -- :: t a b c d e f g -> TypeRep
        gcast1,                 -- :: ... => c (t a) -> Maybe (c (t' a))
        gcast2,                 -- :: ... => c (t a b) -> Maybe (c (t' a b))

        -- * Default instances
        -- | /Note:/ These are not needed by GHC, for which these instances
        -- are generated by general instance declarations.
        typeOfDefault,  -- :: (Typeable1 t, Typeable a) => t a -> TypeRep
        typeOf1Default, -- :: (Typeable2 t, Typeable a) => t a b -> TypeRep
        typeOf2Default, -- :: (Typeable3 t, Typeable a) => t a b c -> TypeRep
        typeOf3Default, -- :: (Typeable4 t, Typeable a) => t a b c d -> TypeRep
        typeOf4Default, -- :: (Typeable5 t, Typeable a) => t a b c d e -> TypeRep
        typeOf5Default, -- :: (Typeable6 t, Typeable a) => t a b c d e f -> TypeRep
        typeOf6Default  -- :: (Typeable7 t, Typeable a) => t a b c d e f g -> TypeRep

  ) where

import Data.OldTypeable.Internal hiding (mkTyCon)

import Unsafe.Coerce
import Data.Maybe

#ifdef __GLASGOW_HASKELL__
import GHC.Base
import GHC.Err          (undefined)

import GHC.Fingerprint.Type
import {-# SOURCE #-} GHC.Fingerprint
   -- loop: GHC.Fingerprint -> Foreign.Ptr -> Data.Typeable
   -- Better to break the loop here, because we want non-SOURCE imports
   -- of Data.Typeable as much as possible so we can optimise the derived
   -- instances.

#endif

#ifdef __HUGS__
import Hugs.Prelude     ( Key(..), TypeRep(..), TyCon(..), Ratio,
                          Handle, Ptr, FunPtr, ForeignPtr, StablePtr )
import Hugs.IORef       ( IORef, newIORef, readIORef, writeIORef )
import Hugs.IOExts      ( unsafePerformIO )
        -- For the Typeable instance
import Hugs.Array       ( Array )
import Hugs.IOArray
import Hugs.ConcBase    ( MVar )
#endif

#ifdef __NHC__
import NHC.IOExtras (IOArray,IORef,newIORef,readIORef,writeIORef,unsafePerformIO)
import IO (Handle)
import Ratio (Ratio)
        -- For the Typeable instance
import NHC.FFI  ( Ptr,FunPtr,StablePtr,ForeignPtr )
import Array    ( Array )
#endif

#include "OldTypeable.h"

{-# DEPRECATED typeRepKey "TypeRep itself is now an instance of Ord" #-}
-- | (DEPRECATED) Returns a unique key associated with a 'TypeRep'.
-- This function is deprecated because 'TypeRep' itself is now an
-- instance of 'Ord', so mappings can be made directly with 'TypeRep'
-- as the key.
--
typeRepKey :: TypeRep -> IO TypeRepKey
typeRepKey (TypeRep f _ _) = return (TypeRepKey f)

        -- 
        -- let fTy = mkTyCon "Foo" in show (mkTyConApp (mkTyCon ",,")
        --                                 [fTy,fTy,fTy])
        -- 
        -- returns "(Foo,Foo,Foo)"
        --
        -- The TypeRep Show instance promises to print tuple types
        -- correctly. Tuple type constructors are specified by a 
        -- sequence of commas, e.g., (mkTyCon ",,,,") returns
        -- the 5-tuple tycon.

newtype TypeRepKey = TypeRepKey Fingerprint
  deriving (Eq,Ord)

----------------- Construction ---------------------

{-# DEPRECATED mkTyCon "either derive Typeable, or use mkTyCon3 instead" #-}
-- | Backwards-compatible API
mkTyCon :: String       -- ^ unique string
        -> TyCon        -- ^ A unique 'TyCon' object
mkTyCon name = TyCon (fingerprintString name) "" "" name

-------------------------------------------------------------
--
--              Type-safe cast
--
-------------------------------------------------------------

-- | The type-safe cast operation
cast :: (Typeable a, Typeable b) => a -> Maybe b
cast x = r
       where
         r = if typeOf x == typeOf (fromJust r)
               then Just $ unsafeCoerce x
               else Nothing

-- | A flexible variation parameterised in a type constructor
gcast :: (Typeable a, Typeable b) => c a -> Maybe (c b)
gcast x = r
 where
  r = if typeOf (getArg x) == typeOf (getArg (fromJust r))
        then Just $ unsafeCoerce x
        else Nothing
  getArg :: c x -> x 
  getArg = undefined

-- | Cast for * -> *
gcast1 :: (Typeable1 t, Typeable1 t') => c (t a) -> Maybe (c (t' a)) 
gcast1 x = r
 where
  r = if typeOf1 (getArg x) == typeOf1 (getArg (fromJust r))
       then Just $ unsafeCoerce x
       else Nothing
  getArg :: c x -> x 
  getArg = undefined

-- | Cast for * -> * -> *
gcast2 :: (Typeable2 t, Typeable2 t') => c (t a b) -> Maybe (c (t' a b)) 
gcast2 x = r
 where
  r = if typeOf2 (getArg x) == typeOf2 (getArg (fromJust r))
       then Just $ unsafeCoerce x
       else Nothing
  getArg :: c x -> x 
  getArg = undefined

