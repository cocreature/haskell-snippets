{-# LANGUAGE DataKinds, FlexibleContexts, GADTs, ScopedTypeVariables, TypeOperators #-}
module PartialSOP
  ( fieldNames
  ) where

import           GHC.TypeLits
import           Generics.SOP
import qualified Generics.SOP.Type.Metadata as T

fieldNames :: forall a proxy modName tyName constrName fields.
  ( Generic a
  , HasDatatypeInfo a
  , KnownSymbol modName
  , KnownSymbol tyName
  , DatatypeInfoOf a ~ 'T.ADT modName tyName '[ 'T.Record constrName fields]
  ) => proxy a -> [String]
fieldNames proxy =
  case datatypeInfo proxy of
    ADT _modName _tyName constrInfos ->
      case constrInfos of
        (constr :* Nil) ->
          case constr of
            Record _name fields ->
              let f :: forall f. FieldInfo f -> K String f
                  f (FieldInfo name) = K name
              in hcollapse (hliftA f fields)
