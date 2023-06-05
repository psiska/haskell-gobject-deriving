{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}


module ListEntry where

import Data.Functor (void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Coerce (coerce)
import Data.GI.Base (
  GObject,
  ManagedPtr (..),
  TypedObject (glibType),
  toGValue,
  unsafeCastTo,
 )
import Data.GI.Base.GObject (
  DerivedGObject (..),
  GObjectClass (..),
  gobjectGetPrivateData,
  gobjectModifyPrivateData,
  registerGType,
 )
import Data.GI.Base.GParamSpec (CStringPropertyInfo (..), PropertyInfo (..), GParamFlag(..))

import Data.GI.Base.Overloading (AttributeList, HasAttributeList, HasParentTypes, IsDescendantOf, OverloadedMethod (overloadedMethod), OverloadedMethodInfo, ParentTypes, SignalList)
import qualified Data.GI.Base.Overloading as O
import Data.GI.Base.ShortPrelude (AttrInfo (..), AttrOpTag (..), GValueConstruct (..), gobjectInstallCStringProperty, gobjectInstallProperty)
import Data.Text
import GHC.OverloadedLabels (IsLabel (..))
import qualified GHC.Records as R
import GI.GObject (Object)
import qualified GI.GObject as GObj

newtype ListEntry = ListEntry (ManagedPtr ListEntry)

instance TypedObject ListEntry where
  glibType = registerGType ListEntry

instance GObject ListEntry

data ListEntryPrivate = ListEntryPrivate
  { someString :: Maybe Text
  , someBool :: Bool
  }

instance DerivedGObject ListEntry where
  type GObjectParentType ListEntry = Object
  type GObjectPrivateData ListEntry = ListEntryPrivate

  objectTypeName = "GI-ListEntry"
  objectClassInit = listEntryClassInit
  objectInstanceInit = listEntryInstanceInit

instance HasParentTypes ListEntry
type instance ParentTypes ListEntry = Object ': ParentTypes Object

class (GObject o, IsDescendantOf ListEntry o) => IsListEntry o
instance (GObject o, IsDescendantOf ListEntry o) => IsListEntry o

toListEntry :: (MonadIO m, IsListEntry o) => o -> m ListEntry
toListEntry = liftIO . unsafeCastTo ListEntry

-- Properties
-- SomeString
someStringProperty :: CStringPropertyInfo ListEntry
someStringProperty = CStringPropertyInfo
  { name = "some-string"
  , nick = "Some String"
  , blurb = "Test string property"
  , defaultValue = Nothing
  , flags = Nothing
  , setter = setSomeStringCString
  , getter = getSomeStringCString
  }

setSomeStringCString :: ListEntry -> Maybe Text -> IO ()
setSomeStringCString obj newValue = do
  oldValue <- someString <$> gobjectGetPrivateData obj
  if newValue == oldValue
    then pure ()
    else do
      void $ gobjectModifyPrivateData obj (\p -> p{someString = newValue})

getSomeStringCString :: ListEntry -> IO (Maybe Text)
getSomeStringCString obj = do
  someString <$> gobjectGetPrivateData obj

-- SomeBool
someBoolProperty :: PropertyInfo ListEntry Bool
someBoolProperty = PropertyInfo
  { name = "some-bool"
  , nick = "Some Bool"
  , blurb = "Test bool property"
  , flags = Just [GParamReadable, GParamWritable, GParamConstruct]
  --, flags = Nothing
  , setter = setSomeBool
  , getter = getSomeBool
  }

setSomeBool :: ListEntry -> Bool -> IO ()
setSomeBool obj newValue = do
  oldValue <- someBool <$> gobjectGetPrivateData obj
  if newValue == oldValue
    then pure ()
    else do
      void $ gobjectModifyPrivateData obj (\p -> p{someBool = newValue})

getSomeBool :: ListEntry -> IO Bool
getSomeBool obj =
  someBool <$> gobjectGetPrivateData obj


data SomeStringAttrInfo
instance AttrInfo SomeStringAttrInfo where
  type AttrAllowedOps SomeStringAttrInfo = '[ 'AttrGet, 'AttrSet, 'AttrConstruct ]
  type AttrBaseTypeConstraint SomeStringAttrInfo = IsListEntry
  type AttrGetType SomeStringAttrInfo = Maybe Text
  type AttrLabel SomeStringAttrInfo = "some-string"
  type AttrOrigin SomeStringAttrInfo = ListEntry

  -- TODO can we use the getSomeStringCString ?
  attrGet entry = do
    someString <$> gobjectGetPrivateData (coerce entry :: ListEntry)

  attrSet entry val = setSomeStringCString (coerce entry :: ListEntry) val

  attrConstruct val = do
    newValue <- case val of
      Just n -> toGValue (Just n)
      Nothing -> toGValue (Just ("" :: Text))
    return $ GValueConstruct "some-string" newValue

data SomeBoolAttrInfo
instance AttrInfo SomeBoolAttrInfo where
  type AttrAllowedOps SomeBoolAttrInfo = '[ 'AttrGet, 'AttrSet, 'AttrConstruct]
  type AttrBaseTypeConstraint SomeBoolAttrInfo = IsListEntry
  type AttrGetType SomeBoolAttrInfo = Bool
  type AttrLabel SomeBoolAttrInfo = "some-bool"
  type AttrOrigin SomeBoolAttrInfo = ListEntry

  attrGet entry = do
    someBool <$> gobjectGetPrivateData (coerce entry :: ListEntry)

  attrSet entry val = setSomeBool (coerce entry :: ListEntry) val

  attrConstruct val = do
    b <- toGValue val
    return $ GValueConstruct "some-bool" b

instance HasAttributeList ListEntry
type instance AttributeList ListEntry =
  '("someString", SomeStringAttrInfo) ': '("someBool", SomeBoolAttrInfo) ': AttributeList Object

type instance SignalList ListEntry = SignalList Object

type family ResolveListEntryMethod t o where
  ResolveListEntryMethod t o = GObj.ResolveObjectMethod t o

instance
  ( info ~ ResolveListEntryMethod method ListEntry
  , OverloadedMethod info ListEntry p
  , R.HasField method ListEntry p
  ) =>
  R.HasField method ListEntry p
  where
  getField = overloadedMethod @info

instance
  ( info ~ ResolveListEntryMethod t ListEntry
  , OverloadedMethod info ListEntry p
  ) =>
  IsLabel t (ListEntry -> p)
  where
  fromLabel = overloadedMethod @info

instance
  ( info ~ ResolveListEntryMethod t ListEntry
  , OverloadedMethodInfo info ListEntry
  ) =>
  IsLabel t (O.MethodProxy info ListEntry)
  where
  fromLabel = O.MethodProxy

listEntryClassInit :: GObjectClass -> IO ()
listEntryClassInit klass = do
  gobjectInstallCStringProperty klass someStringProperty
  gobjectInstallProperty klass someBoolProperty
  return ()

listEntryInstanceInit :: GObjectClass -> ListEntry -> IO ListEntryPrivate
listEntryInstanceInit _klass _ = do
  return $
    ListEntryPrivate
      { someString = Nothing
      , someBool = False
      }
