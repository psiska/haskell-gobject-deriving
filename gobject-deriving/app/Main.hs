{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NegativeLiterals    #-}
{-# LANGUAGE OverloadedLabels    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE DisambiguateRecordFields #-}

module Main where

import           Control.Monad
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Class
import           Data.GI.Base
import qualified Data.Text                   as Text
import qualified GI.GObject                  as GObj
import qualified GI.Gio                      as Gio
import qualified GI.Gtk                      as Gtk
import           System.Environment          (getArgs, getProgName)
import ListEntry

bindListItem :: GObj.Object -> IO ()
bindListItem obj = void $ runMaybeT $  do
      listItem <- MaybeT $ castTo Gtk.ListItem obj
      item <- MaybeT $ Gtk.getListItemItem listItem
      child <- MaybeT $ Gtk.getListItemChild listItem
      liEntry <- MaybeT $ castTo ListEntry item
      label <- MaybeT $ castTo Gtk.Label child
      sString <- MaybeT $ getSomeStringCString liEntry
      sBool <- MaybeT $ Just <$> getSomeBool liEntry
      lift $ putStrLn $ "Str: " <> Text.unpack sString <> " Bool: " <> show sBool
      Gtk.setLabelLabel label ("Str: " <> sString <> " Bool: " <> Text.pack (show sBool))
  --set label [ #label := ("Str: " <> sString <> " Bool: " <> show sBool) ]

createEmptyListItem :: GObj.Object -> IO ()
createEmptyListItem obj = do
  mListItem <- castTo Gtk.ListItem obj
  case mListItem of
    Just li -> do
      label <- new Gtk.Label [#halign := Gtk.AlignStart, #hexpand := False]
      set li [ #child := label ]
      pure ()
    Nothing -> pure ()

onActivate :: Gtk.Application -> IO ()
onActivate app = do
  listType <- glibType @ListEntry
  listModel <- Gio.listStoreNew listType
  i1 <- new ListEntry [ #someString := Just "OneTwo", #someBool := True]
  i2 <- new ListEntry [ #someString := Just "Three", #someBool := False]

  void $ Gio.listStoreAppend listModel i1
  void $ Gio.listStoreAppend listModel i2

  listSelection <- new Gtk.SingleSelection [ #model := listModel]
  listItemFa <- Gtk.signalListItemFactoryNew
  void $ on listItemFa #bind bindListItem
  void $ on listItemFa #setup createEmptyListItem
  listView <- new Gtk.ListView
    [ #model  := listSelection
    , #factory := listItemFa
    , #showSeparators := True
    ]

  window <- new Gtk.ApplicationWindow
    [ #application := app
    , #title := "GObject derivation"
    , #child := listView
    ]
  Gtk.windowPresent window

main :: IO ()
main = do
  app <- new Gtk.Application [ #applicationId := "haskell-gi.GObjDeriving.example" ]
  _ <- on app #activate (onActivate app)

  args <- getArgs
  progName <- getProgName
  void $ #run app (Just $ progName : args)
