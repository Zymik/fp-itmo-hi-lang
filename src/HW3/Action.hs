{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module HW3.Action
  ( HiPermission (..)
  , PermissionException (..)
  , HIO (..)
  )
where

import Control.Exception (Exception)
import Control.Exception.Base (throw)
import Control.Monad.Reader
import qualified Data.ByteString as B
import Data.Sequence (Seq)
import Data.Set (Set, member)
import Data.String (IsString (..))
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8')
import qualified Data.Text.IO as TIO
import Data.Time (getCurrentTime)
import GHC.Exts (IsList (fromList))
import HW3.Base
import HW3.ValueInner (HiValueInner(returnHi))
import System.Directory
import System.Random (getStdRandom, uniformR)

data HiPermission
  = AllowRead
  | AllowWrite
  | AllowTime
  deriving (Show, Eq, Ord, Bounded, Enum)

data PermissionException = PermissionRequired HiPermission
  deriving (Show)

instance Exception PermissionException

newtype HIO a = HIO {runHIO :: Set HiPermission -> IO a}
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader (Set HiPermission)) via (ReaderT (Set HiPermission) IO)

instance HiMonad HIO where
  runAction :: HiAction -> HIO HiValue
  runAction = \case
    HiActionRead s ->
      do
        canRead
        isFile <- liftIO $ doesFileExist s
        if isFile
          then do
            file <- liftIO $ B.readFile s
            case decodeUtf8' file of
              Left _ -> returnHi file
              Right txt -> returnHi txt
          else do
            dirs <- liftIO $ listDirectory s
            returnHi @(Seq Text) $ fromList (fromString <$> dirs)
    
    HiActionWrite s bs ->
      do
        canWrite
        liftIO $ B.writeFile s bs
        return HiValueNull
    
    HiActionMkDir s ->
      do
        canWrite
        liftIO $ createDirectory s
        return HiValueNull
    
    HiActionChDir s ->
      do
        canRead
        liftIO $ setCurrentDirectory s
        return HiValueNull
    
    HiActionCwd ->
      do
        canRead
        dir <- liftIO getCurrentDirectory
        returnHi @Text $ fromString dir
    
    HiActionNow ->
      do
        canGetTime
        time <- liftIO getCurrentTime
        returnHi time
    
    HiActionRand left right ->
      do
        value <- getStdRandom $ uniformR (left, right)
        returnHi value
    
    HiActionEcho text ->
      do
        canWrite
        liftIO $ TIO.putStrLn text
        return HiValueNull
    where
      checkPermission :: HiPermission -> HIO ()
      checkPermission permission =
        do
          permissions <- ask
          if member permission permissions
            then return ()
            else throw $ PermissionRequired permission

      canRead :: HIO ()
      canRead = checkPermission AllowRead

      canWrite :: HIO ()
      canWrite = checkPermission AllowWrite

      canGetTime :: HIO ()
      canGetTime = checkPermission AllowTime
