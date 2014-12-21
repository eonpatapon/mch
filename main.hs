{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Prelude as P

import Control.Monad
import Control.Concurrent

import Data.ByteString.Char8 as BC (pack, unpack, empty)
import qualified Data.ByteString as B
import Data.Maybe
import Data.Char (toLower)

import System.IO
import System.Exit
import System.Directory
import System.Posix.Signals
import System.Process
import System.UDev.Context
import System.UDev.Device
import System.UDev.Monitor
import System.Posix.IO.Select
import System.Posix.IO.Select.Types
import System.Environment.XDG.BaseDir


subSystemDrm :: Device -> Bool
subSystemDrm dev = "drm" == unpack (fromMaybe BC.empty $ getSubsystem dev)


hotplugOn :: Device -> IO Bool
hotplugOn dev = do
    prop <- getProp dev "HOTPLUG"
    return (unpack prop == "1")


monitorChanged :: Device -> IO Bool
monitorChanged dev = do
    hotplug <- hotplugOn dev
    return (hotplug && subSystemDrm dev)


getProp:: Device -> String -> IO B.ByteString
getProp dev prop = do
    res <- getPropertyValue dev $ pack prop
    return (fromMaybe BC.empty res)


runHooks :: IO ()
runHooks = getUserConfigDir "mch" >>=
           getDirectoryContents >>=
           mapM_ (\a -> callProcess  a [""])


watchUdev :: IO()
watchUdev = withUDev $ \ udev -> do
    monitor <- newFromNetlink udev UDevId
    enableReceiving monitor
    fd <- getFd monitor
    forever $ do
      res <- select' [fd] [] [] Never
      case res of
        Just ([_], [], []) -> do
          dev <- receiveDevice monitor
          changed <- monitorChanged dev
          when changed runHooks
        Just (_, _, _) -> return ()
        Nothing -> return ()


exitOnQ :: IO()
exitOnQ = do
    hSetBuffering stdin NoBuffering
    c <- getChar
    when (toLower c /= 'q') exitOnQ
    putStrLn "Exiting..."
    exitSuccess


main :: IO ()
main = do
  tid <- myThreadId
  _ <- installHandler keyboardSignal (Catch (killThread tid)) Nothing
  _ <- forkIO watchUdev
  exitOnQ
