{-# LANGUAGE OverloadedStrings #-}
module Simple where

import Kubernetes.Client
import Kubernetes.OpenAPI
import Kubernetes.OpenAPI.API.CoreV1

import Control.Monad.Catch
import Data.Function       ((&))
import Data.Map
import GHC.Conc
import System.Environment

import qualified Data.Text.IO as T

simple :: IO ()
simple = do
  oidcCache <- newTVarIO $ fromList []
  homeDir <- getEnv "HOME"
  (mgr, cfg) <- kubeClient oidcCache
                $ KubeConfigFile (homeDir <> "/.kube/config")

  podList <- listNamespacedPod (Accept MimeJSON) (Namespace "kube-system")
             & dispatchMime' mgr cfg
             & (>>= either throwM pure)
  mapM_ printPodName (v1PodListItems podList)

printPodName :: V1Pod -> IO ()
printPodName pod = case v1ObjectMetaName =<< v1PodMetadata pod of
                     Nothing   -> print "Name not found"
                     Just name -> T.putStrLn name

instance Exception MimeError
