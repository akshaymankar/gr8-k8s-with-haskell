{-# LANGUAGE OverloadedStrings #-}
module Simple where

import Kubernetes.Client
import Kubernetes.OpenAPI
import Kubernetes.OpenAPI.API.CoreV1

import Data.Function      ((&))
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
  podList <- dispatchK8sRequest mgr cfg
             $ listNamespacedPod (Accept MimeJSON) (Namespace "kube-system")
  mapM_ printPodName (v1PodListItems podList)

printPodName :: V1Pod -> IO ()
printPodName pod = case v1ObjectMetaName =<< v1PodMetadata pod of
                     Nothing   -> print "Name not found"
                     Just name -> T.putStrLn name

dispatchK8sRequest mgr cfg req = dispatchMime' mgr cfg req
                                 >>= either (error . mimeError) pure
