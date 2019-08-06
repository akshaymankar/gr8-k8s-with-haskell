{-# LANGUAGE BlockArguments   #-}
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE PolyKinds        #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TypeOperators    #-}
module Effects.Kube where

import Control.Monad.Catch
import Kubernetes.OpenAPI
import Network.HTTP.Client (Manager)
import Polysemy

import qualified Kubernetes.OpenAPI.API.CoreV1 as CoreV1
import qualified Kubernetes.OpenAPI.API.AppsV1 as AppsV1

data Kube m a where
  KubeReq ::
    (Produces req accept, MimeUnrender accept a, MimeType contentType) =>
    KubernetesRequest req contentType a accept
    -> Kube m a

  ListNamespacedPods        :: Namespace -> Kube m V1PodList
  ListNamespacedReplicaSets :: Namespace -> Kube m V1ReplicaSetList
  ListNamespacedDeployments :: Namespace -> Kube m V1DeploymentList

makeSem ''Kube

kubeToIO ::
  Member (Embed IO) r =>
  Manager
  -> KubernetesClientConfig
  -> Sem (Kube ': r) a
  -> Sem r a
kubeToIO mgr cfg = interpret $ embed . executeKube mgr cfg

executeKube ::
  Manager -> KubernetesClientConfig -> Kube m a -> IO a
executeKube mgr cfg (KubeReq req) = dispatchK8sRequest mgr cfg req
executeKube mgr cfg (ListNamespacedPods ns) =
  executeKube mgr cfg
  $ KubeReq
  $ CoreV1.listNamespacedPod (Accept MimeJSON) ns
executeKube mgr cfg (ListNamespacedReplicaSets ns) =
  executeKube mgr cfg
  $ KubeReq
  $ AppsV1.listNamespacedReplicaSet (Accept MimeJSON) ns
executeKube mgr cfg (ListNamespacedDeployments ns) =
  executeKube mgr cfg
  $ KubeReq
  $ AppsV1.listNamespacedDeployment (Accept MimeJSON) ns

dispatchK8sRequest ::
  (Produces req accept, MimeUnrender accept a,
   MimeType contentType) =>
  Manager
  -> KubernetesClientConfig
  -> KubernetesRequest req contentType a accept
  -> IO a
dispatchK8sRequest mgr cfg req = dispatchMime' mgr cfg req
                                 >>= either (throwM . MimeException) pure

newtype MimeException = MimeException MimeError
  deriving (Show)

instance Exception MimeException
