module Complex where

import Kubernetes.Client.Config
import Kubernetes.OpenAPI
import Kubernetes.OpenAPI.API.AppsV1
import Kubernetes.OpenAPI.API.CoreV1

import Control.Monad             ((>=>))
import Data.Containers.ListUtils (nubOrd)
import Data.Function             ((&))
import Data.List                 (find)
import Data.Map                  (fromList)
import Data.Maybe                (catMaybes)
import GHC.Conc
import Network.HTTP.Client
import System.Environment

import qualified Data.Map     as Map
import qualified Data.Text    as T
import qualified Data.Text.IO as T

complex :: IO ()
complex = do
  oidcCache <- newTVarIO $ fromList []
  homeDir <- getEnv "HOME"
  (mgr, cfg) <- kubeClient oidcCache
                $ KubeConfigFile (homeDir <> "/.kube/config")

  let namespace = Namespace "kube-system"
  pods <- fmap v1PodListItems
          $ dispatchK8sRequest mgr cfg
          $ listNamespacedPod (Accept MimeJSON) namespace
  replicaSets <- fmap v1ReplicaSetListItems
                 $ dispatchK8sRequest mgr cfg
                 $ listNamespacedReplicaSet (Accept MimeJSON) namespace
  deployments <- fmap v1DeploymentListItems
                 $ dispatchK8sRequest mgr cfg
                 $ listNamespacedDeployment (Accept MimeJSON) namespace
  map (mkResultLine pods replicaSets) deployments
    & catMaybes
    & mapM_ printResultLine

data ResultLine = ResultLine { appLabel :: T.Text
                             , ipAdresses :: [T.Text]}

mkResultLine ::
  [V1Pod] -> [V1ReplicaSet] -> V1Deployment -> Maybe ResultLine
mkResultLine pods replicaSets deployment =
  ResultLine <$> label <*> extractIPs pods replicaSets deployment
  where
    label = v1DeploymentMetadata deployment
            >>= v1ObjectMetaLabels
            >>= Map.lookup "k8s-app"

extractIPs ::
  [V1Pod] -> [V1ReplicaSet] -> V1Deployment -> Maybe [T.Text]
extractIPs pods replicaSets deployment = do
  deploymentName <- name deployment
  let ownedRSNames = filter (rsIsOwnedBy deploymentName) replicaSets
                     & map name
                     & catMaybes
      ownedPods = map (filterOwned pods) ownedRSNames
                  & concat
  map (v1PodStatus >=> v1PodStatusPodIp) ownedPods
    & catMaybes
    & return
  where
    isDeployment deploymentName o =
      (v1OwnerReferenceKind o) == "Deployment"
      && (v1OwnerReferenceName o) == deploymentName
    isReplicaSet rsName o =
      (v1OwnerReferenceKind o) == "ReplicaSet"
      && (v1OwnerReferenceName o) == rsName
    rsIsOwnedBy deploymentName rs =
      ownerRefs rs
      & (fmap $ any (isDeployment deploymentName))
      & maybe False id
    podIsOwnedBy rsName pod =
      ownerRefs pod
      & (fmap $ any (isReplicaSet rsName))
      & maybe False id
    filterOwned pods rsName = filter (podIsOwnedBy rsName) pods

printResultLine :: ResultLine -> IO ()
printResultLine (ResultLine label ips) =
  T.putStrLn $ label <> " -> " <> (ipsText ips)
  where
    ipsText []        = "no IP Addresses found"
    ipsText addresses = (T.intercalate ", " addresses)

dispatchK8sRequest mgr cfg req = dispatchMime' mgr cfg req
                                 >>= either (error . mimeError) pure

class HasMetadata k where
  objectMeta :: k -> Maybe V1ObjectMeta

instance HasMetadata V1Pod where
  objectMeta = v1PodMetadata

instance HasMetadata V1Deployment where
  objectMeta = v1DeploymentMetadata

instance HasMetadata V1ReplicaSet where
  objectMeta = v1ReplicaSetMetadata

name :: HasMetadata k => k -> Maybe T.Text
name = objectMeta >=> v1ObjectMetaName

labels :: HasMetadata k => k -> Maybe (Map.Map String T.Text)
labels = objectMeta >=> v1ObjectMetaLabels

ownerRefs :: HasMetadata k => k -> Maybe [V1OwnerReference]
ownerRefs = objectMeta >=> v1ObjectMetaOwnerReferences
