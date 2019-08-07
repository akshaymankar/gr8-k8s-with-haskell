module Complex where

import Kubernetes.Client.Config
import Kubernetes.OpenAPI
import Kubernetes.OpenAPI.API.AppsV1
import Kubernetes.OpenAPI.API.CoreV1

import Control.Monad             ((>=>))
import Control.Monad.Catch
import Data.Function             ((&))
import Data.Map                  (fromList)
import Data.Maybe                (mapMaybe)
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
  program mgr cfg

program :: Manager -> KubernetesClientConfig -> IO ()
program mgr cfg = do
  let namespace = Namespace "kube-system"
  pods <- listNamespacedPod (Accept MimeJSON) namespace
          & dispatchK8sRequest mgr cfg
          & fmap v1PodListItems
  replicaSets <- listNamespacedReplicaSet (Accept MimeJSON) namespace
                 & dispatchK8sRequest mgr cfg
                 & fmap v1ReplicaSetListItems
  deployments <- listNamespacedDeployment (Accept MimeJSON) namespace
                 & dispatchK8sRequest mgr cfg
                 & fmap v1DeploymentListItems
  mapMaybe (mkResultLine pods replicaSets) deployments
    & mapM_ printResultLine

data ResultLine = ResultLine { appLabel   :: T.Text
                             , ipAdresses :: [T.Text]
                             }
mkResultLine ::
  [V1Pod] -> [V1ReplicaSet] -> V1Deployment -> Maybe ResultLine
mkResultLine pods replicaSets deployment = do
  label <- v1DeploymentMetadata deployment
           >>= v1ObjectMetaLabels
           >>= Map.lookup "k8s-app"
  let ips = podsOfDeployment pods replicaSets deployment
            & mapMaybe (v1PodStatus >=> v1PodStatusPodIp)
  return $ ResultLine label ips

podsOfDeployment ::
  [V1Pod] -> [V1ReplicaSet] -> V1Deployment -> [V1Pod]
podsOfDeployment pods replicaSets deployment = do
  case v1DeploymentMetadata deployment >>= v1ObjectMetaName of
    Nothing -> []
    Just deploymentName ->
      let ownedRSNames = filter (isOwnedBy "Deployment" deploymentName) replicaSets
                         & mapMaybe (v1ReplicaSetMetadata >=> v1ObjectMetaName)
      in concatMap (filterOwned pods) ownedRSNames
  where
    isOwner kind name o =
      (v1OwnerReferenceKind o) == kind
      && (v1OwnerReferenceName o) == name
    isOwnedBy kind name x =
      ownerRefs x
      & (fmap $ any (isOwner kind name))
      & maybe False id
    filterOwned ps rsName = filter (isOwnedBy "ReplicaSet" rsName) ps

printResultLine :: ResultLine -> IO ()
printResultLine (ResultLine label ips) =
  T.putStrLn $ label <> " -> " <> (ipsText ips)
  where
    ipsText []        = "no IP Addresses found"
    ipsText addresses = (T.intercalate ", " addresses)

class HasMetadata k where
  objectMeta :: k -> Maybe V1ObjectMeta

instance HasMetadata V1Pod where
  objectMeta = v1PodMetadata

instance HasMetadata V1ReplicaSet where
  objectMeta = v1ReplicaSetMetadata

ownerRefs :: HasMetadata k => k -> Maybe [V1OwnerReference]
ownerRefs = objectMeta >=> v1ObjectMetaOwnerReferences

instance Exception MimeError
dispatchK8sRequest mgr cfg req = dispatchMime' mgr cfg req
                                 >>= either throwM pure
