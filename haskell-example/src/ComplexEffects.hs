module ComplexEffects where

import Kubernetes.Client.Config
import Kubernetes.OpenAPI

import Control.Monad       ((>=>))
import Data.Function       ((&))
import Data.Map            (fromList)
import Data.Maybe          (mapMaybe)
import GHC.Conc
import System.Environment

import Polysemy
import Effects.Kube

import qualified Data.Map     as Map
import qualified Data.Text    as T
import qualified Data.Text.IO as T

complexWithEffects :: IO ()
complexWithEffects = do
  oidcCache <- newTVarIO $ fromList []
  homeDir <- getEnv "HOME"
  (mgr, cfg) <- kubeClient oidcCache
                $ KubeConfigFile (homeDir <> "/.kube/config")

  let namespace = Namespace "kube-system"

  runM . kubeToIO mgr cfg $ complex namespace
  >>= mapM_ printResultLine

data ResultLine = ResultLine { appLabel   :: T.Text
                             , ipAdresses :: [T.Text]}

complex :: Members '[Kube] r => Namespace -> Sem r [ResultLine]
complex ns = do
  pods <- v1PodListItems <$> listNamespacedPods ns
  replicaSets <- v1ReplicaSetListItems <$> listNamespacedReplicaSets ns
  deployments <- v1DeploymentListItems <$> listNamespacedDeployments ns
  return $ mapMaybe (mkResultLine pods replicaSets) deployments

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
