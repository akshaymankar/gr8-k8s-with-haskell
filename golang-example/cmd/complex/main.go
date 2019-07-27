package main

import (
	"fmt"
	"path/filepath"
	"strings"

	appsv1 "k8s.io/api/apps/v1"
	corev1 "k8s.io/api/core/v1"
	metav1 "k8s.io/apimachinery/pkg/apis/meta/v1"
	"k8s.io/client-go/kubernetes"
	_ "k8s.io/client-go/plugin/pkg/client/auth"
	"k8s.io/client-go/tools/clientcmd"
	"k8s.io/client-go/util/homedir"
)

func main() {
	home := homedir.HomeDir()
	kubeconfig := filepath.Join(home, ".kube", "config")
	config, err := clientcmd.BuildConfigFromFlags("", kubeconfig)
	if err != nil {
		panic(err)
	}
	client, err := kubernetes.NewForConfig(config)
	if err != nil {
		panic(err)
	}

	podList, err := client.CoreV1().Pods("kube-system").List(metav1.ListOptions{})
	if err != nil {
		panic(err)
	}

	rsList, err := client.AppsV1().ReplicaSets("kube-system").List(metav1.ListOptions{})
	if err != nil {
		panic(err)
	}

	deploymentList, err := client.AppsV1().Deployments("kube-system").List(metav1.ListOptions{})
	if err != nil {
		panic(err)
	}

	for _, deployment := range deploymentList.Items {
		line, ok := makeResultLine(podList.Items, rsList.Items, deployment)
		if ok {
			line.print()
		}
	}
}

type ResultLine struct {
	label       string
	ipAddresses []string
}

func makeResultLine(
	pods []corev1.Pod,
	replicaSets []appsv1.ReplicaSet,
	deployment appsv1.Deployment) (ResultLine, bool) {

	label, ok := deployment.Labels["k8s-app"]
	if !ok {
		return ResultLine{}, false
	}

	ownedReplicaSets := findOwnedReplicaSets(deployment, replicaSets)
	ownedPods := findOwnedPods(ownedReplicaSets, pods)
	var ips []string
	for _, pod := range ownedPods {
		if pod.Status.PodIP != "" {
			ips = append(ips, pod.Status.PodIP)
		}
	}
	return ResultLine{
		label:       label,
		ipAddresses: ips,
	}, true
}

func findOwnedReplicaSets(deployment appsv1.Deployment, replicaSets []appsv1.ReplicaSet) []appsv1.ReplicaSet {
	var ownedReplicaSets []appsv1.ReplicaSet
	for _, rs := range replicaSets {
		for _, o := range rs.OwnerReferences {
			if o.Kind == "Deployment" && o.Name == deployment.Name {
				ownedReplicaSets = append(ownedReplicaSets, rs)
				break
			}
		}
	}
	return ownedReplicaSets
}

func findOwnedPods(replicaSets []appsv1.ReplicaSet, pods []corev1.Pod) []corev1.Pod {
	var ownedPods []corev1.Pod
	for _, pod := range pods {
		for _, rs := range replicaSets {
			for _, o := range pod.OwnerReferences {
				if o.Kind == "ReplicaSet" && o.Name == rs.Name {
					ownedPods = append(ownedPods, pod)
					break
				}
			}
		}
	}
	return ownedPods
}

func (r ResultLine) print() {
	var ipStr string
	if len(r.ipAddresses) == 0 {
		ipStr = "no IP Addresses found"
	} else {
		ipStr = strings.Join(r.ipAddresses, ", ")
	}

	fmt.Printf("%s -> %s\n", r.label, ipStr)
}
