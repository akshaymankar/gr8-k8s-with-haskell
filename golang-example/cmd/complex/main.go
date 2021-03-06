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

	err = program(client)
	if err != nil {
		panic(err)
	}
}

func program(client kubernetes.Interface) error {
	podList, err := client.CoreV1().Pods("kube-system").List(metav1.ListOptions{})
	if err != nil {
		return err
	}

	rsList, err := client.AppsV1().ReplicaSets("kube-system").List(metav1.ListOptions{})
	if err != nil {
		return err
	}

	deploymentList, err := client.AppsV1().Deployments("kube-system").List(metav1.ListOptions{})
	if err != nil {
		return err
	}

	for _, deployment := range deploymentList.Items {
		line, ok := makeResultLine(podList.Items, rsList.Items, deployment)
		if ok {
			line.print()
		}
	}
	return nil
}

type resultLine struct {
	label       string
	ipAddresses []string
}

func makeResultLine(
	pods []corev1.Pod,
	replicaSets []appsv1.ReplicaSet,
	deployment appsv1.Deployment) (resultLine, bool) {

	label, ok := deployment.Labels["k8s-app"]
	if !ok {
		return resultLine{}, false
	}

	ownedReplicaSets := findOwnedReplicaSets(deployment, replicaSets)
	ownedPods := findOwnedPods(ownedReplicaSets, pods)
	var ips []string
	for _, pod := range ownedPods {
		if pod.Status.PodIP != "" {
			ips = append(ips, pod.Status.PodIP)
		}
	}
	return resultLine{
		label:       label,
		ipAddresses: ips,
	}, true
}

func matchesAnyOwnerRef(ownerRefs []metav1.OwnerReference, kind, name string) bool {
	for _, o := range ownerRefs {
		if o.Kind == kind && o.Name == name {
			return true
		}
	}
	return false
}

func findOwnedReplicaSets(deployment appsv1.Deployment, replicaSets []appsv1.ReplicaSet) []appsv1.ReplicaSet {
	var ownedReplicaSets []appsv1.ReplicaSet
	for _, rs := range replicaSets {
		if matchesAnyOwnerRef(rs.OwnerReferences, "Deployment", deployment.Name) {
			ownedReplicaSets = append(ownedReplicaSets, rs)
		}
	}
	return ownedReplicaSets
}

func findOwnedPods(replicaSets []appsv1.ReplicaSet, pods []corev1.Pod) []corev1.Pod {
	var ownedPods []corev1.Pod
	for _, rs := range replicaSets {
		for _, pod := range pods {
			if matchesAnyOwnerRef(pod.OwnerReferences, "ReplicaSet", rs.Name) {
				ownedPods = append(ownedPods, pod)
			}
		}
	}
	return ownedPods
}

func (r resultLine) print() {
	var ipStr string
	if len(r.ipAddresses) == 0 {
		ipStr = "no IP Addresses found"
	} else {
		ipStr = strings.Join(r.ipAddresses, ", ")
	}

	fmt.Printf("%s -> %s\n", r.label, ipStr)
}
