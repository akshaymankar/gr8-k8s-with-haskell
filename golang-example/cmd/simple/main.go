package main

import (
	"fmt"
	"path/filepath"

	metav1 "k8s.io/apimachinery/pkg/apis/meta/v1"
	"k8s.io/client-go/kubernetes"
	_ "k8s.io/client-go/plugin/pkg/client/auth"
	"k8s.io/client-go/tools/clientcmd"
	"k8s.io/client-go/util/homedir"
)

func main() {
	fmt.Println("hello")
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
	for _, pod := range podList.Items {
		if pod.Name != "" {
			fmt.Println(pod.Name)
		} else {
			fmt.Println("Name not found")
		}
	}
}
