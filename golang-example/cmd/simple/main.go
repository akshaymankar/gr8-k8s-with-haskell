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
	kubeconfig := filepath.Join(homedir.HomeDir(), ".kube", "config")
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
	for _, pod := range podList.Items {
		if pod.Name != "" {
			fmt.Println(pod.Name)
		} else {
			fmt.Println("Name not found")
		}
	}
	return nil
}
