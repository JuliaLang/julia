package main

import (
	"log"
	"net/http"
)

func main() {
	fs := http.FileServer(http.Dir("build"))
	http.Handle("/", fs)

	log.Println("Listening at http://localhost:3000...")
	http.ListenAndServe(":3000", nil)
}
