;; Copyright (c) 2014 Akamai Technologies, Inc. (MIT License)

(in-package :cl-user)

(defpackage :cl-http2-protocol-asd
  (:use :cl :asdf))

(in-package :cl-http2-protocol-asd)

(defsystem :cl-http2-protocol
  :description "HTTP/2 draft-14/hpack-09 (h2-14) implementation with CL-ASYNC client/server examples.
For HTTP/2 draft-14, see: http://tools.ietf.org/html/draft-ietf-httpbis-http2-14
For other implementations, see: https://github.com/http2/http2-spec/wiki/Implementations
Originally a port of Ruby code by Ilya Grigorik, see: https://github.com/igrigorik/http-2"
  :version "0.9.0"
  :author "Martin Flack"
  :licence "MIT"
  :depends-on (:alexandria
	       :anaphora
	       :babel
	       :puri
	       :usocket
	       :cl+ssl
	       :cl-async
	       :cl-async-ssl)
  :components ((:file "packages")
	       (:file "util" :depends-on ("packages"))
	       (:file "error" :depends-on ("util"))
	       (:file "buffer" :depends-on ("util" "error"))
	       (:file "framer" :depends-on ("util" "error" "buffer"))
	       (:file "flow-buffer" :depends-on ("util" "buffer" "framer"))
	       (:file "emitter" :depends-on ("util"))
	       (:file "connection" :depends-on ("util" "error" "buffer" "flow-buffer" "emitter"))
	       (:file "huffman" :depends-on ("util" "error" "buffer"))
	       (:file "compressor" :depends-on ("util" "error" "buffer" "huffman"))
	       (:file "stream" :depends-on ("util" "error" "buffer" "flow-buffer" "framer" "emitter"))
	       (:file "client" :depends-on ("util" "connection" "compressor" "stream"))
	       (:file "server" :depends-on ("util" "error" "connection" "compressor" "stream"))
	       (:file "ssl" :depends-on ("util"))
	       (:file "tcp" :depends-on ("util"))
	       (:file "tcp-ssl" :depends-on ("util"))
	       (:file "example" :depends-on ("util" "ssl" "tcp" "tcp-ssl" "client" "server"))))
