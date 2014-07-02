; Copyright (c) 2014 Akamai Technologies, Inc. (MIT License)

(in-package :cl-user)

(defpackage :cl-http2-protocol-asd
  (:use :cl :asdf))

(in-package :cl-http2-protocol-asd)

(defsystem :cl-http2-protocol
  :description "HTTP/2 draft-13/hpack-08 (h2-13) implementation with client/server examples.
Originally a port of Ruby code by Ilya Grigorik, see: https://github.com/igrigorik/http-2
For HTTP/2 draft-13, see: http://tools.ietf.org/html/draft-ietf-httpbis-http2-13
For other implementations, see: https://github.com/http2/http2-spec/wiki/Implementations"
  :version "0.6.3"
  :author "Martin Flack"
  :licence "MIT"
  :depends-on (:alexandria
	       :anaphora
	       :babel
	       :puri
	       :usocket
	       :cl+ssl)
  :components ((:file "packages")
	       (:file "util" :depends-on ("packages"))
	       (:file "error" :depends-on ("util"))
	       (:file "buffer" :depends-on ("util" "error"))
	       (:file "flow-buffer" :depends-on ("util" "buffer"))
	       (:file "emitter" :depends-on ("util"))
	       (:file "connection" :depends-on ("util" "error" "flow-buffer" "emitter" "error" "buffer"))
	       (:file "framer" :depends-on ("util" "error" "buffer"))
	       (:file "huffman" :depends-on ("util" "error" "buffer"))
	       (:file "compressor" :depends-on ("util" "error" "buffer" "huffman"))
	       (:file "stream" :depends-on ("util" "error" "flow-buffer" "emitter" "error" "buffer"))
	       (:file "client" :depends-on ("util" "connection" "compressor" "stream"))
	       (:file "server" :depends-on ("util" "error" "connection" "compressor" "stream"))
	       (:file "ssl" :depends-on ("util"))
	       (:file "net" :depends-on ("util" "ssl"))
	       (:file "example" :depends-on ("util" "ssl" "net" "client" "server"))))
