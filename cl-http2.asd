(in-package :cl-user)

(defpackage :cl-http2-asd
  (:use :cl :asdf))

(in-package :cl-http2-asd)

(defsystem :cl-http2
  :description "HTTP/2.0 draft-06 implementation with client/server examples.
Originally a port of Ruby code by Ilya Grigorik, see: https://github.com/igrigorik/http-2
For HTTP/2.0 draft-06, see: http://tools.ietf.org/html/draft-ietf-httpbis-http2-06
For other implementations, see: https://github.com/http2/http2-spec/wiki/Implementations"
  :version "0.6.3"
  :author "Martin Flack <mflack@akamai.com>"
  :licence "MIT"
  :depends-on (:alexandria
	       :babel
	       :puri
	       :usocket
	       :cl+ssl)
  :components ((:file "packages")
	       (:file "util" :depends-on ("packages" (:require :alexandria)))
	       (:file "buffer" :depends-on ("util"))
	       (:file "flow-buffer" :depends-on ("util" "buffer"))
	       (:file "emitter" :depends-on ("util"))
	       (:file "error" :depends-on ("util"))
	       (:file "connection" :depends-on ("util" "flow-buffer" "emitter" "error" "buffer"))
	       (:file "framer" :depends-on ("util" "buffer"))
	       (:file "compressor" :depends-on ("util" "error" "buffer"))
	       (:file "stream" :depends-on ("util" "flow-buffer" "emitter" "error" "buffer"))
	       (:file "client" :depends-on ("util" "connection" "compressor" "stream" (:require :puri)))
	       (:file "server" :depends-on ("util" "connection" "compressor" "stream"))
	       (:file "ssl" :depends-on ("util" (:require :cl+ssl)))
	       (:file "example" :depends-on ("util" "ssl" "client" "server"))))
