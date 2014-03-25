; HTTP/2.0 draft-06 implementation in Common Lisp
; Copyright (c) 2014 Akamai Technologies, Inc.
;
; Originally a port of Ruby code (as of Feb 2014) at https://github.com/igrigorik/http-2
; Which contains the following license/copyright statement:
; (MIT License) - Copyright (c) 2013 Ilya Grigorik GA
;
; Permission is hereby granted, free of charge, to any person obtaining a copy
; of this software and associated documentation files (the "Software"), to deal
; in the Software without restriction, including without limitation the rights
; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
; copies of the Software, and to permit persons to whom the Software is
; furnished to do so, subject to the following conditions:
;
; The above copyright notice and this permission notice shall be included in
; all copies or substantial portions of the Software.
;
; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
; THE SOFTWARE.

; TODO:
; -- ensure the file compiles on a clean image -BUG
; -- infinity support is SBCL-specific -PORTABILITY
; -- loop keywords are not consistent local/keyword -STYLE
; -- dependency on ALEXANDRIA -LIBRARIES
; -- dependency on USOCKET -LIBRARIES
; -- dependency on TRIVIAL-GRAY-STREAMS -LIBRARIES
; -- incomplete protection on array lengths -STABILITY
; -- array sizing and bounds checking is weak -STABILITY
; -- incomplete comment preservation from original ruby -STYLE
; -- note when initialize methods are rolled into CLOS defclass operation -STYLE
; -- make sure bytesize vs length is done OK -STABILITY
; -- use modules for namespacing as in the original, or rename some generics -STYLE
; -- @ sigil for avoiding shadowing/conflicts with CL -STYLE
; -- PUSH/SHIFT used as per Ruby but REVERSE/NREVERSE added -STYLE
; -- WHEN NOT models Ruby if ! but could become UNLESS -STYLE
; -- review DEFCLASS's for consistency -STYLE
; -- BUFFER-CONCAT-* might be rolled into a generic BUFFER-CONCAT -STYLE
; -- might be best to forego the #" reader -STYLE
; -- COPY-TREE on default tables might be able to be COPY-ALIST -SPEED
; -- not sure if BUFFER-INSPECT is exactly like Ruby .inspect -DEBUGABILITY
; -- document data structure choices in port from Ruby types -DOCUMENTATION
; -- should speed test against the Ruby -SPEED

(require :alexandria)
(require :babel)
(require :puri)
(require :usocket)
(require :cl+ssl)

(defpackage :http2
  (:documentation "HTTP/2.0 draft-06 implementation.")
  (:use :common-lisp :alexandria :puri :usocket :cl+ssl)
  (:shadow #:stream #:stream-error))

(in-package :http2)

(declaim (optimize (debug 3) (safety 3) (speed 0) (space 0) (compilation-speed 0)))
