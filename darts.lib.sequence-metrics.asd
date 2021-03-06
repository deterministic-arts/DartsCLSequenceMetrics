#|                                           -*- mode: lisp; coding: utf-8 -*-
  Deterministic Arts -- Sequence Metrics Library
  Copyright (c) 2011 Dirk Esser

  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files (the "Software"), to deal
  in the Software without restriction, including without limitation the rights
  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
  copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in
  all copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
  THE SOFTWARE.
|#

(in-package "COMMON-LISP-USER")

(defpackage "DARTS.ASDF" (:use "COMMON-LISP" "ASDF"))

(in-package "DARTS.ASDF")

(defsystem :darts.lib.sequence-metrics
  :name "darts.lib.sequence-metrics"
  :author "Dirk Esser"
  :version "0.1"
  :maintainer "Dirk Esser"
  :licence "MIT"
  :description "Provides various distance metrics on sequences"
  :long-description ""
  :depends-on ()
  :components
  ((:module :src
    :components
    ((:file "package") 
     (:file "types" :depends-on ("package"))
     (:file "levenshtein" :depends-on ("types"))
     (:file "hamming" :depends-on ("types"))
     (:file "lcs" :depends-on ("types"))
     (:file "jaro-winkler" :depends-on ("types"))
     (:file "ngrams" :depends-on ("types"))
     ))))
