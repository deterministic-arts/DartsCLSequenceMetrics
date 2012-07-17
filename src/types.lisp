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

(in-package "DARTS.LIB.SEQUENCE-METRICS")

(deftype array-index ()
  "Integer type, which is large enough to hold an index into some
arbitrary array (in particular, into a string)."
  `(integer 0 #.array-dimension-limit))


(deftype sequence-function (result &rest additional-keys)
  "Type of sequence metric. This function type is the essential
sequence metric function type as is provided by most functions 
exposed by this package, if they operate on two generic sequences."
  `(function (sequence sequence 
			  &key (:start1 array-index) (:end1 (or null array-index))
			       (:start2 array-index) (:end2 (or null array-index))
				   (:test t) 
				   (:test-not t) 
				   (:key t)
				   ,@additional-keys) ,result))


(deftype string-function (result &rest additional-keys)
  "Type of string metric. This function type is the essential
sequence metric function type as is provided by most functions 
exposed by this package, if they operate on two actual strings."
  `(function (string string 
			  &key (:start1 array-index) (:end1 (or null array-index))
			       (:start2 array-index) (:end2 (or null array-index))
				   (:case-sensitive t)
				   ,@additional-keys)
			 ,result))
