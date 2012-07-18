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

(defpackage "DARTS.LIB.SEQUENCE-METRICS"
  (:use "COMMON-LISP")
  (:export "LEVENSHTEIN-DISTANCE"
           "JARO-DISTANCE"
           "JARO-WINKLER-DISTANCE"
           "HAMMING-DISTANCE"
           "LONGEST-COMMON-SUBSEQUENCE-LENGTH"
           "LONGEST-COMMON-SUBSEQUENCE*-LENGTH"
           "LONGEST-COMMON-SUBSEQUENCES*"
           "LONGEST-COMMON-SUBSTRINGS"
           "LONGEST-COMMON-SUBSTRING-LENGTH"
           "STRING-LEVENSHTEIN-DISTANCE"
           "STRING-JARO-DISTANCE"
           "STRING-JARO-WINKLER-DISTANCE"
           "STRING-HAMMING-DISTANCE"
           "MAP-NGRAMS" "DO-NGRAMS" "LIST-NGRAMS"
           )
  (:documentation "This package exports various forms of metric functions
on sequences. Among the ones provided are:

  - Levenshtein distance
  - Jaro and Jaro/Winkler distance
  - Hamming distance

Most distances are provided in a very general form, working an arbitrary
sequences. However, since most of these distance functions are usually 
applied to strings, for some frequently used metrics, optimized string
versions are provided.

This package also exports a few other utility functions, which strictly
speaking don't really belong here, such as the n-gram related stuff. They
live in this package, because they used to do so since the dawn of 
time..."))
