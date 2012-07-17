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

(declaim (ftype (sequence-function (integer 0)) hamming-distance))
(declaim (ftype (string-function (integer 0)) string-hamming-distance))


(defun hamming-distance (seq1 seq2 
						 &key (start1 0) (end1 nil)
						      (start2 0) (end2 nil)
						      (test #'eql have-test)
						      (test-not nil have-test-not)
						      (key #'identity have-key))
  (when (and have-test have-test-not)
	(error "cannot use ~S and ~S at the same time" :test :test-not))
  (let* ((len1 (- (or end1 (length seq1)) start1))
		 (len2 (- (or end2 (length seq2)) start2))
		 (min (min len1 len2))
		 (max (max len1 len2))
		 (diff (- max min)))
	(labels ((getplain (seq k) (elt seq k))
			 (getkey (seq k) (funcall key (elt seq k)))
			 (noteql (x y) (not (eql x y)))
			 (matchpos (x y) (not (funcall test x y)))
			 (matchneg (x y) (funcall test-not x y)))
	  (declare (inline getplain getkey matchpos matchneg noteql))
	  (macrolet ((counter (getter test)
				   `(loop
					   :repeat min
					   :for i :upfrom start1 :for e1 = (,getter seq1 i)
					   :for j :upfrom start2 :for e2 = (,getter seq2 j)
					   :counting (,test e1 e2))))
		(+ diff
		   (if have-key
			   (if have-test-not
				   (counter getkey matchneg)
				   (if have-test 
					   (counter getkey matchpos)
					   (counter getkey noteql)))
			   (if have-test-not
				   (counter getplain matchneg)
				   (if have-test
					   (counter getplain matchpos)
					   (counter getplain noteql)))))))))



(defun string-hamming-distance (seq1 seq2 
								&key (start1 0) (end1 nil)
								     (start2 0) (end2 nil)
								     (case-sensitive nil))

  (let* ((len1 (- (or end1 (length seq1)) start1))
		 (len2 (- (or end2 (length seq2)) start2))
		 (min (min len1 len2))
		 (max (max len1 len2))
		 (diff (- max min)))
	(+ diff
	   (if case-sensitive
		   (loop
			  :repeat min
			  :for i :upfrom start1
			  :for j :upfrom start2 
			  :counting (not (char= (char seq1 i) (char seq2 j))))
		   (loop
			  :repeat min
			  :for i :upfrom start1
			  :for j :upfrom start2 
			  :counting (not (char-equal (char seq1 i) (char seq2 j))))))))
