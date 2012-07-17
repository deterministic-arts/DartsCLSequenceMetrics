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

(declaim (ftype (sequence-function (integer 0)) longest-common-subsequence-length))
(declaim (ftype (sequence-function (integer 0)) longest-common-subsequence*-length))
(declaim (ftype (sequence-function list) longest-common-subsequences*))
(declaim (ftype (string-function list) longest-common-substrings))
(declaim (ftype (string-function (integer 0)) longest-common-substring-length))

(defun longest-common-subsequence-length (seq1 seq2
										  &key (start1 0) (end1 nil)
										       (start2 0) (end2 nil)
										       (test #'eql have-test)
										       (test-not nil have-test-not)
										       (key #'identity have-key))

  (when (and have-test have-test-not)
	(error "cannot use both, ~S and ~S in the same call" :test :test-not))
  
  (let* ((len1 (- (or end1 (length seq1)) start1))
		 (len2 (- (or end2 (length seq2)) start2))
		 (c (make-array (list (1+ len1) (1+ len2)) :element-type '(unsigned-byte 32) :initial-element 0)))
	
	(macrolet ((get-key (seq k) `(funcall key (elt ,seq ,k)))
			   (call-test (x y) `(funcall test ,x ,y))
			   (call-test-not (x y) `(not (funcall test-not ,x ,y)))
			   (lcs-loop (getter tester)
				 `(loop
					 :for i :upfrom 1 :to len1
					 :for i* = (- i 1)
					 :do (loop 
							:for j :upfrom 1 :to len2
							:for j* = (- j 1)
							:do (if (,tester (,getter seq1 i*) (,getter seq2 j*))
									(setf (aref c i j) (1+ (aref c i* j*)))
									(setf (aref c i j) (max (aref c i j*) (aref c i* j))))))))
	  (if have-test-not
		  (if have-key
			  (lcs-loop get-key call-test-not)
			  (lcs-loop elt call-test-not))
		  (if have-test
			  (if have-key 
				  (lcs-loop get-key call-test)
				  (lcs-loop elt call-test))
			  (if have-key 
				  (lcs-loop get-key eql)
				  (lcs-loop elt eql))))
	  (aref c len1 len2))))



(defun longest-common-subsequence*-length (seq1 seq2
									 	   &key (start1 0) (end1 nil)
										        (start2 0) (end2 nil)
										        (test #'eql have-test)
										        (test-not nil have-test-not)
										        (key #'identity have-key))

  "longest-common-subsequence*-length SEQ1 SEQ2 &key START1 END1 START2 END2 TEST TEST-NOT KEY => LENGTH

Returns the length of the (or: a) longest common contiguous subsequence
of sequences SEQ1 and SEQ2. This problem is usually called the longest
common `substring´ problem, but in order to avoid confusion, the we use the
term subsequence* here to refer to contiguous subsequences."

  (when (and have-test have-test-not)
	(error "cannot use both, ~S and ~S in the same call" :test :test-not))
  
  (let* ((len1 (- (or end1 (length seq1)) start1))
		 (len2 (- (or end2 (length seq2)) start2))
		 (c (make-array (list len1 len2) :element-type '(unsigned-byte 32) :initial-element 0))
		 (z 0))
	
	(macrolet ((get-key (seq k) `(funcall key (elt ,seq ,k)))
			   (call-test (x y) `(funcall test ,x ,y))
			   (call-test-not (x y) `(not (funcall test-not ,x ,y)))
			   (lcs-loop (getter tester)
				 `(loop
					 :for i :upfrom 0 :below len1 
					 :for e1 = (,getter seq1 i)
					 :do (loop 
							:for j :upfrom 0 :below len2 
							:for e2 = (,getter seq2 j)
							:do (when (,tester e1 e2)
								  (let ((n (1+ (if (or (zerop i) (zerop j)) 0
												   (aref c (- i 1) (- j 1))))))
									(setf (aref c i j) n)
									(when (< z n)
									  (setf z n))))))))
	  (if have-test-not
		  (if have-key
			  (lcs-loop get-key call-test-not)
			  (lcs-loop elt call-test-not))
		  (if have-test
			  (if have-key 
				  (lcs-loop get-key call-test)
				  (lcs-loop elt call-test))
			  (if have-key 
				  (lcs-loop get-key eql)
				  (lcs-loop elt eql)))))
	z))




(defun longest-common-subsequences* (seq1 seq2
									 &key (start1 0) (end1 nil)
									      (start2 0) (end2 nil)
									      (test #'eql have-test)
									      (test-not nil have-test-not)
									      (key #'identity have-key))

  "longest-common-subsequences* SEQ1 SEQ2 &key START1 END1 START2 END2 TEST TEST-NOT KEY => LENGTH

Returns a list containing all contigous longest common subsequences
of SEQ1 and SEQ2. This problem is usually called the longest common 
`substring´ problem, but in order to avoid confusion, the we use the
term subsequence* here to refer to contiguous subsequences."

  (when (and have-test have-test-not)
	(error "cannot use both, ~S and ~S in the same call" :test :test-not))
  
  (let* ((len1 (- (or end1 (length seq1)) start1))
		 (len2 (- (or end2 (length seq2)) start2))
		 (c (make-array (list len1 len2) :element-type '(unsigned-byte 32) :initial-element 0))
		 (r '())
		 (z 0))
	
	(macrolet ((get-key (seq k) `(funcall key (elt ,seq ,k)))
			   (call-test (x y) `(funcall test ,x ,y))
			   (call-test-not (x y) `(not (funcall test-not ,x ,y)))
			   (lcs-loop (getter tester)
				 `(loop
					 :for i :upfrom 0 :below len1 
					 :for e1 = (,getter seq1 i)
					 :do (loop 
							:for j :upfrom 0 :below len2 
							:for e2 = (,getter seq2 j)
							:do (when (,tester e1 e2)
								  (let ((n (1+ (if (or (zerop i) (zerop j)) 0
												   (aref c (- i 1) (- j 1))))))
									(setf (aref c i j) n)
									(when (< z n)
									  (setf r '())
									  (setf z n))
									(when (= n z)
									  (push i r))))))))
	  (if have-test-not
		  (if have-key
			  (lcs-loop get-key call-test-not)
			  (lcs-loop elt call-test-not))
		  (if have-test
			  (if have-key 
				  (lcs-loop get-key call-test)
				  (lcs-loop elt call-test))
			  (if have-key 
				  (lcs-loop get-key eql)
				  (lcs-loop elt eql)))))
	(loop
	   :for end* :in r
	   :for end = (1+ end*)
	   :for start = (- end z)
	   :collecting (subseq seq1 start end))))


(defun longest-common-substrings (seq1 seq2
								  &key (start1 0) (end1 nil)
								       (start2 0) (end2 nil)
								       (case-sensitive nil))

  (check-type seq1 string)
  (check-type seq2 string)
  (check-type start1 array-index)
  (check-type start2 array-index)
  (check-type end1 (or null array-index))
  (check-type end2 (or null array-index))
  (assert (<= 0 start1 (or end1 start1) (length seq1)))
  (assert (<= 0 start2 (or end2 start2) (length seq2)))

  (locally (declare (type string seq1 seq2)
					(type array-index start1 start2)
					(type (or null array-index) end1 end2))
					

	(let* ((len1 (- (or end1 (length seq1)) start1))
		   (len2 (- (or end2 (length seq2)) start2))
		   (c (make-array (list len1 len2) :element-type '(unsigned-byte 32) :initial-element 0))
		   (r '())
		   (z 0))
	
	  (macrolet ((lcs-loop (tester)
				   `(loop
					   :for i :upfrom 0 :below len1 
					   :for e1 = (char seq1 i)
					   :do (loop 
							  :for j :upfrom 0 :below len2 
							  :for e2 = (char seq2 j)
							  :do (when (,tester e1 e2)
									(let ((n (1+ (if (or (zerop i) (zerop j)) 0
													 (aref c (- i 1) (- j 1))))))
									  (setf (aref c i j) n)
									  (when (< z n)
										(setf r '())
										(setf z n))
									  (when (= n z)
										(push i r))))))))
		(if case-sensitive
			(lcs-loop char=)
			(lcs-loop char-equal)))
	  (loop
		 :for end* :in r
		 :for end = (1+ end*)
		 :for start = (- end z)
		 :collecting (subseq seq1 start end)))))


(defun longest-common-substring-length (seq1 seq2
									 	&key (start1 0) (end1 nil)
										     (start2 0) (end2 nil)
										     (case-sensitive nil))  

  (check-type seq1 string)
  (check-type seq2 string)
  (check-type start1 array-index)
  (check-type start2 array-index)
  (check-type end1 (or null array-index))
  (check-type end2 (or null array-index))
  (assert (<= 0 start1 (or end1 start1) (length seq1)))
  (assert (<= 0 start2 (or end2 start2) (length seq2)))
  
  (locally (declare (type string seq1 seq2)
					(type array-index start1 start2)
					(type (or null array-index) end1 end2))
					
  
	(let* ((len1 (- (or end1 (length seq1)) start1))
		   (len2 (- (or end2 (length seq2)) start2))
		   (c (make-array (list len1 len2) :element-type '(unsigned-byte 32) :initial-element 0))
		   (z 0))

	  (macrolet ((lcs-loop (tester)
				   `(loop
					   :for i :upfrom 0 :below len1 
					   :for e1 = (char seq1 i)
					   :do (loop 
							  :for j :upfrom 0 :below len2 
							  :for e2 = (char seq2 j)
							  :do (when (,tester e1 e2)
									(let ((n (1+ (if (or (zerop i) (zerop j)) 0
													 (aref c (- i 1) (- j 1))))))
									  (setf (aref c i j) n)
									  (when (< z n)
										(setf z n))))))))
		(if case-sensitive
			(lcs-loop char=)
			(lcs-loop char-equal)))
	  z)))
