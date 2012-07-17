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

(declaim (ftype (string-function (integer 0))string-levenshtein-distance))

(defun string-levenshtein-distance (s1 s2
									&key (start1 0) (end1 nil)
									     (start2 0) (end2 nil)
									     (case-sensitive t))
  "string-levenshtein-distance S1 S2 &key START1 END1 START2 END2 
                                          CASE-SENSITIVE => DISTANCE

Computes the Levenshtein distance between strings S1 and S2. The result
value DISTANCE is the minimum number of edit operations required to 
transform S1 into S2 (or vice versa), where allowed operations are:

 - insert a single character at some position
 - delete a single character at some position
 - substitute a single character at some position by another one

The Levenshtein distance is a measure of similarity between strings. If
two strings have a distance of 0, they are equal.

If CASE-SENSITIVE is true (which is the default), then comparing of
characters is done in a case sensitive way, distinguishing between lower
and upper case letters. If CASE-SENSITIVE is false, then this function
does not distinguish between lower case and upper case letters.

See function levenshtein-distance for a generalization of this function
to arbitrary sequences."
  (declare (optimize (speed 3) (debug 0) (safety 1)))
  (check-type s1 string)
  (check-type s2 string)
  (check-type start1 array-index)
  (check-type start2 array-index)
  (check-type end1 (or null array-index))
  (check-type end2 (or null array-index))
  (assert (<= 0 start1 (or end1 start1) (length s1)))
  (assert (<= 0 start2 (or end2 start2) (length s2)))
  (let* ((s s1) (u s2)
		 (end1 (or end1 (length s)))
		 (end2 (or end2 (length u)))
		 (m (- end1 start1))
		 (n (- end2 start2)))
	(declare (type string s u) (type fixnum m n))
	(let ((d (make-array (list (1+ m) (1+ n)) :element-type '(unsigned-byte 32))))
	  (declare (type (simple-array (unsigned-byte 32) (* *)) d))
	  (loop :for i :upfrom 0 :to m :do (setf (aref d i 0) i))
	  (loop :for j :upfrom 0 :to n :do (setf (aref d 0 j) j))
	  (macrolet ((levenloop (predicate)
				   ;; This macro exists, so we don't need to check the
				   ;; case-sensitive stuff in the inner loop, or have to use
				   ;; funcall in the inner loop. The drawback is, that the 
				   ;; function's compiled size increases.
				   `(locally (declare (inline ,predicate))
					  (loop 
						 :for j :of-type fixnum :upfrom 1 :to n
						 :for rj :of-type fixnum :upfrom start2
						 :for j* :of-type fixnum = (the fixnum (- j 1))
						 :for uj* :of-type character = (char u rj)
						 :do (loop
								:for i :of-type fixnum :upfrom 1 :to m
								:for ri :of-type fixnum :upfrom start1
								:for i* :of-type fixnum = (the fixnum (- i 1))
								:do (if (,predicate (char s ri) uj*)
										(setf (aref d i j) (aref d i* j*))
										(setf (aref d i j) 
											  (1+ (min (aref d i* j)
													   (aref d i j*)
													   (aref d i* j*))))))))))
		(if case-sensitive 
			(levenloop char=)
			(levenloop char-equal))
	  (aref d m n)))))


(declaim (ftype (sequence-function (integer 0)) levenshtein-distance))

(defun levenshtein-distance (s1 s2 
							 &key (start1 0) (end1 nil)
							      (start2 0) (end2 nil)
							      (test #'eql have-test) 
							      (test-not nil have-test-not)
							      (key #'identity have-key))
  "levenshtein-distance S1 S2 &key START1 END1 START2 END2 TEST TEST-NOT KEY => NUMBER

Computes the Levenshtein distance between sequences S1 and S2. The result
value DISTANCE is the minimum number of edit operations required to 
transform S1 into S2 (or vice versa), where allowed operations are:

 - insert a single character at some position
 - delete a single character at some position
 - substitute a single character at some position by another one

The Levenshtein distance is a measure of similarity between strings. If
two strings have a distance of 0, they are equal. The TEST function is 
used to compare sequence elements. The default test function is eql.

This function is a generalization of string-levenshtein-distance
for arbitrary sequence types. Use string-levenshtein-distance, if you
need a version optimized for use with strings."

  (declare (optimize (speed 3) (debug 0) (safety 1)))

  (when (and have-test have-test-not)
	(error "cannot use both: ~S and ~S at the same time" :test :test-not))
  
  (let* ((s s1) (u s2)
		 (end1 (or end1 (length s)))
		 (end2 (or end2 (length u)))
		 (m (- end1 start1))
		 (n (- end2 start2)))
	
	(let ((d (make-array (list (1+ m) (1+ n)) :element-type '(unsigned-byte 32))))

	  (declare (type (simple-array (unsigned-byte 32) (* *)) d))
	  
	  (loop :for i :upfrom 0 :to m :do (setf (aref d i 0) i))
	  (loop :for j :upfrom 0 :to n :do (setf (aref d 0 j) j))
	  
	  (macrolet ((levenloop ( invert getkey predicate )
				   `(loop 
					   :for j :upfrom 1 :to n
					   :for rj :upfrom start2
					   :for j* = (- j 1)
					   :for uj* = (,@getkey (elt u rj))
					   :do (loop
							  :for i :upfrom 1 :to m
							  :for ri :upfrom start1
							  :for i* = (- i 1)
							  :do (if ,(let ((compare `(,@predicate (,@getkey (elt s ri)) uj*)))
										 (if invert 
											 (list 'not compare)
											 compare))
									  (setf (aref d i j) (aref d i* j*))
									  (setf (aref d i j) 
											(1+ (min (aref d i* j)
													 (aref d i j*)
													 (aref d i* j*)))))))))
		(if have-test-not
			(levenloop t (funcall key) (funcall test-not))
			(if (or have-test have-key)
				(levenloop nil (funcall key) (funcall test))
				(levenloop nil (progn) (eql))))
		(aref d m n)))))
