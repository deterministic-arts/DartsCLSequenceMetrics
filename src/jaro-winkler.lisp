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

(defconstant jaro-winkler-prefix-adjustment-scale 1/10)
(defconstant jaro-winkler-min-prefix-length 6)

(declaim (ftype (string-function real) string-jaro-distance))

(defun string-jaro-distance (str1 str2 
					         &key (start1 0) (end1 nil)
							      (start2 0) (end2 nil)
							      (case-sensitive nil))
  
  "string-jaro-distance STR1 STR2 &key START1 END1 START2 END2 CASE-SENSITIVE => DISTANCE"

  (declare (optimize (speed 3) (safety 1) (debug 0))
		   (inline char= char-equal))

  (assert (<= 0 start1 (or end1 start1) (length str1)))
  (assert (<= 0 start2 (or end2 start2) (length str2)))
  
  (let* ((len1 (- (or end1 (length str1)) start1))
		 (len2 (- (or end2 (length str2)) start2))
		 (smallest (min len1 len2)))

	(macrolet ((common-chars-finder (test)
				 `(let ((exclude (make-array len2 :element-type 'bit :initial-element 0 :adjustable nil :fill-pointer nil))
						(buffer (make-array smallest :element-type 'character :fill-pointer 0 :adjustable t)))
					(declare (type (vector bit) exclude)
							 (type (vector character) buffer))
					(loop 
					   :for i :upfrom 0 :below len1
					   :do (let ((ch (char str1 i)))
							 (loop 
								:named inner
								:for j :of-type (integer 0) :upfrom (max 0 (- i distance)) :below (min (+ i distance) len2)
								:do (when (= 0 (bit exclude j))
									  (let ((ch2 (char str2 j)))
										(when (,test ch ch2)
										  (setf (bit exclude j) 1)
										  (vector-push-extend ch buffer)
										  (return-from inner nil)))))))
					buffer))
			   (transposition-counter (test)
				 `(loop 
					 :with count = 0
					 :for i :upfrom 0 :below length
					 :unless (,test (char common1 i) (char common2 i)) 
					 :do (incf count)
					 :finally (return-from count-transpositions count))))
	
	  (labels ((find-common-chars  (str1 str2 distance)
				 (declare (type string str1 str2)
						  (type (integer 0) distance))
				 (if case-sensitive
					 (common-chars-finder char=)
					 (common-chars-finder char-equal)))
			   (count-transpositions (common1 common2 length)
				 (declare (type string common1 common2)
						  (type (integer 0) length))
				 (if case-sensitive 
					 (transposition-counter char=)
					 (transposition-counter char-equal))))
		(- 1 
		   (if (zerop len1)
			   (if (zerop len2) 1 0)
			   (let* ((halflen (multiple-value-call #'+ (floor smallest 2)))
					  (common1 (find-common-chars str1 str2 halflen))
					  (common2 (find-common-chars str2 str1 halflen))
					  (clen1 (fill-pointer common1)) 
					  (clen2 (fill-pointer common2)))
				 (if (or (= 0 clen1) (= 0 clen2) (/= clen1 clen2)) 0
					 (let ((transpositions (count-transpositions common1 common2 clen1)))
					   (/ (+ (/ clen1 len1)
							 (/ clen2 len2)
							 (/ (- clen1 (/ transpositions 2)) clen1))
						  3))))))))))

(declaim (ftype (string-function real 
				   (:prefix-length array-index)
				   (:adjustment-scale real))
				string-jaro-winkler-distance))

(defun string-jaro-winkler-distance (str1 str2 
							         &key (start1 0) (end1 nil)
									      (start2 0) (end2 nil)
									      (case-sensitive nil)
									      (prefix-length jaro-winkler-min-prefix-length)
							              (adjustment-scale jaro-winkler-prefix-adjustment-scale))

  (declare (optimize (speed 3) (safety 1) (debug 0))
		   (inline char= char-equal))

  (let ((len1 (- (or end1 (length str1)) start1))
		(len2 (- (or end2 (length str2)) start2)))
	(labels ((prefix-length ()
			   (let ((n (min prefix-length len1 len2)))
				 (if case-sensitive
					 (loop 
						:for i :upfrom 0 :below n
						:unless (char= (char str1 i) (char str2 i))
						:do (return-from prefix-length i)
						:finally (return-from prefix-length n))
					 (loop 
						:for i :upfrom 0 :below n
						:unless (char-equal (char str1 i) (char str2 i))
						:do (return-from prefix-length i)
						:finally (return-from prefix-length n))))))
	  (let* ((distance (string-jaro-distance str1 str2
											 :start1 start1 :end1 end1
											 :start2 start2 :end2 end2
											 :case-sensitive case-sensitive))
			 (proximity (- 1 distance))
			 (prefix-length (prefix-length)))
		(- 1 (+ proximity
				(* prefix-length 
				   adjustment-scale 
				   distance)))))))


(declaim (ftype (sequence-function real) jaro-distance))

(defun jaro-distance (str1 str2 
					  &key (start1 0) (end1 nil)
					       (start2 0) (end2 nil)
					       (test #'eql have-test)
					       (test-not nil have-test-not)
					       (key #'identity))
  
  "jaro-distance SEQ1 SEQ2 &key START1 END1 START2 END2 TEST TEST-NOT KEY => DISTANCE"

  (assert (<= 0 start1 (or end1 start1) (length str1)))
  (assert (<= 0 start2 (or end2 start2) (length str2)))
  
  (when (and have-test have-test-not)
	(error "cannot use both ~S and ~S at the same time" :test :test-not))
  
  (let* ((len1 (- (or end1 (length str1)) start1))
		 (len2 (- (or end2 (length str2)) start2))
		 (smallest (min len1 len2)))
	
	(labels ((matchp (c1 c2)
			   (if have-test-not 
				   (not (funcall test-not c1 c2))
				   (funcall test c1 c2)))
			 
			 (gete (seq n)
			   (funcall key (elt seq n)))
			 
			 (find-common-characters (str1 str2 distance)
			   (let ((exclude (make-array len2 :element-type 'bit :initial-element 0 :adjustable nil :fill-pointer nil))
					 (buffer (make-array smallest :element-type 't :fill-pointer 0 :adjustable t)))
				 (loop 
					:for i :upfrom 0 :below len1
					:do (let ((ch (gete str1 i)))
						  (loop 
							 :named inner
							 :for j :upfrom (max 0 (- i distance)) :below (min (+ i distance) len2)
							 :do (when (= 0 (bit exclude j))
								   (let ((ch2 (gete str2 j)))
									 (when (matchp ch ch2)
									   (setf (bit exclude j) 1)
									   (vector-push-extend ch buffer)
									   (return-from inner nil)))))))
				 buffer))
		   
			 (count-transpositions (common1 common2 length)
			   (loop 
				  :with count = 0
				  :for i :upfrom 0 :below length
				  :unless (matchp (aref common1 i) (aref common2 i)) 
				  :do (incf count)
				  :finally (return-from count-transpositions count))))
	  
	  (- 1 
		 (if (zerop len1)
			 (if (zerop len2) 1 0)
			 (let* ((halflen (multiple-value-call #'+ (floor smallest 2)))
					(common1 (find-common-characters str1 str2 halflen))
					(common2 (find-common-characters str2 str1 halflen))
					(clen1 (fill-pointer common1)) 
					(clen2 (fill-pointer common2)))
			   (if (or (= 0 clen1) (= 0 clen2) (/= clen1 clen2)) 0
				   (let ((transpositions (count-transpositions common1 common2 clen1)))
					 (/ (+ (/ clen1 len1)
						   (/ clen2 len2)
						   (/ (- clen1 (/ transpositions 2)) clen1))
						3)))))))))


(declaim (ftype (string-function real 
				   (:prefix-length array-index)
				   (:adjustment-scale real))
				jaro-winkler-distance))

(defun jaro-winkler-distance (str1 str2 
							  &key (start1 0) (end1 nil)
							       (start2 0) (end2 nil)
							       (test #'eql have-test)
							       (test-not nil have-test-not)
							       (key #'identity have-key)
							       (prefix-length jaro-winkler-min-prefix-length)
							       (adjustment-scale jaro-winkler-prefix-adjustment-scale))
  (let ((len1 (- (or end1 (length str1)) start1))
		(len2 (- (or end2 (length str2)) start2)))
	(labels ((matchp (c1 c2) 
			   (if have-test-not
				   (not (funcall test-not c1 c2))
				   (funcall test c1 c2)))
			 (gete (seq n) (funcall key (elt seq n)))
			 (prefix-length ()
			   (let ((n (min prefix-length len1 len2)))
				 (loop 
					:for i :upfrom 0 :below n
					:unless (matchp (gete str1 i) (gete str2 i))
					:do (return-from prefix-length i)
					:finally (return-from prefix-length n)))))
	  (let* ((distance (apply #'jaro-distance str1 str2
							  :start1 start1 :end1 end1
							  :start2 start2 :end2 end2
							  (nconc (when have-key (list :key key))
									 (when have-test (list :test test))
									 (when have-test-not (list :test-not test-not)))))
							  
			 (proximity (- 1 distance))
			 (prefix-length (prefix-length)))
		(- 1 (+ proximity
				(* prefix-length 
				   adjustment-scale 
				   distance)))))))

