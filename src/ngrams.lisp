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

(defun map-ngrams (function size list
				   &key (start-padding nil) 
				        (end-padding nil))
  "map-ngrams FUNCTION N LIST &key START-PADDING END-PADDING => UNDEFINED

Calls FUNCTION for each n-gram of size N constructed from the elements
in the given LIST. Initially, the value of START-PADDING is used to fill
the first N - 1 elements in the call. When the list is exhausted, the 
value of END-PADDING is used to pad to a size of N. The function must
accept N arguments."
  (assert (<= 2 size))
  (when list
	(case size
	  ((2) (loop
			  :for previous = start-padding :then element
			  :for element :in list
			  :do (funcall function previous element)
			  :finally (funcall function element end-padding)))
	  ((3) (loop
			  :for previous1 = start-padding :then previous0
			  :for previous0 = start-padding :then element
			  :for element :in list
			  :do (funcall function previous1 previous0 element)
			  :finally (loop 
						  :repeat 2
						  :do (progn
								(funcall function previous1 previous0 end-padding)
								(shiftf previous1 previous0 end-padding)))))
	  (t (let ((buffer (make-array size :element-type t :initial-element start-padding)))
		   (labels ((call-function ()
					  (apply function
							 (loop
								:with list = ()
								:for k :downfrom size :above 0
								:do (push (aref buffer (- k 1)) list)
								:finally (return list))))
					(shift-value (element)
					  (loop
						 :for k :upfrom 0 :below (- size 1)
						 :do (setf (aref buffer k) (aref buffer (1+ k)))
						 :finally (setf (aref buffer (- size 1)) element))))
			 (loop
				:for element :in list 
				:do (progn (shift-value element)
						   (call-function))
				:finally (loop 
							:repeat (- size 1)
							:do (progn (shift-value end-padding)
									   (call-function))))
			 (values)))))))


(defun list-ngrams (size list
					&key (start-padding nil) 
					     (end-padding nil)
					     (transform #'list))
  (let (head tail)
	(map-ngrams #'(lambda (&rest args)
					(let ((link (cons (apply transform args) nil)))
					  (setf tail 
							(if head 
								(setf (cdr tail) link)
								(setf head link)))))
				size list 
				:start-padding start-padding
				:end-padding end-padding)
	head))


(defmacro do-ngrams ((&rest vars) (list-form &key start-padding end-padding) &body body)
  (let ((size (length vars)))
	(unless (>= size 2) (error "invalid bindings list"))
	(let ((element (gensym "ELEMENT-"))
		  (sequence (gensym "LIST-"))
		  (function (gensym "BODY-"))
		  (start (gensym "START-PADDING-"))
		  (end (gensym "END-PADDING-")))
	  (let* ((temps (nreverse (loop
								 :for previous = element :then sym
								 :for k :upfrom 0 :below (- size 1)
								 :for sym = (gensym (format nil "TEMP-~D-" k))
								 :collecting (list 
											  sym
											  (list :for sym := start :then previous)))))
			 (names (mapcar #'car temps))
			 (inits (mapcar #'cadr temps)))
			
		`(block nil
		   (let ((,sequence ,list-form))
			 (when ,sequence
			   (let ((,start ,start-padding)
					 (,end ,end-padding))
				 (flet ((,function (,@vars) ,@body))
				   (loop
					  ,@(loop :for e :in inits :nconcing e)
					  :for ,element :in ,sequence
					  :do (,function ,@names ,element)
					  :finally (loop 
								  :repeat ,(- size 1)
								  :do (progn
										(,function ,@names ,end)
										(shiftf ,@names ,end)))))))))))))

