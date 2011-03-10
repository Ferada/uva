(in-package #:cl-user)

(dolist (system '(#:iterate #:anaphora #:logv))
  (asdf:oos 'asdf:load-op system)
  (use-package system))

(defun leafp (tree)
  "Tests whether the TREE is a leaf."
  (and (atom (cadr tree))
       (atom (caddr tree))))

(defun #1=sum-tree (sum current tree)
  "Recursively sums up the tree and compares"
  (when tree
    (alet (+ current (car tree))
      (if (leafp tree)
	  (= it sum)
	  (and (< it sum) ;; break if we're already over the limit
	       ;; else, check both subtrees
	       (or (#1# sum it (cadr tree))
		   (#1# sum it (caddr tree))))))))

(defun solve (&optional (input #P"112-input-1.txt") (stream *standard-output*))
  (with-open-file (*standard-input* input)
    (iterate
      (for sum = #1=(read *standard-input* NIL NIL))
      (for tree = #1#)
      (while sum)
      (format stream "~:[no~;yes~]~%" (sum-tree sum 0 tree)))))

(defun compare-solution (&optional (input #P"112-input-1.txt") (output #P"112-output-1.txt"))
  "Compares solution and (hopefully) correct output line by line."
  (let ((stream (make-string-output-stream)))
    (time (solve input stream))
    (iterate
      (for solution in-stream (make-string-input-stream (get-output-stream-string stream)) using #'read-line)
      (for correct in-file output using #'read-line)
      (if (string= solution correct)
	  (format T "~A~%" solution)
	  (format T "~A != ~A, wrong solution~%" solution correct)))))
