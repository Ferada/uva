(in-package #:cl-user)

(dolist (system '(#:iterate #:anaphora #:logv))
  (asdf:oos 'asdf:load-op system)
  (use-package system))

(defun parse-account (line)
  (values (parse-integer line :end 3)
	  (subseq line 3)))

(defun parse-record (line)
  (values (parse-integer line :end 3)
	  (parse-integer line :start 3 :end 6)
	  (parse-integer line :start 6)))

(defun merge-record (transaction account cents)
  (aif (assoc account transaction :test #'=)
       (prog1 transaction
	 (incf (cdr it) cents))
       `((,account . ,cents)
	 ,.transaction)))

(defun print-transaction (accounts transaction &optional (stream *standard-output*))
  (iterate
    (for (account . cents) in (reverse transaction))
    (format stream "~A ~30A ~2,1,10$~%" account (gethash account accounts "Out of Balance") (/ cents 100))))

(defun transaction-difference (transaction)
  (reduce #'+ transaction :key #'cdr))

(defun solve (&optional (input #P"187-input-1.txt") (stream *standard-output*))
  (with-open-file (*standard-input* input)
    (let ((accounts (make-hash-table)))
      ;; read account names
      (iterate
	(for line = #1=(read-line *standard-input*))
	(for (values number name) = (parse-account line))
	(until (= number 0))
	(setf (gethash number accounts) name))
      ;; read records and transactions
      (iterate
	(with transaction)
	(for line = #1#)
	(for (values id account cents) = (parse-record line))
	(for previous-id previous id)
	(for donep = (= id 0))
	(unless (or transaction donep)
	  (setf transaction NIL))
	(when (and previous-id
		   (/= previous-id id))
	  (alet (transaction-difference transaction)
	    (when (/= it 0)
	      (format stream "*** Transaction ~D is out of balance ***~%" previous-id)
	      (print-transaction accounts (merge-record transaction 999 (- it)) stream)
	      (terpri stream)))
	  (setf transaction NIL))
	(until donep)
	(setf transaction (merge-record transaction account cents))))))

(defun compare-solution (&optional (input #P"187-input-1.txt") (output #P"187-output-1.txt"))
  "Compares solution and (hopefully) correct output line by line."
  (let ((stream (make-string-output-stream)))
    (time (solve input stream))
    (iterate
      (for solution in-stream (make-string-input-stream (get-output-stream-string stream)) using #'read-line)
      (for correct in-file output using #'read-line)
      (if (string= solution correct)
	  (format T "~A~%" solution)
	  (format T "~A != ~A, wrong solution~%" solution correct)))))
