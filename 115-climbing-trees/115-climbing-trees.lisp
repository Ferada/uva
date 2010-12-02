(in-package #:cl-user)

;; TODO: maybe compile it via ecl?

(dolist (system '(#:iterate))
  (asdf:oos 'asdf:load-op system)
  (use-package system))

(declaim (optimize (debug 0) (safety 0) (speed 3) (space 0) (compilation-speed 0)))
;; (declaim (optimize (debug 3) (safety 3) (speed 0) (space 0) (compilation-speed 0)))

(defvar *graph* NIL
  "Contains at least all top-level nodes and then some (remnants of graph creation).")

(defvar *translation* (make-hash-table :test 'eq)
  "Translates from symbols to actual nodes.")

(defstruct (node (:constructor make-node (value parent &optional children)))
  "A n-ary tree node, which also knows its parent."
  value
  parent
  children)

(defmethod print-object ((node node) stream)
  (format stream "#<~A, ~A>" (node-value node) (node-children node)))

(defun insert-relation (child father)
  "Inserts new nodes into the graph and translation table, updating the old
ones if necessary."
  (let* ((child-node (or (person child) (make-node child NIL)))
	 (father-exists (person father))
	 (parent-node (or father-exists (make-node father NIL (list child-node)))))
    (setf (person child) child-node
	  (node-parent child-node) parent-node)
    (if father-exists
	(push child-node (node-children parent-node))
	(progn
	  (push parent-node *graph*)
	  (setf (person father) parent-node)))))

(defun person (name)
  (gethash name *translation*))
(defun set-person (name person)
  (setf (gethash name *translation*) person))
(defsetf person set-person)

(defun up-distance (person1 person2)
  "Calculates the distance going from PERSON1 to PERSON2 up in the tree."
  (iterate
    (with current = -1)
    (unless person1
      (error "PERSON1 got NIL"))
    (until (eq person1 person2))
    (setf person1 (node-parent person1)
	  ;; if this overflows, we're already fucked
	  current (the fixnum (1+ current)))
    (finally (return current))))

(defun insert-if-not-hashed (objects hash-table)
  "Inserts all OBJECTS into the HASH-TABLE if necessary and returns a LIST
containing the newly inserted objects."
  (iterate
    (for object in objects)
    (unless (gethash object hash-table)
      (setf (gethash object hash-table) T)
      (collect object))))

(defun compute-relationship (person1 person2)
  "Computes the relationship between PERSON1 and PERSON2 and returns a 
SYMBOL or LIST describing it.  See also PRINT-RELATIONSHIP."
  (let ((person1 (person person1))
	(person2 (person person2))
	visited)
    (labels ((aux (current top)
	       (cond
		 ((eq current person2)
		  (cond
		    ;; person1 is above person2 and top-most -> parent
		    ((eq top person1)
		     ;; (logv person2 top)
		     `(parent ,(up-distance person2 top)))
		    ;; person1 is below person2, which is also top-most -> child
		    ((eq top person2)
		     ;; (logv person1 top)
		     `(child ,(up-distance person1 top)))
		    ;; arbitrary cousin
		    (T
		     (let ((up1 (up-distance person1 top))
			   (up2 (up-distance person2 top)))
		       `(cousin ,(min up1 up2) ,(abs (- up2 up1)))))))
		 (T
		  (setf (gethash current visited) T)
		  (or
		   ;; first go down
		   (iterate
		     (for child in (node-children current))
		     (when (gethash child visited)
		       (next-iteration))
		     (for result = (aux child top))
		     (until result)
		     (finally (return result)))
		   ;; then go up, if possible
		   (aif (node-parent current)
			(unless (gethash it visited)
			  (aux it it))
			'no-relation))))))
      (cond
	((not (and person1 person2)) 'no-relation)
	((eq person1 person2) 'same) ;; no valid input, so this shouldn't happen
	(T (setf visited (make-hash-table :test 'eq))
	   (aux person1 person1))))))

(defun print-relationship (relation)
  (if (not (atom relation))
      (if (eq (car relation) 'cousin)
	  (let ((nth (the fixnum (cadr relation)))
		(removed (the fixnum (caddr relation))))
	    (if (= removed 0)
		(if (= nth 0)
		    "sibling"
		    (format NIL "~D cousin" nth))
		(format NIL "~D cousin removed ~D" nth removed)))
	  (let ((relation (ecase (car relation)
			    (child "child")
			    (parent "parent")))
		(depth (the fixnum (cadr relation))))
	    (format NIL "~[~*~;~*grand ~:;~{~*great ~}grand ~]~A"
		    depth (when (> depth 1) (make-list (1- depth))) relation)))
      (ecase relation
	(no-relation "no relation")
	(same (prog1 "same" (warn "~A is no valid relation" relation))))))

(defun solve (&optional (input #P"115-input-1.txt") (output #P"115-output-1.txt"))
  (setf *graph* NIL)
  (clrhash *translation*)
  (with-open-file (*standard-input* input)
    (with-open-file (output output)
      (iterate
	(with mode = 'input)
	(for line = (read-line *standard-input* NIL NIL))
	(while line)
	(when (= (length line) 0)
	  (next-iteration))
	(for (values child offset) = (read-from-string line))
	(for father = (read-from-string line T NIL :start offset))
	;; mode switch between insertion and query
	(when (eq child 'no.child)
	  (setf mode 'query)
	  (next-iteration))
	(if (eq mode 'input)
	    (insert-relation child father)
	    (let* ((solution (format NIL "~A" (print-relationship (compute-relationship child father))))
		   (correct (read-line output)))
	      (if (string= solution correct)
		  (format T "~A~%" solution)
		  (format T "~A != ~A, wrong solution~%" solution correct))))))))
