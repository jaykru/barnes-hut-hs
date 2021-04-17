(defclass body ()
  ((mass :initarg :mass) ; in SI kilograms
   (position :initarg :position) ; a vector #(x y)
   ))

(defun v+ (v1 v2)
  (map 'vector #'+ v1 v2))
(defun v/ (v1 k)
  (map 'vector #'(lambda (x) (/ x k)) v1))
(defun v* (v1 k)
  (map 'vector #'(lambda (x) (* x k)) v1))

;; computes the center of mass of a set of bodies
(defun compcenter (bodies)
  (let ((M (reduce #'+ (mapcar #'(lambda (body) (slot-value body 'mass)) bodies))))
    (v/ (reduce #'v+
		(mapcar #'(lambda (body)
			    (with-slots (mass position) body
			      (v* position mass)))
			bodies))
	M)))

(defclass qtree ()
  ((bodies :initarg :bodies) ;; the bodies contained in this quadtree,
                             ;; initialized as a list of bodies. only
                             ;; non-empty for a leaf node, and will
                             ;; contain at most one body.
   
   (extent :initarg :extent
	   :initform nil) ; a list '(xmin xmax ymin max) which keeps
                	     ; track of the area a given qtree node
			     ; covers

   ;; the mass of all the objects contained within the extent of this
   ;; tree
   (treemass
    :initarg :treemass
    :initform nil)
   ;; in the case of a tree whose extent covers just one object, this
   ;; is just the object mass.

   ;; center of mass of the extent represented by this tree
   (treecenter
    :initarg :treecenter
    :initform nil)

   ;; quadrants
   (q1 :initarg :q1)
   (q2 :initarg :q2)
   (q3 :initarg :q3)
   (q4 :initarg :q4)))

; decides whether a certain body falls within an extent
(defun inextp (body extent)
  (with-slots (position) body
    (let ((xmin (car extent))
	  (xmax (cadr extent))
	  (ymin (caddr extent))
	  (ymax (cadddr extent))
	  
	  (x (elt position 0))
	  (y (elt position 1)))
      (and (and (<= xmin x) (<= x xmax))
	   (and (<= ymin y) (<= y ymax))))))

(defmethod fill-qtree ((tree qtree))
  (with-slots (extent bodies) tree
    (unless (<= 1 (length bodies))
      (let ((xavg (/2 (+ xmin xmax)))
	    (yavg (/2 (+ ymin ymax)))
	    
	    (q1extent '(xavg xmax yavg ymax))
	    
	    (q2extent '(xmin xavg yavg ymax))
	    
	    (q3extent '(xmin xavg ymin yavg))
	    
	    (q4extent '(xavg xmax ymin yavg))

	    (q1bodies (remove-if-not #'(lambda (body) (inextp body q1extent)) bodies))
	    (q2bodies (remove-if-not #'(lambda (body) (inextp body q2extent)) bodies))
	    (q3bodies (remove-if-not #'(lambda (body) (inextp body q3extent)) bodies))
	    (q4bodies (remove-if-not #'(lambda (body) (inextp body q4extent)) bodies)))
	()
	)))
  )

;; '(;botleft
;;   ;botright
;;   ;topleft
;;   ;topright
;; )
(defmethod initialize-instance :after ((tree qtree) &key)
  (with-slots (extent bodies treemass treecenter q1 q2 q3 q4) tree
    (progn
      ;; compute extent of the bodies held in the tree
      (let ((xmin nil)
	    (xmax nil)
	    (ymin nil)
	    (ymax nil))
	(loop for body in bodies do
	  (with-slots (position) body
	    (let ((x (elt position 0))
		  (y (elt position 1)))
	      (cond ((or (not xmax) (> x xmax))
		     (setf xmax x))
		    ((or (not xmin (< x xmin)))
		     (setf xmin x))
		    ((or (not ymax) (> y ymax))
		     (setf ymax y))
		    ((or (not ymin) (< y ymin))
		     (setf ymin y))))))
	(setf extent '(xmin xmax ymin ymax)))
		   
      ;; compute the treemass
      (setf treemass
	    (let ((treemasses
		    (mapcar (lambda (subtree)
			      (with-slots (treemass) subtree
				treemass))
			    '(q1 q2 q3 q4))))
	      (reduce #'+ treemasses)))

      ;; compute the treecenter
      (setf treecenter
	    (let ((treebodies
		    (mapcar (lambda (subtree)
			      (with-slots (treemass treecenter) subtree
				(make-instance 'body :mass treemass :position treecenter)))
			    '(q1 q2 q3 q4))))
	      (compcenter treebodies)))
      
      ;; fill the quadrant trees
      (let ((xmin (car extent))
	    (xmax (cadr extent))
	    (ymin (caddr extent))
	    (ymax (cadddr extent))
	    
	    (xavg (/2 (+ xmin xmax)))
	    (yavg (/2 (+ ymin ymax)))
	    
	    (q1extent '(xavg xmax yavg ymax))
	    (q2extent '(xmin xavg yavg ymax))
	    (q3extent '(xmin xavg ymin yavg))
	    (q4extent '(xavg xmax ymin yavg))

	    (q1bodies (remove-if-not #'(lambda (body) (inextp body q1extent)) bodies))
	    (q2bodies (remove-if-not #'(lambda (body) (inextp body q2extent)) bodies))
	    (q3bodies (remove-if-not #'(lambda (body) (inextp body q3extent)) bodies))
	    (q4bodies (remove-if-not #'(lambda (body) (inextp body q4extent)) bodies)))
	(progn
	  (setf q1 (make-instance 'qtree :extent q1extent :bodies q1bodies))
	  (setf q2 (make-instance 'qtree :extent q2extent :bodies q2bodies))
	  (setf q3 (make-instance 'qtree :extent q3extent :bodies q3bodies))
	  (setf q4 (make-instance 'qtree :extent q4extent :bodies q4bodies)))))


(defmethod update-treemass (tree)
  (with-slots (treemass) tree
    (setf treemass
	  (let ((treemasses
		  (mapcar (lambda (subtree)
			    (with-slots (treemass) subtree
			      treemass))
			  '(q1 q2 q3 q4))))
	    (reduce #'+ treemasses)))))

;;; update treecenter
(defmethod update-treecenter (tree)
  (with-slots (treecenter) tree 
    (setf treecenter
	  (let ((treebodies
		  (mapcar (lambda (subtree)
			    (with-slots (treemass treecenter) subtree
			      (make-instance 'body :mass treemass :position treecenter)))
			  '(q1 q2 q3 q4))))
	    (compcenter treebodies)))))

;;; TODO
;;; update tree

;;; 





(defparameter *earth* (make-instance 'body
				     :mass 5.972e24
				     :position #(100 100)))

(defparameter *sun* (make-instance 'body
				   :mass 2e30
				   :position #(0 0)))

(defparameter *mybods* (list *sun* *earth*))
(compcenter *mybods*)

(defun initqtree (bodies)
  (make-instance 'qtree ))
