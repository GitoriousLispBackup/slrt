
;; To load, execute in REPL:
#|
(lisp-cabinet:load-quicklisp)
(ql:quickload "lispbuilder-sdl")
(ql:quickload "lispbuilder-sdl-gfx")
|#

;; Testing:
;; Compile and load: C-c C-k
#|
(kd::load-geometry "v:/Programming/car/car/icosahedron.obj")
(progn (setf *res* (kd::build-kd))
	   T) ;; progn - to suppress noise.
(kd::draw-kd-structure *res*)
|#


(defpackage #:kd
  (:use #:cl))

(in-package #:kd)

;; Data types

(defstruct vertex
  (x 0.0 :type single-float)
  (y 0.0 :type single-float)
  (z 0.0 :type single-float))

(defstruct triangle
  (v0 0 :type (unsigned-byte 28))
  (v1 0 :type (unsigned-byte 28))
  (v2 0 :type (unsigned-byte 28))
  (square 0.0 :type single-float))

(defstruct aabb
  "0 is min, 1 is max"
  (x0 0.0 :type single-float)
  (y0 0.0 :type single-float)
  (z0 0.0 :type single-float)
  (x1 0.0 :type single-float)
  (y1 0.0 :type single-float)
  (z1 0.0 :type single-float))

(defstruct kd-node
  (bbox nil)
  (contents nil)
  (left nil)
  (right nil))

;; OBJ files

(defmacro aif (test-form then-form &optional else-form)
  `(let ((it ,test-form))
     (if it ,then-form ,else-form)))

(defun parse-line (line prefix &key (type 'single-float))
  (and line
	   (< (length prefix) (length line))
	   (setf line (string-trim '(#\Space #\Tab)
							   (subseq line 0 (position #\# line))))
	   (string/= "" line)
	   (string= line prefix :end1 (length prefix) :end2 (length prefix))
	   (let ((value (map 'list
						 #'(lambda(x)(when (numberp x) (coerce x type)))
						 (read-from-string (concatenate 'string
														"("
														(subseq line (length prefix) nil)
														")"))))) 
		 (let ((value (delete-if #'null value)))
		   (when (= 3 (length value)) 
			 value)))))

(defun load-vertexes (filename &key (scale 100.0))
  "Loads the vertex data from OBJ file. Skips unacceptable elements.
Returns new array of vertexes.
Very simple reader, does not understand vt and vn. Everything starting from"
  (let ((result (make-array 0 :element-type 'vertex :adjustable t :fill-pointer 0)))
	(with-open-file (stream filename :direction :input)
	  (let ((eof-reached) (line)) 
		(loop while (not eof-reached)
		   do (multiple-value-setq (line eof-reached) (read-line stream nil))
			 (and (not eof-reached)
				  (aif (parse-line line "v ")
					   (vector-push-extend
						(make-vertex :x (* scale (nth 0 it)) :y (* scale (nth 1 it)) :z (* scale (nth 2 it)))
						result))))))
	result))

(defun load-faces (filename)
  "Loads the faces' data from OBJ file. Skips unacceptable elements.
Returns new array of indexes."
  (let ((result (make-array 0 :element-type 'triangle :adjustable t :fill-pointer 0)))
	(with-open-file (stream filename :direction :input)
	  (let ((eof-reached) (line)) 
		(loop while (not eof-reached)
		   do (multiple-value-setq (line eof-reached) (read-line stream nil))
			 (and (not eof-reached)
				  (aif (parse-line line "f " :type '(unsigned-byte 28))
					   (vector-push-extend
						(make-triangle :v0 (1- (nth 0 it)) :v1 (1- (nth 1 it)) :v2 (1- (nth 2 it)))
						result))))))
	result))

;; ;; Displaying -------------------------------------------------------------------

;; (defun kd-tree-output-to-png (file)
;;   (let ((width 1024) (height 768))
;; 	(format t "Writing file ~a, width = ~a, height = ~a pixels." file width height)
;; 	(let ((png (make-instance 'pixel-streamed-png
;; 							  :color-type :truecolor-alpha
;; 							  :width width :height height)))
;; 	  (with-open-file (stream file :direction :output
;; 							  :if-exists :supersede
;; 							  :if-does-not-exist :create
;; 							  :element-type '(unsigned-byte 8))
;; 		(start-png png stream)
;; 		(dotimes (y height)
;; 		  (dotimes (x width)
;; 			(let ((r (floor (* 255 (/ x width))))
;; 				  (g (random 1))
;; 				  (b (random 1))
;; 				  (a (floor (let ((xn (/ x width))
;; 								  (yn (/ y height)))
;; 							  (if (> xn yn)
;; 								  (* xn 255)
;; 								  (* yn 255))))))
;; 			  (write-pixel (list r g b a) png))))
;; 		(finish-png png)))))

;;; Global variables ------------------------------------------------------------

(defvar *points-soup*    nil "All points.")

(defvar *triangles-soup* nil "All triangles.")

(defparameter *kd-colors*
  (list (sdl:color :r 255 :g   0 :b   0 :a  30)
		(sdl:color :r   0 :g 255 :b   0 :a  30)
		(sdl:color :r   0 :g   0 :b 255 :a  30)
		(sdl:color :r 255 :g   0 :b   0 :a  30)
		(sdl:color :r   0 :g 255 :b   0 :a  30)
		(sdl:color :r   0 :g   0 :b 255 :a  30)
		(sdl:color :r 255 :g   0 :b   0 :a  30)
		(sdl:color :r   0 :g 255 :b   0 :a  30)
		(sdl:color :r   0 :g   0 :b 255 :a  30)
		(sdl:color :r 255 :g   0 :b   0 :a  30)
		(sdl:color :r   0 :g 255 :b   0 :a  30)
		(sdl:color :r   0 :g   0 :b 255 :a  30)))

(defun load-geometry (filename &key (scale 100.0))
  (setf *points-soup* (load-vertexes filename :scale scale))
  (setf *triangles-soup* (load-faces filename))
  #|(and (not (null *points-soup*)) (not (null *triangles-soup*)))|#
  )

;;; Helpers ---------------------------------------------------------------------
;; (defun macroexpand-n (n form &key (full nil))
;;   "Provided by Ander Skirnir 07.12.2010 07:15 at http://lisper.ru/forum/thread/461
;; as an example of simple alternative to macroexpand-all"
;;   (if (= 0 n)
;; 	  form
;;       (mapcar #'(lambda (form) (if (atom form)
;; 								   form
;; 								   (macroexpand-n (1- n) form)))
;;               (funcall (if full
;; 						   #'macroexpand
;; 						   #'macroexpand-1)
;; 					   form))))

;;; KD-tree -------------------------------------------------------------------

(defun calc-vx-aabb (&key (start -1) (end -1))
  "Calculates the AABB on full *points-soup* vector.
:start and :end are indexes of vertexes in the *points-"
  (let ((start (if (= -1 start)
                   0
                   start))
        (end (if (= -1 end)
				 (length *points-soup*)
				 end))
		(v nil))
	(loop for i from start below end do
		 (setf v (aref *points-soup* i))
	   minimizing (vertex-x v) into x0
	   minimizing (vertex-y v) into y0
	   minimizing (vertex-z v) into z0
	   maximizing (vertex-x v) into x1
	   maximizing (vertex-y v) into y1
	   maximizing (vertex-z v) into z1
	   finally (return (make-aabb :x0 x0 :y0 y0 :z0 z0
								  :x1 x1 :y1 y1 :z1 z1)))))

(defun calc-tri-aabb (tri)
  "Calculates the AABB on a triangle given the index of the triangle."
  (let ((x0 (vertex-x (aref *points-soup* (triangle-v0 tri))))
		(y0 (vertex-y (aref *points-soup* (triangle-v0 tri))))
		(z0 (vertex-z (aref *points-soup* (triangle-v0 tri))))
		(x1 (vertex-x (aref *points-soup* (triangle-v1 tri))))
		(y1 (vertex-y (aref *points-soup* (triangle-v1 tri))))
		(z1 (vertex-z (aref *points-soup* (triangle-v1 tri))))
		(x2 (vertex-x (aref *points-soup* (triangle-v2 tri))))
		(y2 (vertex-y (aref *points-soup* (triangle-v2 tri))))
		(z2 (vertex-z (aref *points-soup* (triangle-v2 tri)))))
	(let ((min-x (min x0 x1 x2))
		  (min-y (min y0 y1 y2))
		  (min-z (min z0 z1 z2))
		  (max-x (min x0 x1 x2))
		  (max-y (min y0 y1 y2))
		  (max-z (min z0 z1 z2))) 
	  (make-aabb :x0 min-x :y0 min-y :z0 min-z
				 :x1 max-x :y1 max-y :z1 max-z)))) 

(defun triangle-fully-in-aabb (triangle outer-box)
  (let ((inner-box (calc-tri-aabb triangle)))
	#|(format t "inner box: ~A~%outer box: ~A~%" inner-box outer-box)|#
	(and (>= (aabb-x0 inner-box) (aabb-x0 outer-box))
		 (>= (aabb-y0 inner-box) (aabb-y0 outer-box))
		 (>= (aabb-z0 inner-box) (aabb-z0 outer-box))
		 (<= (aabb-x1 inner-box) (aabb-x1 outer-box))
		 (<= (aabb-y1 inner-box) (aabb-y1 outer-box))
		 (<= (aabb-z1 inner-box) (aabb-z1 outer-box)))))

(defun make-lx (box value-of-axis)
  (make-aabb :x0 (aabb-x0 box) :y0 (aabb-y0 box) :z0 (aabb-z0 box)
			 :x1 value-of-axis :y1 (aabb-y1 box) :z1 (aabb-z1 box)))
(defun make-rx (box value-of-axis)
  (make-aabb :x0 value-of-axis :y0 (aabb-y0 box) :z0 (aabb-z0 box)
			 :x1 (aabb-x1 box) :y1 (aabb-y1 box) :z1 (aabb-z1 box)))

(defun make-ly (box value-of-axis)
  (make-aabb :x0 (aabb-x0 box) :y0 (aabb-y0 box) :z0 (aabb-z0 box)
			 :x0 (aabb-x1 box) :y1 value-of-axis :z1 (aabb-z1 box)))
(defun make-ry (box value-of-axis)
  (make-aabb :x0 (aabb-x0 box) :y0 value-of-axis :z0 (aabb-z0 box)
			 :x1 (aabb-x1 box) :y1 (aabb-y1 box) :z1 (aabb-z1 box)))

(defun make-lz (box value-of-axis)
  (make-aabb :x0 (aabb-x0 box) :y0 (aabb-y0 box) :z0 (aabb-z0 box)
			 :x0 (aabb-x1 box) :y1 (aabb-y1 box) :z1 value-of-axis))
(defun make-rz (box value-of-axis)
  (make-aabb :x0 (aabb-x0 box) :y0 (aabb-y0 box) :z0 value-of-axis
			 :x1 (aabb-x1 box) :y1 (aabb-y1 box) :z1 (aabb-z1 box)))

(defun split-aabb (aabb &key over on)
  "Usage: (split-aabb aabb :over :x) :over can be: :x :y :z.
AABB is Axis-Aligned Bounding Box."
  (case over
	(:x (values (make-lx aabb on) (make-rx aabb on)))
	(:y (values (make-ly aabb on) (make-ry aabb on)))
	(:z (values (make-lz aabb on) (make-rz aabb on)))))

(defun limit-triangles (triangles)
  "Returns true if number of TRIANGLES is more than 3."
  (< 10 (length triangles)))

(defun split-space-of-group (&key by aabb)
  "DRAFT(to test BUILD-KD): Splits the space taken by TRIANGLES by axis \"BY\" and returns the position."
  (let ((div-position  (+ 0.25 (random 0.5))))
	#|(when (> 0.2 div-position) (setf div-position 0.2))|#
	#|(when (< 0.8 div-position) (setf div-position 0.8))|#
	(case by
	  (:x (+ (aabb-x0 aabb) (* div-position (- (aabb-x1 aabb) (aabb-x0 aabb)))))
	  (:y (+ (aabb-y0 aabb) (* div-position (- (aabb-y1 aabb) (aabb-y0 aabb)))))
	  (:z (+ (aabb-z0 aabb) (* div-position (- (aabb-z1 aabb) (aabb-z0 aabb))))))))

(defun build-kd (&key
                 (axis :x)
				 (level 0)
                 (node (make-kd-node :bbox (calc-vx-aabb)))
                 (triangles (coerce *triangles-soup* 'list))
                 (aabb (calc-vx-aabb))
                 (split-function #'split-space-of-group)
                 (continue-function #'limit-triangles))
  #|(format t "level ~a, ~a~%" level axis)|#
  (if (funcall continue-function triangles)
	  (let* ((split-position (funcall split-function :by axis :aabb aabb))
			 l-triangles r-triangles here)
		(multiple-value-bind (l-aabb r-aabb) 
			(split-aabb aabb :over axis :on split-position)
		  (dolist (triangle triangles)
			(if (triangle-fully-in-aabb triangle l-aabb)
				(push triangle l-triangles);; triangle goes left
				(if (triangle-fully-in-aabb triangle r-aabb)
					(push triangle r-triangles) ;; triangle goes right
					(push triangle here)))) ;; triangle goes here, in this node
		  (if (or (null l-triangles)
				  (null r-triangles))
			  (setf (kd-node-contents node) triangles)
			  (let ((next-axis (case axis (:x :y) (:y :z) (:z :x))))
				(when (not (null here))
				  (setf (kd-node-contents node) here))
				(setf (kd-node-left node)
                                      (build-kd :axis next-axis
												:level (1+ level)
                                                :node (make-kd-node :bbox l-aabb)
                                                :triangles l-triangles :aabb l-aabb
                                                :split-function split-function
                                                :continue-function  continue-function))
				(setf (kd-node-right node)
                                      (build-kd :axis next-axis
												:level (1+ level)
                                                :node (make-kd-node :bbox r-aabb)
                                                :triangles r-triangles :aabb r-aabb
                                                :split-function split-function
                                                :continue-function  continue-function))))))
	  (setf (kd-node-contents node) triangles))
  node)

(defun print-kd (prefix level node)
  (if (or (kd-node-left node) (kd-node-right node))
	  (progn
		(if (kd-node-contents node)
			(format t "~%[~a] ~a" (length (kd-node-contents node)) prefix))
		(if (kd-node-left node)
			(print-kd (concatenate 'string prefix "->l") (1+ level) (kd-node-left node)))
		(if (kd-node-right node)
			(print-kd (concatenate 'string prefix "->r") (1+ level) (kd-node-right node))))
	  (if (kd-node-contents node)
		  (format t "~%[~a] ~a" (length (kd-node-contents node)) prefix))))

(defun v->screen (x y z &key (x0 512) (y0 384))
  (sdl:point :x (- x0 (* 1.7 x) (* -1.7 y))
			 :y (+ y0 (* -2 z) x y)))

(defun vi->screen (index &key (x0 512) (y0 384))
  (v->screen (vertex-x (aref *points-soup* index))
			 (vertex-y (aref *points-soup* index))
			 (vertex-z (aref *points-soup* index))
			 :x0 x0 :y0 y0))

(defmacro abbrev (short long)
  `(defmacro ,short (&rest args)
	 `(,',long ,@args)))

(defun group (source n)
  (if (zerop n) (error "zero length"))
  (labels ((rec (source acc)
			 (let ((rest (nthcdr n source)))
			   (if (consp rest)
				   (rec rest (cons (subseq source 0 n) acc))
				   (nreverse (cons source acc))))))
	(if source (rec source nil) nil)))

(defun draw-borders (box color-index)
  (when box
	(let* ((d 2.5)
		   #|(-d (* -1.0 d))|# 
		   (x0 (aabb-x0 box)) (y0 (aabb-y0 box)) (z0 (aabb-z0 box))
		   (x1 (aabb-x1 box)) (y1 (aabb-y1 box)) (z1 (aabb-z1 box)))
	  (let ((the-color (nth color-index *kd-colors*)))
		(sdl:with-color (the-color)
		  ;; 000-100 ;; 000-010 ;; 000-001
		  ;; 111-011 ;; 111-101 ;; 111-110
		  ;; 001-011 ;; 001-101 ;; 100-101
		  ;; 010-011 ;; 010-110 ;; 100-110
		  #|(mapc #'(lambda (x y) (sdl:draw-line (apply #'sc-w2s-fn x) (apply #'sc-w2s-fn y)))
		  (list (list x0 y0 z0 0 d)
		  (list x0 y0 z0 1 d)
		  (list x0 y0 z0 2 d)
		  (list x1 y1 z1 0 -d)
		  (list x1 y1 z1 1 -d)
		  (list x1 y1 z1 2 -d)
		  (list x0 y0 z1 1 d)
		  (list x0 y0 z1 0 d)
		  (list x1 y0 z0 2 d)
		  (list x0 y1 z0 2 d)
		  (list x0 y1 z0 0 d)
		  (list x1 y0 z0 1 d))
		  (list (list x1 y0 z0 0 -d)
		  (list x0 y1 z0 1 -d)
		  (list x0 y0 z1 2 -d)
		  (list x0 y1 z1 0 d) 
		  (list x1 y0 z1 1 d) 
		  (list x1 y1 z0 2 d) 
		  (list x0 y1 z1 1 -d)
		  (list x1 y0 z1 0 -d)
		  (list x1 y0 z1 2 -d)
		  (list x0 y1 z1 2 -d)
		  (list x1 y1 z0 0 -d)
		  (list x1 y1 z0 1 -d)))|#
		  (progn
			(sdl:draw-line (v->screen (+ d x0) y0 z0) (v->screen (- x1 d) y0 z0))
			(sdl:draw-line (v->screen x0 (+ d y0) z0) (v->screen x0 (- y1 d) z0))
			(sdl:draw-line (v->screen x0 y0 (+ d z0)) (v->screen x0 y0 (- z1 d))) 
			(sdl:draw-line (v->screen (- x1 d) y1 z1) (v->screen (+ d x0) y1 z1))
			(sdl:draw-line (v->screen x1 (- y1 d) z1) (v->screen x1 (+ d y0) z1))
			(sdl:draw-line (v->screen x1 y1 (- z1 d)) (v->screen x1 y1 (+ d z0))) 
			(sdl:draw-line (v->screen x0 (+ d y0) z1) (v->screen x0 (- y1 d) z1))
			(sdl:draw-line (v->screen (+ d x0) y0 z1) (v->screen (- x1 d) y0 z1))
			(sdl:draw-line (v->screen x1 y0 (+ d z0)) (v->screen x1 y0 (- z1 d)))
			(sdl:draw-line (v->screen x0 y1 (+ d z0)) (v->screen x0 y1 (- z1 d)))
			(sdl:draw-line (v->screen (+ d x0) y1 z0) (v->screen (- x1 d) y1 z0)) 
			(sdl:draw-line (v->screen x1 (+ d y0) z0) (v->screen x1 (- y1 d) z0))
			))))))

(defun draw-divider (box color-index)
  "Draws only one face of the box.
The face is calculated from the color-index,
because the color-index is a index of the axis as well."
  (when box
	(let ((index (rem color-index 3))
		  (x0 (aabb-x0 box))
		  (y0 (aabb-y0 box))
		  (z0 (aabb-z0 box))
		  (x1 (aabb-x1 box))
		  (y1 (aabb-y1 box))
		  (z1 (aabb-z1 box))
		  v0 v1 v2 v3)
	  (case index
		(0 (setf v0 (v->screen x0 y0 z0))
		   (setf v1 (v->screen x0 y1 z0))
		   (setf v2 (v->screen x0 y1 z1))
		   (setf v3 (v->screen x0 y0 z1)))
		(1 (setf v0 (v->screen x0 y0 z0))
		   (setf v1 (v->screen x0 y0 z1))
		   (setf v2 (v->screen x1 y0 z1))
		   (setf v3 (v->screen x1 y0 z0)))
		(2 (setf v0 (v->screen x0 y0 z0))
		   (setf v1 (v->screen x1 y0 z0))
		   (setf v2 (v->screen x1 y1 z0))
		   (setf v3 (v->screen x0 y1 z0))))
	  #|(sdl-gfx:draw-filled-polygon (list v0 v1 v2 v3) :color (nth color-index *kd-colors*))|#
	  #|(sdl-gfx:draw-polygon (list v0 v1 v2 v3) :color (sdl:color :r 255 :g 255 :b 255 :a 80))|#)))

(defun draw-kd-node-aabb (node color-index depth)
  (when (and node (< 0 depth))
	(when (kd-node-contents node)
	  (dolist (tri (kd-node-contents node))
		(let ((v0 (vi->screen (triangle-v0 tri)))
			  (v1 (vi->screen (triangle-v1 tri)))
			  (v2 (vi->screen (triangle-v2 tri))))
		  (sdl-gfx:draw-filled-polygon (list v0 v1 v2) :color (sdl:color :r 255 :g 255 :b 255 :a 20))
		  (sdl-gfx:draw-polygon (list v0 v1 v2) :color (nth color-index *kd-colors*)))))
	(if (or (kd-node-left node)
			(kd-node-left node))
		(let ((next-color (if (= (1+ color-index) (length *kd-colors*))
							  0
							  (1+ color-index))))
		  (draw-kd-node-aabb (kd-node-left node) next-color (1- depth))
		  (draw-borders (kd-node-bbox node) color-index)
		  (draw-divider (kd-node-bbox (kd-node-right node)) color-index)
		  (draw-kd-node-aabb (kd-node-right node) next-color (1- depth))
		  ))))

(defun draw-kd-structure (given &key (depth 4))
  (let ((need-redraw t))
	(sdl:with-init ()
	  (sdl:window 1024 768 :title-caption  "Isometric view. Hit any key to quit.")
	  (setf (sdl:frame-rate) 60)
	  #|(sdl-gfx:initialise-default-font sdl-gfx:*Font-6x12*)|# 

	  (sdl:with-events ()
		(:quit-event () t)
		(:key-down-event () (sdl:push-quit-event))
		(:idle ()
			   (when (sdl:mouse-left-p)
				 (setf need-redraw t))
			   (when need-redraw
				 (sdl:clear-display sdl:*black*)

				 (draw-borders (kd-node-bbox given) 0)
				 #|(draw-divider (kd-node-bbox given) 0)|#
				 #|(draw-divider (kd-node-bbox given) 1)|#
				 #|(draw-divider (kd-node-bbox given) 2)|#

				 (draw-kd-node-aabb given 0 depth)

				 (sdl:update-display)
				 (setf need-redraw nil)))))))

;; Testing
(defvar *kd-tree* nil)

(defparameter *windows-test-data-file-name* "d:\\Paul.revised\\git.repos\\github\\gourd.obj")
(defparameter *macosx-test-data-file-name* "./gourd.obj")

(load-geometry *windows-test-data-file-name* :scale 80.0)
;;(load-geometry *macosx-test-data-file-name* :scale 80.0)

(setf *kd-tree* (kd::build-kd))
(draw-kd-structure *kd-tree* :depth 10)

