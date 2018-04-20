;; vim: set sts=2 sw=2 et sm lisp :

(declaim (optimize (speed 3)
                   (debug 3)
                   (safety 1)
                   (space 0)
                   (compilation-speed 0)))

(when t
  (ql:quickload :sdl2)
  (ql:quickload :cl-opengl))

;; FIXME: these should be elsewhere
(defvar *cam-pos*   (list  255.5f0   31.5f0  255.5f0))
(defvar *cam-dir*   (list    0.0f0    0.0f0    1.0f0))
(defvar *cam-accel* (list    0.0f0    0.0f0    0.0f0))
(defvar *cam-roty*  0.0d0)
(defvar *cam-rotx*  0.0d0)
(defvar *key-mvxp*  nil)
(defvar *key-mvxn*  nil)
(defvar *key-mvyp*  nil)
(defvar *key-mvyn*  nil)
(defvar *key-mvzp*  nil)
(defvar *key-mvzn*  nil)
(defvar *key-rtyp*  nil)
(defvar *key-rtyn*  nil)
(defvar *key-rtxp*  nil)
(defvar *key-rtxn*  nil)

(defparameter *window-width* 1280)
(defparameter *window-height* 800)
(defvar *sdl-window* nil)
(defvar *sdl-gl-context* nil)

(defun load-file-contents (fname)
  (uiop:read-file-string fname))

(defparameter *main-shader-vertex-source*
  (load-file-contents "shader-main.vert"))

(defparameter *main-shader-fragment-source*
  (load-file-contents "shader-main.frag"))

(defvar *main-shader* nil)
(defvar *main-vao* nil)
(defvar *main-vao-vbo* nil)
(defvar *main-vao-va* nil)
(defvar *main-vao-va-data* nil)

(defvar *tex-map* nil)
(declaim (type (simple-array (unsigned-byte 8)
                             (#x4000000))
               *world-map*))
(declaim (type (array (unsigned-byte 8)
                      (512 64 512 4))
               *displaced-world-map*))
(defvar *world-map*
  (make-array #x4000000
              :element-type '(unsigned-byte 8)
              :initial-element 0))
(defvar *displaced-world-map*
  (make-array '(512 64 512 4)
              :element-type '(unsigned-byte 8)
              :displaced-to *world-map*))

(defmacro with-my-system-context (&body body)
  `(sdl2:with-init (:everything)
     (sdl2:with-window (*sdl-window*
                         :w *window-width*
                         :h *window-height*
                         :flags '(:shown :opengl))
       (sdl2:gl-set-attrs
         :red-size 8
         :green-size 8
         :blue-size 8
         :alpha-size 8
         :depth-size 24
         :doublebuffer 1
         :context-major-version 3
         :context-minor-version 3
         :context-profile-mask sdl2-ffi:+sdl-gl-context-profile-core+
         )
       (sdl2:with-gl-context (*sdl-gl-context* *sdl-window*)
         ,@body))))

(defun load-shader (&key vertex tesselation-control tesselation-evaluation geometry fragment compute)
  (declare (ignore tesselation-control))
  (declare (ignore tesselation-evaluation))
  (declare (ignore geometry))
  (declare (ignore compute))

  (let* ((program         (gl:create-program))
         (vertex-shader   (when vertex   (gl:create-shader :vertex-shader)))
         (fragment-shader (when fragment (gl:create-shader :fragment-shader)))
         ;; and whatever else goes here
         )
    (dolist (tuple
              `(,@(when vertex   `(("VERTEX"   ,vertex-shader   ,vertex)))
                ,@(when fragment `(("FRAGMENT" ,fragment-shader ,fragment)))
                ))
      (destructuring-bind (name shader source) tuple
        (gl:shader-source shader source)
        (gl:compile-shader shader)
        (format t "------------------------------~%")
        (format t "Log for ~a SHADER:~%" name)
        (format t "~a~%" (gl:get-shader-info-log shader))
        (gl:attach-shader program shader)))
    (gl:bind-frag-data-location program 0 "f_color")
    (gl:bind-attrib-location program    0 "i_vertex")
    (gl:link-program program)
    (format t "------------------------------~%")
    (format t "Log for PROGRAM:~%")
    (format t "~a~%" (gl:get-program-info-log program))
    (format t "------------------------------~%")
    program))

(defun tick-draw ()
  (gl:clear-color 1.0d0 0.0d0 0.0d0 0.0d0)
  (gl:clear :color-buffer)

  (gl:use-program *main-shader*)
  (apply #'gl:uniformf (gl:get-uniform-location *main-shader* "cam_pos")   *cam-pos*)
  (apply #'gl:uniformf (gl:get-uniform-location *main-shader* "cam_dir")   *cam-dir*)
  (apply #'gl:uniformf (gl:get-uniform-location *main-shader* "cam_accel") *cam-accel*)
  (gl:bind-vertex-array *main-vao*)
  (gl:uniformi (gl:get-uniform-location *main-shader* "tex_map") 0)
  (gl:bind-texture :texture-3d *tex-map*)
  (gl:draw-arrays :triangles 0 3)
  (gl:bind-texture :texture-3d 0)
  (gl:bind-vertex-array 0)

  (gl:finish)
  (sdl2:gl-swap-window *sdl-window*))

(defun tick-physics ()
  (let* ((sec-delta 0.01f0)
         (mvxd (+ (if *key-mvxn* -1 0) (if *key-mvxp* 1 0)))
         (mvyd (+ (if *key-mvyn* -1 0) (if *key-mvyp* 1 0)))
         (mvzd (+ (if *key-mvzn* -1 0) (if *key-mvzp* 1 0)))
         )
    (incf (nth 0 *cam-pos*) mvxd)
    (incf (nth 1 *cam-pos*) mvyd)
    (incf (nth 2 *cam-pos*) mvzd)
    ))

(defun tick-all ()
  (tick-physics)
  (tick-draw))

(defun handle-key (keysym is-down)
  (case keysym
    ;; Movements
    ((:scancode-w)     (setf *key-mvzp* is-down))
    ((:scancode-s)     (setf *key-mvzn* is-down))
    ((:scancode-a)     (setf *key-mvxn* is-down))
    ((:scancode-d)     (setf *key-mvxp* is-down))
    ((:scancode-space) (setf *key-mvyp* is-down))
    ((:scancode-lctrl) (setf *key-mvyn* is-down))
    ;; Rotations
    ((:scancode-up)    (setf *key-rtxp* is-down))
    ((:scancode-down)  (setf *key-rtxn* is-down))
    ((:scancode-left)  (setf *key-rtyn* is-down))
    ((:scancode-right) (setf *key-rtyp* is-down))
    ))

(defun main/direct ()
  (with-my-system-context
    
    ;; Load main shader
    (setf *main-shader*
          (load-shader
            :vertex *main-shader-vertex-source*
            :fragment *main-shader-fragment-source*))

    ;; Build VBO + VA
    (setf *main-vao-va-data*
          #(-1.0f0 -1.0f0 
            -1.0f0  3.0f0
             3.0f0 -1.0f0))
    (setf *main-vao-va* (gl:alloc-gl-array :float 6))
    (setf *main-vao-vbo* (gl:gen-buffer))
    (dotimes (i (length *main-vao-va-data*))
      (setf (gl:glaref *main-vao-va* i)
            (aref *main-vao-va-data* i)))
    (gl:bind-buffer :array-buffer *main-vao-vbo*)
    (gl:buffer-data :array-buffer :static-draw *main-vao-va*)
    (gl:bind-buffer :array-buffer 0)
    (gl:free-gl-array *main-vao-va*)

    ;; Build VAO
    (setf *main-vao* (gl:gen-vertex-array))
    (gl:bind-vertex-array *main-vao*)
    (gl:bind-buffer :array-buffer *main-vao-vbo*)
    (gl:vertex-attrib-pointer 0 2 :float nil 0 (cffi:null-pointer))
    (gl:enable-vertex-attrib-array 0)
    (gl:bind-vertex-array 0)

    ;; Load map
    (time
      (with-open-file (file ;
                            "normandie.vxl"
                            ;"DeckII.vxl"
                            :direction :input
                            :element-type '(unsigned-byte 8))
        (symbol-macrolet ((py (- 63 y))
                          (out-r (aref *world-map* (+ (* 512 64 4 z) (* 512 4 py) (* 4 x) 0)))
                          (out-g (aref *world-map* (+ (* 512 64 4 z) (* 512 4 py) (* 4 x) 1)))
                          (out-b (aref *world-map* (+ (* 512 64 4 z) (* 512 4 py) (* 4 x) 2)))
                          (out-a (aref *world-map* (+ (* 512 64 4 z) (* 512 4 py) (* 4 x) 3)))
                          )
          (dotimes (z 512)
            (declare (type fixnum z))
            (dotimes (x 512)
              (declare (type fixnum x))
              ;(format t "~3d ~3d~%" x z)
              (dotimes (y 64)
                (declare (type fixnum y))
                (setf out-b #xFF)
                (setf out-g #x00)
                (setf out-r #xFF)
                (setf out-a #xFF))

              (let* ((cn (read-byte file))
                     (cs (read-byte file))
                     (ce (read-byte file))
                     (ca (read-byte file))
                     (ocs cs)
                     (extra-cells (list))
                     (y 0))
                (declare (type fixnum cn cs ce ca))
                (declare (type (integer 0 64) y))
                ;(declare (type fixnum y))
                (block nil
                  (do () (nil)
                    ;(format t "~2,'0X ~2,'0X ~2,'0X ~2,'0X = ~2,'0X~%" cn cs ce ca y)
                    (do () ((>= y cs))
                      (setf out-b #x00)
                      (setf out-g #x00)
                      (setf out-r #x00)
                      (setf out-a #x00)
                      (incf y))
                    ;(format t "~2,'0X ~2,'0X ~2,'0X ~2,'0X = ~2,'0X~%" cn cs ce ca y)

                    ;; Do floor
                    (setf y cs)
                    (dotimes (i (- ce cs -1))
                      (setf out-b (read-byte file))
                      (setf out-g (read-byte file))
                      (setf out-r (read-byte file))
                      (read-byte file)
                      (setf out-a #xFF)
                      (incf y))
                    ;(format t "~2,'0X ~2,'0X ~2,'0X ~2,'0X = ~2,'0X~%" cn cs ce ca y)

                    (when (= cn 0)
                      (return-from nil nil))

                    ;; Grab ceiling bytes
                    (dotimes (i (- (- cn 1) (- ce cs -1)))
                      (push `(,(read-byte file)
                              ,(read-byte file)
                              ,(read-byte file)
                              ,(read-byte file))
                            extra-cells))

                    ;; Grab next gap
                    (setf cn (read-byte file))
                    (setf cs (read-byte file))
                    (setf ce (read-byte file))
                    (setf ca (read-byte file))

                    ;; Do ceiling
                    (setf y (- ca (length extra-cells)))
                    (dolist (cell extra-cells)
                      (setf out-b (nth 0 cell))
                      (setf out-g (nth 1 cell))
                      (setf out-r (nth 2 cell))
                      (setf out-a #xFF)
                      (incf y))
                    (setf extra-cells (list))

                    (setf y ca)

                    ))

                ;; this can be used to force a global ceiling for testing
                ;(format t "~d~%" ocs)
                '(do ((y ocs (1+ y))) ((>= y 64))
                  (declare (type fixnum y))
                  (setf out-b #xFF)
                  (setf out-g #x00)
                  (setf out-r #xFF)
                  (setf out-a #xFF))

                '(dotimes (y 29)
                  (declare (type fixnum y))
                  (setf out-b #xFF)
                  (setf out-g #x00)
                  (setf out-r #xFF)
                  (setf out-a #xFF))

                )))

          '(let* ((x 255) (z 252))
            (dotimes (y 64)
              (setf out-b #xFF)
              (setf out-g #xAA)
              (setf out-r #x55)
              (setf out-a #xFF)))

          )))

    ;; Optimise map
    (let* ((optimise-level 32))
      ;; Calculate distance to wall
      (dotimes (reps optimise-level)
        (time
          (let* ((a-neighb (if (= reps 0) #xFF reps))
                 (a-to (1+ reps)))
            (declare (type fixnum reps))
            (declare (type (unsigned-byte 8) a-neighb a-to))

            (dotimes (i (* 512 512 64))
              (declare (type fixnum i))
              (let* ((ic0 (+ (* 4 i) 3))
                     (ixn (- ic0 4))
                     (ixp (+ ic0 4))
                     (iyn (- ic0 (* 4 512)))
                     (iyp (+ ic0 (* 4 512)))
                     (izn (- ic0 (* 4 512 64)))
                     (izp (+ ic0 (* 4 512 64)))
                     (imx (* 512 64 512 4)))
                (when (= (aref *world-map* ic0) 0)
                  (when (or (and (>= ixn 0)   (= (aref *world-map* ixn) a-neighb))
                            (and (<  ixp imx) (= (aref *world-map* ixp) a-neighb))
                            (and (>= iyn 0)   (= (aref *world-map* iyn) a-neighb))
                            (and (<  iyp imx) (= (aref *world-map* iyp) a-neighb))
                            (and (>= izn 0)   (= (aref *world-map* izn) a-neighb))
                            (and (<  izp imx) (= (aref *world-map* izp) a-neighb)))
                    (setf (aref *world-map* ic0) a-to)))))
            )))

      ;; Sanitise remaining blocks
      (time
        (dotimes (i (* 512 512 64))
          (declare (type fixnum i))
          (let* ((ri (+ (* i 4) 3)))
            (cond ((=    (aref *world-map* ri) 0)
                   (setf (aref *world-map* ri) (1- optimise-level)))
                  ((<    (aref *world-map* ri) #x80)
                   (decf (aref *world-map* ri))))))))

    ;; Upload map texture
    (setf *tex-map* (gl:gen-texture))
    (gl:bind-texture :texture-3d *tex-map*)
    (gl:tex-parameter :texture-3d :texture-min-filter :nearest)
    (gl:tex-parameter :texture-3d :texture-mag-filter :nearest)
    ;; FIXME: find out how to do this WITHOUT holepunching the library
    (%gl::tex-storage-3d :texture-3d 1 :rgba8ui
                         (the fixnum 512)
                         (the fixnum 64)
                         (the fixnum 512))
    (gl:tex-sub-image-3d :texture-3d 0 0 0 0 512 64 512
                         :rgba-integer :unsigned-byte
                         *world-map*)
    (gl:bind-texture :texture-3d 0)

    (block quit
      (sdl2:with-event-loop ()
        (:keydown (:keysym keysym)
          (handle-key (sdl2:scancode keysym) t))
        (:keyup (:keysym keysym)
          (handle-key (sdl2:scancode keysym) nil))
        (:quit (format t "quit you shit~%") (finish-output) (return-from quit))
        (:idle () (tick-all))))))

(defun main ()
  (sdl2:make-this-thread-main #'main/direct))

(main)

