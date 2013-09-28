(define-module (leapmotion-commands)
  #:use-module ((emacsy emacsy) #:hide (state position))
  #:use-module (leapmotion-event)
  #:use-module (LeapGuile)
  #:use-module ((LeapGuile-primitive) :renamer (symbol-prefix-proc 'primitive:))
  #:use-module (oop goops)
  #:use-module ((srfi srfi-1) #:select (fold)) ;; fold
  #:use-module (camera)
  #:use-module (guile-user)
  #:use-module (vector-math)
  )

(define (Vector->vector V)
  (vector (slot-ref V 'x) (slot-ref V 'y) (slot-ref V 'z)))

(define (vector->Vector v)
  (let ((V (make <Vector>)))
    (slot-set! V 'x (vector-ref v 0))
    (slot-set! V 'y (vector-ref v 1))
    (slot-set! V 'z (vector-ref v 2))
    V))

(define (*List->list List-object)
  "Convert a HandList, PointableList, etc. to a regular scheme list."
  (map (lambda (i) (elem-ref List-object i)) (iota (count List-object))))

(define pi (acos -1))

(define (radians->degrees rad)
  (/ (* rad 180.) pi))

(define (leap-camera-move)
    (when (is-a? this-command-event <leapmotion-pointable-event>)
      (let* ((scale 1.)
             (translate (vector->Vector #(0. -100. 0.)))
             (finger (le:pointable this-command-event))
             (actual-pos (tipPosition finger))
             (pos (Leap* (Leap+ translate actual-pos) scale))
             (dir (direction finger))
             (position-speed 0.07)
             (direction-speed 0.07))
        (format #t "Setting camera to position ~a and direction ~a ~%" (Vector->vector pos) (Vector->vector dir))
        #;(set-parameter! 'camera-position (Vector->vector pos))
        (set-camera-position-smoothly! (Vector->vector pos) position-speed)
        #;(set-parameter! 'camera-target (Vector->vector (Leap+ dir pos)))
        (set-camera-target-smoothly! (Vector->vector (Leap+ dir pos)) direction-speed))))

(define-key eracs-mode-map (kbd "S-lm-hand-1-finger-1") 'leap-camera-move)

;(define-key eracs-mode-map (kbd "lm-hand-2-finger-1") 'leap-camera-move)

(define* (set-camera-position-smoothly! new-pos #:optional (speed 0.01))
  (let* ((old-pos (get-parameter 'camera-position))
         (new-pos* (vector-lerp old-pos new-pos speed)))
    (set-parameter! 'camera-position new-pos*)))

(define* (set-camera-target-smoothly! new-target #:optional (speed 0.01))
  (let* ((old-target (get-parameter 'camera-target))
         (new-target* (vector-lerp old-target new-target speed)))
    (set-parameter! 'camera-target new-target*)))

(define-key eracs-mode-map 
  (kbd "lm-hand-1") 
  (lambda ()
    (when (is-a? this-command-event <leapmotion-hand-event>)
      (let* ((the-hand (le:hand this-command-event))
             (hand-x-basis (normalized (cross (palmNormal the-hand)
                                              (direction the-hand))))
             (hand-y-basis (Leap- (palmNormal the-hand)))
             (hand-z-basis (Leap- (direction the-hand)))
             (hand-origin (palmPosition the-hand))
             (hand-transform (rigidInverse (make <Matrix> #:args (list hand-x-basis hand-y-basis hand-z-basis)))))
        (for-each 
         (lambda (the-finger index)
           (let ((transformed-position (transformPoint hand-transform (tipPosition the-finger)))
                 (transformed-direction (transformDirection hand-transform (direction the-finger))))
             (format #t "Finger ~a transformed position ~a and direction ~a ~%" (id the-finger) (Vector->vector transformed-position) (Vector->vector transformed-direction))
             
             (format #t "Finger ~a angle ~a ~%" (id the-finger) (radians->degrees (angle-between-palm-and-finger the-hand the-finger)))
             (when (or #t (= index 0))
               (let* ((angle (radians->degrees (angle-between-palm-and-finger the-hand the-finger)))
                      (t (clamp 0 1 (lerp-inverse 45 90 angle)))
                      (pi/4 (/ pi 4))
                      (leg-angle (lerp (- pi/4) pi/4 t))
                      (leg-index (* 2 index))
                      (joint-speed 0.1))
                 (when (< leg-index 8)
                  (format #t "Setting leg angle to ~a, t = ~a~%" leg-angle t)
                  (set-target-angle-smoothly! leg-index leg-angle joint-speed)
                  #;
                  (vector-set! (target-angles robot)
                               leg-index 
                               leg-angle)
                  (set-target-angle-smoothly! (1+ leg-index) leg-angle joint-speed)
                  #;
                  (vector-set! (target-angles robot)
                               (1+ leg-index) 
                               leg-angle))))))
         (sort (*List->list (fingers the-hand)) (lambda (finger-a finger-b)
                                                  (< (elem-ref (tipPosition finger-a) 0)
                                                     (elem-ref (tipPosition finger-b) 0))))
         (iota (count (fingers the-hand))))))))

(define* (set-target-angle-smoothly! index new-target #:optional (speed 0.01))
  (let* ((old-target (vector-ref (target-angles robot) index))
         (new-target* (lerp old-target new-target speed)))
    (vector-set! (target-angles robot) index new-target*)))



(define (clamp a b x)
  "Clamp x to [a, b]."
  (max a (min b x)))

(define (lerp v0 v1 t)
  (+ v0 (* t (- v1 v0))))

(define (vector-lerp v0 v1 t)
  (vector+ v0 (vector* t (vector- v1 v0))))

(define (lerp-inverse v0 v1 v-of-t)
  (/ (- v-of-t v0)
     (- v1 v0)))

(define (angle-between-palm-and-finger hand finger)
  (let* ((normal (normalized (palmNormal hand)))
         (direction (normalized (direction finger)))
         (angle (acos (dot normal direction))))
    angle))

