(define-module (leapmotion-commands)
  #:use-module ((emacsy emacsy) #:hide (state position))
  #:use-module (leapmotion-event)
  #:use-module (LeapGuile)
  #:use-module ((LeapGuile-primitive) :renamer (symbol-prefix-proc 'primitive:))
  #:use-module (oop goops)
  #:use-module ((srfi srfi-1) #:select (fold)) ;; fold
  #:use-module (camera)
  #:use-module (guile-user)
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

(define-key eracs-mode-map 
  (kbd "lm-frame") 
  (lambda ()
    (when (is-a? this-command-event <leapmotion-event>)
      (let ((frame (lme:frame this-command-event)))
        (unless (empty (hands frame))
          (let* ((hand (elem-ref (hands frame) 0))
                 (fingers (fingers hand)))
            ;; Check if the hand has any fingers
            (unless (empty fingers)
              (let* ((scale 1.)
                     (translate (vector->Vector #(0. -100. 0.)))
                     (finger (leftmost fingers))
                     (actual-pos (tipPosition finger))
                     (pos (Leap* (Leap+ translate actual-pos) scale))
                     (dir (direction finger)))
                (format #t "Setting camera to position ~a and direction ~a ~%" (Vector->vector pos) (Vector->vector dir))
                (set-parameter! 'camera-position (Vector->vector pos))
                (set-parameter! 'camera-target (Vector->vector (Leap+ dir pos)))
                )
              ;; Calculate the hand's average finger tip position
              (let ((avg-pos (make <Vector>)))
                (Leap+ avg-pos avg-pos)
                (set! avg-pos (fold Leap+ avg-pos (map tipPosition (*List->list fingers))))
                (set! avg-pos (Leap/ avg-pos (count fingers)))
                #;(format #t "Hand has ~a fingers, average finger tip position ~a~%" (count fingers) avg-pos)
                                        ;(format #t "Setting camera to position ~a~%" (Vector->vector avg-pos))
                                        ;(set-parameter! 'camera-position (Vector->vector avg-pos))))
                
                ;; Get the hand's sphere radius and palm position
                #;(format #t "Hand sphere radius: ~a mm, palm position: ~a~%"
                (sphereRadius hand)
                (palmPosition hand))
                
                ;; Get the hand's normal vector and direction
                (let ((normal (palmNormal hand))
                      (direction (direction hand)))
                  ;; Calculate the hand's pitch, roll, and yaw angles
                  #f
                  #;(apply format #t
                  "Hand pitch: ~a degrees, roll: ~a degrees, yaw: ~a degrees~%"
                  (map radians->degrees
                  (list (pitch direction)
                  (roll direction)
                  (yaw direction))))))))
          )))
    (message "Got frame")
    ))
