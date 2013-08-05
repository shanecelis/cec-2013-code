(use-modules ((rnrs) #:select (vector-map vector-for-each mod))
             (emacsy emacsy)
             (nsga-ii)
             (statprof)
             )

(define profile? #t)

(define (stop-profiling)
    (statprof-stop)
    (statprof-display))

(when profile?
  (statprof-reset 0 50000 #t)
  (statprof-start)
  (add-hook! emacsy-terminate-hook stop-profiling))

(load "quadruped2.scm")
(load "hill-climber.scm")

(nsga-ii-search (lambda (weights)
                  (set-nn-weights! weights)
                  (vector (eval-robot-headless)))
                #:objective-count 1
                #:gene-count 504
                #:population-count 4
                #:generation-count 10)
(stop-profiling)
(quit)
