;;; Copyright (C) 2025 Justin St-Amant <jstamant24@gmail.com>
;;;
;;; Licensed under the Apache License, Version 2.0 (the "License");
;;; you may not use this file except in compliance with the License.
;;; You may obtain a copy of the License at
;;;
;;;    http://www.apache.org/licenses/LICENSE-2.0
;;;
;;; Unless required by applicable law or agreed to in writing, software
;;; distributed under the License is distributed on an "AS IS" BASIS,
;;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;; See the License for the specific language governing permissions and
;;; limitations under the License.

;;; Commentary:
;;;
;;; A remix on the Asteroids arcade game. Asteroids gravitate towards you, and
;;; you must lure them to collide with eachother
;;;
;;; Code:

(use-modules (dom canvas)
             (dom document)
             (dom element)
             (dom event)
             (dom image)
             (dom media)
             (dom window)
             (hoot ffi)
             (hoot hashtables)
             (ice-9 match)
             (math)
             (math rect)
             (math vector)
             (srfi srfi-9))

;; Data types
(define-record-type <ship>
  (make-ship position rotation velocity)
  ship-type?
  (position ship-position)
  (rotation ship-rotation set-ship-rotation!)
  ;; TODO need to split velocity into polar vector. currently setup as
  ;; x->magnitude, y->angle - see chickadee for Dave's implementation
  (velocity ship-velocity))

(define-record-type <asteroid>
  (make-asteroid position rotation velocity hit)
  asteroid-type?
  (position asteroid-position)
  (rotation asteroid-rotation set-asteroid-rotation!)
  (velocity asteroid-velocity)
  (hit asteroid-hit? set-asteroid-hit!))


(define-record-type <level>
  (make-level state ship asteroids score move-left? move-right? move-up?)
  level?
  (state level-state set-level-state!) ; play, win, lose
  (ship level-ship)
  (asteroids level-asteroids set-level-asteroids!)
  (score level-score set-level-score!)
  (move-left? level-move-left? set-level-move-left!)
  (move-right? level-move-right? set-level-move-right!)
  (move-up? level-move-up? set-level-move-up!))

;; Assets (example)
;; (define image:paddle (make-image "assets/images/paddle.png"))
;; (define audio:brick (make-audio "assets/sounds/brick.wav"))

;; Game data
(define game-width    640.0)
(define game-height   480.0)
(define game-fg "#ffffff")
(define game-bg "#140c1c")
(define ship-acceleration 0.02)
(define ship-turn-rate 4.0)
(define asteroid-timer-default 200)
(define asteroid-timer asteroid-timer-default)

(define (make-level-1)
  (make-level 'play
              (let* ((initial-velocity (vec2
                                        0.4
                                        ;; TODO implement initial velocity angle
                                        ;; (* 360 (random))))
                                        0))
                     (initial-rotation (+ (- (* (random) 60) 30)
                                          (vec2-y initial-velocity))))
                (make-ship (vec2 (/ game-width 2) (/ game-height 2))
                           initial-rotation
                           initial-velocity))
              (list (make-asteroid (vec2 0 0) 0 (vec2 0 0) #f))
              0 #f #f #f))

;; Game state
(define *level* (make-level-1))

(define (update-ship-velocity! level)
  (when (level-move-up? level)
    (let* ((ship (level-ship level))
           (velocity (ship-velocity ship))
           (rotation (ship-rotation ship))
           (new-velocity (vec2
                          (+ (* ship-acceleration (cos (degrees->radians rotation)))
                             (vec2-x velocity))
                          (+ (* ship-acceleration (sin (degrees->radians rotation)))
                             (vec2-y velocity)))))
      (set-vec2-x! velocity (vec2-x new-velocity))
      (set-vec2-y! velocity (vec2-y new-velocity)))))

(define (update-ship-rotation! level)
  (let* ((rotation (+ (ship-rotation (level-ship level))
                      (if (level-move-left? level) (- 0 ship-turn-rate) 0)
                      (if (level-move-right? level) ship-turn-rate 0))))
    (set-ship-rotation! (level-ship level) rotation)))

(define (move-asteroid asteroid ship)
  (let* ((asteroid-speed 0.4)
         (position (asteroid-position asteroid))
         (x (vec2-x position))
         (y (vec2-y position))
         (ship-pos (ship-position ship))
         (ship-x (vec2-x ship-pos))
         (ship-y (vec2-y ship-pos))
         (v (vec2 (- ship-x x) (- ship-y y))))
    (vec2-normalize! v)
    (set-vec2-x! position (+ (vec2-x position) (vec2-x v)))
    (set-vec2-y! position (+ (vec2-y position) (vec2-y v)))))

(define (collision? obj1 obj2)
  (let* ((pos1 (asteroid-position obj1))
         (pos2 (asteroid-position obj2))
         (distance (vec2-copy pos1)))
    (vec2-sub! pos1 pos2)
    (if (<= (vec2-magnitude distance) 10)
        #f
        ;; #t
        #f)))

(define dt (/ 1000.0 60.0)) ; aim for updating at 60Hz
(define (update)
  (match (level-state *level*)
    ('play
     (let* ((ship (level-ship *level*))
            (ship-pos (ship-position ship))
            (ship-vel (ship-velocity ship))
            (asteroids (level-asteroids *level*))
            (score (level-score *level*)))
       ;; Read input
       (update-ship-velocity! *level*)
       (update-ship-rotation! *level*)

       ;; Move ship
       (set-vec2-x! ship-pos (+ (vec2-x ship-pos) (vec2-x ship-vel)))
       (set-vec2-y! ship-pos (+ (vec2-y ship-pos) (vec2-y ship-vel)))

       ;; Move asteroids
       (for-each (lambda (asteroid)
                   (move-asteroid asteroid ship))
                 asteroids)

       ;; Spawn asteroids
       (set! asteroid-timer (- asteroid-timer 1))
       (when (<= asteroid-timer 0)
         (set! asteroid-timer asteroid-timer-default)
         (set-level-asteroids!
          *level*
          (append asteroids
                  (list (make-asteroid (vec2 0 0) 0 (vec2 0 0) #f)))))

       ;; Calculate collisions
       ;; (for-each
        ;; (lambda (asteroid)
          ;; (let ((hit (asteroid-hit? asteroid)))
            ;; (when (not hit)
              ;; (for-each
               ;; (lambda (other)
                 ;; (cond ((equal? asteroid other) #f)
                       ;; ((collision? asteroid other)
                        ;; (set-asteroid-hit! asteroid #t)
                        ;; (set-asteroid-hit! other #t))))
               ;; asteroids))))
        ;; asteroids)
       ;; Remove collided asteroids
       ;; (let ((purged-asteroids '()))
         ;; (for-each
          ;; (lambda (asteroid)
            ;; (unless (asteroid-hit? asteroid)
              ;; (set! purged-asteroids (append purged-asteroids (list asteroid)))))
          ;; asteroids)
         ;; (set-level-asteroids! *level* asteroids))
         ;; (set! asteroids purged-asteroids))

       ;; Final set - required to avoid overwriting
       ))
    (_ #t))
  (timeout update-callback dt))
(define update-callback (procedure->external update))

;; Rendering
(define number->string*
  (let ((cache (make-eq-hashtable))) ; assuming fixnums only
    (lambda (x)
      (or (hashtable-ref cache x)
          (let ((str (number->string x)))
            (hashtable-set! cache x str)
            str)))))

(define (draw prev-time)
  (let ((ship (level-ship *level*))
        (asteroids (level-asteroids *level*))
        (score (level-score *level*)))
    ;; Draw background
    (set-fill-color! context game-bg)
    (fill-rect context 0.0 0.0 game-width game-height)
    ;; Draw ship
    (draw-ship context (ship-position ship) (ship-rotation ship))
    ;; Draw asteroids
    (for-each (lambda (asteroid)
                (draw-asteroid context asteroid))
              asteroids)

    ;; Print score
    (set-fill-color! context game-fg)
    (set-font! context "bold 24px monospace")
    (set-text-align! context "left")
    (fill-text context "SCORE:" 16.0 36.0)
    (fill-text context (number->string* score) 108.0 36.0)
    (match (level-state *level*)
      ('win
       (set-text-align! context "center")
       (fill-text context "YAY YOU DID IT!!!" (/ game-width 2.0) (/ game-height 2.0)))
      ('lose
       (set-text-align! context "center")
       (fill-text context "OH NO, GAME OVER :(" (/ game-width 2.0) (/ game-height 2.0)))
      (_ #t)))
  (request-animation-frame draw-callback))
(define draw-callback (procedure->external draw))

(define (draw-ship context position rotation)
  (let ((x (vec2-x position))
        (y (vec2-y position)))
    (set-fill-color! context game-fg)
    (translate context x y)
    (rotate context (degrees->radians rotation))
    (begin-path context)
    (line-to context -10 -8)
    (line-to context 10 0)
    (line-to context -10 8)
    (close-path context)
    (fill context)
    (set-transform! context 1 0 0 1 0 0)))

(define (draw-asteroid context asteroid)
  (let* ((position (asteroid-position asteroid))
         (x (vec2-x position))
         (y (vec2-y position)))
    (set-stroke-style! context game-fg)
    (translate context x y)
    (begin-path context)
    (arc context 0 0 20 0 (* 2 pi))
    (close-path context)
    (stroke context)
    (set-transform! context 1 0 0 1 0 0)))

;; Input
(define key:left "ArrowLeft")
(define key:right "ArrowRight")
(define key:up "ArrowUp")
(define key:confirm "Enter")

(define (on-key-down event)
  (let ((key (keyboard-event-code event)))
    (match (level-state *level*)
      ('play
       (cond
        ((string=? key key:left)
         (set-level-move-left! *level* #t))
        ((string=? key key:right)
         (set-level-move-right! *level* #t))
        ((string=? key key:up)
         (set-level-move-up! *level* #t))))
      ((or 'win 'lose)
       (when (string=? key key:confirm)
         (set! *level* (make-level-1)))))))

(define (on-key-up event)
  (let ((key (keyboard-event-code event)))
    (match (level-state *level*)
      ('play
       (cond
        ((string=? key key:left)
         (set-level-move-left! *level* #f))
        ((string=? key key:right)
         (set-level-move-right! *level* #f))
        ((string=? key key:up)
         (set-level-move-up! *level* #f))))
      (_ #t))))

;; Canvas and event loop setup
(define canvas (get-element-by-id "canvas"))
(define context (get-context canvas "2d"))
(set-element-width! canvas (inexact->exact game-width))
(set-element-height! canvas (inexact->exact game-height))
(add-event-listener! (current-document) "keydown"
                     (procedure->external on-key-down))
(add-event-listener! (current-document) "keyup"
                     (procedure->external on-key-up))
(request-animation-frame draw-callback)
(timeout update-callback dt)
