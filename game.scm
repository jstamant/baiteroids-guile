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
  (make-asteroid position rotation velocity)
  asteroid-type?
  (position asteroid-position)
  (rotation asteroid-rotation set-asteroid-rotation!)
  (velocity asteroid-velocity))

(define-record-type <brick-type>
  (make-brick-type image points)
  brick-type?
  (image brick-type-image)
  (points brick-type-points))

(define-record-type <brick>
  (make-brick type hitbox)
  brick?
  (type brick-type)
  (hitbox brick-hitbox)
  (broken? brick-broken? set-brick-broken!))

(define-record-type <ball>
  (make-ball velocity hitbox)
  ball?
  (velocity ball-velocity)
  (hitbox ball-hitbox))

(define-record-type <paddle>
  (make-paddle velocity hitbox)
  paddle?
  (velocity paddle-velocity)
  (hitbox paddle-hitbox))

(define-record-type <level>
  (make-level state ship asteroids bricks ball paddle score move-left? move-right? move-up?)
  level?
  (state level-state set-level-state!) ; play, win, lose
  (ship level-ship)
  (asteroids level-asteroids)
  (bricks level-bricks)
  (ball level-ball)
  (paddle level-paddle)
  (score level-score set-level-score!)
  (move-left? level-move-left? set-level-move-left!)
  (move-right? level-move-right? set-level-move-right!)
  (move-up? level-move-up? set-level-move-up!))

;; Assets
(define image:paddle       (make-image "assets/images/paddle.png"))
(define image:ball         (make-image "assets/images/ball.png"))
(define image:brick-red    (make-image "assets/images/brick-red.png"))
(define image:brick-green  (make-image "assets/images/brick-green.png"))
(define image:brick-blue   (make-image "assets/images/brick-blue.png"))
(define audio:brick        (make-audio "assets/sounds/brick.wav"))
(define audio:paddle       (make-audio "assets/sounds/paddle.wav"))

;; Game data
(define game-width    640.0)
(define game-height   480.0)
(define game-fg "#ffffff")
(define game-bg "#140c1c")
(define brick-width   64.0)
(define brick-height  32.0)
(define ball-width    22.0)
(define ball-height   22.0)
(define paddle-width  104.0)
(define paddle-height 24.0)
(define paddle-speed  6.0)
(define ship-acceleration 0.02)
(define ship-turn-rate 4.0)
(define asteroid-timer 100)
(define brick:red     (make-brick-type image:brick-red 10))
(define brick:green   (make-brick-type image:brick-green 20))
(define brick:blue    (make-brick-type image:brick-blue 30))

(define (make-brick* type x y)
  (make-brick type (make-rect x y brick-width brick-height)))

(define (make-brick-grid types)
  (let* ((h (vector-length types))
         (w (vector-length (vector-ref types 0)))
         (offset-x (/ (- game-width (* w brick-width)) 2.0))
         (offset-y 48.0)
         (bricks (make-vector (* w h))))
    (do ((y 0 (+ y 1)))
        ((= y h))
      (let ((row (vector-ref types y)))
        (do ((x 0 (+ x 1)))
            ((= x w))
          (vector-set! bricks (+ (* y w) x)
                       (make-brick* (vector-ref row x)
                                    (+ offset-x (* x brick-width))
                                    (+ offset-y (* y brick-height)))))))
    bricks))

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
              (make-asteroid (vec2 100 100) 0 (vec2 0 0))
              (make-brick-grid
               (vector
                (vector brick:red brick:green brick:blue brick:red brick:green brick:blue brick:red brick:green)
                (vector brick:green brick:blue brick:red brick:green brick:blue brick:red brick:green brick:blue)
                (vector brick:blue brick:red brick:green brick:blue brick:red brick:green brick:blue brick:red)
                (vector brick:red brick:green brick:blue brick:red brick:green brick:blue brick:red brick:green)
                (vector brick:green brick:blue brick:red brick:green brick:blue brick:red brick:green brick:blue)
                (vector brick:blue brick:red brick:green brick:blue brick:red brick:green brick:blue brick:red)))
              (make-ball (vec2 1.0 3.0)
                         (make-rect (/ game-width 2.0) (/ game-height 2.0)
                                    ball-width ball-height))
              (make-paddle (vec2 0.0 0.0)
                           (make-rect (- (/ game-width 2.0)
                                         (/ paddle-width 2.0))
                                      (- game-height paddle-height 8.0)
                                      paddle-width paddle-height))
              0 #f #f #f))

;; Game state
(define *level* (make-level-1))

(define (level-clear? level)
  (let ((bricks (level-bricks level)))
    (let loop ((i 0))
      (if (< i (vector-length bricks))
          (if (brick-broken? (vector-ref bricks i))
              (loop (+ i 1))
              #f)
          #t))))

(define (win! level)
  (set-level-state! level 'win))

(define (lose! level)
  (set-level-state! level 'lose))

(define (update-paddle-velocity! level)
  (let ((speed (* paddle-speed
                  (+ (if (level-move-left? level) -1.0 0.0)
                     (if (level-move-right? level) 1.0 0.0)))))
    (set-vec2-x! (paddle-velocity (level-paddle level)) speed)))

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

(define (speed-up-ball! ball)
  (let* ((v (ball-velocity ball))
         (speed (+ (vec2-magnitude v) (* (random) 0.1)))
         ;; Also change its angle slightly.  Not the proper Breakout
         ;; behavior but I don't want to write the code for that. :)
         (dir (+ (atan (vec2-y v) (vec2-x v))
                 (- (* (random) 0.5) 0.25))))
    (set-vec2-x! v (* (cos dir) speed))
    (set-vec2-y! v (* (sin dir) speed))))

(define (reflect-ball! ball x? y?)
  (let ((v (ball-velocity ball)))
    (when x? (set-vec2-x! v (- (vec2-x v))))
    (when y? (set-vec2-y! v (- (vec2-y v))))))

(define (collide-ball! ball hitbox)
  (let ((b-hitbox (ball-hitbox ball)))
    (and (rect-intersects? b-hitbox hitbox)
         (let ((overlap (rect-clip b-hitbox hitbox)))
           ;; Resolve collision by adjusting the ball's position the
           ;; minimum amount along the X or Y axis.
           (if (< (rect-width overlap) (rect-height overlap))
               (begin
                 (reflect-ball! ball #t #f)
                 (if (= (rect-x b-hitbox) (rect-x overlap))
                     (set-rect-x! b-hitbox (+ (rect-x b-hitbox) (rect-width overlap)))
                     (set-rect-x! b-hitbox (- (rect-x b-hitbox) (rect-width overlap)))))
               (begin
                 (reflect-ball! ball #f #t)
                 (if (= (rect-y b-hitbox) (rect-y overlap))
                     (set-rect-y! b-hitbox (+ (rect-y b-hitbox) (rect-height overlap)))
                     (set-rect-y! b-hitbox (- (rect-y b-hitbox) (rect-height overlap))))))))))

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

(define dt (/ 1000.0 60.0)) ; aim for updating at 60Hz
(define (update)
  (match (level-state *level*)
    ('play
     (let* ((bricks (level-bricks *level*))
            (ball (level-ball *level*))
            (b-velocity (ball-velocity ball))
            (b-hitbox (ball-hitbox ball))
            (paddle (level-paddle *level*))
            (p-velocity (paddle-velocity paddle))
            (p-hitbox (paddle-hitbox paddle))
            (ship (level-ship *level*))
            (ship-pos (ship-position ship))
            (ship-vel (ship-velocity ship))
            (asteroid (level-asteroids *level*))
            (asteroid-pos (asteroid-position asteroid))
            (score (level-score *level*)))
       ;; Read input
       (update-ship-velocity! *level*)
       (update-ship-rotation! *level*)

       ;; ;; Move ball and paddle
       ;; (set-rect-x! b-hitbox (+ (rect-x b-hitbox) (vec2-x b-velocity)))
       ;; (set-rect-y! b-hitbox (+ (rect-y b-hitbox) (vec2-y b-velocity)))
       ;; ;; We only move the paddle along the x-axis.
       ;; (set-rect-x! p-hitbox
       ;;              (clamp (+ (rect-x p-hitbox) (vec2-x p-velocity))
       ;;                     0.0
       ;;                     (- game-width paddle-width)))

       ;; Move ship
       (set-vec2-x! ship-pos (+ (vec2-x ship-pos) (vec2-x ship-vel)))
       (set-vec2-y! ship-pos (+ (vec2-y ship-pos) (vec2-y ship-vel)))

       ;; Move asteroids
       (move-asteroid asteroid ship)

       ;; Spawn asteroids
       ;; (set! asteroid-timer (- asteroid-timer 1))
       ;; (when (<= asteroid-timer 0)
         ;; (make-asteroid (vec2 100 100) 0 (vec2 0 0))
         ;; (set! asteroid-timer 100))

       ;; Collide ball against walls, bricks, and paddle.
       (cond
        ((< (rect-x b-hitbox) 0.0)      ; left wall
         (set-rect-x! b-hitbox 0.0)
         (reflect-ball! ball #t #f))
        ((> (+ (rect-x b-hitbox) (rect-width b-hitbox)) game-width) ; right wall
         (set-rect-x! b-hitbox (- game-width (rect-width b-hitbox)))
         (reflect-ball! ball #t #f))
        ((< (rect-y b-hitbox) 0.0)      ; top wall
         (set-rect-y! b-hitbox 0.0)
         (reflect-ball! ball #f #t))
        ((> (+ (rect-y b-hitbox) (rect-height b-hitbox)) game-height) ; bottom wall
         (lose! *level*))
        ((collide-ball! ball (paddle-hitbox paddle))
         (media-play audio:paddle)
         (speed-up-ball! ball))
        (else
         (let loop ((i 0) (hit? #f))
           (if (< i (vector-length bricks))
               (let ((brick (vector-ref bricks i)))
                 (if (and (not (brick-broken? brick))
                          (collide-ball! ball (brick-hitbox brick)))
                     (begin
                       (media-play audio:brick)
                       (speed-up-ball! ball)
                       (set-brick-broken! brick #t)
                       (set-level-score! *level*
                                         (+ (level-score *level*)
                                            (brick-type-points (brick-type brick))))
                       (loop (+ i 1) #t))
                     (loop (+ i 1) hit?)))
               ;; Maybe change to win state if all bricks are broken.
               (when (and hit? (level-clear? *level*))
                 (win! *level*))))))))
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
  (let (;;(bricks (level-bricks *level*))
        ;;(ball (level-ball *level*))
        ;;(paddle (level-paddle *level*))
        (ship (level-ship *level*))
        (asteroids (level-asteroids *level*))
        (score (level-score *level*)))
    ;; Draw background
    (set-fill-color! context game-bg)
    (fill-rect context 0.0 0.0 game-width game-height)
    ;; Draw ship
    (draw-ship context (ship-position ship) (ship-rotation ship))
    ;; Draw asteroids
    (draw-asteroid context asteroids)
    ;; (do ((i 0 (+ i 1)))
    ;;     ((= i (vector-length asteroids)))
    ;;   (let* ((asteroid (vector-ref asteroids i))
    ;;          (type (asteroid-type asteroid))
    ;;          (hitbox (asteroid-hitbox asteroid)))
    ;;     (unless (asteroid-broken? asteroid)
    ;;       (draw-image context (asteroid-type-image type)
    ;;                   0.0 0.0
    ;;                   asteroid-width asteroid-height
    ;;                   (rect-x hitbox) (rect-y hitbox)
    ;;                   asteroid-width asteroid-height))))
    ;; ;; Draw paddle
    ;; (let ((w 104.0)
    ;;       (h 24.0)
    ;;       (hitbox (paddle-hitbox paddle)))
    ;;   (draw-image context image:paddle
    ;;               0.0 0.0 w h
    ;;               (rect-x hitbox) (rect-y hitbox) w h))
    ;; ;; Draw ball
    ;; (let ((w 22.0)
    ;;       (h 22.0)
    ;;       (hitbox (ball-hitbox ball)))
    ;;   (draw-image context image:ball
    ;;               0.0 0.0 w h
    ;;               (rect-x hitbox) (rect-y hitbox) w h))
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
         (set-level-move-left! *level* #t)
         (update-paddle-velocity! *level*))
        ((string=? key key:right)
         (set-level-move-right! *level* #t)
         (update-paddle-velocity! *level*))
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
         (set-level-move-left! *level* #f)
         (update-paddle-velocity! *level*))
        ((string=? key key:right)
         (set-level-move-right! *level* #f)
         (update-paddle-velocity! *level*))
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
