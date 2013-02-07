;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%;
;     Iteration-Based Image Generation:       ;
;               Image-Series                  ;
;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%;

;Hi Alex, I'm editing but not really.

;;;;;;;;;
;Vectors;
;;;;;;;;;
;Color vectors
(define monochrome-vec (vector "black" "darkgray" "gray" "lightgray" "white"))
(define red-vec (vector "red" "deeppink" "hotpink" "lightpink" "pink"))
(define orange-vec (vector "darkorange" "sandybrown" "sandybrown" "khaki" "moccasin"))
(define yellow-vec (vector "yellow" "yellow" "khaki" "palegoldenrod" "lemonchiffon"))   
(define green-vec (vector "lime" "limegreen" "mediumseagreen" "skyblue" "silver"))
(define blue-vec (vector "dodgerblue" "dodgerblue" "dodgerblue" "cornflowerblue" "lightblue"))
(define purple-vec (vector "blueviolet" "mediumpurple" "mediumorchid" "orchid" "plum"))  
(define rainbow-vec (vector "red" "orange" "yellow" "lime" "mediumblue"))
;Reversed color vectors
(define red-vec-reversed (vector "pink" "lightpink" "hotpink" "deeppink" "red"))        
(define orange-vec-reversed (vector "moccasin" "khaki" "sandybrown" "sandybrown" "darkorange"))  
(define yellow-vec-reversed (vector "lemonchiffon" "palegoldenrod" "khaki" "yellow" "yellow"))                  
(define green-vec-reversed (vector "silver" "skyblue" "mediumseagreen" "limegreen" "lime"))               
(define blue-vec-reversed (vector "dodgerblue" "dodgerblue" "dodgerblue" "cornflowerblue" "lightblue"))
(define purple-vec-reversed (vector "plum" "orchid" "mediumorchid" "mediumpurple" "blueviolet"))
(define rainbow-vec-reversed (vector "mediumblue" "lime" "yellow" "orange" "red"))

;These vectors are directories used by image-series to select an appropriate color scheme for a given gradient
(define light-bg-vec (vector green-vec green-vec-reversed blue-vec blue-vec-reversed purple-vec purple-vec-reversed rainbow-vec rainbow-vec-reversed monochrome-vec)) 
(define dark-bg-vec (vector red-vec red-vec-reversed orange-vec orange-vec-reversed yellow-vec yellow-vec-reversed rainbow-vec rainbow-vec-reversed monochrome-vec))

;;;;;;;;;;;;
;Procedures;
;;;;;;;;;;;;
;;; Procedure:
;;;   collatz
;;; Parameters:
;;;   n, a non-negative integer
;;; Purpose:
;;;   output how many pre-defined operations are necessary to reach 1 from n (clean up??)
;;; Produces:
;;;   collatz-steps-n, a non-negative natural number
;;; Preconditions:
;;;   [No additional.]
;;; Postconditions:
;;;   collatz-steps-n represents the number of situational operations detailed below necessary for n to reach 1.
;;;      If n is odd, tally two operations* and recurse with input (3n+1)/2.  
;;;      If n is even, tally one operation and recurse with input n/2
;;;      If n is equal to one, tally zero operations and output
;;;       * Two operations are tallied because one more than thrice an integer is always an even number
(define collatz 
  (lambda (n)
    (cond ((<= n 1)
           0)
          (else
           (+ 1 (if (odd? n) 
                    (+ 1 (collatz (/ (+ 1 (* n 3)) 2)))
                    (collatz (/ n 2))))))))

;;; Procedure:
;;;   image-palette
;;; Parameters:
;;;   
;;; Purpose:
;;;   
;;; Produces:
;;;   
;;; Preconditions:
;;;   
;;; Postconditions:
;;;   
;Generalize color vectors?
(define test-vector (vector 0 0 0 255 255 255))
(define image-palette
  (lambda (n)
    (let* ((palette (image-new (+ 1 (round (/ (collatz n) (+ 3 (modulo n 10))))) 7))
           (width (image-width palette))
           (height (image-height palette)))
          (image-compute-pixels! palette 
                                 (lambda (col row)
                                   (let ((col-scale (/ (- col width -1) width)))
                                     (cond ((= row 0)
                                            (rgb-new (* col-scale
                                                        (abs (- (vector-ref test-vector 0)
                                                                (vector-ref test-vector 3))))
                                                     (* col-scale
                                                        (abs (- (vector-ref test-vector 1)
                                                                (vector-ref test-vector 4))))
                                                     (* col-scale
                                                        (abs (- (vector-ref test-vector 2)
                                                                (vector-ref test-vector 5)))))))))))))

;;; Procedure:
;;;   yelps-brush-generator
;;; Parameters:
;;;   n, a non-zero integer
;;;   image, an image
;;; Purpose:
;;;   generate a brush whose properties can be modified on-the-fly
;;; Produces:
;;;   "yelps-brush", a brush
;;; Preconditions:
;;;   [No additional.]
;;; Postconditions:
;;;   "yelps-brush" will not modify any of the other brushes in the brush library
;;;   "yelps-brush" will not create copies of itself
(define yelps-brush-generator
  (lambda (n image)
    (let ((width (image-width image))
          (height (image-height image)))
    (when (not (equal? null (context-list-brushes "yelps-brush"))) ;Searches for, and destroys, old "yelps-brush" instances
           (gimp-brush-delete "yelps-brush"))
    (gimp-brush-new "yelps-brush") 
      (cond ((integer? (/ n 3)) ;Checks to see if n should be a fuzzy brush
             (gimp-brush-set-hardness "yelps-brush" 0.25)
             (gimp-brush-set-spacing "yelps-brush" 25))
            (else
             (gimp-brush-set-hardness "yelps-brush" 1.0)
             (gimp-brush-set-spacing "yelps-brush" 20)))
    (gimp-context-set-opacity 100)))) 

;;; Procedure:
;;;   image-series-gradient!
;;; Parameters:
;;;   n, a non-negative integer
;;;   image, an image
;;; Purpose:
;;;   picks among seven gradients depending on the result of the modulo n 7 expression, and generates the gradient on image
;;; Produces:
;;;   nothing - image-series-gradient! modifies image
;;; Preconditions:
;;;   [No additional.]
;;; Postconditions:
;;;   image-series-gradient! will overwrite the existing image with one of seven pre-designed gradients
(define image-series-gradient!
  (lambda (n image)
    (let* ((width (image-width image))
           (height (image-height image))
           (grad-scaler-h (/ 256 height))
           (grad-scaler-w (/ 256 width))
           (half-width (round (/ width 2)))   ;Half-width and half-height save image-series-gradient! tens of thousands of division operations for performing image-compute on a 200x200 image (for certain gradients)
           (half-height (round (/ height 2)))
           (collatz-steps-n (collatz n)))
      (image-compute-pixels! image                        
                             (lambda (col row)
                               (cond ((= 0 (modulo n 7)) ;Gradient 0 - a light, vertical gradient; yellow fades to white
                                      (let ((optimizer (* grad-scaler-h (+ 128 row)))) ;The optimizers in the gradients serve to cut down on repetition in operations
                                        (rgb-new optimizer optimizer (* grad-scaler-h row))))
                                     ((= 1 (modulo n 7)) ;Gradient 1 - a light, multicolored gradient; green, pink, black, and white duel for dominance
                                      (let ((optimizer (* grad-scaler-w col)))
                                        (rgb-new optimizer (* grad-scaler-h row) optimizer)))
                                     ((= 2 (modulo n 7)) ;Gradient 2 - a light, monotone gradient with vertical bars that are mini-gradients from light gray to dark gray
                                      (let ((transforming-gray (+ 90 (* 2 collatz-steps-n) (modulo col 14))))
                                        (rgb-new transforming-gray transforming-gray transforming-gray)))
                                     ((= 3 (modulo n 7)) ;Gradient 3 - a dark, radial gradient; bright blue corners fade into a black center
                                      (let ((col-less (< col half-width)) ;Locally binding the results of these tests cuts down on inequality checks
                                            (row-less (< row half-height)))
                                        (rgb-new 10 10 
                                                 (cond 
                                                   ((and col-less row-less)       ;Quadrant 1
                                                    (+ (- half-width (modulo col half-width)) 
                                                       (- half-height (modulo row half-height))))
                                                   ((and (not col-less) row-less) ;Quadrant 2
                                                    (+ (modulo col half-width)
                                                       (- half-height (modulo row half-height))))
                                                   ((and col-less (not row-less)) ;Quadrant 3
                                                    (+ (- half-width (modulo col half-width)) 
                                                       (modulo row half-height)))
                                                   (else                          ;Quadrant 4
                                                    (+ (modulo col half-width) (modulo row half-height)))))))
                                     ((= 4 (modulo n 7)) ;Gradient 4 - a dark, inverted radial gradient fades from black to red.
                                      (let ((col-less (< col half-width))
                                            (row-less (< row half-height)))
                                        (rgb-new  
                                         (cond  
                                           ((and col-less row-less)               ;Quadrant 1
                                            (+ (modulo col half-width) (modulo row half-height)))
                                           ((and (not col-less) row-less)         ;Quadrant 2
                                            (+ (- half-width (modulo col half-width)) 
                                               (modulo row half-height)))                      
                                           ((and col-less (not row-less))         ;Quadrant 3
                                            (+ (modulo col half-width)
                                               (- half-height (modulo row half-height))))
                                           (else                                  ;Quadrant 4
                                            (+ (- half-width (modulo col half-width)) 
                                               (- half-height (modulo row half-height)))))
                                         10 10)))
                                     ((= 5 (modulo n 7)) ;Gradient 5 - a dark, soft gradient that fades from green (top left) to black to red (bottom right)
                                      (rgb-new (* .5 row (/ col width))
                                               (* .5 (- height row) (/ (- height col) height))
                                               0))
                                     (else               ;Gradient 6 - a soft, dark gradient that fades from yellow (bottom left) to blue (top right)
                                      (let ((optimizer (* .5 row (/ (- width col) width))))
                                        (rgb-new optimizer optimizer
                                                 (* .5 (- height row) (/ col width)))))))))))

;;; Procedure:
;;;   angle-selector
;;; Parameters:
;;;   n, a non-negative integer
;;;   iteration, a non-negative integer
;;; Purpose:
;;;   based on the properties of the number n and the value of iteration, angle-selector determines which angle equation should be output
;;; Produces:
;;;   local-angle, a non-negative integer
;;; Preconditions:
;;;   [No additional.]
;;; Postconditions:
;;;   [No additional.]  ADD SOME?
(define angle-selector
  (lambda (n iteration)
    (cond
      ((integer? (/ n 5))
       (* iteration (collatz n)))
      ((integer? (/ n 7))
       (* -1 iteration 
          ((o round (l-s / 36) (l-s + 1) (r-s modulo 13)) n)))
      ((integer? (/ n 11))
       90)
      (else
       ((o round (l-s / 36) (l-s + 1) (r-s modulo 13)) n)))))

;;; Procedure:
;;;   turtle-spiral-arm!
;;; Parameters:
;;;   n, a non-zero integer
;;;   image, an image
;;;   iteration, a positive integer
;;;   turtle, a turtle
;;; Purpose:
;;;   creates a "galaxy-arm" shape using a turtle
;;; Produces:
;;;   nothing; modifies image
;;; Preconditions:
;;;   [No additional.]
;;; Postconditions:
;;;   
(define turtle-spiral-arm! 
  (lambda (n image iteration turtle)
    (let* ((width (image-width image))
           (height (image-height image))
           (steps (+ 1 (modulo n 100))) ;Steps tells the turtle-spiral-arm! procedure how many steps should be used in creating each arm of the "galaxy" shape
           (is-light (> 3 (modulo n 7)))
           (color-ref-optimizer (/ 5 steps))
           (color-vec-optimizer (modulo n 9))
           (vector-ref-optimizer ((o (r-s modulo 9) (r-s + n)) iteration))
           (turtle-forward-optimizer (/ (* height width) 4000))
           (color-vec (cond  
                        ((odd? n) ;If n is odd, then the same palette will be used for all iterations
                         (if is-light
                             (vector-ref light-bg-vec color-vec-optimizer)
                             (vector-ref dark-bg-vec color-vec-optimizer)))
                        (else ;If n is not odd, then the palette will change with each iteration
                         (if is-light
                             (vector-ref light-bg-vec vector-ref-optimizer)
                             (vector-ref dark-bg-vec vector-ref-optimizer))))))
      (turtle-set-brush! turtle "yelps-brush")
      (let turtle-spiral ((current-step 0))
        (when (< current-step steps) ;Keeps iterating until current-step is not less than steps
          (gimp-brush-set-radius "yelps-brush" (* (/ (- steps current-step) 9.5) ;(- steps current-step) creates a narrow->big spiral, maybe . param1?
                                                  .5 turtle-forward-optimizer))
          (turtle-set-color! turtle (vector-ref color-vec (abs (- (round (* color-ref-optimizer current-step)) 1)))) ;This expression selects the color to be used and ensures a smooth distribution of color
          (turtle-forward! turtle (+ 5 (/ turtle-forward-optimizer (+ 1 current-step)))) 
          (turtle-turn! turtle (angle-selector n iteration))                             
          (turtle-spiral (+ current-step 1)))))))

;;; Procedure:
;;;   image-series
;;; Parameters:
;;;   n, a positive integer
;;;   width, a non-zero integer
;;;   height, a non-zero integer
;;;   Optional parameters:
;;;   parameter1, a symbol for use with object-oriented extensions of image-series
;;; Purpose:
;;;   image-series produces a wide variety images that are tagged and reproducible
;;; Produces:
;;;   generated-image, an image 
;;; Preconditions:
;;;   [No additional.]
;;; Postconditions:
;;;   generated-image will have dimensions of width by height
;;;   generated-image will consist of one of seven preset gradients shadowing a systematically generated spiral of varying shape, size, length, and color
;;;   generated-image will be tagged by a blot in the lower left-hand corner - coordinates: (1, (height - 1)) - of color n
;;;   generated-image's recursive complexity will be related to the last digit of n - 0 being the least complex, 9 being the most
(define image-series
  (lambda (n width height . parameter1)
    (let* ((n-error (not (and (integer? n) (>= n 0))))
           (width-error (not (and (integer? width) (positive? width))))
           (height-error (not (and (integer? height) (positive? height)))))
      (cond ((or n-error width-error height-error)  
             (error (string-append "image-series: "
                                   (cond (n-error "n")
                                         (width-error "width")
                                         (height-error "height"))
                                   " expected "
                                         (cond (n-error "a non-zero integer")
                                               (else "a positive integer"))
                                         ", given " 
                                               (cond (n-error (number->string n))
                                                     (width-error (number->string width))
                                                     (height-error (number->string height))) ".")))
            (else     ;Kernel
             (let* ((reduced-collatz-steps-n (+ 1 (round (/ (collatz n) (+ 3 (modulo n 10)))))) ;Reduced-collatz-steps-n helps to prevent the creation of extremely complicated images
                    (is-light (> 3 (modulo n 7)))
                    (generated-image (image-new width height))
                    (yelps (turtle-down! (turtle-new generated-image))))
               (yelps-brush-generator n generated-image)
               (image-series-gradient! n generated-image)    ;Creates the background gradient
               (let series ((iteration 0))
                 (cond ((= iteration reduced-collatz-steps-n) ;Stops when it has recursed a number of times equal to the reduced-collatz-steps-n
                        (context-set-fgcolor! n)
                        (context-set-brush! "Circle (05)")
                        (image-blot! generated-image 1 (- height 1))) ;The blob's RGB value is equal to n; it acts as an earmark for the images produced by this procedure
                       (else
                        (turtle-teleport! yelps (/ width 2) (/ height 2)) ;The turtle, Yelps, is moved to the center of the canvas
                        (turtle-face! yelps (round (* iteration (/ 360 reduced-collatz-steps-n)))) ;Turns Yelps to an angle such that image-series creates a full spiral
                        (turtle-spiral-arm! n generated-image iteration yelps)
                        (series (+ iteration 1)))))))))))
