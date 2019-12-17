#!/usr/bin/env scheme --script


(load "utils.ss")

;; --- Day 8: Space Image Format ---

; The Elves' spirits are lifted when they realize you have an opportunity to
; reboot one of their Mars rovers, and so they are curious if you would spend a
; brief sojourn on Mars. You land your ship near the rover.
; 
; When you reach the rover, you discover that it's already in the process of
; rebooting! It's just waiting for someone to enter a BIOS password. The Elf
; responsible for the rover takes a picture of the password (your puzzle input)
; and sends it to you via the Digital Sending Network.
; 
; Unfortunately, images sent via the Digital Sending Network aren't encoded
; with any normal encoding; instead, they're encoded in a special Space Image
; Format. None of the Elves seem to remember why this is the case. They send
; you the instructions to decode it.
; 
; Images are sent as a series of digits that each represent the color of a
; single pixel. The digits fill each row of the image left-to-right, then move
; downward to the next row, filling rows top-to-bottom until every pixel of the
; image is filled.
; 
; Each image actually consists of a series of identically-sized layers that are
; filled in this way. So, the first digit corresponds to the top-left pixel of
; the first layer, the second digit corresponds to the pixel to the right of
; that on the same layer, and so on until the last digit, which corresponds to
; the bottom-right pixel of the last layer.
; 
; For example, given an image 3 pixels wide and 2 pixels tall, the image data
; 123456789012 corresponds to the following image layers:
; 
; Layer 1: 123
;          456
; 
; Layer 2: 789
;          012
; 
; The image you received is 25 pixels wide and 6 pixels tall.
; 
; To make sure the image wasn't corrupted during transmission, the Elves would
; like you to find the layer that contains the fewest 0 digits. On that layer,
; what is the number of 1 digits multiplied by the number of 2 digits?


;; SpaceImageFormat is string of numbers
;; SpaceImageFormat -> (listof Integer)
(define (SIF->list sif)
  (map string->number (map string (string->list sif))))


;; (listof Integer) Integer Integer -> (listof (listof Integer))
(define (list->layers lst w h)
  (define (iter remaining rsf)
    (cond [(null? remaining) (reverse rsf)]
          [else
            (iter (list-tail remaining (* w h))
                  (cons (take remaining (* w h))
                        rsf))]))
  (iter lst '()))


;; (listof Integer) (listof Integer) -> Boolean
(define (less-zeros l1 l2)
  (< (length (filter zero? l1))
     (length (filter zero? l2))))


(define W 25)
(define H 6)

(define layers 
  (list->layers (SIF->list (car (read-file "inputs/08.txt"))) 25 6))


(let* [(fewest-zeros (car (sort less-zeros layers)))
       (1s (length (filter (lambda (n) (= n 1)) fewest-zeros)))
       (2s (length (filter (lambda (n) (= n 2)) fewest-zeros)))]
  (begin
    (display (* 1s 2s))
    (display "\n")))
;; Your puzzle answer was 1905.

;; --- Part Two ---

; Now you're ready to decode the image. The image is rendered by stacking the
; layers and aligning the pixels with the same positions in each layer. The
; digits indicate the color of the corresponding pixel: 0 is black, 1 is white,
; and 2 is transparent.
; 
; The layers are rendered with the first layer in front and the last layer in
; back. So, if a given position has a transparent pixel in the first and second
; layers, a black pixel in the third layer, and a white pixel in the fourth
; layer, the final image would have a black pixel at that position.
; 
; For example, given an image 2 pixels wide and 2 pixels tall, the image data
; 0222112222120000 corresponds to the following image layers:
; 
; Layer 1: 02
;          22
; 
; Layer 2: 11
;          22
; 
; Layer 3: 22
;          12
; 
; Layer 4: 00
;          00
; 
; Then, the full image can be found by determining the top visible pixel in
; each position:
; 
;     The top-left pixel is black because the top layer is 0.
;     The top-right pixel is white because the top layer is 2 (transparent),
;     but the second layer is 1.
;     The bottom-left pixel is white because the top two layers are 2, but the
;     third layer is 1.
;     The bottom-right pixel is black because the only visible pixel in that
;     position is 0 (from layer 4).
; 
; So, the final image looks like this:
; 
; 01
; 10
; 
; What message is produced after decoding your image?


(define (overlay . args)
  (define (iter args last)
    (if (null? args) last
      (case last
        [(0 1) last]
        [else
          (iter (cdr args) (car args))])))
  (iter args -1))


;(define (render-sif img w)
;  (define (display-row row)
;    (begin
;      (map (lambda (pix)
;             (begin
;               (if (zero? pix)
;                 (display #\#)
;                 (display #\ ))
;               (display #\ )))
;           row)
;      (display "\n")))
;  (define (iter row remaining)
;    (cond [(null? remaining) (display-row row)]
;          [else
;            (begin
;              (display-row row)
;              (iter (take remaining w) (list-tail remaining w)))]))
;  (iter '() img))
;
(define (display-row r)
  (begin
    (for-each
      (lambda (p)
        (if (zero? p) (display #\ ) (display #\#))) r)
    (display "\n")))


(define (to-rows img w)
  (define (iter acc todo)
    (cond [(null? todo) (reverse acc)]
          [else
            (iter (cons (take todo w) acc) (list-tail todo w))]))
  (iter '() img))


(define image (apply map overlay layers))
(define row-img (to-rows image W))
(for-each display-row row-img)

