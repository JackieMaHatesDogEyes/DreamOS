;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SET TYPEMATIC RATE ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;
(letrec
  ((key-command
     (lambda (x)
       (if (not (zero? (& 2 (sys-in-byte #x64))))
         (key-command x)
         (sys-out-byte x #x60)))))
  (for-each key-command '(#xf3 #x00)))

(if (> (vga-columns) 80)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; BACKWARD COMPATIBILITY ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define (vga-set-attribute n)
    (define rgb
      #(0 #xf #x3e0 #x3ef #x7800 #x780f #x7be0 #x7bef
        #x821 #x1f #x7e0 #x7ff #xf800 #xf81f #xffe0 #xffff))
    (vga-set-foreground (vector-ref rgb (& n #x0f)))
    (vga-set-background (vector-ref rgb (quotient (& n #xf0) #x10))))
  (begin
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SET CURSOR SCAN LINE START ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (sys-out-byte #x0a #x3d4)
    (sys-out-byte #x00 #x3d5)))

;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SET CURSOR PIXMAP ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;
(if (> (vga-columns) 80)
  (vga-set-cursor
    (apply string
      (map
        integer->char
       '(#xff #xff #xff #xff #xff #xff #xff #xff #xff #xff #xff #xff #xff #xff #xff #xff
         #xff #xff #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #xff #xff
         #xff #xff #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #xff #xff
         #xff #xff #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #xff #xff
         #xff #xff #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #xff #xff
         #xff #xff #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #xff #xff
         #xff #xff #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #xff #xff
         #xff #xff #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #xff #xff
         #xff #xff #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #xff #xff
         #xff #xff #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #xff #xff
         #xff #xff #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #xff #xff
         #xff #xff #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #xff #xff
         #xff #xff #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #xff #xff
         #xff #xff #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #xff #xff
         #xff #xff #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #xff #xff
         #xff #xff #xff #xff #xff #xff #xff #xff #xff #xff #xff #xff #xff #xff #xff #xff)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ADDITIONAL SYS PROCEDURES ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (sys-create-file f)
  (let ((o (open-output-file f)))
    (close-output-port o)))

(define (sys-execute-file f . x)
  (sys-copy-image f #x20000)
  (apply sys-call #x20000 x))

(define sys-copy-files #f)
(define sys-paste-files #f)
(let ((sys-file-names #f)
      (sys-files #f))
  (set! sys-copy-files
    (lambda x
      (define (read-file file)
        (reverse
          (call-with-input-file file
            (lambda (i) (read-text i '())))))
      (define (read-text i t)
        (let ((s (read-chars i '())))
          (if s
            (read-text i (cons (list->string s) t))
            t)))
      (define (read-chars i chars)
        (let ((c (read-char i)))
          (cond
            ((eof-object? c)
             (if (null? chars)
               #f
               (reverse chars)))
            ((char=? c #\newline)
             (reverse chars))
            (else
             (read-chars i (cons c chars))))))
      (set! sys-file-names x)
      (set! sys-files (map read-file x))
      #t))
  (set! sys-paste-files
    (lambda ()
      (define (write-file file text)
        (call-with-output-file file
          (lambda (o)
            (define emit-newline #f)
            (for-each
              (lambda (r)
                (if emit-newline
                  (newline o)
                  (set! emit-newline #t))
                (display r o))
              text))))
      (for-each write-file sys-file-names sys-files))))

(load "edit.scm")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ADDITIONAL VGA PROCEDURES ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (vga-draw-box-safe x y w h)
  (if (and (>= x 0) (>= y 0) (< x 1024) (< y 768))
    (if (< (+ x w) 1024)
      (if (< (+ y h) 768)
        (vga-draw-box x y w h)
        (vga-draw-box x y w (- 768 y)))
      (if (< (+ y h) 768)
        (vga-draw-box x y (- 1024 x) h)
        (vga-draw-box x y (- 1024 x) (- 768 y))))))

;;;;;;;;;;;;;;;;;;;;;;;;
;;; ASCII-ART SPLASH ;;;
;;;;;;;;;;;;;;;;;;;;;;;;
(vga-set-attribute 9)
(call-with-input-file "splash.txt"
  (lambda (in)
    (define (loop)
      (let ((c (read-char in)))
        (if (not (eof-object? c))
          (begin
            (write-char c)
            (loop)))))
    (loop)))
(vga-set-attribute 7)
