(define code 0)
(define pixmaps (make-vector (* 256 16) (integer->char 0)))
(define (icon-load f)
  (define in (open-input-file f))
  (define (loop i)
    (let ((c (read-char in)))
      (cond
        ((eof-object? c) #f)
        (else (vector-set! pixmaps i c)
              (loop (+ i 1))))))
  (loop 0)
  (close-input-port in))
(define (icon-save f)
  (define out (open-output-file f))
  (define (loop i)
    (cond
      ((< i (vector-length pixmaps))
       (write-char (vector-ref pixmaps i) out)
       (loop (+ i 1)))))
  (loop 0)
  (close-output-port out))
(define (icon n)
  (icon-choose n)
  (vga-set-position 0 0)
  (icon-interact)
  (sys-echo #t))
(define (icon-choose n)
  (if n (set! code n))
  (vga-clear)
  (vga-set-position 16 0)
  (write-char (integer->char code))
  (write-char #\space)
  (display code)
  (vga-set-position 0 0)
  (icon-display 0))
(define (icon-display y)
  (cond
    ((< y 16)
     (icon-display-row
       (char->integer (vector-ref pixmaps (+ y (* code 16))))
       128)
     (newline)
     (icon-display (+ y 1)))))
(define (icon-display-row c mask)
  (cond
    ((positive? mask)
     (write-char
       (if (positive? (& c mask)) wall empty))
     (icon-display-row c (quotient mask 2)))))
(define escape (integer->char 27))
(define (icon-interact)
  (sys-echo #f)
  (let ((c (sys-read-char)))
    (cond
      ((char=? c escape)
       (vga-set-attribute 7)
       (vga-set-position 16 0))
      (else
        (case c
          ((#\,) (if (positive? code) (icon-choose (- code 1))))
          ((#\.) (if (< code #xff) (icon-choose (+ code 1))))
          ((#\h) (icon-move 0 -1))
          ((#\k) (icon-move 1 0))
          ((#\j) (icon-move -1 0))
          ((#\l) (icon-move 0 1))
          ((#\y) (icon-draw) (icon-move 0 -1))
          ((#\i) (icon-draw) (icon-move 1 0))
          ((#\u) (icon-draw) (icon-move -1 0))
          ((#\o) (icon-draw) (icon-move 0 1)))
        (icon-interact)))))
(define (icon-move dy dx)
  (vga-set-position
    (remainder
      (+ dy 16 (vga-position-y))
      16)
    (remainder
      (+ dx 8 (vga-position-x))
      8)))
(define wall #\�)
(define empty #\.)
(define (icon-draw)
  (let* ((x (vga-position-x))
         (y (vga-position-y))
         (set (pixmap-toggle y x)))
    (write-char (if set wall empty))
    (vga-set-position y x)))
(define (pixmap-toggle y x)
  (let* ((i (+ y (* code 16)))
         (mask (power 2 (- 7 x)))
         (b (char->integer (vector-ref pixmaps i))))
    (vector-set! pixmaps i
      (integer->char
        (^ mask b)))
    (zero? (& mask b))))
(define (power b p)
  (if (zero? p)
    1
    (* b (power b (- p 1)))))