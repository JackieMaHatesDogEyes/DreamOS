;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;This is a scheme interpreter written in x86 assembly     ;;;
;;;thanks to the description given by Abelson and Sussman in;;;
;;;Structure and Interpretation of Computer Programs.       ;;;
;;;                                                         ;;;
;;;This program is distributed under the terms of the       ;;;
;;;GNU General Public License.                              ;;;
;;;Copyright (C) 2009 David Joseph Stith                    ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load "x86.scm")
(x86-set-text-start! #x7c00)
(define program '(begin
  ;;;;;;;;;;;;;;;;;;;;;
  ;;;Compile options;;;
  ;;;;;;;;;;;;;;;;;;;;;
  (define SVGA #f)
  (define BOOT_FLOPPY #t)
  (define KERNEL_INDEX 1) ;list-ref of kernel in (sys-file-list)
  ;;;Set the following to the size desired in Kb for
  ;;;object, symbol, and string storage.
  (define MEMSIZE 6144)
  (define SYMSIZE 128)
  (define STRSIZE 6144)
  (define DICTSIZE 4096)
  ;;; Hashtable for symbols will be 128*(2<<HASHPOWER) bytes long
  ;;; and will therefore accomodate 32*(2<<HASHPOWER) symbols
  (define HASHPOWER 7)
  ;;; Hashtable for top-level definitions will be a vector
  ;;; 2<<TOPLEVELPOWER elements long
  (define TOPLEVELPOWER 8)
  ;;;Set DEBUG to #t to force continual garbage collection for debugging 
  (define DEBUG #f)
  (define WELCOME "Dream version 2.4, Copyright (C) 2009 David Joseph Stith")
  (define BOOTSTRAP_FILE "bootstrap.scm")
  (define INIT_FILE "init.scm")
  (define CYLINDERS 80)
  (define HEADS 2)
  (define SECTORS_PER_TRACK 18)
  (define BYTES_PER_SECTOR 512)
  (define BYTES_PER_TRACK (* HEADS SECTORS_PER_TRACK BYTES_PER_SECTOR))
  (define BYTES_PER_FLOPPY (* BYTES_PER_TRACK CYLINDERS))
  (define TABLE_OFFSET BYTES_PER_SECTOR)
  (define (report-address name)
    (display name)
    (display ": #x")
    (display (number->string x86-address 16))
    (display " (<")
    (display (+ 1 (quotient x86-address #x100000)))
    (display " Mb)")
    (newline))
  (load "global.scm")
  (load "boot.scm")
  (load "dreamos.scm")
  (for-each load
    '("dream.scm" "compiler.scm" "number.scm" "mp.scm" "pred.scm" "list.scm"
      "char.scm" "string.scm" "vector.scm" "control.scm" "extra.scm" "data.scm"
      "osdata.scm"))
  (newline)
  (report-address "free-memory-low")
  (: 'free_memory)
  (set! x86-address #x100000)
  (bss 'floppy_buffer BYTES_PER_FLOPPY)
  (load "bss.scm")
  (load "osbss.scm")
  (report-address "free-memory-high")))

(let ((code (x86-assemble program))
      (out (open-output-file "dream.img")))
  (display code out)
  (close-output-port out))

;;;
;;; Create boot image (cylinder 0 sector 0)
;;;
; Copy boot sector from 'dream.img'
(display "Creating boot.img")
(newline)
(call-with-output-file "boot.img"
  (lambda (out)
    ((lambda (f n)
       (call-with-input-file f
         (lambda (in)
           (define (transfer n)
             (cond
               ((positive? n)
                (write-char (read-char in) out)
                (transfer (- n 1)))))
           (transfer n))))
     "dream.img" 512)))
