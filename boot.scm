;;;;;;;;;;;;;;;;;;;
;;; Boot Sector ;;;
;;;;;;;;;;;;;;;;;;;
(define CYLINDER_PRESENT #b01)
(define CYLINDER_DIRTY #b10)
; A file descriptor is:
(define FD_OFFSET 0) ; start offset from beginning of floppy
(define FD_POSITION 4) ; position in bytes
(define FD_LENGTH 8) ; length in bytes
(define FD_NAME 12) ; name as null terminated string
(define FD_NEXT 36)
(define (alloc-table-entry index offset)
  (+ BYTES_PER_SECTOR
     (lookup 'floppy_buffer)
     (* FD_NEXT index)
     offset))
(define (fill-until o b e)
  (cond
    ((< (file-offset x86-address) o)
     (byte b)
     (fill-until o b e))
    ((> (file-offset x86-address) o)
     (x86-error e))))

(define (end-boot-sector)
  (fill-until 510 0 "Boot segment is too long!")
  (wyde #xaa55))

(: 'image_base)
(real-mode)
(jmp 'boot0)

;;;
;;; Global Descriptor Table
;;;
(: 'gdt)
  (wyde (- (lookup 'gdt_end) (lookup 'gdt))) ;length of gdt
  (tetra 'gdt0)
(: 'gdt0)
(wydes #x0000 #x0000 #x0000 #x0000) ;NULL Selector
(define (gdt-offset) (- x86-address (lookup 'gdt0)))
(define codesel (gdt-offset)) (wydes #xffff #x0000 #x9a00 #x00cf)
(define datasel (gdt-offset)) (wydes #xffff #x0000 #x9200 #x00cf)
(: 'gdt_end)

(: 'boot0)
  (mov (if SVGA #x0117 #x03) bx) ;VESA video mode
  (mov #x4f02 ax)
  (int #x10)
  (cli)
  (mov cs bx)
  (mov bx ds)
;;;
;;; Enter Protected Mode
;;;
  (lgdt 'gdt)
  (mov cr0 ax)
  (orb 1 al)
  (mov ax cr0)
  (far-jmp codesel 'pm)

  (protected-mode)
(: 'pm)
  (mov datasel ax)
  (mov ax ds)
  (mov ax es)
  (mov ax ss)
  (mov #xa0000 esp)

;;;
;;; A20 VooDoo
;;;
  (movb #xd1 al)
  (outb #x64)
(: 'a20loop)
  (inb #x64)
  (andb 2 al)
  (jnz 'a20loop)
  (mov #x4b al)
  (outb #x60)

(if BOOT_FLOPPY
  (begin
    (call 'spin)
    (call 'dma)
    (: 'cold)
    (call 'sense)
    (jns 'cold)
    (mov 'floppy_buffer edi)
    (call 'read) ;Read file allocation table to buffer (first cylinder)
    (movb CYLINDER_PRESENT (@ 'cylinder_state))
    (mov (@ (alloc-table-entry KERNEL_INDEX 0)) eax) ;Offset of kernel file
    (mov 'floppy_buffer edi)
    (add eax edi)
    (push edi) ;Buffer destination of kernel file
    (mov BYTES_PER_TRACK ecx)
    (cdq)
    (div ecx)
    (movb al (@ 'cylinder))
    (mov edx eax)
    (: 'read_cylinders)
    (mov (@ (alloc-table-entry KERNEL_INDEX FD_LENGTH)) ecx)
    (push ecx) ;Length of kernel
    (add eax ecx) ;Length from beginning of cylinder
    (: 'read_cylinders_loop)
    (push ecx)
    (call 'read)
    (pop ecx)
    (movzb (@ 'cylinder) eax)
    (movb CYLINDER_PRESENT (@ 'cylinder_state eax))
    (incb (@ 'cylinder))
    (sub BYTES_PER_TRACK ecx)
    (jns 'read_cylinders_loop)
    (call 'stop)
    (pop ecx) ;Length of kernel
    (pop esi) ;Kernel file in floppy buffer
    (shr 2 ecx) ;Length of kernel in tetras
    (mov 'image_base edi)
    (rep)(movs)
    (jmpl 'startup)
    (load "floppy.scm")
    (end-boot-sector)
    (load "hd.scm"))
  (begin
    (call 'hd_mount)
    (mov (@ (alloc-table-entry KERNEL_INDEX 0)) eax) ;Offset of kernel file
    (mov 'floppy_buffer esi)
    (add eax esi)
    (mov (@ (alloc-table-entry KERNEL_INDEX FD_LENGTH)) ecx)
    (shr 2 ecx) ;Length of kernel in tetras
    (mov 'image_base edi)
    (rep)(movs)
    (call 'dma)
    (: 'cold)
    (call 'sense)
    (jns 'cold)
    (jmpl 'startup)
    (load "hd.scm")
    (end-boot-sector)
    (load "floppy.scm")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
