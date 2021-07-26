;;;
;;; Chuck Moore's Floppy Driver Code
;;; (from ColorForth)
;;;

;;;
;;; Floppy drive parameters
;;;
(: 'command)
  (byte 0)
(: 'command1)
  (byte 0) ;head 0, drive 0
(: 'cylinder)
  (byte 0)
(: 'head)
  (byte 0) ;constant
(: 'sector)
  (byte 1) ;constant
  (byte 2) ;512 bytes/sector
  (byte SECTORS_PER_TRACK)
  (byte #x1b) ;gap
  (byte #xff)

(define ms (quotient 1000000 6))
(: 'spin)
  (movb #x1c cl)
  (call 'onoff)
(: 'spin_sense_loop)
  (call 'sense)
  (jns 'spin_sense_loop)
  (movb 0 (@ 'cylinder))
  (movb 7 al)
  (movb 2 cl)
  (call 'cmd)
  (mov (* 500 ms) ecx)
(: 'spin_wait_loop)
  (loop 'spin_wait_loop)
(: 'cmdi)
  (call 'sense)
  (js 'cmdi)
  (ret)

(: 'ready)
  (mov #x03f4 edx) ;Main status register
(: 'ready_loop)
  (inb dx)
  (outb #xe1)
  (shlb 1 al) ;RQM -> carry
  (jnc 'ready_loop) ;wait until set (can transfer data)
  (lea (@ 1 edx) edx) ;Data register -> edx
  (ret)

(: 'transfer) ; al = command
  (movb 9 cl)
(: 'cmd) ; al = command, cl = parameter byte count
  (lea (@ 'command) edx)
  (movb al (@ edx))
(: 'cmd0)
  (push esi)
  (mov edx esi)
(: 'cmd1)
  (call 'ready) ;dx is left at Data Register
  (jns 'transfer_ready) ;If DIO is clear, we are ready to write
  (inb dx) ;Ignore status results
  (jmp 'cmd1)
(: 'transfer_ready)
  (lodsb)
  (outb dx)
  (outb #xe1)
  (loop 'cmd1)
  (pop esi)
  (ret)

(: 'sense)
  (movb 8 al) ;sense interrupt status
  (mov 1 ecx)
  (call 'cmd)
(: 'sense_loop)
  (call 'ready)
  (jns 'sense_loop) ;Loop until DIO says read
  (inb dx) ;ST0
  (outb #xe1)
  (andb al al)
  (ret)

(: 'seek)
  (call 'sense)
  (jns 'seek)
  (ret)

(: 'stop)
  (mov #x0c cl) ;motor off
(: 'onoff)
  (push eax)
  (movb cl al)
  (mov #x03f2 edx)
  (outb dx)
  (outb #xe1)
  (pop eax)
  (ret)

(: 'dma)
  (opd-size)(mov #x03a2 (@ 'command1)) ;12 s6 u32 ms (e 2) non-dma
  (movb 3 al) ;specify timing
  (movb 3 cl)
  (call 'cmd)
  (opd-size)(mov #x7000 (@ 'command1)) ;+seek -fifo -poll
  (movb #x13 al) ;configure
  (movb 4 cl)
  (call 'cmd)
  (mov ecx (@ 'command)) ;ecx (0) -> command, cylinder, (and head)
  (ret)

(: 'read)
  (call 'seek)
  (movb #xe6 al) ;read multi-track, double-density, skip deleted sectors
  (call 'transfer)
  (mov (* HEADS SECTORS_PER_TRACK BYTES_PER_SECTOR) ecx)
(: 'read_loop)
  (call 'ready)
  (inb dx)
  (outb #xe1)
  (stosb)
  (loop 'read_loop)
  (ret)