(align 4)
(bss 'cursor 4)
(bss 'keyboard_time 4)
(bss 'displ 4)
(bss 'displ_end 4)
(if SVGA
  (begin
    (bss 'cursor_foreground (* BYTES_PER_COLUMN PIXHEIGHT))
    (bss 'cursor_background (* BYTES_PER_COLUMN PIXHEIGHT))))
(: 'bss_end) ;;; No initialization to zero beyond here
(bss 'cylinder_state CYLINDERS)
(align 4)
(bss 'keyboard_buffer 1024)
(bss 'input_ports 2048)
