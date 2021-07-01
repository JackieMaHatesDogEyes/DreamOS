(define HD_DATA #x1f0)
(define HD_ERROR #x1f1)
(define HD_SECTOR_COUNT #x1f2)
(define HD_STATUS #x1f7)
(define HD_MASK_READY #x08)
(define HD_MASK_BUSY #x80)
(define HD_COMMAND_READ #x20)
(define HD_COMMAND_WRITE #x30)
(define HD_BLOCK_CYLINDERS 11)

(: 'hd_wait_busy)
  (mov HD_STATUS edx)
(: 'hd_wait_busy_loop)
  (inb dx) ;status
  (test HD_MASK_BUSY eax) ;busy
  (jnz 'hd_wait_busy_loop)
  (mov 1000 ecx)
(: 'hd_wait_busy_wait)
  (loop 'hd_wait_busy_wait)
  (ret)

(: 'hd_wait_ready)
  (call 'hd_wait_busy)
(: 'hd_wait_ready_loop)
  (inb dx)
  (test HD_MASK_READY eax)
  (jz 'hd_wait_ready_loop)
  (ret)

(: 'hd_sector) ;cylinder (low) in ebp
  (clear eax)
  (mov HD_SECTOR_COUNT edx)
  (outb dx) ;sector count (0=256)
  (inc edx)
  (outb dx) ;sector
  (inc edx)
  (mov (@ 'hd_cylinder_offset) eax)
  (add ebp eax)
  (outb dx) ;cylinder low
  (inc edx)
  (clear eax)
  (outb dx) ;cylinder high
  (inc edx)
  (movb #xe0 al) ;LBA, master, address 0
  (outb dx)
  (inc edx)
  (ret)

(: 'hd_mount)
  (mov 'floppy_buffer edi)
  (mov 0 ebp) ;cylinder
(: 'sys_read_disk_cylinder_loop)
  (clear eax)
  (call 'hd_wait_busy)
  (call 'hd_sector)
  (movb HD_COMMAND_READ al)
  (outb dx) ;command

  (mov 256 ebx) ;sector count
(: 'sys_read_disk_loop)
  (call 'hd_wait_ready)
  (mov 128 ecx)
  (mov HD_DATA edx)
(: 'sys_read_disk_loop_data)
  (in dx)
  (stos)
  (loop 'sys_read_disk_loop_data)

  (dec ebx) ;next sector
  (jnz 'sys_read_disk_loop)

  (inc ebp) ;next cylinder
  (cmp HD_BLOCK_CYLINDERS ebp)
  (jne 'sys_read_disk_cylinder_loop)

  (mov 'cylinder_state edi)
  (mov CYLINDERS ecx)
  (mov (+ CYLINDER_PRESENT CYLINDER_DIRTY) eax)
  (rep)(stosb)
  (ret)
(: 'hd_cylinder_offset)
  (tetra 0)