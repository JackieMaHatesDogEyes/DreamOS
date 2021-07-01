;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;This program is distributed under the terms of the       ;;;
;;;GNU General Public License.                              ;;;
;;;Copyright (C) 2009 David Joseph Stith                    ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define STDERR 0)
(define GENERIC_READ 1)
(define GENERIC_WRITE 2)

(define SCREEN_COLS (if SVGA 128 80))
(define SCREEN_ROWS (if SVGA 48 25))
(define SCROLL_ROWS (if SVGA 24 1))

(define MAXKEY (- (lookup 'shiftkeymap) (lookup 'keymap)))
(define nokey 0)
(define escapekey 27)
(define shiftkey #xff)

(define (insure-mounted)
  (cmpb 0 (@ 'cylinder_state))
  (jel 'error_floppy_not_mounted))

(define (return-integer-constant c)
  (mov INTEGER (@ FREE))
  (mov c (@ 4 FREE))
  (mov FREE VAL)
  (jmpl 'advance_free))

(define (exit-with-code x)
  (movb #xfe al) ;reset
  (outb #x64)
  (let ((spin (symbol-seq)))
    (: spin)
    (jmp spin)))

(: 'write)
  (call 'seek)
  (movb #xc5 al) ;write data
  (call 'transfer)
  (mov (* HEADS SECTORS_PER_TRACK BYTES_PER_SECTOR) ecx)
(: 'write_loop)
  (call 'ready)
  (lodsb)
  (outb dx)
  (outb #xe1)
  (loop 'write_loop)
  (ret)

(: 'flop)
  (mov #x03f2 edx) ;Digital Output Register
  (inb dx)
  (outb #xe1)
  (testb #x10 al) ;Motor on?
  (jnz 'flop_end)
  (jmpl 'spin)
(: 'flop_end)
  (ret)

(: 'error_floppy_not_mounted)
(new-message 'msg_floppy_not_mounted "Floppy is not mounted")
  (init-message 'msg_floppy_not_mounted)
  (jmpl 'error_msg)

(: 'initialize_ports)
  (mov (object INPUT_PORT 0) (@ 'current_input_port))
  (add 8 FREE)
  (mov (object OUTPUT_PORT 0) (@ 'current_output_port))
  (add 8 FREE)
  (ret)

(: 'parse_command_line)
(if INIT_FILE
 (begin
  (mov 'init_file TEMP)
  (mov (string-length INIT_FILE) (@ 'str_len))
  (call 'prim_load_scratch)))
  (ret)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Read one character from file given in io_file (tetra) and place
;;character in input (tetra).
(: 'getch)
  (pusha)
  (cmp 0 (@ 'io_file))
  (je 'getch_keyboard_buffer)

(: 'getch_file)
  (mov (@ 'io_file) eax)
  (cmp -1 eax)
  (je 'getch_eof) ;file is closed
  (mov (@ FD_POSITION eax) ebx)
  (cmp -1 ebx)
  (je 'getch_eof)
  (mov (@ eax) ecx) ;file descriptor
  (cmp (@ FD_LENGTH ecx) ebx) ;length
  (jge 'getch_eof)
  (inc (@ FD_POSITION eax)) ;increment position in advance
  (add (@ FD_OFFSET ecx) ebx) ;start offset + position
  (clear eax)
  (movb (@ 'floppy_buffer ebx) al)
  (mov eax (@ 'input))
  (popa)
  (ret)

(: 'getch_eof)
  (mov -1 (@ 'input))
  (popa)
  (ret)

(: 'getch_keyboard_buffer)
  (mov (@ 'keyboard_buffer_pos) edi)
  (cmp (@ 'keyboard_buffer_eob) edi)
  (jb 'getch_keyboard_read_buffer)
  (mov 'keyboard_buffer edi)
  (mov edi (@ 'keyboard_buffer_pos))
(: 'getch_keyboard_buffer_loop)
  (call 'getch_keyboard)
  (mov (@ 'input) eax)
  (cmpb escapekey al)
  (je 'escape)
  (cmpb 9 al) ;tab
  (je 'getch_keyboard_buffer_tab)
  (cmpb 8 al) ;backspace
  (jne 'getch_keyboard_buffer_store)
  (dec edi)
  (cmp 'keyboard_buffer edi)
  (jae 'getch_keyboard_buffer_loop)
  (inc edi)
  (jmp 'getch_keyboard_buffer_loop)

(: 'escape)
  (popa)
  (mov 'true (@ 'toggle_echo))
  (call 'close_files)
  (mov 0 (@ 'keyboard_buffer_eob))
  (jmpl 'break)

(: 'getch_keyboard_buffer_tab)
  (mov #\space al)
  (stosb)
(: 'getch_keyboard_buffer_store)
  (stosb)
  (cmpb 10 al) ;newline
  (jne 'getch_keyboard_buffer_loop)

  (mov edi (@ 'keyboard_buffer_eob))
  (mov 'keyboard_buffer edi)
(: 'getch_keyboard_read_buffer)
  (movzb (@ edi) ecx)
  (mov ecx (@ 'input))
  (inc edi)
  (mov edi (@ 'keyboard_buffer_pos))
(: 'getch_keyboard_return)
  (popa)
  (ret)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(: 'keyboard_read_buffer)
  (movzb (@ edi) ecx)
  (mov ecx (@ 'input))
  (inc edi)
  (mov edi (@ 'keyboard_buffer_pos))
  (clear edi)
(: 'sys_read_char_return)
  (mov (@ 'input) TEMP)
  (jmpl 'return_char)

(new-additional-primitive "sys-read-char")
  (mov (@ 'keyboard_buffer_pos) edi)
  (cmp (@ 'keyboard_buffer_eob) edi)
  (jb 'keyboard_read_buffer)
  (push 'sys_read_char_return)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(: 'getch_keyboard)
  (pusha)
  (if SVGA
     (call 'draw_cursor))
  (jmp 'getch_spin)

(: 'getch_ready_spin)
  (inc (@ 'keyboard_time))
  (inb #x64)
  (testb 1 al)
  (jz 'getch_ready_spin)
  (call 'read_key_ascii)
(: 'getch_spin)
  (mov (@ 'key_ascii) eax)
  (test eax eax)
  (js 'getch_ready_spin)

(: 'getch_keyboard_continue)
  (mov eax (@ 'input))
  (mov -1 (@ 'key_ascii))
  (if SVGA (call 'erase_cursor))
  (cmp 'false (@ 'toggle_echo))
  (je 'getch_keyboard_return)
  (mov 'input esi)
  (mov 1 ecx)
  (jmpl 'puts_video_esi_ecx)

;;read_key_ascii assumes a keycode is ready
;;If the key code maps to an ascii code then
;;(@ 'key_ascii) is set.
(: 'read_key_ascii)
  (xor eax eax)
  (inb #x60)
  (testb al al)
  (js 'getch_release)
  (cmpb MAXKEY al)
  (ja 'read_key_ascii_return)
  (mov (@ 'current_keymap) ebx)
  (add eax ebx)
  (movb (@ ebx) al)
  (cmpb shiftkey al)
  (je 'getch_shift)
  (cmpb nokey al)
  (je 'read_key_ascii_return)
  (mov eax (@ 'key_ascii))
(: 'read_key_ascii_return)
  (ret)

(: 'getch_release)
  (andb #x7f al)
  (cmpb MAXKEY al)
  (ja 'read_key_ascii_return)
  (mov (@ 'current_keymap) ebx)
  (add eax ebx)
  (movb (@ ebx) al)
  (cmpb shiftkey al)
  (jne 'read_key_ascii_return)
  (mov 'keymap (@ 'current_keymap))
  (ret)

(: 'getch_shift)
  (mov 'shiftkeymap (@ 'current_keymap))
  (ret)

(new-primitive "char-ready?")
  ;;Are there characters on the buffer?
  (mov (@ 'keyboard_buffer_pos) ecx)
  (cmp (@ 'keyboard_buffer_eob) ecx)
  (jbl 'return_true)
  ;;Is there a character buffered by read_key_ascii?
  (cmp 0 (@ 'key_ascii))
  (jnsl 'return_true)
  ;;Is there a keypress in the hardware buffer?
  (inb #x64)
  (testb 1 al)
  (jzl 'return_false)
  ;;Read a character if there is one.
  (pusha)
  (call 'read_key_ascii)
  (popa)
  ;;Was there one?
  (cmp 0 (@ 'key_ascii))
  (jsl 'return_false)
  (return-true)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Display character in output (byte) to file given in io_file (tetra)
(: 'putch)
  (pusha)
  (mov 'output ecx)
  (mov 1 (@ 'str_len))
  (cmp 0 (@ 'io_file))
  (jel 'puts_video)
  (jmp 'puts_file)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Display string at ecx with length in str_len to file given in io_file (tetra)
(: 'puts)
  (cmp 0 (@ 'str_len))
  (je 'puts_end)
  (pusha)
  (cmp 0 (@ 'io_file))
  (jel 'puts_video)

(: 'puts_file)
  (mov esp ebp)
  (push ecx)
  (mov (@ 'io_file) eax)
  (cmp -1 eax)
  (je 'error_file_closed)
  (mov (@ FD_POSITION eax) edi) ;position
  (cmp -1 edi)
  (je 'error_file_closed)
  (add (@ FD_OFFSET eax) edi) ;start offset + position
  (push edi)
  (mov edi eax)
  (mov eax ebx)
  (push CYLINDER_DIRTY)
  (call 'insure_floppy_offsets)
  (pop edi) ;discard parameter
  (pop edi)
  (mov (@ 'io_file) eax)
  (mov (@ 'str_len) ecx)
  (add edi ecx) ;offset of projected end of file
  (sub (@ (+ FD_NEXT FD_OFFSET) eax) ecx) ;minus start of next
  (jb 'puts_ram) ;negative if we have room
  (shr 9 ecx)
  (inc ecx) ;Number of sectors of additional room we need
  (shl 9 ecx) ;;Number of bytes in the additional integral sectors we need.
  (push edi) ;offset of end of file
  (call 'make_room)
  (pop edi)
(: 'puts_ram)
  (lea (@ 'floppy_buffer edi) edi)
  (pop esi)
  (mov (@ 'str_len) ecx)
  (add ecx (@ FD_POSITION eax))
  (add ecx (@ FD_LENGTH eax))
  (rep)(movsb)
  (popa)
(: 'puts_end)
  (ret)

(: 'error_file_closed)
  (popa)
(new-message 'msg_file_closed "File is closed.")
  (init-message 'msg_file_closed)
  (jmpl 'error_msg)

(: 'make_room)
  (cmp -1 (@ (+ FD_NEXT FD_LENGTH) eax)) ;is next file the last?
  (je 'error_disk_full)
  (push eax) ;caller's file descriptor
  (add FD_NEXT eax)

  (push ecx) ;bytes to move forward
  (push eax) ;file descriptor
  (add (@ FD_LENGTH eax) ecx)
  (add (@ FD_OFFSET eax) ecx)
  (mov ecx ebx) ;ebx = last byte
  (mov (@ FD_OFFSET eax) eax) ;eax = first byte
  (push CYLINDER_DIRTY)
  (call 'insure_floppy_offsets)
  (pop ecx) ;discard
  (pop eax) ;file descriptor
  (pop ecx) ;bytes to move forward

  (mov (@ FD_OFFSET eax) esi)
  (add (@ FD_LENGTH eax) esi)
  (mov esi edi)
  (add ecx edi)
  (cmp (@ (+ FD_NEXT FD_OFFSET) eax) edi)
  (jb 'make_room_mem)
  (push esi)
  (push edi)
  (call 'make_room)
  (pop edi)
  (pop esi)
(: 'make_room_mem)
  (add ecx (@ FD_OFFSET eax))
  (jmp 'make_room_mem_loop_begin)

(: 'make_room_mem_loop)
  (movb (@ 'floppy_buffer esi) dl)
  (movb dl (@ 'floppy_buffer edi))
(: 'make_room_mem_loop_begin)
  (sub 1 esi)
  (sub 1 edi)
  (cmp (@ FD_OFFSET eax) edi)
  (jae 'make_room_mem_loop)

  (pop eax) ;caller's file descriptor
  (ret)

(: 'error_disk_full)
  (mov ebp esp)
  (popa)
(new-message 'msg_disk_full "Disk is full.")
  (init-message 'msg_disk_full)
  (jmpl 'error_msg)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Insure that floppy offset eax through ebx is present
;;; (@ 4 esp) = CYLINDER_PRESENT | CYLINDER_DIRTY
;;;
(: 'insure_floppy_offsets)
  (push eax)
  (mov ebx eax)
  (mov BYTES_PER_TRACK ecx)
  (clear edx)
  (cdq)
  (div ecx)
  (mov eax ebx) ;last cylinder
  (pop eax)
  (clear edx)
  (cdq)
  (div ecx) ;first cylinder
(: 'insure_floppy_cylinders)
  (cmpb 0 (@ 'cylinder_state eax))
  (jne 'cylinder_present)
(: 'cylinder_not_present)
  (push ebx)
  (push eax) ;cylinder
  (call 'flop)
  (mov (@ esp) eax)
  (mov (@ 4 esp) ecx)
  (orb CYLINDER_PRESENT ecx)
  (movb ecx (@ 'cylinder_state eax))
  (movb al (@ 'cylinder))
  (mov BYTES_PER_TRACK ecx)
  (mul ecx)
  (lea (@ 'floppy_buffer eax) edi)
  (call 'read)
  (pop eax)
  (pop ebx)
(: 'cylinder_present)
  (mov (@ 4 esp) ecx)
  (orb ecx (@ 'cylinder_state eax))
  (inc eax)
  (cmp ebx eax)
  (jbe 'insure_floppy_cylinders)

  (pusha)
  (call 'stop)
  (popa)
  (ret)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(new-additional-primitive "sys-mount-floppy")
  (insure-no-more-args ARGL)
  (pusha)
  (mov 'cylinder_state edi)
  (mov CYLINDERS ecx)
  (clear eax)
  (rep)(stosb)
  (push CYLINDER_PRESENT)
  (clear eax ebx)
  (call 'insure_floppy_cylinders)
  (pop ecx)
  (call 'close_files)
  (popa)
  (ret)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(new-additional-primitive "sys-read")
  (mov CYLINDER_PRESENT (@ 'io_file))
  (jmp 'sys_copy)

(new-additional-primitive "sys-copy")
  (mov CYLINDER_DIRTY (@ 'io_file))
(: 'sys_copy)
  (insure-no-more-args ARGL)
  (insure-mounted)
  (pusha)
  (mov (+ TABLE_OFFSET (lookup 'floppy_buffer)) ebx) ;First file entry

(: 'sys_copy_loop)
  (cmp -1 (@ FD_LENGTH ebx))
  (jel 'sys_copy_end)
  (mov (@ FD_OFFSET ebx) eax)
  (add (@ FD_LENGTH ebx) eax)
  (add FD_NEXT ebx)
  (jmp 'sys_copy_loop)

(: 'sys_copy_end)
  (push (@ 'io_file))
  (mov eax ebx)
  (mov ebx (@ 'output))
  (clear eax)
  (call 'insure_floppy_offsets)
  (pop eax)
  (popa)
  (mov (@ 'output) TEMP)
  (mov (object INTEGER TEMP) VAL)
  (jmpl 'advance_free)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(new-additional-primitive "sys-file-exists?")
  (insure-mounted)
  (call 'get_last_string)
  (shr LENGTH_SHIFT TEMP)
  (mov TEMP (@ 'str_len))
  (mov (@ 4 ARGL) ecx)
  (pusha)
  (call 'find_file_name)
  (cmp -1 (@ FD_LENGTH ebx))
  (je 'sys_file_does_not_exist)
  (popa)
  (jmpl 'return_true)

(: 'sys_file_does_not_exist)
  (popa)
  (jmpl 'return_false)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(new-additional-primitive "sys-rename-file")
  (insure-mounted)
  (insure-more-args ARGL)
  (mov (@ ARGL) VAL)
  (insure-string VAL TEMP)
  (shr LENGTH_SHIFT TEMP)
  (mov TEMP (@ 'str_len))
  (mov (@ 4 ARGL) ARGL)
  (push (@ 4 VAL))
  (call 'get_last_string)
  (pop TEMP)
  (pusha)
  (push ARGL)
  (call 'find_file_name)
  (pop esi)
  (mov (@ esi) ecx)
  (mov (@ 4 esi) esi)
  (shr LENGTH_SHIFT ecx)
  (mov ecx (@ 'str_len))
  (cmp -1 (@ FD_LENGTH ebx))
  (jel 'error_file_not_found)
  (lea (@ FD_NAME ebx) edi)
  (mov 23 ecx)
(: 'sys_rename_loop)
  (cmp 0 (@ 'str_len))
  (je 'sys_rename_end)
  (movsb)
  (dec (@ 'str_len))
  (loop 'sys_rename_loop)
(: 'sys_rename_end)
  (clear eax)
  (movsb)
  (orb CYLINDER_DIRTY (@ 'cylinder_state))
  (popa)
  (ret)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(new-additional-primitive "sys-delete-file")
  (insure-mounted)
  (insure-more-args ARGL)
  (call 'get_last_string)
  (shr LENGTH_SHIFT TEMP)
  (mov TEMP (@ 'str_len))
  (mov (@ 4 ARGL) TEMP)
  (pusha)
  (call 'find_file_name)
  (cmp -1 (@ FD_LENGTH ebx))
  (jel 'error_file_not_found)
  (cmp -1 (@ FD_POSITION ebx))
  (jnel 'error_file_already_open)
  (mov ebx edi)
  (mov FD_NEXT esi)
  (add edi esi)
(: 'sys_delete_file_loop)
  (mov FD_NEXT ecx)
  (rep)(movsb)
  (cmp -1 (@ FD_LENGTH edi))
  (jne 'sys_delete_file_loop)
  (orb CYLINDER_DIRTY (@ 'cylinder_state))
  (popa)
  (ret)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Find file descriptor whose path is at ecx, (@ 'str_len).
;;Returns descriptor, or end of descriptor table if not found, in ebx
(: 'find_file_name)
  (mov (+ TABLE_OFFSET (lookup 'floppy_buffer)) ebx) ;First file entry
  (push ecx)
  (jmp 'find_file_loop)

(: 'find_file_next)
  (add FD_NEXT ebx)
(: 'find_file_loop)
  (cmp -1 (@ FD_LENGTH ebx))
  (jel 'find_file_name_end)
  (lea (@ FD_NAME ebx) esi)
  (mov (@ esp) edi)
  (mov (@ 'str_len) ecx)
  (test ecx ecx)
  (jz 'find_file_compare_end)
(: 'find_file_compare_loop)
  (cmpsb) 
  (jne 'find_file_next)
  (loop 'find_file_compare_loop)
(: 'find_file_compare_end)
  (cmpb 0 (@ esi))
  (jne 'find_file_next)
(: 'find_file_name_end)
  (pop ecx)
  (ret)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Open file whose path is at ecx, (@ 'str_len)
;;using flags in io_file (tetra).
;;Place descriptor in io_file
(: 'open_file)
  (insure-mounted)
  (pusha)
  (call 'find_file_name)
  (cmp -1 (@ FD_LENGTH ebx))
  (jel 'file_not_found)

  (cmp GENERIC_READ (@ 'io_file))
  (je 'open_file_readonly)
  (cmp -1 (@ FD_POSITION ebx))
  (jne 'error_file_already_open)
(: 'open_found_file)
  (inc (@ FD_POSITION ebx)) ;clear position
  (mov 0 (@ FD_LENGTH ebx)) ;truncate
  (orb CYLINDER_DIRTY (@ 'cylinder_state))
  (push CYLINDER_DIRTY)
  (mov ebx (@ 'io_file))
(: 'open_floppy_file)
  (mov (@ FD_OFFSET ebx) eax)
  (mov (@ FD_LENGTH ebx) ebx)
  (add eax ebx)
  (call 'insure_floppy_offsets)
  (pop ebx) ;discard parameter
  (popa)
  (ret)

(: 'open_file_readonly)
  (mov 'input_ports eax)
  (jmp 'open_file_readonly_loop_begin)
(: 'open_file_readonly_loop)
  (add 8 eax)
(: 'open_file_readonly_loop_begin)
  (cmp (@ 'last_input_port) eax)
  (je 'open_file_readonly_last)
  (cmp -1 (@ FD_POSITION eax))
  (jne 'open_file_readonly_loop)
(: 'open_file_readonly_found)
  (mov ebx (@ eax))
  (mov 0 (@ FD_POSITION eax))
  (mov eax (@ 'io_file))
  (push CYLINDER_PRESENT)
  (jmp 'open_floppy_file)

(: 'open_file_readonly_last)
  (add 8 (@ 'last_input_port))
  (jmp 'open_file_readonly_found)

(: 'error_file_already_open)
  (popa)
(new-message 'msg_file_already_open "File already open.")
  (init-message 'msg_file_already_open)
  (jmpl 'error_msg)

(: 'file_not_found)
  (cmp GENERIC_READ (@ 'io_file))
  (je 'error_file_not_found)
;;; Create new file
  (mov -1 (@ (+ FD_NEXT FD_LENGTH) ebx))
  (mov (@ FD_OFFSET ebx) edx)
  (mov edx (@ (+ FD_NEXT FD_OFFSET) ebx))
  (mov -1 (@ (+ FD_NEXT FD_POSITION) ebx))

  (mov 0 (@ FD_LENGTH ebx))
  (mov 0 (@ FD_OFFSET ebx)) ;in case this is the first file
  (cmp (+ TABLE_OFFSET (lookup 'floppy_buffer)) ebx) ;First file entry
  (je 'new_file_name)
  (mov (@ (- FD_OFFSET FD_NEXT) ebx) eax)
  (add (@ (- FD_LENGTH FD_NEXT) ebx) eax)
  (dec eax)
  (shr 9 eax)
  (inc eax)
  (shl 9 eax)
  (mov eax (@ FD_OFFSET ebx))
(: 'new_file_name)
  (lea (@ FD_NAME ebx) edi)
  (mov ecx esi)
(: 'new_file_name_loop)
  (lodsb)
  (cmpb 0 al)
  (je 'new_file_name_loop_end)
  (stosb) 
  (jmp 'new_file_name_loop)

(: 'new_file_name_loop_end)
  (stosb)
  (jmpl 'open_found_file)

(: 'error_file_not_found)
  (popa)
(new-message 'msg_file_not_found "File not found.")
  (init-message 'msg_file_not_found)
  (jmpl 'error_msg)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(new-additional-primitive "sys-file-list")
  (insure-no-more-args ARGL)
  (insure-mounted)
  (clear VAL)
  (mov (+ TABLE_OFFSET (lookup 'floppy_buffer)) TEMP)
  (jmp 'sys_file_list_loop_begin)

(: 'sys_file_list_loop)
  (lea (@ FD_NAME TEMP) EXP)
  (mov EXP (@ 4 FREE))
  (jmp 'sys_file_list_length_loop_begin)
(: 'sys_file_list_length_loop)
  (inc EXP)
(: 'sys_file_list_length_loop_begin)
  (cmpb 0 (@ EXP))
  (jne 'sys_file_list_length_loop)
  (sub (@ 4 FREE) EXP)
  (shl LENGTH_SHIFT EXP)
  (add IMMUTABLE_STRING EXP)
  (mov EXP (@ FREE))
  (mov FREE EXP)
  (add 8 FREE)
  (mov (object EXP VAL) VAL)
  (push TEMP)
  (call 'advance_free)
  (pop TEMP)
  (add FD_NEXT TEMP)
(: 'sys_file_list_loop_begin)
  (cmp -1 (@ FD_LENGTH TEMP))
  (jne 'sys_file_list_loop)
  (ret)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Close file whose descriptor is ecx
(: 'close_file)
  (cmp -1 ecx)
  (je 'close_file_already)
  (mov -1 (@ FD_POSITION ecx))
(: 'close_file_already)
  (ret)

;;;
;;; Insure files are closed
;;;
(: 'close_files)
  (mov (+ TABLE_OFFSET (lookup 'floppy_buffer)) eax) ;First file entry
  (mov -1 ecx)
  (jmp 'close_files_loop_begin)

(: 'close_files_loop)
  (mov ecx (@ FD_POSITION eax))
  (add FD_NEXT eax)
(: 'close_files_loop_begin)
  (cmp ecx (@ FD_LENGTH eax))
  (jne 'close_files_loop)
  (ret)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(new-additional-primitive "sys-write-floppy")
  (insure-no-more-args ARGL)
  (insure-mounted)
  (pusha)
  (call 'flop)

  (clear eax)
  (mov 'floppy_buffer esi)
(: 'sys_write_floppy_loop)
  (test CYLINDER_DIRTY (@ 'cylinder_state eax))
  (jz 'sys_write_floppy_next)
  (push eax)
  (push esi)
  (movb al (@ 'cylinder))
  (call 'write)
  (pop esi)
  (pop eax)
  (xorb CYLINDER_DIRTY (@ 'cylinder_state eax))
(: 'sys_write_floppy_next)
  (inc eax)
  (add BYTES_PER_TRACK esi)
  (cmp CYLINDERS eax)
  (jle 'sys_write_floppy_loop)

  (call 'stop)
  (popa)
  (ret)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(new-additional-primitive "sys-key-time")
  (insure-no-more-args ARGL)
  (mov (@ 'keyboard_time) ecx)
  (mov (object INTEGER TEMP) VAL)
  (jmpl 'advance_free)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(new-additional-primitive "sys-time")
  (insure-no-more-args ARGL)
  (rdtsc)
  (clear edx)
  (mov (object INTEGER VAL) VAL)
  (jmpl 'advance_free)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(new-additional-primitive "sys-echo")
  (cmp 0 ARGL)
  (jne 'sys_set_echo)
  (mov (@ 'toggle_echo) VAL)
  (ret)
(: 'sys_set_echo)
  (insure-last-arg ARGL)
  (mov (@ ARGL) VAL)
  (cmp 'false VAL)
  (je 'sys_set_echo_boolean)
  (mov 'true VAL)
(: 'sys_set_echo_boolean)
  (mov VAL (@ 'toggle_echo))
  (ret)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(new-additional-primitive "sys-read-byte")
  (call 'get_last_exact_natural)
  (mov (@ 4 TEMP) VAL)
  (movzb (@ VAL) VAL)
  (jmpl 'return_exact_integer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(new-additional-primitive "sys-write-byte")
  (insure-more-args ARGL)
  (call 'get_exact_natural)
  (mov (@ 4 TEMP) VAL)
  (mov (@ 4 ARGL) ARGL)
  (call 'get_last_exact_natural)
  (mov (@ 4 TEMP) TEMP)
  (movb VAL (@ TEMP))
  (clear VAL)
  (ret)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(new-additional-primitive "sys-in-byte")
  (call 'get_last_exact_natural)
  (mov (@ 4 TEMP) edx)
  (inb edx)
  (clear edx)
  (jmpl 'return_exact_integer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(new-additional-primitive "sys-out-byte")
  (insure-more-args ARGL)
  (call 'get_exact_natural)
  (mov (@ 4 TEMP) VAL)
  (mov (@ 4 ARGL) ARGL)
  (call 'get_last_exact_natural)
  (mov (@ 4 TEMP) edx)
  (outb edx)
  (clear eax edx)
  (ret)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(new-additional-primitive "sys-in")
  (call 'get_last_exact_natural)
  (mov (@ 4 TEMP) edx)
  (in edx)
  (clear edx)
  (jmpl 'return_exact_integer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(new-additional-primitive "sys-out")
  (insure-more-args ARGL)
  (call 'get_exact_natural)
  (mov (@ 4 TEMP) VAL)
  (mov (@ 4 ARGL) ARGL)
  (call 'get_last_exact_natural)
  (mov (@ 4 TEMP) edx)
  (out edx)
  (clear eax edx)
  (ret)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(new-additional-primitive "sys-call")
  (insure-more-args ARGL)
  (call 'get_exact_natural)
  (mov TEMP EXP)
  (mov (@ 4 ARGL) ARGL)
  (call 'reverse)
  (mov VAL ARGL)
  (pusha)
  (mov esp (@ 'backup))
  (push 0)
  (jmp 'sys_call_loop_begin)

(: 'sys_call_loop)
  (mov (@ ARGL) VAL)
  (test VAL VAL)
  (jzl 'error_expected_string)
  (cmpb TYPE_STRING (@ VAL))
  (jnel 'error_expected_string)
  (push (@ 4 VAL))
  (mov (@ 4 ARGL) ARGL)
(: 'sys_call_loop_begin)
  (test ARGL ARGL)
  (jnz 'sys_call_loop)

  (calln (@ 4 EXP))
  (mov VAL (@ 'output))
  (mov (@ 'backup) esp)
  (popa)
  (mov (@ 'output) VAL)
  (mov INTEGER (@ FREE))
  (mov VAL (@ 4 FREE))
  (mov FREE VAL)
  (jmpl 'advance_free)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(new-additional-primitive "sys-free-memory")
  (insure-no-more-args ARGL)
  (return-integer-constant 'free_memory)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(new-additional-primitive "sys-copy-image")
  (insure-more-args ARGL)
  (mov (@ ARGL) EXP) ;string argument
  (test EXP EXP)
  (jzl 'error_expected_string)
  (mov (@ EXP) TEMP)
  (cmpb TYPE_STRING TEMP)
  (jnel 'error_expected_string)
  (shr LENGTH_SHIFT TEMP)
  (mov TEMP (@ 'str_len))
  (mov (@ 4 ARGL) ARGL)
  (insure-more-args ARGL)
  (call 'get_last_exact_natural)
  (insure-mounted)
  (pusha)
  (push (@ 4 TEMP))
  (mov (@ 4 EXP) ecx)
  (call 'find_file_name)
  (cmp -1 (@ FD_LENGTH ebx))
  (jel 'file_not_found)
  (push CYLINDER_PRESENT)
  (mov ebx (@ 'io_file))
  (mov (@ FD_OFFSET ebx) eax)
  (mov (@ FD_LENGTH ebx) ebx)
  (add eax ebx)
  (call 'insure_floppy_offsets)
  (pop ebx) ;discard parameter
  (mov (@ 'io_file) ebx)
  (mov (@ FD_LENGTH ebx) ecx)
  (mov (@ FD_OFFSET ebx) esi)
  (add 'floppy_buffer esi)
  (shr 2 ecx)
  (inc ecx)
  (pop edi)
  (rep)(movs)
  (popa)
  (jmpl 'return_true)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(new-additional-primitive "sys-boot")
  (cmp 0 ARGL)
  (jnel 'error_too_many_args)
  (exit-with-code 0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(new-additional-primitive "sys-gc")
  (cmp 0 ARGL)
  (jel 'gc_sans_strings)
  (cmp 0 (@ 4 ARGL))
  (jnel 'error_too_many_args)
  (cmp 'false (@ ARGL))
  (jel 'gc_sans_strings)
  (jmpl 'gc_strings)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(new-additional-primitive "vga-rows")
  (insure-no-more-args ARGL)
  (return-integer-constant SCREEN_ROWS)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(new-additional-primitive "vga-columns")
  (insure-no-more-args ARGL)
  (return-integer-constant SCREEN_COLS)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(new-additional-primitive "sys-save")
  (call 'set_hd_cylinder_offset)
  (pusha)
  (mov 'floppy_buffer esi)
  (mov 0 ebp) ;cylinder
(: 'sys_save_cylinder_loop)
  (clear eax)
  (call 'hd_wait_busy)
  (call 'hd_sector)
  (movb HD_COMMAND_WRITE al)
  (outb dx) ;command

  (mov 256 ebx) ;sector count
(: 'sys_save_loop)
  (call 'hd_wait_ready)
  (mov 128 ecx)
  (mov HD_DATA edx)
(: 'sys_save_loop_data)
  (lods)
  (out dx)
  (loop 'sys_save_loop_data)

  (dec ebx) ;next sector
  (jnz 'sys_save_loop)

  (inc ebp) ;next cylinder
  (cmp HD_BLOCK_CYLINDERS ebp)
  (jne 'sys_save_cylinder_loop)

  (popa)
  (ret)

(: 'set_hd_cylinder_offset)
  (test ARGL ARGL)
  (jz 'set_hd_cylinder_offset_end)
  (call 'get_last_exact_natural)
  (mov (@ 4 TEMP) eax)
  (cmp 22 eax)
  (jgl 'error_overflow)
  (mov HD_BLOCK_CYLINDERS TEMP)
  (mul TEMP)
  (mov eax (@ 'hd_cylinder_offset))
  (clear eax)
(: 'set_hd_cylinder_offset_end)
  (ret)

(new-additional-primitive "sys-mount")
  (call 'set_hd_cylinder_offset)
  (pusha)
  (call 'hd_mount)
  (call 'close_files)
  (popa)
  (ret)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(new-additional-primitive "exit")
  (jmpl 'break)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (if SVGA
    (load "svga.scm")
    (load "vga.scm"))
  (: 'startup)
    (call 'close_files)
    (mov 0 eax)
    (mov 'bss_start edi)
    (let ((bss-length (- (lookup 'bss_end) (lookup 'bss_start))))
      (mov (quotient bss-length 4) ecx))
    (rep)(stos)
  (if SVGA (call 'ati0))
  ;(jmp 'start)
