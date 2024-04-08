; Super-8 MOS
; based upon
; BeebEater v0.9.0 - BBC BASIC port for the Ben Eater 6502.
; by Chelsea Wilkinson (chelsea6502)
; https://github.com/chelsea6502/BeebEater

; -- Constants --

; First, let's set some addresses...
BASIC = $8000 ; the entry point for language rom.
START = $F000 ; the entry point for BeebEater.

; We're going to use the same constant names from the original BBC Micro/Master OS (a.k.a 'BBC MOS').
; See https://mdfs.net/Docs/Comp/BBC/AllMem for details.
OSVDU  =$D0
OSKBD1 =$EC
OSKBD2 =OSKBD1+1
OSKBD3 =OSKBD1+2
OSAREG =$EF
OSXREG =$F0
OSYREG =$F1
OSINTA =$FC
OSFAULT=$FD
OSESC  =$FF
TIME   =$0292 ; A 5-byte memory location ($0292-$0296) that counts the number of 'centiseconds' since booting up. We use this for the TIME function.
OSVDUWS=$0300

; For some, we'll set some aliases so it's easier to understand their purpose.
READBUFFER      = OSKBD1  ; this stores the latest ASCII character that was sent into the ACIA
KEYBOARD_FLAGS  = OSKBD2  ; This byte helps us keep track of the state of a key presses on the keyboard. See below.

; Super-8 I/O Registers
UART_OUT_status = $c000
UART_OUT        = $c001
UART_IN_status  = $c002
UART_IN         = $c003

PS2_IN_status   = $c010
PS2_IN          = $c011

TIMER_IRQ       = $c020
TIMER_IRQ_EN    = $c021
TIMER_IRQ_ACK   = $c022

TERMINAL_status = $d000
TERMINAL_OUT    = $d001

; Keyboard flag constants:
RELEASE = %00000001 ; Flag for if a key has just been released.
SHIFT   = %00000010 ; Flag for if we are holding down the shift key.

; BBC MOS "OS Calls". These addresses point to routines that access your hardware.
OSRDCH = $FFE0 ; "OS Read Character" - Transfers the characters read from the 6551 ACIA into Register A (Accumulator)
OSASCI = $FFE3 ; "OS ASCII" - Print an ASCII character stored in Register A (Accumulator)
OSNEWL = $FFE7 ; "OS New Line" - Print the 'CR' ASCII character, followed by the 'LF' character. These two characters make up a new line.
OSWRCH = $FFEE ; "OS Write Character" - Print a byte stored in the Accumulator. This doesn't necessarily have to be an ASCII one.
OSWORD = $FFF1 ; "OS Word" - A group of system calls that have parameters passed in a control block pointed to by the XY registers.
OSBYTE = $FFF4 ; "OS Byte" - A group of system calls that have byte parameters in the registers. This one is much simpler than OSWORD.

; Hardware 6502-specific addresses
NMI = $FFFA ; This is the entry point for when we trigger a 'Non-Maskable Interupt'.
RST = $FFFC ; RESET
IRQ = $FFFE ; Maskable interupts

; -- Entry Points --
.segment "STARTUP"
    .org START ; set the start of BeebEater at $F000.

; -- ROM Constants. Unlike the constants before, these are actually stored in the EEPROM. --

; Define the boot message. By default, you should see this at boot:
;;; BeebEater Computer 16k
;;;
;;; BASIC
;;;
;;; >
bootMessage:
    .byte $84,$01,"Super8 MOS",$85,$1f ; Describes the computer system.
    .byte $0D
    .byte "from BeebEater MOS"
    .byte $0D
    .byte "32K RAM " ; 16k for 16 kilobytes of RAM available. Feel free to change it if you change your RAM capacity.
    .byte $84,$01,"65c02 BBC BASIC",$85,$1f
    .byte $0A ; Give a one-line gap.
    .byte $0D
    .byte $07 ; Send a bell character
    .byte $00 ; End with NUL

; -- Start of Program --

; Set up BeebEater. The reset addresses of $FFFC and $FFFD point to here.
; Let's set any hardware-specific things here.
reset:
    ; Reset the part in memory that stores the time elapsed (in 'centiseconds') since boot.
    STZ TIME
    STZ TIME + 1
    STZ TIME + 2
    STZ TIME + 3
    STZ TIME + 4

    ; Initialise KEYBOARD_FLAGS to 0
    STZ READBUFFER
    STZ KEYBOARD_FLAGS

    ; To print characters, BBC BASIC uses the address stored in $020F-$020E. We need to load those addresses with our OSWRCH routine.
    LDA #>OSWRCHV ; Get the high byte of the write character routine.
    STA $020F ; Store it in $020F.
    LDA #<OSWRCHV ; Get the low byte of the write character routine.
    STA $020E ; Store it in $020E

    ; -- Print the boot message --

    LDY #<bootMessage ; Store the lower 4 bits of the boot message address into the Y register.
    LDA #>bootMessage ; Store the upper 4 bits of the address into the A register.
    STA $FE ; Store the high byte of the source address.
    STZ $FD ; Clear the low byte in memory.
printBootMessageLoop:
    LDA ($FD),Y ; Read the character at $FE-$FD, offset by the value of Y.
    JSR OSASCI ; Send the character to the ACIA to transmit out of the 'Tx' pin.
    INY ; Step to the next character.
    CMP #0 ; If we read a '0', that's when we stop reading the string.
    BNE printBootMessageLoop ;  If A is not 0, read the next character.

    ; -- Enter BBC BASIC --
    CLC ; Clear the carry bit to tell the BBC BASIC we are entering from RESET.
    LDA #$01 ; Load a '1' into the accumulator to tell BBC BASIC we are starting up.
    sta TIMER_IRQ_EN                                        ; enable timer interrupts
    CLI ; Enable interrupts, now that we're done initialising all our memory and peripherals.
    JMP BASIC ; Enter BBC BASIC!
    ; This is the end of the reset sequence.

; -- OS Call Routines --

; OSRDCH: 'OS Read Character'
; This subroutine waits for a character to arrive from the ACIA, then returns it in A. Cy=Esc pressed.
; We use this to receive input from your keyboard to the the caller.
; It also checks if the escape key has been pressed. If it has, it lets the caller know so it needs to leave whatever it's running.
OSRDCHV:
    BBR7 OSESC, readCharacterBuffer ; Is the escape flag set? If not, jump ahead to read the character.
    SEC ; If the escape flag IS set, set the carry bit and exit early without reading the character.
    RTS
readCharacterBuffer:
    ; If there's no escape flag set, let's check the READBUFFER to see if it's full.
    ; We don't read the ACIA directly here. We use the IRQ interrupt handler to read the character and place it into READBUFFER.
    ; A full READBUFFER essentially means that there's a character that's been received by the ACIA that hasn't been read yet.
    LDA READBUFFER ; Read what's in READBUFFER.
    BEQ readCharacterBuffer ; If it's empty, keep reading until it's full.
    STZ READBUFFER ; Is it full? Keep what's in A, and clear the character buffer
    CLC ; Clear the carry bit. BBC BASIC uses the carry bit to track if we're in an 'escape condition' or not.
    RTS ; Return to the main routine.

; OSWRCH: 'OS Write Character'
; System call that displays whatever character is in A. This doesn't necessarily have to be an ASCII character.
; The 'V' in "OSWRCHV" means "Vector". When BBC BASIC jumps to the OSWRCH address, it jumps straight to here.
OSWRCHV:
    php
    pha
wait_term:
    lda TERMINAL_status
    bne wait_term
    pla
    sta TERMINAL_OUT
    plp
    rts

; OSBYTE: 'OS Byte'
; A group of system calls that only involve up to two bytes into the X and Y registers.
; Which system call to do is determined by whatever value is currently in the A register.
; There are much more OSBYTE system calls, but we only need three for the time being:
; On exit: A=preserved, X=any return value, Y=any return value for calls >$7F else preserved, Cy=any return value for calls >$7F
    ; OSBYTE $7E: "Acknowledge Escape" - Handles how BBC BASIC leaves what it's doing when the user presses the escape key.
    ; OSBYTE $84: "Read HIMEM" - This tells the caller the maximum memory address we can use for BASIC programs. $4000 by default
    ; OSBYTE $83: "Read OSHWM" - This tells the caller the minimum memory address we can use for BASIC programs (A.K.A the start of 'PAGE' memory).
        ; $0800 by default, because we need to reserve $0100-$03FF for the MOS, and $400-$7FF for fixed space for the language.
OSBYTEV:
    CMP #$7E ; Is it the 'acknowledge escape' system call?
    BEQ OSBYTE7E ; Jump to the 'acknowledge escape' routine.
    CMP #$84 ; Is it the 'read top of memory' system call?
    BEQ OSBYTE84 ; Put address '$8000' in YX registers.
    CMP #$83 ; Is it the 'read bottom of memory' system call?
    BEQ OSBYTE83 ; Put address '$0800' in YX registers.
    RTS ; Otherwise, return with nothing.

OSBYTE7E: ; Routine that 'acknowledges' the escape key has been pressed.
    LDX #0 ; Reset X, in case X is currently set to #$FF aleady.
    BBR7 OSESC,clearEscape  ; if there's no ESCAPE flag, then just clear the ESCAPE condition.
    LDX #$FF   ; If escape HAS been pressed, set X=$FF to indicate ESCAPE has been acknowledged.
clearEscape:
    CLC    ; Clear the carry bit
    RMB7 OSESC ; Clear bit 7 of the ESCAPE flag.
    RTS

OSBYTE84: ; Routine to return the highest address of free RAM space.
    ; Put address '$8000' in YX registers.
    LDY #$80 ; High byte goes into Y
    LDX #$00  ; Low byte goes into X
    RTS

OSBYTE83: ; Routine to return the lowest address of free RAM space.
    ; Put address '$0800' in YX registers. Anything below $0800 is memory space reserved by BBC MOS.
    LDY #$08 ; High byte goes into Y
    LDX #$00  ; Low byte goes into X
    RTS

; OSWORD: 'OS Word'
; A group of system calls that involves more than just a couple of bytes, but an area in RAM.
; BBC MOS uses 'Control Blocks' to define a sequence of bytes. They're a bit hard to explain, but all you need to know right now is that it's an area in RAM.
; There are much more OSWORD system calls, but we only need three for the time being:
    ; OSWORD 0: "Read line from current input" - This is how BBC MOS lets you input a line of text.
    ; OSWORD 1: "Read system clock" - Get the number of 'centiseconds' since boot. This is called by the TIME function in BASIC.
    ; OSWORD 2: "Write system clock" - Set the number of 'centiseconds' since boot to a certain value. This is called by "TIME=[value]" in BASIC.
OSWORDV:
    PHP  ; Preserve caller's IRQ state.
	CLI	 ; Enable Interrupts

    ; Store A, X, and Y registers in MOS API workspace.
	STA	OSAREG
	STX	OSXREG
	STY	OSYREG

    CMP #$00        ; Is it the 'Read Line' system call?
    BEQ OSWORD0V    ; If yes, start reading input from the user.
    CMP #$01        ; Is it the 'Read Clock' system call?
    BEQ OSWORD1V    ; Jump to it if yes
    CMP #$02        ; Is it the 'Write Clock' system call?
    BEQ OSWORD2V_JUMP    ; Jump to it if yes
    PLP             ; Restore caller's IRQs
    RTS             ; Otherwise, return with no change.

OSWORD2V_JUMP: JMP OSWORD2V ; OSWORD2V is too far away to directly jump, so we have to make a JMP here instead.

OSWORD0V:
    ; An OSWORD 0 control block has a couple of bytes of metadata to help us:
    ; byte 0: address of input buffer for result (low)
    ; byte 1: address of input buffer for result (high)
    ; byte 2: maximum line length
    ; byte 3: minimum acceptable ASCII code
    ; byte 4: maximum acceptable ASCII code
    STZ READBUFFER ; Clear the character buffer.
    LDY #4
osword0setup:
    ; Store max/min ASCII codes, and max line length from zero page memory to main memory
    LDA (OSXREG),Y
    STA $02B1,Y                 ; Copy bytes 2, 3, and 4 to memory for BBC BASIC to process.
    DEY
    CPY #1                      ; Loop until Y = 1.
    BNE osword0setup

    ; Store the input buffer addresses into a temporary buffer
    LDA (OSXREG),Y              ; Get value (high byte) from zero-page. Y is 1 right now.
    STA $E9                     ; Store into temporary buffer (high byte)
    DEY                         ; Set Y from 1 to 0.
    LDA (OSXREG),Y              ; Get value (low byte) from zero-page
    STA $E8                     ; Store into temporary buffer (low byte)

    CLI                         ; Explicitly enable interrupts to allow background keypress processing.
    JMP readInputCharacter      ; Jump ahead to process the next character.
readLineInputBufferFull:
    LDA #$07                     ; Send a 'bell character'
retryWithoutIncrement:
    DEY                         ; Decrement Y. We are essentially 'cancelling out' the next instruction.
retryWithIncrement:
    INY                         ; Decrement Y. Y is currently holding the current position in the input.
outputAndReadAgain:
    JSR OSWRCH                  ; Print the character. Fall through to 'readInputCharacter'
readInputCharacter:
    JSR OSRDCH                  ; Read the next character from ACIA
    BCS Escape                  ; If OSRDCH has set the carry bit, that means the escape key was pressed. Leave early.

    CMP #$08                    ; Is it a backspace? Let's delete the last character.
    BEQ delete
    CMP #$7F                    ; Or, is it a delete? Let's delete the last character.
    BEQ delete

    JMP convertToUppercase      ; Otherwise, move on

delete:
    CPY #0                      ; Are we at the first character?
    BEQ readInputCharacter      ; Then do nothing
    DEY                         ; Otherwise, go back 1.
    JMP outputAndReadAgain      ; Write the delete character

convertToUppercase:
    CMP #'a'                    ; Compare with 'a'
    BCC continueRead                ; If less than 'a', it's not a lowercase letter
    CMP #'z'+1                  ; Compare with 'z'. Add 1 to include 'z' itself.
    BCS continueRead                ; If greater than 'z', it's not a lowercase letter
    AND #%11011111              ; In ASCII, you can clear the 5th bit to convert any lowercase to uppercase.
continueRead:
    STA ($E8),Y                 ; Store character into a buffer that BBC BASIC uses to process it.
    CMP #$0D                    ; Is it the newline character?
    BEQ newLineAndExit          ; ...then finish

    CPY $02B3                   ; check current length against max word length
    BCS readLineInputBufferFull ; send a bell character if full

    CMP $02B4                   ; check minimum ASCII character
    BCC retryWithoutIncrement   ; less than minimum? reject and retry

    CMP $02B5                   ; check maximum ASCII character
    BCS retryWithoutIncrement   ; If it's more than the maximum, reject and retry.
    JMP retryWithIncrement      ; Otherwise, accept and retry.

newLineAndExit:
    JSR OSNEWL
    LDA OSAREG
    LDX OSXREG
    LDY OSYREG
    PLP ; Restore flags
    CLC
    RTS
Escape:
    PLP
    LDA OSESC                   ; Get escape flag
    ROL                         ; If the escape flag is set, also set the carry bit.
    CLI                         ; Re-enable interrupts
    RTS

; OSWORD 1: Read System Timer
; The variable TIME is a 5-byte variable starting at address 'TIME'.
; To read the timer, let's loop through the 5 bytes and store them in the control block
OSWORD1V:
    LDX #0 ; Use this to read the 5 bytes. This will run up from 0 to 4.
    LDY #4 ; Use this to write the 5 bytes. This will run down from 4 to 0.
readTimerLoop:
    LDA TIME,X ; Load the TIME byte, offset by X. X will be either 0, 1, 2, 3, or 4.
    STA (OSXREG),Y ; Store into control block offset by Y. Y will be either 4, 3, 2, 1, or 0.
    INX
    DEY
    BPL readTimerLoop ; Loop while Y is still greater than 0. BPL = "Branch on PLus"
    PLP ; Restore caller's IRQ state
    RTS

; OSWORD 2: Write System Timer
; To write the timer, let's essentially do the opposite of 'Read System Timer'
; Let's loop through the 5 bytes in control block, and store them in the 5-byte variable starting at address 'TIME'.
OSWORD2V:
    LDX #0
    LDY #4
writeTimerLoop:
    LDA (OSXREG),Y ; Same principle as 'readTimerLoop'.
    STA TIME,X
    INX
    DEY
    BPL writeTimerLoop
    PLP ; Restore caller's IRQ state
    RTS


; -- Interrupt Handling --
keyboard_interrupt:
    PHA                         ; Save A
    PHX                         ; Save X
    BBR1 KEYBOARD_FLAGS, handle_pressed_key ; If 'release' flag is not set, skip ahead to read_key
handle_released_key:
    RMB1 KEYBOARD_FLAGS         ; If we ARE releasing a key, let's clear the release flag.
    LDA PS2_IN                  ; read PORTA to clear the interrupt
clear_left_shift:
    CMP #$12                    ; Left shift was pressed?
    BNE clear_right_shift       ; if not, skip ahead
    RMB2 KEYBOARD_FLAGS         ; otherwise, clear the shift flag.
clear_right_shift:
    CMP #$59                    ; Right shift was pressed?
    BNE keyboard_interrupt_exit ; if not, leave.
    RMB2 KEYBOARD_FLAGS         ; otherwise, clear the shift flag.
    JMP keyboard_interrupt_exit ; Finished processing all released keys. Exit.

handle_pressed_key:
    ; Process what's in PORTA, and store it into READBUFFER for reading later.
    LDA PS2_IN
    CMP #$F0                    ; If we've read $F0, that means the keyboard is signalling a key was released.
    BNE set_left_shift          ; If it's not a released key, skip ahead to shift checking
    SMB1 KEYBOARD_FLAGS         ; If it IS a released key, set the release bit in KEYBOARDS_FLAGS.
    JMP keyboard_interrupt_exit
set_left_shift:
    CMP #$12                    ; Left shift was pressed?
    BNE set_right_shift         ; if not, skip ahead
    SMB2 KEYBOARD_FLAGS         ; otherwise, set the shift flag.
    JMP keyboard_interrupt_exit
set_right_shift:
    CMP #$59                    ; Right shift was pressed?
    BNE not_shift               ; if not, skip ahead
    SMB2 KEYBOARD_FLAGS         ; otherwise, set the shift flag.
    JMP keyboard_interrupt_exit

not_shift:
    ; Convert the PS/2 scancode to an ASCII code.
    CMP #$7F
    BCS keyboard_interrupt_exit ; Is it outside the valid scancodes? Leave early.
    TAX                         ; Otherwise, transfer the scancode to X register.
    BBS2 KEYBOARD_FLAGS, shifted_key ; Is the shift flag set? Use the shifted keymap.
    LDA keymap,X                ; Use the 'keymap' to convert the scancode. Scancode is in X, which will convert to an ASCII stored in A.
    JMP push_key                ; Move ahead to store the ASCII for processing.
shifted_key:
    LDA keymap_shifted,X
    ; fall through...
push_key:
    ; Now that we have the ASCII character stored in A, let's store it in READBUFFER for processing later.
    STA READBUFFER              ; Store the ASCII into READBUFFER
    CMP #$1B                    ; Is the character an escape character?
    BNE keyboard_interrupt_exit ; If not, we are done.
    LDA #$FF                    ; If it IS the escape character, we need to signal that an escape state is active.
    STA OSESC                   ; set the 'escape flag' address at $FF to the value #$FF.
keyboard_interrupt_exit:
    PLX                         ; Restore X
    PLA                         ; Restore A
    RTS                         ; Return back to the interrupt handler

    ; Define the mapping from PS/2 Scancode to ASCII
keymap:
    .byte "????????????? `?" ; 00-0F
    .byte "?????q1???zsaw2?" ; 10-1F
    .byte "?cxde43?? vftr5?" ; 20-2F
    .byte "?nbhgy6???mju78?" ; 30-3F
    .byte "?,kio09??./l;p-?" ; 40-4F
    .byte "??'?[=????",$0d,"]?\??" ; 50-5F
    .byte "??????",$08,"??1?47???" ; 60-6F
    .byte "0.2568",$1b,"??+3-*9??" ; 70-7F
keymap_shifted:
    .byte "????????????? ~?" ; 00-0F
    .byte "?????Q!???ZSAW@?" ; 10-1F
    .byte "?CXDE#$?? VFTR%?" ; 20-2F
    .byte "?NBHGY^???MJU&*?" ; 30-3F
    .byte "?<KIO)(??>?L:P_?" ; 40-4F
    .byte "??",$22,"?{+?????}?|??" ; 50-5F
    .byte "?????????1?47???" ; 60-6F
    .byte "0.2568???+3-*9??" ; 70-7F

; Subroutine called after every NMI or IRQ in hardware, or the BRK instruction in software.
interrupt:
    STA OSINTA ; Save A for later.
    PLA ; Get the status register. IRQ/BRK puts it on the stack.
    PHA ; Keep the status register on the stack for later.
    AND #$10 ; Check if it's a BRK or an IRQ.
    BNE BRKV ; If it's BRK, that's an error. Go to the BRK vector.

    BIT TIMER_IRQ_ACK       ; ACKNOWLEDGE THE TIMER INTERRUPT
    lda READBUFFER          ; check if basic has cleared readbuffer
    bne irq_via_tick

    lda PS2_IN_status       ; check PS2 in buffer
    beq irq_uart_check
    jsr keyboard_interrupt  ; deal with PS2 key up/down
    bra irq_via_tick

irq_uart_check:
    lda UART_IN_status      ; if so, check if uart available
    beq irq_via_tick
    lda UART_IN             ; read in character from uart
    sta READBUFFER
    CMP #$1B ; Check if an escape key was pressed
    BNE irq_via_tick ; If it's not an escape key, we've done everything we need. Skip to the end.
    LDA #$FF ; If an escape key was pressed, let's set the escape flag.
    STA OSESC ; set the 'escape flag'.

irq_via_tick: ; If we've ruled out the ACIA & keyboard, then let's assume it was the VIA timer.
    INC TIME + 4 ; Increment the 4th byte, which holds the lowest byte.
    BNE end_irq ; If the byte didn't overflow from FF to 00, then we've done all we need. Skip to the end.
    INC TIME + 3 ; If it DID overflow, then let's carry the 1 to the next register.
    BNE end_irq ; If the byte didn't overflow from FF to 00, then we've done all we need. Skip to the end.
    INC TIME + 2 ; If it DID overflow, then let's carry the 1 to the next register.
    BNE end_irq ; etc etc
    INC TIME + 1
    BNE end_irq
    INC TIME
end_irq:
    LDA OSINTA ; Restore A
    RTI ; "ReTurn from Interrupt" Restore caller's flags, return to caller.

; -- BREAK Handler --

; Handler for interrupts that we know were called by the BRK instruction. This means an error was reported.
; The BBC MOS API defines the structure of an error message. To get the message, we need to store the location of the error message in addresses $FD and $FE.
BRKV:
    PHX                 ; Save X
    TSX                 ; Get the stack pointer value
    LDA $0103,X         ; Get the low byte of the error message location, offset by the stack pointer.
    SEC
    SBC #1               ; Subtract one, as BRK stores BRK+2 to the stack by default, rather than the BRK+1 that we need.
    STA OSFAULT         ; Store the low byte into the fault handler.
    LDA $0104,X         ; Get the high byte of the error message location.
    SBC #0              ; Did subtracting 1 from the low byte cause the carry bit to set? Subtract 1 from the high byte too.
    STA OSFAULT+1       ; Store the high byte into the fault handler.
    STX OSXREG          ; Store the location of the last break for the error handler.
    PLX                 ; Restore X
    LDA OSINTA
    CLI
    JMP ($0202)         ; Jump to BBC BASIC's error handler routine, which takes it from there. Address $0202 points to the routine.

    ; BBC MOS system calls. Code call these by jumping to their place in memory.
    ; Most of them jump to a 'vector' that properly handles the system call.

.segment "MOS_JMP"                                              ; Switch to MOS JUMPTABLE @ Fxxx
    .org $FFB9
    ; Fill the unused system calls from $FFB9 to $FFDD with the 'RTS' instruction, so we can safely return in case they are called.
    ; If you want to use BBC BASIC system calls that use these addresses, you'll have to break this '.fill' apart to make space for it.
    ;.fill 39, $60
    .byte $60, $60, $60, $60, $60, $60, $60, $60, $60, $60, $60, $60, $60
    .byte $60, $60, $60, $60, $60, $60, $60, $60, $60, $60, $60, $60, $60
    .byte $60, $60, $60, $60, $60, $60, $60, $60, $60, $60, $60, $60, $60

    JMP OSRDCHV ; FFE0
    .org OSASCI ; FFE3
    CMP #$0D ; Is it carriage return? Jump to OSNEWL, otherwise fall through to OSWRCH.
    BNE OSWRCH
    ; If it's carriage return, fall through to OSNEWL

    .org OSNEWL ; OSNEWL is essentially OSWRCH, but with a line break (CR+LF)
    LDA #$0A ; Send 'Carriage Return' character.
    JSR OSWRCH
    LDA #$0D ; Send a 'Line Feed' character. CR+LF make up a complete line break.
    ; fall through to OSWRCH

    .org OSWRCH
    JMP OSWRCHV ; At address 'OSWRCH', jump to the 'OSWRCHV' routine (AKA a 'vector').

    .org OSWORD
    JMP OSWORDV

    .org OSBYTE
    JMP OSBYTEV

    .byte $60,$60,$60 ; 'OSCLI' is unused, so we'll write 'RTS' to it.

    ; 6502-specific calls, such as interrupts and resets.
.segment "VECTORS"                                              ; Switch to 6502 CPU VECTORS @ FFFA
    .org NMI
    .word interrupt ; at NMI, go to interrupt handler
    .word reset ; at RESET address, go to reset label
    .word interrupt ; When IRQ goes low or BRK is called, go to the interrupt handler
