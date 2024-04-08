; HELP FROM https://github.com/chelsea6502/BeebEater/blob/main/BeebEater.asm
; TO SET UP A SUFFICIENT BBC MOS TO SUPPORT BBC BASIC

OSAREG =$00
OSXREG =$01
OSYREG =$02
OSINTA =$03

.segment "STARTUP"
ENTRY:  sei                                                     ; mask interrupts
        ldx #$ff                                                ; set stack pointer
        txs

        ldy #<BOOTMSG
        lda #>BOOTMSG
        sta $FE
        sty $FD
        ldy #0
BOOTMSGLOOP:
        lda ($FD),y
        jsr PUTASCII
        iny
        cmp #0
        bne BOOTMSGLOOP

        clc                                                     ; JUMP TO BBC BASIC
        lda #1
        cli
        jmp $0400

PUTASCII:                                                        ; OSWRCH: 'OS Write Character'
                                                                ; System call that displays whatever character is in A. This doesn't necessarily have to be an ASCII character.
                                                                ; The 'V' in "OSWRCHV" means "Vector". When BBC BASIC jumps to the OSWRCH address, it jumps straight to here.
        sta $c001

        pha
wait_term:
        lda $d001
        bne wait_term
        pla
        sta $d000
        rts

GETASCII:                                                        ; OSRDCH: 'OS Read Character'
readCharacterBuffer:
        LDA  $C002
        BEQ  readCharacterBuffer
        LDA  $C003
        RTS

; Subroutine called after every NMI or IRQ in hardware, or the BRK instruction in software.
IRQV:
    STA OSINTA ; Save A for later.
    PLA ; Get the status register. IRQ/BRK puts it on the stack.
    PHA ; Keep the status register on the stack for later.
    AND #$10 ; Check if it's a BRK or an IRQ.
    BNE BRKV ; If it's BRK, that's an error. Go to the BRK vector.
end_irq:
    LDA OSINTA ; Restore A
    RTI ; "ReTurn from Interrupt" Restore caller's flags, return to caller.

; -- BREAK Handler --

; Handler for interrupts that we know were called by the BRK instruction. This means an error was reported.
; The BBC MOS API defines the structure of an error message. To get the message, we need to store the location of the error message in addresses $FD and $FE.
BRKV:
    LDA OSINTA ; Restore A
    RTI ; "ReTurn from Interrupt" Restore caller's flags, return to caller.

NMIV:
    rti

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


BOOTMSG:
    .byte   $0c, "Super8 ULX3s [2 x 65c02]", $0a, $0d, "32k RAM + 4k MOS ROM", $0a, $0d, $0a, $0d, "6502 TEST ROM", $0a, $0d, $07, $00

.segment "MOS_JMP"                                              ; Switch to SUPER8-MOS VECTORS
    jmp   PUTASCII                                            ; $FF00
    jmp   GETASCII                                            ; $FF03

.segment "VECTORS"                                              ; Switch to 6502 CPU VECTORS @ FFFA
        .word   NMIV
        .word   $0400
        .word   IRQV
