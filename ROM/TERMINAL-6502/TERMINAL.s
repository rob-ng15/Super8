CHARACTER = $8000                                           ; PAGE FOR CHARACTER MAP
FOREGROUND = $8400                                          ; PAGE FOR FOREGROUND MAP
BACKGROUND = $8800                                          ; PAGE FOR BACKGROUND MAP
ATTRIBUTE = $8C00                                           ; PAGE FOR ATTRIBUTES

terminal_queue_status = $c000
terminal_queue_in = $c001
terminal_cursor_x = $c002
terminal_cursor_y = $c003
terminal_cursor_on = $c004
terminal_foreground = $80
terminal_background = $81
terminal_attribute = $82
terminal_new_character = $83

terminal_address_low = $84
terminal_address_high = terminal_address_low + 1

scroll_address_low = $85
scroll_address_high = $86
scroll_address2_low = $87
scroll_address2_high = $88

.segment "STARTUP"

ENTRY:
    sei                                                     ; switch off interrupts
    ldx #$ff                                                ; initialise the stack pointer
    txs

    jsr TERMINAL_INIT                                       ; call the terminal initialisation routines
    bra poll                                                ; jump to the polling loop


TERMINAL_POLL_QUEUE:                                        ; wait for a command in the fifo, subroutine so can be used to fetch follow on characters by escape routines
    lda terminal_queue_status                               ; check FIFO availabale
    beq TERMINAL_POLL_QUEUE
    lda terminal_queue_in                                   ; load character
    sta terminal_new_character                              ; save character
    rts


poll:                                                       ; main terminal loop
    jsr TERMINAL_POLL_QUEUE                                 ; wait for command in the FIFO

    bit #$80                                                ; check if the top bit is set, escape command
    bne TERMINAL_ESCAPE
    cmp #$20                                                ; check if a control character
    bcc TERMINAL_CONTROL
    jsr TERMINAL_OUTPUT                                     ; ascii output character
    bra poll

TERMINAL_CONTROL:                                           ; pass to the terminal control character handler
    jsr TERMINAL_CONTROL_CHARACTER
    bra poll


TERMINAL_ESCAPE:
    jsr TERMINAL_ESCAPE_CHARACTER                           ; pass to the terminal escape command handler
    bcs terminal_escape_symbol                              ; if carry set, output the symbol character
    bra poll
terminal_escape_symbol:
    jsr TERMINAL_OUTPUT
    bra poll


TERMINAL_OUTPUT:                                            ; output the character ( stored in terminal_new_character )
    stz terminal_address_high                               ; generate the address in the character memory
    lda terminal_cursor_y                                   ;   terminal_cursor_y * 5 + terminal_cursor_x + $8000
    sta terminal_address_low
    jsr X32_ADDR

    lda terminal_cursor_x                                   ;   add in the terminal_cursor_x ( 0 - 31 )
    ora terminal_address_low
    sta terminal_address_low
    smb7 terminal_address_high                              ;   add $8000 to the resulting address

    lda terminal_new_character                              ; retrieve the character to display
    sta (terminal_address_low)                              ; store character into calculated cell
    jsr ADDR_NEXT_PAGE                                      ; move to foreground
    lda terminal_foreground
    sta (terminal_address_low)                              ; store foreground into calculated cell
    jsr ADDR_NEXT_PAGE                                      ; move to background
    lda terminal_background
    sta (terminal_address_low)                              ; store background into calculated cell
    jsr ADDR_NEXT_PAGE                                      ; move to attribute
    lda terminal_attribute
    sta (terminal_address_low)                              ; store attribute into calculated cell

    jsr move_right                                          ; move the cursor
    rts                                                     ; return back to poll loop


move_right:                                                 ; move the cursor right
    lda terminal_cursor_x
    cmp #31                                                 ; at end of line?
    beq end_line
    inc terminal_cursor_x
    rts
end_line:
    stz terminal_cursor_x


move_down:                                                  ; move the cursor down
    lda terminal_cursor_y
    cmp #23                                                 ; at last line?
    beq TERMINAL_SCROLL                                     ;   call scroll up
    inc terminal_cursor_y
    rts


TERMINAL_SCROLL:                                            ; scroll the terminal up
    ldx #0
sl:
    lda CHARACTER + 32,x
    sta CHARACTER,x
    lda CHARACTER + $100 + 32,x
    sta CHARACTER + $100,x
    lda CHARACTER + $200 + 32,x
    sta CHARACTER + $200,x

    lda FOREGROUND + 32,x
    sta FOREGROUND,x
    lda FOREGROUND + $100 + 32,x
    sta FOREGROUND + $100,x
    lda FOREGROUND + $200 + 32,x
    sta FOREGROUND + $200,x

    lda BACKGROUND + 32,x
    sta BACKGROUND,x
    lda BACKGROUND + $100 + 32,x
    sta BACKGROUND + $100,x
    lda BACKGROUND + $200 + 32,x
    sta BACKGROUND + $200,x

    lda ATTRIBUTE + 32,x
    sta ATTRIBUTE,x
    lda ATTRIBUTE + $100 + 32,x
    sta ATTRIBUTE + $100,x
    lda ATTRIBUTE + $200 + 32,x
    sta ATTRIBUTE + $200,x

    inx
    bne sl

    ldx #0                                                  ; set bottom row of screen to 0, foreground, background, attribute
ssl:
    stz CHARACTER+$2e0,x
    lda terminal_foreground
    sta FOREGROUND+$2e0,x
    lda terminal_background
    sta BACKGROUND+$2e0,x
    lda terminal_attribute
    sta ATTRIBUTE+$2e0,x

    inx
    cpx #$20
    bne ssl
    rts


move_left:                                                  ; move the cursor left
    lda terminal_cursor_x
    beq at_left                                             ; at left?
    dec terminal_cursor_x
    rts
at_left:
    lda #$1f
    sta terminal_cursor_x
    lda terminal_cursor_y
    beq move_up                                             ; at top of screen
    dec terminal_cursor_y
    rts


move_up:                                                    ; move the cursor up
    lda terminal_cursor_y                                   ; at the top of the screen
    beq TERMINAL_SCROLL_DOWN                                ;   call scroll down
    dec terminal_cursor_y
    rts


TERMINAL_SCROLL_DOWN:
    lda #$82                                                ; set address to 82ff and address2 to 82df ( last cell of character map, and cell above )
    sta scroll_address_high
    sta scroll_address2_high
    lda #$ff
    sta scroll_address_low
    lda #$df
    sta scroll_address2_low
csdl:
    lda (scroll_address2_low)                               ; scroll down character map
    sta (scroll_address_low)
    jsr DEC_ADDRS
    lda scroll_address2_high                                ; loop if not copied top line
    cmp #$7f
    bne csdl

; scroll down foreground map
    lda #$86                                                ; set address to 86ff and address2 to 86df ( last cell of foreground map, and cell above )
    sta scroll_address_high
    sta scroll_address2_high
    lda #$ff
    sta scroll_address_low
    lda #$df
    sta scroll_address2_low
fsdl:
    lda (scroll_address2_low)                               ; scroll down character map
    sta (scroll_address_low)
    jsr DEC_ADDRS
    lda scroll_address2_high                                ; loop if not copied top line
    cmp #$83
    bne fsdl

; scroll down background map
    lda #$8a                                                ; set address to 8aff and address2 to 8adf ( last cell of foreground map, and cell above )
    sta scroll_address_high
    sta scroll_address2_high
    lda #$ff
    sta scroll_address_low
    lda #$df
    sta scroll_address2_low
dsdl:
    lda (scroll_address2_low)                               ; scroll down character map
    sta (scroll_address_low)
    jsr DEC_ADDRS
    lda scroll_address2_high                                ; loop if not copied top line
    cmp #$87
    bne dsdl


; scroll down attribute map
    lda #$8e                                                ; set address to 8eff and address2 to 8edf ( last cell of foreground map, and cell above )
    sta scroll_address_high
    sta scroll_address2_high
    lda #$ff
    sta scroll_address_low
    lda #$df
    sta scroll_address2_low
asdl:
    lda (scroll_address2_low)                               ; scroll down character map
    sta (scroll_address_low)
    jsr DEC_ADDRS
    lda scroll_address2_high                                ; loop if not copied top line
    cmp #$8b
    bne asdl


    ldx #$0
ssdl:
    stz CHARACTER,x                                         ; set top line to 0, foreground, background, attribute
    lda terminal_foreground
    sta FOREGROUND,x
    lda terminal_background
    sta BACKGROUND,x
    lda terminal_attribute
    sta ATTRIBUTE,x

    inx
    cpx #$20
    bne ssdl

    rts


TERMINAL_CLEAR:
    ldx #0
tcl:
    stz  CHARACTER,x
    stz  CHARACTER+$100,x
    stz  CHARACTER+$200,x

    lda terminal_foreground
    sta FOREGROUND,x
    sta FOREGROUND+$100,x
    sta FOREGROUND+$200,x

    lda terminal_background
    sta BACKGROUND,x
    sta BACKGROUND+$100,x
    sta BACKGROUND+$200,x

    lda terminal_attribute
    sta ATTRIBUTE,x
    sta ATTRIBUTE+$100,x
    sta ATTRIBUTE+$200,x

    inx
    bne tcl
    rts


TERMINAL_INIT:
    stz terminal_cursor_x
    lda #$03
    sta terminal_cursor_y
    stz terminal_cursor_on
    lda #$02
    sta terminal_foreground
    lda #$f6
    sta terminal_background
    stz terminal_attribute

    jsr TERMINAL_CLEAR
    jsr TERMINAL_BANNER

    lda #$01
    sta terminal_cursor_on
    rts


TERMINAL_BANNER:
    ldx #$00
TB_LOOP:
    lda SB_TEXT,x
    sta CHARACTER,x
    lda SB_FORE,x
    sta FOREGROUND,x
    lda SB_BACK,x
    sta BACKGROUND,x
    lda SB_ATTR,x
    sta ATTRIBUTE,x
    inx
    cpx #$40
    bne TB_LOOP
    rts


STARTUP_BANNER:
SB_TEXT:
    .byte "SSUUPPEERR--88         [2x65c02]"
    .byte "SSUUPPEERR--88                  "
SB_FORE:
    .byte $c1,$c1,$e9,$e9,$f9,$f9,$38,$38,$06,$06,$4d,$4d,$87,$87,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    .byte $c1,$c1,$e9,$e9,$f9,$f9,$38,$38,$06,$06,$4d,$4d,$87,$87,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00

SB_BACK:
    .byte $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
    .byte $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff

SB_ATTR:
    .byte $07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01
    .byte $07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01


TERMINAL_CONTROL_CHARACTER:
    asl a
    tax
    jmp (CONTROL_CHARACTER_JUMP_TABLE,x)

UNHANDLED_CONTROL_CHARACTER:
    rts
CONTROL_CHARACTER_08:
    lda terminal_cursor_x
    beq backspace_at_0
    dec a
    sta terminal_cursor_x
backspace_at_0:
    rts
CONTROL_CHARACTER_0D:
    stz terminal_cursor_x
    rts

CONTROL_CHARACTER_JUMP_TABLE:
    .word   UNHANDLED_CONTROL_CHARACTER
    .word   UNHANDLED_CONTROL_CHARACTER
    .word   UNHANDLED_CONTROL_CHARACTER
    .word   UNHANDLED_CONTROL_CHARACTER
    .word   UNHANDLED_CONTROL_CHARACTER
    .word   UNHANDLED_CONTROL_CHARACTER
    .word   UNHANDLED_CONTROL_CHARACTER
    .word   UNHANDLED_CONTROL_CHARACTER
    .word   move_left
    .word   move_right
    .word   move_down
    .word   move_up
    .word   TERMINAL_CLEAR
    .word   CONTROL_CHARACTER_0D
    .word   UNHANDLED_CONTROL_CHARACTER
    .word   UNHANDLED_CONTROL_CHARACTER
    .word   UNHANDLED_CONTROL_CHARACTER
    .word   UNHANDLED_CONTROL_CHARACTER
    .word   UNHANDLED_CONTROL_CHARACTER
    .word   UNHANDLED_CONTROL_CHARACTER
    .word   UNHANDLED_CONTROL_CHARACTER
    .word   UNHANDLED_CONTROL_CHARACTER
    .word   UNHANDLED_CONTROL_CHARACTER
    .word   UNHANDLED_CONTROL_CHARACTER
    .word   UNHANDLED_CONTROL_CHARACTER
    .word   UNHANDLED_CONTROL_CHARACTER
    .word   UNHANDLED_CONTROL_CHARACTER
    .word   UNHANDLED_CONTROL_CHARACTER
    .word   UNHANDLED_CONTROL_CHARACTER
    .word   UNHANDLED_CONTROL_CHARACTER
    .word   UNHANDLED_CONTROL_CHARACTER
    .word   UNHANDLED_CONTROL_CHARACTER


TERMINAL_ESCAPE_CHARACTER:
    and #$0f
    asl a
    tax
    jsr TERMINAL_POLL_QUEUE
    jmp (ESCAPE_CHARACTER_JUMP_TABLE,x)

ESCAPE_CHARACTER_00:                                        ; set x cursor, snap to 0 - 31
    and #$1f
    sta terminal_cursor_x
    clc
    rts
ESCAPE_CHARACTER_01:                                        ; set y cursor, snap to 0 - 23
    cmp #$17
    bcc ES_01_SET
    beq ES_01_SET
    lda #$17
ES_01_SET:
    sta terminal_cursor_y
    clc
    rts
ESCAPE_CHARACTER_02:                                        ; set foreground colour
    sta terminal_foreground
    clc
    rts
ESCAPE_CHARACTER_03:                                        ; set background colour
    sta terminal_background
    clc
    rts
ESCAPE_CHARACTER_04:                                        ; set attribute by bitmap
    and #$1f
    ora terminal_attribute
    sta terminal_attribute
    clc
    rts
ESCAPE_CHARACTER_05:                                        ; clear attribute by bitmap ( #$1f will set to normal text )
    eor #$ff
    and #$1f
    and terminal_attribute
    sta terminal_attribute
    clc
    rts
ESCAPE_CHARACTER_06:
    clc
    rts
ESCAPE_CHARACTER_07:
    clc
    rts
ESCAPE_CHARACTER_08:
    clc
    rts
ESCAPE_CHARACTER_09:
    clc
    rts
ESCAPE_CHARACTER_0A:
    clc
    rts
ESCAPE_CHARACTER_0B:
    clc
    rts
ESCAPE_CHARACTER_0C:
    clc
    rts
ESCAPE_CHARACTER_0D:
    clc
    rts
ESCAPE_CHARACTER_0E:
    clc
    rts
ESCAPE_CHARACTER_0F:
    sec                                                     ; set carry flag so as to output character symbol
    rts

ESCAPE_CHARACTER_JUMP_TABLE:
    .word   ESCAPE_CHARACTER_00                             ; set x coordinate
    .word   ESCAPE_CHARACTER_01                             ; set y coordinate
    .word   ESCAPE_CHARACTER_02                             ; set foreground
    .word   ESCAPE_CHARACTER_03                             ; set background
    .word   ESCAPE_CHARACTER_04                             ; set attribute ( by bits )
    .word   ESCAPE_CHARACTER_05                             ; clear attribute ( by bits )
    .word   ESCAPE_CHARACTER_06
    .word   ESCAPE_CHARACTER_07
    .word   ESCAPE_CHARACTER_08
    .word   ESCAPE_CHARACTER_09
    .word   ESCAPE_CHARACTER_0A
    .word   ESCAPE_CHARACTER_0B
    .word   ESCAPE_CHARACTER_0C
    .word   ESCAPE_CHARACTER_0D
    .word   ESCAPE_CHARACTER_0E
    .word   ESCAPE_CHARACTER_0F                             ; output next character, allows output of symbols


; LIBRARY ROUTINES

X32_ADDR:                                                   ; multiply the cursor address by 5
    phx
    ldx #5
X32_loop:
    asl terminal_address_low                                ;   16 bit ASL
    rol terminal_address_high
    dex
    bne X32_loop                                            ;   loop 5 times, x2 x4 x8 x16 x32
    plx
    rts

ADDR_NEXT_PAGE:                                             ; add $400 to move from character -> foreground -> background -> attribute pages
    php
    pha
    clc
    lda terminal_address_high
    adc #$04
    sta terminal_address_high
    pla
    plp
    rts


DEC_ADDRS:                                                  ; decrement the scrolling addresses
    pha
    lda scroll_address_low                                  ; decrement address
    bne lowdne0
    dec scroll_address_high
lowdne0:
    dec scroll_address_low

    lda scroll_address2_low                                 ; decremnt address2
    bne low2dne0
    dec scroll_address2_high
low2dne0:
    dec scroll_address2_low
    pla
    rts


NMI:
    rti

IRQ:
    rti

RETJMP:
    rts

.segment "VECTORS"                                              ; Switch to 6502 CPU VECTORS @ FFFA
        .word   NMI
        .word   ENTRY
        .word   IRQ
