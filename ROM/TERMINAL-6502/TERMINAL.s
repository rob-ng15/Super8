CHARACTER  = $8000                                          ; PAGE FOR CHARACTER MAP
FOREGROUND = $8400                                          ; PAGE FOR FOREGROUND MAP
BACKGROUND = $8800                                          ; PAGE FOR BACKGROUND MAP
ATTRIBUTE  = $8C00                                          ; PAGE FOR ATTRIBUTES

terminal_queue_status = $c000                               ; I/O REGISTERS     queue availabel?
terminal_queue_in     = $c001                               ;                   queue entry
terminal_cursor_x     = $c002                               ;                   where cursor will display x
terminal_cursor_y     = $c003                               ;                                             y
terminal_cursor_on    = $c004                               ;                                             on/off

terminal_new_character  = $80                               ; ZP    last received character from queue
terminal_foreground     = $81                               ;       presently requested foreground colour
terminal_background     = $82                               ;                           background colour
terminal_attribute      = $83                               ;                           attribute set

terminal_address_low    = $84                               ;       16-bit indirect address in character, foreground, background, attribute page
terminal_address_high   = $85

copy_dest_low           = $86                               ;       16-bit indirect addresses used in copying memory blocks
copy_dest_high          = $87
copy_src_low            = $88
copy_src_high           = $89
copy_count_low          = $8a                               ;       16-bit counter for number of bytes to copy
copy_count_high         = $8c

    .segment "STARTUP"                                      ; ENTRY POINT INTO TERMINAL ROM
ENTRY:
    ldx #$ff                                                ; initialise the stack pointer
    txs

    jsr TERMINAL_INIT                                       ; call the terminal initialisation routines

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

TERMINAL_POLL_QUEUE:                                        ; wait for a command in the fifo, subroutine so can be used to fetch follow on characters by escape routines
    lda terminal_queue_status                               ; check FIFO availabale
    beq TERMINAL_POLL_QUEUE
    lda terminal_queue_in                                   ; load character
    sta terminal_new_character                              ; save character
    rts


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
    lda #$80                                                ; set address to 8000 and address2 to 8020 ( last cell of character map, and cell above )
    jsr TERMINAL_SCROLL_UP_SET_ADDRS
    jsr MEMCPY_INC

; scroll down foreground map
    lda #$84                                                ; set address to 8200 and address2 to 8220 ( last cell of character map, and cell above )
    jsr TERMINAL_SCROLL_UP_SET_ADDRS
    jsr MEMCPY_INC

; scroll down background map
    lda #$88                                                ; set address to 8400 and address2 to 8420 ( last cell of character map, and cell above )
    jsr TERMINAL_SCROLL_UP_SET_ADDRS
    jsr MEMCPY_INC

; scroll down attribute map
    lda #$8c                                                ; set address to 8600 and address2 to 8620 ( last cell of foreground map, and cell above )
    jsr TERMINAL_SCROLL_UP_SET_ADDRS
    jsr MEMCPY_INC

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

TERMINAL_SCROLL_UP_SET_ADDRS:                               ; using A as the base HIGH address, set addresses and counts for scrolling up
    sta copy_dest_high
    sta copy_src_high
    stz copy_dest_low
    lda #$20
    sta copy_src_low
    lda #$02
    sta copy_count_high
    lda #$e0
    sta copy_count_low
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
    jsr TERMINAL_SCROLL_DOWN_SET_ADDRS
    jsr MEMCPY_DEC

; scroll down foreground map
    lda #$86                                                ; set address to 86ff and address2 to 86df ( last cell of foreground map, and cell above )
    jsr TERMINAL_SCROLL_DOWN_SET_ADDRS
    jsr MEMCPY_DEC

; scroll down background map
    lda #$8a                                                ; set address to 8aff and address2 to 8adf ( last cell of foreground map, and cell above )
    jsr TERMINAL_SCROLL_DOWN_SET_ADDRS
    jsr MEMCPY_DEC

; scroll down attribute map
    lda #$8e                                                ; set address to 8eff and address2 to 8edf ( last cell of foreground map, and cell above )
    jsr TERMINAL_SCROLL_DOWN_SET_ADDRS
    jsr MEMCPY_DEC

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

TERMINAL_SCROLL_DOWN_SET_ADDRS:                             ; using A as the base HIGH address, set addresses and counts for scrolling down
    sta copy_dest_high
    sta copy_src_high
    lda #$ff
    sta copy_dest_low
    lda #$df
    sta copy_src_low
    lda #$02
    sta copy_count_high
    lda #$e0
    sta copy_count_low
    rts


TERMINAL_CLEAR:
    lda #>CHARACTER
    jsr TERMINAL_CLEAR_SET_ADDRS
    lda #$00
    jsr MEMSET

    lda #>FOREGROUND
    jsr TERMINAL_CLEAR_SET_ADDRS
    lda terminal_foreground
    jsr MEMSET

    lda #>BACKGROUND
    jsr TERMINAL_CLEAR_SET_ADDRS
    lda terminal_background
    jsr MEMSET

    lda #>ATTRIBUTE
    jsr TERMINAL_CLEAR_SET_ADDRS
    lda terminal_attribute
    jsr MEMSET

    rts

TERMINAL_CLEAR_SET_ADDRS:                             ; using A as the base HIGH address, set addresses and counts for scrolling down
    sta copy_dest_high
    stz copy_dest_low
    lda #$03
    sta copy_count_high
    stz copy_count_low
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
    .word   move_left                                       ; use standard left routine, handles moving lines and scrolling down
    .word   move_right                                      ; use standard right routine, handles moving lines and scrolling up
    .word   move_down                                       ; use standard down routine, handles scrolling up
    .word   move_up                                         ; use standard up rotuine, handles scrolling down
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
    and #$0f                                                ; extract lower nibble
    asl a                                                   ; double it
    tax                                                     ; copy to x
    jsr TERMINAL_POLL_QUEUE                                 ; get next character, stored in terminal_new_character and A
    clc                                                     ; clear the carry flag to indicate handled
    jmp (ESCAPE_CHARACTER_JUMP_TABLE,x)                     ; jump to the escape character control routine

ESCAPE_CHARACTER_00:                                        ; set x cursor, snap to 0 - 31
    and #$1f
    sta terminal_cursor_x
    rts
ESCAPE_CHARACTER_01:                                        ; set y cursor, snap to 0 - 23
    cmp #$17
    bcc ES_01_SET
    beq ES_01_SET
    lda #$17
ES_01_SET:
    sta terminal_cursor_y
    rts
ESCAPE_CHARACTER_02:                                        ; set foreground colour
    sta terminal_foreground
    rts
ESCAPE_CHARACTER_03:                                        ; set background colour
    sta terminal_background
    rts
ESCAPE_CHARACTER_04:                                        ; set attribute by bitmap
    and #$1f                                                ; mask lower 5 bits
    ora terminal_attribute                                  ; or with present attributes
    sta terminal_attribute                                  ; store new attributes
    rts
ESCAPE_CHARACTER_05:                                        ; clear attribute by bitmap ( #$1f will set to normal text )
    eor #$ff                                                ; invert the bits
    and #$1f                                                ; select lower 5 bits
    and terminal_attribute                                  ; and with present attributes
    sta terminal_attribute                                  ; store new attributes
    rts
ESCAPE_CHARACTER_06:
    rts
ESCAPE_CHARACTER_07:
    rts
ESCAPE_CHARACTER_08:
    rts
ESCAPE_CHARACTER_09:
    rts
ESCAPE_CHARACTER_0A:
    rts
ESCAPE_CHARACTER_0B:
    rts
ESCAPE_CHARACTER_0C:
    rts
ESCAPE_CHARACTER_0D:
    rts
ESCAPE_CHARACTER_0E:
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


MEMCPY_INC:                                                 ; MEMCPY incrementing addresses
    lda (copy_src_low)
    sta (copy_dest_low)
    jsr INC_ADDRS
    jsr DEC_COUNT
    bcc MEMCPY_INC
    rts

MEMCPY_DEC:                                                 ; MEMCPY decrementing addresses
    lda (copy_src_low)
    sta (copy_dest_low)
    jsr DEC_ADDRS
    jsr DEC_COUNT
    bcc MEMCPY_DEC
    rts

MEMSET:                                                     ; MEMSET to value in A
    sta (copy_dest_low)
    jsr INC_ADDRS
    jsr DEC_COUNT
    bcc MEMSET
    rts

DEC_ADDRS:                                                  ; decrement the copy addresses
    pha
    lda copy_dest_low                                       ; decrement dest
    bne lowdne0
    dec copy_dest_high
lowdne0:
    dec copy_dest_low
    lda copy_src_low                                        ; decremnt src
    bne low2dne0
    dec copy_src_high
low2dne0:
    dec copy_src_low
    pla
    rts

INC_ADDRS:                                                  ; increment the copy addresses
    pha
    lda copy_dest_low                                       ; increment dest
    cmp #$ff
    bne lowneff
    inc copy_dest_high
lowneff:
    inc copy_dest_low

    lda copy_src_low                                        ; increment src
    cmp #$ff
    bne low2neff
    inc copy_src_high
low2neff:
    inc copy_src_low
    pla
    rts


DEC_COUNT:                                                  ; DECREMENT THE COPY COUNT
    pha
    lda copy_count_low
    bne clowdne0
    dec copy_count_high
clowdne0:
    dec copy_count_low

    clc                                                     ; CLEAR CARRY, SET IF COUNT == 0
    lda copy_count_high                                     ; CHECK HIGH == 0
    bne dec_count_exit
    lda copy_count_low                                      ;   AND LOW == 0
    bne dec_count_exit
    sec                                                     ; SET CARRY, COPY FINISHED
dec_count_exit:
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
