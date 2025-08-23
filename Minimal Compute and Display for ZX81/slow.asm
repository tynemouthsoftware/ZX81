; Minimal ZX81 / Minstrel 3 display system
; Dave Curran 2025-08-02
;
; 20ms / 50Hz frame
; 64us / 15.625KHz line
; 312 lines
;
; 8 lines VSync
; 56 lines top border
; 192 lines display 32 characters, 24 rows, 8x8 characters
; 56 lines bottom border

; TASM -80 -b -L slow.asm slow.bin

; These locations are based on the ZX81 and can be moved to fit your code
DFILE_PTR       .EQU    $400C   ; display file pointer (ZX81 location)
MARGIN          .EQU    $4028   ; store for calculated value of margin 31d or 55d for NTSC/PAL
DFILE           .EQU    $4100   ; location of DFLIE (moveable)

CHAR_SPACE      .EQU    $00     ; space character
ASTERISK        .EQU    $17     ; * character
CHAR_0          .EQU    $1C     ; 0 character
CHAR_Z          .EQU    $3F     ; Z character
NEW_LINE        .EQU    $76     ; NEWLINE / HALT character


.ORG $0000
; ----------
; START HERE
; ----------
START:
        OUT     ($FD),A         ; Turn off NMI generator
        LD      SP, $7FFF       ; Set the stack-pointer to top of RAM
        JP      INIT            ; System init

;############################### SPACE HERE ####################################
; Space for RST handlers etc.

.ORG $0038
; --------------------------
; MASKABLE INTERRUPT HANDLER
; --------------------------
INTERRUPT:  
        DEC     C               ; Scan line counter
        JP      NZ,SCAN_LINE    ; Still got scanlines to do

NEW_ROW:
        POP     HL              ; Address of the start of the next line
        DEC     B               ; Row counter
        RET     Z               ; Return to display generation code
        SET     3,C             ; Reset scanline counter to 8

WAIT_INT:  
        LD      R,A             ; Refresh register to $DD
        EI                      ; Enable maskable interrupt
        JP      (HL)            ; Jump to display file (will halt and then interrupt)

SCAN_LINE:  
        POP     DE              ; Throw away address of the next line
        RET     Z               ; Should never happen
        JR      WAIT_INT        ; Continue above

;############################### SPACE HERE ####################################
; Space here

.ORG $0066
; ------------------------------
; NON MASKABLE INTERRUPT HANDLER
; ------------------------------
NMI:    
        EX      AF,AF'          ; Switch to alt AF
        INC     A               ; Border line counter
        JP      M,NMI_RET       ; Still counting up?
        JR      Z,END_OF_BORDER ; End of border?

NMI_RET:  
        EX      AF,AF'          ; Restore AF
        RET                     ; Return to user code

; ------------------
; BORDER IS COMPLETE
; ------------------
END_OF_BORDER:  
        EX      AF,AF'          ; Restore AF
        PUSH    AF              ; Save the user code main registers
        PUSH    BC              ; 
        PUSH    DE              ;
        PUSH    HL              ;
        LD      HL,(DFILE_PTR)  ; Get the display file pointer
        SET     7,H             ; Move to the high RAM echo
        HALT                    ; Halt -> Wait to sync timing
        OUT     ($FD),A         ; Stop the NMI generator
        JP      (IX)            ; -> AFTER_TOP_BORDER or AFTER_BOTTOM_BORDER

; ----------------------------
; START THE DISPLAY GENERATION
; ----------------------------
INIT_DISPLAY:  
        PUSH    AF              ; Save registers
        PUSH    BC              ; 
        PUSH    DE              ; 
        PUSH    HL              ; 

START_FRAME: 
        LD      B, 10           ; short delay before VSync
PRE_VSYNC_LOOP:
        DJNZ PRE_VSYNC_LOOP     ; or replace with code matching timing (~129 cycles)

        LD      BC,$FFFE        ; Set full address bus for IORE, port $FE, no row selected
        IN      A,(C)           ; Read the port, start VSYNC
        RLA                     ; Bit 7 -> carry
        RLA                     ; Bit 6 -> carry
        SBC     A,A             ; A = $FF (UK), $00 (US)
        AND     $18             ; A = $18 (UK), $00 (US)
        ADD     A,$1F           ; A = $37 (UK), $1F (US)
        LD      (MARGIN),A      ; Set MARGIN to number of border lines -1, 55 (UK) or 31 (US)

        LD      B, 124          ; delay adjusted to make VSync 512us (~1600 cycles)
VSYNC_LOOP:
        DJNZ VSYNC_LOOP         ; or replace with code matching timing

        OUT     ($FF),A         ; End VSync

        LD      HL,(DFILE_PTR)  ; HL = address of display file
        SET     7,H             ; HL = address of display file echo in high RAM

        CALL    DRAW_BORDER     ; Draw the top border

        ; continues after top border is complete

AFTER_TOP_BORDER:
        LD      A,R             ; For timing?
        LD      BC,$1901        ; line counter
        LD      A,$F5           ; R -> F5 later
        CALL    DRAW_DISPLAY    ; Draw the display

        ; continues after display is complete

AFTER_DISPLAY:      
        DEC     HL              ; point HL to the last NEWLINE/HALT
        CALL    DRAW_BORDER     ; Draw the bottom border

        ; continues after bottom border is complete

AFTER_BOTTOM_BORDER:
        JP      START_FRAME     ; Back to the start

; --------------------------------
; Begin to draw one of the borders
; --------------------------------
DRAW_BORDER:  
        POP     IX              ; IX = AFTER_TOP_BORDER or AFTER_BOTTOM_BORDER
        LD      A, (MARGIN)     ; A = $37 (UK) or $1F (US)
        NEG                     ; A = $C9 (UK) or $E1 (US)
        INC     A               ; A = $CA (UK) or $E2 (US)
        EX      AF,AF'          ; A' = $CA (UK) or $E2 (US)

        OUT     ($FE),A         ; Enable the NMI generator

        POP     HL              ; Restore user registers
        POP     DE              ; 
        POP     BC              ; 
        POP     AF              ; 

        RET                     ; Return to user code during borders, with NMI interruptions

; -------------------------
; Begin to draw the display
; -------------------------
DRAW_DISPLAY:
        LD      R,A             ; R = $F5
        LD      A,$DD           ; R will be set to $DD later
        EI                      ; Enable interrupts
        JP      (HL)            ; Jump to the first halt in the DFILE echo

; -----------
; SYSTEM INIT
; -----------
INIT:                                   
        LD      HL, DFILE       ; Create the display file
        LD      (DFILE_PTR), HL ; set pointer to DFILE
        LD      C, 24           ; row counter
        LD      A, CHAR_SPACE   ; all spaces

DFILE_ROW:
        LD      (HL), NEW_LINE  ; first character is a newline
        INC     HL              ; pointer++
        LD      B, 32           ; 32 characters
DFILE_CHAR:
        LD      (HL), A         ; write character
        INC     HL              ; pointer++   
        DJNZ    DFILE_CHAR      ; go back for more

        DEC     C               ; row done
        JR      NZ, DFILE_ROW   ; do more rows?

        LD      (HL), NEW_LINE  ; one final newline

        LD      A,$1E           ; address of the character set is $1E00
        LD      I,A             ; set I register, used by the display hardware
        IM      1               ; set interrupt mode 1

        CALL    INIT_DISPLAY    ; initialise the display

        ; continues to

; ---------
; USER CODE
; ---------
; user code keeps running, interrupted by the display routines
; sample code fills the display file with a character set then animates it. 

; ---------
; USER INIT
; ---------
USER_INIT:                      ; fill the display with a pattern
        LD      C, 24           ; row counter
        LD      HL, (DFILE_PTR) ; get pointer to DFILE
        LD      A, CHAR_SPACE   ; start with space
NEXT_ROW:
        LD      B, 32           ; 32 characters
        LD      (HL), NEW_LINE  ; start with a newline
        INC     HL              ; pointer++
NEXT_CHAR:
        LD      (HL), A         ; write character
        INC     HL              ; pointer++
        INC     A               ; character++
        DJNZ    NEXT_CHAR       ; back for more

        SUB     31              ; start the next row 1 character later
        DEC     C               ; row done
        JR      NZ, NEXT_ROW    ; do more rows
        
        LD      (HL), NEW_LINE  ; one final newline

; ---------
; USER LOOP
; ---------
USER_LOOP:  
        LD      HL, (DFILE_PTR) ; get a pointer to the start of the display file
        LD      B, 24           ; number of lines to look for
NEXT_C:
        INC     HL              ; pointer++
        LD      A, (HL)         ; get char
        CP      NEW_LINE        ; newline?
        JP      Z, CHECK_EOL    ; yes
        INC     A               ; character++
        AND     CHAR_Z          ; mask characters to 00-3F
        LD      (HL), A         ; update display file      
        JR      NEXT_C          ; back for more

CHECK_EOL:
        DJNZ    NEXT_C          ; completed enough lines?

        JR      USER_LOOP       ; repeat forever


;############################### SPACE HERE ####################################


.ORG $1E00

; ------------------------
; THE 'ZX81 CHARACTER SET'
; ------------------------

; $00 - Character: ' '          CHR$(0)

        .BYTE   %00000000
        .BYTE   %00000000
        .BYTE   %00000000
        .BYTE   %00000000
        .BYTE   %00000000
        .BYTE   %00000000
        .BYTE   %00000000
        .BYTE   %00000000

; $01 - Character: mosaic       CHR$(1)

        .BYTE   %11110000
        .BYTE   %11110000
        .BYTE   %11110000
        .BYTE   %11110000
        .BYTE   %00000000
        .BYTE   %00000000
        .BYTE   %00000000
        .BYTE   %00000000


; $02 - Character: mosaic       CHR$(2)

        .BYTE   %00001111
        .BYTE   %00001111
        .BYTE   %00001111
        .BYTE   %00001111
        .BYTE   %00000000
        .BYTE   %00000000
        .BYTE   %00000000
        .BYTE   %00000000


; $03 - Character: mosaic       CHR$(3)

        .BYTE   %11111111
        .BYTE   %11111111
        .BYTE   %11111111
        .BYTE   %11111111
        .BYTE   %00000000
        .BYTE   %00000000
        .BYTE   %00000000
        .BYTE   %00000000

; $04 - Character: mosaic       CHR$(4)

        .BYTE   %00000000
        .BYTE   %00000000
        .BYTE   %00000000
        .BYTE   %00000000
        .BYTE   %11110000
        .BYTE   %11110000
        .BYTE   %11110000
        .BYTE   %11110000

; $05 - Character: mosaic       CHR$(5)

        .BYTE   %11110000
        .BYTE   %11110000
        .BYTE   %11110000
        .BYTE   %11110000
        .BYTE   %11110000
        .BYTE   %11110000
        .BYTE   %11110000
        .BYTE   %11110000

; $06 - Character: mosaic       CHR$(6)

        .BYTE   %00001111
        .BYTE   %00001111
        .BYTE   %00001111
        .BYTE   %00001111
        .BYTE   %11110000
        .BYTE   %11110000
        .BYTE   %11110000
        .BYTE   %11110000

; $07 - Character: mosaic       CHR$(7)

        .BYTE   %11111111
        .BYTE   %11111111
        .BYTE   %11111111
        .BYTE   %11111111
        .BYTE   %11110000
        .BYTE   %11110000
        .BYTE   %11110000
        .BYTE   %11110000

; $08 - Character: mosaic       CHR$(8)

        .BYTE   %10101010
        .BYTE   %01010101
        .BYTE   %10101010
        .BYTE   %01010101
        .BYTE   %10101010
        .BYTE   %01010101
        .BYTE   %10101010
        .BYTE   %01010101

; $09 - Character: mosaic       CHR$(9)

        .BYTE   %00000000
        .BYTE   %00000000
        .BYTE   %00000000
        .BYTE   %00000000
        .BYTE   %10101010
        .BYTE   %01010101
        .BYTE   %10101010
        .BYTE   %01010101

; $0A - Character: mosaic       CHR$(10)

        .BYTE   %10101010
        .BYTE   %01010101
        .BYTE   %10101010
        .BYTE   %01010101
        .BYTE   %00000000
        .BYTE   %00000000
        .BYTE   %00000000
        .BYTE   %00000000

; $0B - Character: '"'          CHR$(11)

        .BYTE   %00000000
        .BYTE   %00100100
        .BYTE   %00100100
        .BYTE   %00000000
        .BYTE   %00000000
        .BYTE   %00000000
        .BYTE   %00000000
        .BYTE   %00000000

; $0B - Character:  £           CHR$(12)

        .BYTE   %00000000
        .BYTE   %00011100
        .BYTE   %00100010
        .BYTE   %01111000
        .BYTE   %00100000
        .BYTE   %00100000
        .BYTE   %01111110
        .BYTE   %00000000

; $0B - Character: '$'          CHR$(13)

        .BYTE   %00000000
        .BYTE   %00001000
        .BYTE   %00111110
        .BYTE   %00101000
        .BYTE   %00111110
        .BYTE   %00001010
        .BYTE   %00111110
        .BYTE   %00001000

; $0B - Character: ':'          CHR$(14)

        .BYTE   %00000000
        .BYTE   %00000000
        .BYTE   %00000000
        .BYTE   %00010000
        .BYTE   %00000000
        .BYTE   %00000000
        .BYTE   %00010000
        .BYTE   %00000000

; $0B - Character: '?'          CHR$(15)

        .BYTE   %00000000
        .BYTE   %00111100
        .BYTE   %01000010
        .BYTE   %00000100
        .BYTE   %00001000
        .BYTE   %00000000
        .BYTE   %00001000
        .BYTE   %00000000

; $10 - Character: '('          CHR$(16)

        .BYTE   %00000000
        .BYTE   %00000100
        .BYTE   %00001000
        .BYTE   %00001000
        .BYTE   %00001000
        .BYTE   %00001000
        .BYTE   %00000100
        .BYTE   %00000000

; $11 - Character: ')'          CHR$(17)

        .BYTE   %00000000
        .BYTE   %00100000
        .BYTE   %00010000
        .BYTE   %00010000
        .BYTE   %00010000
        .BYTE   %00010000
        .BYTE   %00100000
        .BYTE   %00000000

; $12 - Character: '>'          CHR$(18)

        .BYTE   %00000000
        .BYTE   %00000000
        .BYTE   %00010000
        .BYTE   %00001000
        .BYTE   %00000100
        .BYTE   %00001000
        .BYTE   %00010000
        .BYTE   %00000000

; $13 - Character: '<'          CHR$(19)

        .BYTE   %00000000
        .BYTE   %00000000
        .BYTE   %00000100
        .BYTE   %00001000
        .BYTE   %00010000
        .BYTE   %00001000
        .BYTE   %00000100
        .BYTE   %00000000

; $14 - Character: '='          CHR$(20)

        .BYTE   %00000000
        .BYTE   %00000000
        .BYTE   %00000000
        .BYTE   %00111110
        .BYTE   %00000000
        .BYTE   %00111110
        .BYTE   %00000000
        .BYTE   %00000000

; $15 - Character: '+'          CHR$(21)

        .BYTE   %00000000
        .BYTE   %00000000
        .BYTE   %00001000
        .BYTE   %00001000
        .BYTE   %00111110
        .BYTE   %00001000
        .BYTE   %00001000
        .BYTE   %00000000

; $16 - Character: '-'          CHR$(22)

        .BYTE   %00000000
        .BYTE   %00000000
        .BYTE   %00000000
        .BYTE   %00000000
        .BYTE   %00111110
        .BYTE   %00000000
        .BYTE   %00000000
        .BYTE   %00000000

; $17 - Character: '*'          CHR$(23)

        .BYTE   %00000000
        .BYTE   %00000000
        .BYTE   %00010100
        .BYTE   %00001000
        .BYTE   %00111110
        .BYTE   %00001000
        .BYTE   %00010100
        .BYTE   %00000000

; $18 - Character: '/'          CHR$(24)

        .BYTE   %00000000
        .BYTE   %00000000
        .BYTE   %00000010
        .BYTE   %00000100
        .BYTE   %00001000
        .BYTE   %00010000
        .BYTE   %00100000
        .BYTE   %00000000

; $19 - Character: ';'          CHR$(25)

        .BYTE   %00000000
        .BYTE   %00000000
        .BYTE   %00010000
        .BYTE   %00000000
        .BYTE   %00000000
        .BYTE   %00010000
        .BYTE   %00010000
        .BYTE   %00100000

; $1A - Character: ','          CHR$(26)

        .BYTE   %00000000
        .BYTE   %00000000
        .BYTE   %00000000
        .BYTE   %00000000
        .BYTE   %00000000
        .BYTE   %00001000
        .BYTE   %00001000
        .BYTE   %00010000

; $1B - Character: '"'          CHR$(27)

        .BYTE   %00000000
        .BYTE   %00000000
        .BYTE   %00000000
        .BYTE   %00000000
        .BYTE   %00000000
        .BYTE   %00011000
        .BYTE   %00011000
        .BYTE   %00000000

; $1C - Character: '0'          CHR$(28)

        .BYTE   %00000000
        .BYTE   %00111100
        .BYTE   %01000110
        .BYTE   %01001010
        .BYTE   %01010010
        .BYTE   %01100010
        .BYTE   %00111100
        .BYTE   %00000000

; $1D - Character: '1'          CHR$(29)

        .BYTE   %00000000
        .BYTE   %00011000
        .BYTE   %00101000
        .BYTE   %00001000
        .BYTE   %00001000
        .BYTE   %00001000
        .BYTE   %00111110
        .BYTE   %00000000

; $1E - Character: '2'          CHR$(30)

        .BYTE   %00000000
        .BYTE   %00111100
        .BYTE   %01000010
        .BYTE   %00000010
        .BYTE   %00111100
        .BYTE   %01000000
        .BYTE   %01111110
        .BYTE   %00000000

; $1F - Character: '3'          CHR$(31)

        .BYTE   %00000000
        .BYTE   %00111100
        .BYTE   %01000010
        .BYTE   %00001100
        .BYTE   %00000010
        .BYTE   %01000010
        .BYTE   %00111100
        .BYTE   %00000000

; $20 - Character: '4'          CHR$(32)

        .BYTE   %00000000
        .BYTE   %00001000
        .BYTE   %00011000
        .BYTE   %00101000
        .BYTE   %01001000
        .BYTE   %01111110
        .BYTE   %00001000
        .BYTE   %00000000

; $21 - Character: '5'          CHR$(33)

        .BYTE   %00000000
        .BYTE   %01111110
        .BYTE   %01000000
        .BYTE   %01111100
        .BYTE   %00000010
        .BYTE   %01000010
        .BYTE   %00111100
        .BYTE   %00000000

; $22 - Character: '6'          CHR$(34)

        .BYTE   %00000000
        .BYTE   %00111100
        .BYTE   %01000000
        .BYTE   %01111100
        .BYTE   %01000010
        .BYTE   %01000010
        .BYTE   %00111100
        .BYTE   %00000000

; $23 - Character: '7'          CHR$(35)

        .BYTE   %00000000
        .BYTE   %01111110
        .BYTE   %00000010
        .BYTE   %00000100
        .BYTE   %00001000
        .BYTE   %00010000
        .BYTE   %00010000
        .BYTE   %00000000

; $24 - Character: '8'          CHR$(36)

        .BYTE   %00000000
        .BYTE   %00111100
        .BYTE   %01000010
        .BYTE   %00111100
        .BYTE   %01000010
        .BYTE   %01000010
        .BYTE   %00111100
        .BYTE   %00000000

; $25 - Character: '9'          CHR$(37)

        .BYTE   %00000000
        .BYTE   %00111100
        .BYTE   %01000010
        .BYTE   %01000010
        .BYTE   %00111110
        .BYTE   %00000010
        .BYTE   %00111100
        .BYTE   %00000000

; $26 - Character: 'A'          CHR$(38)

        .BYTE   %00000000
        .BYTE   %00111100
        .BYTE   %01000010
        .BYTE   %01000010
        .BYTE   %01111110
        .BYTE   %01000010
        .BYTE   %01000010
        .BYTE   %00000000

; $27 - Character: 'B'          CHR$(39)

        .BYTE   %00000000
        .BYTE   %01111100
        .BYTE   %01000010
        .BYTE   %01111100
        .BYTE   %01000010
        .BYTE   %01000010
        .BYTE   %01111100
        .BYTE   %00000000

; $28 - Character: 'C'          CHR$(40)

        .BYTE   %00000000
        .BYTE   %00111100
        .BYTE   %01000010
        .BYTE   %01000000
        .BYTE   %01000000
        .BYTE   %01000010
        .BYTE   %00111100
        .BYTE   %00000000

; $29 - Character: 'D'          CHR$(41)

        .BYTE   %00000000
        .BYTE   %01111000
        .BYTE   %01000100
        .BYTE   %01000010
        .BYTE   %01000010
        .BYTE   %01000100
        .BYTE   %01111000
        .BYTE   %00000000

; $2A - Character: 'E'          CHR$(42)

        .BYTE   %00000000
        .BYTE   %01111110
        .BYTE   %01000000
        .BYTE   %01111100
        .BYTE   %01000000
        .BYTE   %01000000
        .BYTE   %01111110
        .BYTE   %00000000

; $2B - Character: 'F'          CHR$(43)

        .BYTE   %00000000
        .BYTE   %01111110
        .BYTE   %01000000
        .BYTE   %01111100
        .BYTE   %01000000
        .BYTE   %01000000
        .BYTE   %01000000
        .BYTE   %00000000

; $2C - Character: 'G'          CHR$(44)

        .BYTE   %00000000
        .BYTE   %00111100
        .BYTE   %01000010
        .BYTE   %01000000
        .BYTE   %01001110
        .BYTE   %01000010
        .BYTE   %00111100
        .BYTE   %00000000

; $2D - Character: 'H'          CHR$(45)

        .BYTE   %00000000
        .BYTE   %01000010
        .BYTE   %01000010
        .BYTE   %01111110
        .BYTE   %01000010
        .BYTE   %01000010
        .BYTE   %01000010
        .BYTE   %00000000

; $2E - Character: 'I'          CHR$(46)

        .BYTE   %00000000
        .BYTE   %00111110
        .BYTE   %00001000
        .BYTE   %00001000
        .BYTE   %00001000
        .BYTE   %00001000
        .BYTE   %00111110
        .BYTE   %00000000

; $2F - Character: 'J'          CHR$(47)

        .BYTE   %00000000
        .BYTE   %00000010
        .BYTE   %00000010
        .BYTE   %00000010
        .BYTE   %01000010
        .BYTE   %01000010
        .BYTE   %00111100
        .BYTE   %00000000

; $30 - Character: 'K'          CHR$(48)

        .BYTE   %00000000
        .BYTE   %01000100
        .BYTE   %01001000
        .BYTE   %01110000
        .BYTE   %01001000
        .BYTE   %01000100
        .BYTE   %01000010
        .BYTE   %00000000

; $31 - Character: 'L'          CHR$(49)

        .BYTE   %00000000
        .BYTE   %01000000
        .BYTE   %01000000
        .BYTE   %01000000
        .BYTE   %01000000
        .BYTE   %01000000
        .BYTE   %01111110
        .BYTE   %00000000

; $32 - Character: 'M'          CHR$(50)

        .BYTE   %00000000
        .BYTE   %01000010
        .BYTE   %01100110
        .BYTE   %01011010
        .BYTE   %01000010
        .BYTE   %01000010
        .BYTE   %01000010
        .BYTE   %00000000

; $33 - Character: 'N'          CHR$(51)

        .BYTE   %00000000
        .BYTE   %01000010
        .BYTE   %01100010
        .BYTE   %01010010
        .BYTE   %01001010
        .BYTE   %01000110
        .BYTE   %01000010
        .BYTE   %00000000

; $34 - Character: 'O'          CHR$(52)

        .BYTE   %00000000
        .BYTE   %00111100
        .BYTE   %01000010
        .BYTE   %01000010
        .BYTE   %01000010
        .BYTE   %01000010
        .BYTE   %00111100
        .BYTE   %00000000

; $35 - Character: 'P'          CHR$(53)

        .BYTE   %00000000
        .BYTE   %01111100
        .BYTE   %01000010
        .BYTE   %01000010
        .BYTE   %01111100
        .BYTE   %01000000
        .BYTE   %01000000
        .BYTE   %00000000

; $36 - Character: 'Q'          CHR$(54)

        .BYTE   %00000000
        .BYTE   %00111100
        .BYTE   %01000010
        .BYTE   %01000010
        .BYTE   %01010010
        .BYTE   %01001010
        .BYTE   %00111100
        .BYTE   %00000000

; $37 - Character: 'R'          CHR$(55)

        .BYTE   %00000000
        .BYTE   %01111100
        .BYTE   %01000010
        .BYTE   %01000010
        .BYTE   %01111100
        .BYTE   %01000100
        .BYTE   %01000010
        .BYTE   %00000000

; $38 - Character: 'S'          CHR$(56)

        .BYTE   %00000000
        .BYTE   %00111100
        .BYTE   %01000000
        .BYTE   %00111100
        .BYTE   %00000010
        .BYTE   %01000010
        .BYTE   %00111100
        .BYTE   %00000000

; $39 - Character: 'T'          CHR$(57)

        .BYTE   %00000000
        .BYTE   %11111110
        .BYTE   %00010000
        .BYTE   %00010000
        .BYTE   %00010000
        .BYTE   %00010000
        .BYTE   %00010000
        .BYTE   %00000000

; $3A - Character: 'U'          CHR$(58)

        .BYTE   %00000000
        .BYTE   %01000010
        .BYTE   %01000010
        .BYTE   %01000010
        .BYTE   %01000010
        .BYTE   %01000010
        .BYTE   %00111100
        .BYTE   %00000000

; $3B - Character: 'V'          CHR$(59)

        .BYTE   %00000000
        .BYTE   %01000010
        .BYTE   %01000010
        .BYTE   %01000010
        .BYTE   %01000010
        .BYTE   %00100100
        .BYTE   %00011000
        .BYTE   %00000000

; $3C - Character: 'W'          CHR$(60)

        .BYTE   %00000000
        .BYTE   %01000010
        .BYTE   %01000010
        .BYTE   %01000010
        .BYTE   %01000010
        .BYTE   %01011010
        .BYTE   %00100100
        .BYTE   %00000000

; $3D - Character: 'X'          CHR$(61)

        .BYTE   %00000000
        .BYTE   %01000010
        .BYTE   %00100100
        .BYTE   %00011000
        .BYTE   %00011000
        .BYTE   %00100100
        .BYTE   %01000010
        .BYTE   %00000000

; $3E - Character: 'Y'          CHR$(62)

        .BYTE   %00000000
        .BYTE   %10000010
        .BYTE   %01000100
        .BYTE   %00101000
        .BYTE   %00010000
        .BYTE   %00010000
        .BYTE   %00010000
        .BYTE   %00000000

; $3F - Character: 'Z'          CHR$(63)

        .BYTE   %00000000
        .BYTE   %01111110
        .BYTE   %00000100
        .BYTE   %00001000
        .BYTE   %00010000
        .BYTE   %00100000
        .BYTE   %01111110
        .BYTE   %00000000

.END
