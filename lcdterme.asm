**************************************************************************
*                                                                        *
* LCDTERM.ASM                                                            *
*                                                                        *
* LCD terminal using a C68HC808QY4 and a HD44780 compatible LCD display  *
* PortA Bit 0 is used for RS232 receiving with the GebByte Rom routine   *
* Because the LCD is a little slow, Getbyte is combined wit a Keyboard   *
* interrupt. KBInt is active on PTA0 and is activated on falling edge    *
* The ISR calls GetByte and stores the received data                     *
* The MCU uses the user monitor, so the alternative vectoradresses are   *
* used.                                                                  *
*                                                                        *
*                                                                        *
*                                                                        *
* Everything really needed is programmed                                 *
* The possibility to upload user characters would be nice                *
* Characters above 127 are not blocked, but it is not a standard         *
* characterset                                                           *
* Control characters like Cr Lf or Backspace are not impemented          *
* The intention was to use the terminalwith another mcu and not with a   *
* Keyboard                                                               *
**************************************************************************


**************************************************************************
* VT100 Commands                                                         *
*                                                                        *
* Reset Device          <ESC>c                       ok                  *
* Cursor Home 	        <ESC>[H                      ok                  *
* Cursor Up	        <ESC>[{COUNT}A                                   *
* Cursor Down		<ESC>[{COUNT}B                                   *
* Cursor Forward        <ESC>[{COUNT}C                                   *
* Force Cursor Position	<ESC>[{ROW};{COLUMN}f        ok                  *
* Scroll Up		<ESC>M                       ok                  *
* Erase End of Line	<ESC>[K                      ok                  *
* Erase Line		<ESC>[2K                     ok                  *
* Erase Screen		<ESC>[2J                     ok                  *
*                                                                        *
**************************************************************************



	INCLUDE	'qtqy_registers.inc'

*** SYSTEM DEFINITIONS AND EQUATES ***************************************

**** Equates to setup alternate vectors **********************************
* actual vectors will pass control to these locations.                   *
* user code would include jump instructions at these locations that      *
* jump to the user's interrupt service routines, for example             *
*           org    AltADC                                                *
*           jmp    ADCisr                                                *
*           jmp    KBDisr                                                *
*                                                                        *
**************************************************************************
AltADC:      equ    $FDEB               ;Alternate ADC interrupt vector
AltKBD:      equ    $FDEE               ;    '     KBD wakeup '    '
AltTOF:      equ    $FDF1               ;    '     TOF        '    '
AltTCH1:     equ    $FDF4               ;    '     Timer Ch.1 '    '
AltTCH0:     equ    $FDF7               ;    '     Timer Ch.0 '    '
AltIRQ:      equ    $FDFA               ;    '     IRQ        '    '
AltRESET:    equ    $FDFD               ;    '     RESET      '    '


TRIMLOC:     equ    $FFC0               ; nonvolatile trim value (flash)
InitConfig1: equ    %01001001           ; Config Register1
GetByte:     equ    $2D6b               ; ROM routine Getbyte auf PTA0

*** Application Specific Definitions ************************************

LCD_CTRL     equ    $00                 ; PORTA
LCD_DATA     equ    $01                 ; PORTB
E            equ    4T                  ; PORTA, bit 4
RS           equ    5T                  ; PORTA, bit 5
ESC          equ    $01B                ; Escape Character

*** SCI Definitionen ****************************************************

PTA:         equ    $0000
PTA0:        equ    0                   ; bit #0 for PTA0

*** LCD Kommandos ********************************************************

CLRDISP      equ    %00000001           ; Clear Display
CURHOME      equ    %00000010           ; Cursor Home
ENTMODI      equ    %00000111           ; Character Entry Mode with Increment
ENTMODD      equ    %00000101           ; Character Entry Mode with Decrement
ENTMODO      equ    %00000100           ; Character Entry Mode Off
DSPON        equ    %00001100           ; Diplay On
DSPOFF       equ    %00001000           ; Display Off
CURON        equ    %00001111           ; Cursor Underline blinking on
CUROFF       equ    %00001100           ; Cursor off
SETRAMADR    equ    %01000000           ; Set Ram Adress
SETDSPADR    equ    %10000000           ; Set Display Adress
FUNCSET      equ    %00111000           ; Function Set 8 Bit 5*7 Pixel

*** LCD Adressen *********************************************************

row1adr      equ    $80                 ; starting adress row 1
row2adr      equ    $C0                 ; starting adress row 2

*** Memory Definitions****************************************************

ROM          equ    $EE00               ;start of Flash mem
RAM          equ    $80                 ;start of RAM mem

MSG_STORAGE  equ    $FA00               ;start of message block




*** RAM VARIABLEN ********************************************************

		ORG	RAM

TIME		ds      1              ; used in delay
zeichen         ds      1              ; actual character
row             ds      1              ; row      for cursor position
col             ds      1              ; coluomn
inptr           ds      1              ; input position in receiving buffer
outptr          ds      1              ; output position in receiving buffer
data            ds      32T            ; receiving buffer for rs232 32 Bytes
screen          ds      32T            ; a mirror of the screen in mem 2 rows of 16 Byte
parm1           ds      1              ; Parameter für VT commands
parm2           ds      1              ;

***      71 Byte Ram used

*** MAIN ROUTINE *********************************************************

                ORG    ROM              ; begin of FLASH ROMs

                lda    InitConfig1      ; write config register
                sta    CONFIG1
                lda    TRIMLOC          ; load oscillator trim value
                sta    OSCTRIM          ; and set trim value


*** Initialize ram *******************************************************

                jsr raminit             ; initialize ram



*** Intialize Ports ******************************************************

START           BCLR    E,LCD_CTRL      ; clear LCD_CTRL
                BCLR    RS,LCD_CTRL     ;
		clr	LCD_DATA        ; clear LCD_DATA
		lda	#$FF            ; make ports outputs
		sta	DDRB            ; PortB output
                BSET    E,DDRA          ; PORTA Pins as outputs
                BSET    RS,DDRA


*** Initialize Keyboard int on PTA0 **************************************

                SEI                    ; disable interrupts
                mov #%00000100,KBSCR   ; clear pending interrupts       a
                                       ; and enable Kbd Int on fallling edge
                mov #%00000001,KBIER   ; PTA0 enable interrupts on PTA0

*** LCD Initialisieren ***************************************************


                jsr LCD_init

*** other initialisations ************************************************

                lda	#$80            ; Load adress of row 1
  		jsr     LCD_ADDR        ; send addr to LCD
                clr     row             ; write cursorposition
                clr     col
                cli                     ; from now on enable interrupts

*** start of work begins here ********************************************

loop:
                jsr getchar             ; get character from inputbuffer
                cmp #$20
                blo loop3               ; < $20 is control character

loop1
                sta zeichen             ; save character
                jsr  LCD_WRITE          ; output character on LCD
                lda row                 ; and save character in screen memory
                nsa
                add col
                tax
                lda zeichen
                sta screen,x
                inc col                 ; increment Cursor position
                lda col                 ; last coloumn ?
                cmp #$10
                bne loop                ; no the get nnext character
                lda row                 ; second row  ?
                bne loop2               ; yes the set cursor to the begin of row 1
                inc row                 ; else increment row
loop2:
                lda #row2adr            ; set LCD adress at beginning of row 1
                jsr LCD_ADDR            ;
                clr col                 ; set coloumn to zero
                bra loop                ; and go back

loop3:
                cmp #ESC                ; ESC is a command
                bne loop4               ; ignore the rest
                clr parm1               ; initialize parameters
                clr parm2
                jsr vt_command          ; and check the command
                                        ;
loop4:
                bra  loop               ; do it again
DUMMY		bra	DUMMY           ; it never should en up here





**************************************************************************
*  subroutines                                                           *
**************************************************************************

**************************************************************************
* VAR_DELAY                                                              *
*                                                                        *
* waits TIME * 100 us                                                    *
* at 3,2 MHz internal Busfrequency 320 Taclocks                          *
*                                                                        *
**************************************************************************


VAR_DELAY	lda	#33T            ;2

VAR_DEL1:
         	deca                    ;1
                nop                     ;1
                nop                     ;1
                nop                     ;1
                nop                     ;1
                nop                     ;1 =6 clocks
                nop
                nop

		bne	VAR_DEL1        ;9 * 33 clocks = 297 clocks + 2 = 299 clocks

		dec	TIME            ;4 = 303 clocks

                brn     VAR_DEL1        ;3 = 306
                brn     VAR_DEL1        ;3 = 309
                brn     VAR_DEL1        ;3 = 312
                nop                     ;1 = 313
                nop                     ;1 = 314
		bne	VAR_DEL1        ;3 = 317


		rts                     ;4

**************************************************************************
* LCD Routinen                                                           *
**************************************************************************

**************************************************************************
* LCD_init                                                               *
*                                                                        *
* Initialize the display                                                 *
*                                                                        *
**************************************************************************

LCD_init:

*** wait 15ms

                BCLR    RS,LCD_CTRL  ;set LCD instruction mode
                lda	#150T
		sta	TIME         ;set delay time
		jsr	VAR_DELAY    ;sub for 0.1ms delay



*** Send Init Command

		lda	#FUNCSET     ;LCD init command
		sta	LCD_DATA
		bset	E,LCD_CTRL   ;clock in data
		bclr	E,LCD_CTRL



*** wait 4.1ms

		lda	#41T
		sta	TIME         ;set delay time
       		jsr	VAR_DELAY    ;sub for 0.1ms delay



*** Send Init Command

		lda	#FUNCSET     ;LCD init command
		sta	LCD_DATA
		bset	E,LCD_CTRL   ;clock in data
		bclr	E,LCD_CTRL



*** wait 100us

		lda	#1T
		sta	TIME            ;set delay time
		jsr	VAR_DELAY       ;sub for 0.1ms delay



*** Send Init Command

		lda	#FUNCSET        ;LCD init command
		jsr	LCD_WRITE       ;write data to LCD



*** Send Function Set Command

*** 8 bit bus, 2 rows, 5x7 dots

		lda	#FUNCSET       ;function set command
		jsr	LCD_WRITE      ;write data to LCD



*** Send Display Ctrl Command

*** display on, cursor off, no blinking

		lda	#DSPON          ;display ctrl command
		jsr	LCD_WRITE       ;write data to LCD



*** Send Clear Display Command

*** clear display, cursor addr=0

		jsr	LCD_clear



*** Send Entry Mode Command

*** increment, no display shift

		lda	#$06            ;entry mode command
		jsr	LCD_ADDR        ;write data to LCD



*** SEND MESSAGES

*** Set the address, send data



                jsr     MESSAGE1        ;send Message1

                jsr     MESSAGE2        ;send Message2

                rts

**************************************************************************
* LCD_clear                                                              *
*                                                                        *
* clears the display                                                     *
*                                                                        *
**************************************************************************

LCD_clear:
		lda	#CLRDISP        ;clear display command
		jsr	LCD_ADDR        ;write data to LCD
		lda	#16T
		sta	TIME            ;set delay time for 1.6ms
		jsr	VAR_DELAY       ;sub for 0.1ms delay
                ldx #0
                lda #' '
LCD_clear1:
                sta screen,x            ; and fill thescreen buffer
                incx
                cpx #$20                ; with blanks
                bne LCD_clear1
                rts

**************************************************************************
* LCD_home                                                               *
*                                                                        *
* Cursor Home                                                            *
*                                                                        *
**************************************************************************

LCD_home:
		lda	#CURHOME        ; clear display command
		jsr	LCD_ADDR        ; write data to LCD
                clr     row             ; set cursor position 0,0
                clr     col
		lda	#16T
		sta	TIME            ;set delay time for 1.6ms
		jsr	VAR_DELAY       ;sub for 0.1ms delay
                rts

**************************************************************************
* LCD_scroll                                                             *
*                                                                        *
* scroll one row                                                         *
*                                                                        *
**************************************************************************

LCD_scroll:
               clrx                     ; clear x register
lcd_scr1:
               lda screen+$10,x         ; copy row 1
               sta screen,x             ; into row 0
               lda #' '                 ; and fill row 1 with blanks
               sta screen+$10,x
               incx
               cpx #$10
               bne lcd_scr1
               jsr write_scr            ; and write it on the screen
               lda #row2adr
               jsr LCD_ADDR
               mov #0,col
               rts

**************************************************************************
* LCD_clreol                                                             *
*                                                                        *
* Clear until the en of the line                                         *
*                                                                        *
**************************************************************************

LCD_clreol:

               lda row                  ; load row counter
               bne LCD_clreol1          ; row 1 1 ?
               lda #row1adr             ; else load adress of row 0
               bra LCD_clreol2          ; go on
LCD_clreol1:
               lda #row2adr             ; load adress of row 1

LCD_clreol2:
               add col                  ; add coloumn
               jsr LCD_ADDR             ; set LCD adress
               ldx col                  ; load coloumn of cursor position into x
               lda row
               bne LCD_clreol4          ; row 1 then move on
LCD_clreol3:
               lda #' '                 ; load blank
               jsr LCD_WRITE            ; and output to lcd
               incx                     ; increment coloumn counter
               cpx #$10                 ; until end of line
               bne LCD_clreol3
               lda #row1adr             ; load adress of row 0
               add col                  ; add coloumn
               jsr LCD_ADDR             ; and set adress
               bra LCD_clreol5          ; and go on
LCD_clreol4:
               lda #' '                 ; load blank
               jsr LCD_WRITE            ; and output to lcd
               incx                     ; increment counter
               cpx #$10                 ; until end of line
               bne LCD_clreol4
               lda #row2adr             ; load adress of row 1
               add col                  ; add coloumn
               jsr LCD_ADDR             ; and set adress

LCD_clreol5:

               ldx col                  ; load coloumn into x
               lda row                  ; load row
               bne LCD_clreol7          ; row 1 the go ahead

LCD_clreol6:
               lda #' '                 ; load blanks
               sta screen,x             ; and write screenbuffer
               incx                     ; increment x
               cpx #$10                 ; until end of line
               bne LCD_clreol6
               bra LCD_clreol9          ; and leave
LCD_clreol7:
               aix #$10                 ; add $10 for row 1
LCD_clreol8:
               lda #' '                 ; load blanks
               sta screen,x             ; write to screen buffer
               incx                     ; increment counter
               cpx #$20                 ; intil end of line
               bne LCD_clreol8

LCD_clreol9:

               rts


**************************************************************************
* LCD_WRITE                                                              *
*                                                                        *
* writes the character in A to the LCD                                   *
*                                                                        *
**************************************************************************

LCD_WRITE:
        	sta	LCD_DATA        ; put character on the data port
		bset	E,LCD_CTRL      ; clock in data
		bclr	E,LCD_CTRL
		lda	#30T		;2 40us delay for LCD
LCD_W1:
		deca			;3
		bne	LCD_W1		;3
		rts



**************************************************************************
* LCD_ADDR                                                               *
*                                                                        *
* Sets the LCD adress to the value in A                                  *
*                                                                        *
**************************************************************************

LCD_ADDR        bclr    RS,LCD_CTRL     ; LCD in command mode
                sta	LCD_DATA        ; output to data port
		bset	E,LCD_CTRL      ; clock in data
		bclr	E,LCD_CTRL
		lda	#30T            ;2 40us delay
LCD_ADDR1:
          	deca                    ;3
		bne	LCD_ADDR1       ;3
                bset	RS,LCD_CTRL     ; LCD in data mode
                rts



**************************************************************************
* write_scr                                                              *
*                                                                        *
* ouputs the screen buffer to the LCD                                    *
**************************************************************************

write_scr:
                lda	#$80            ; addr = $80 row0 col0
		jsr     LCD_ADDR        ; send addr to LCD
                clrx
write_scr1:
                lda     screen,X        ; load character from screen buffer
                jsr     LCD_WRITE       ; write data to LCD
                incx
                cpx     #$10
                bne     write_scr1      ; first row
                lda     #$C0            ; addr = $C0 row1 col0
                jsr     LCD_ADDR        ; send addr to LCD
write_scr2:
                lda     screen,X        ; load character from screen buffer
                jsr     LCD_WRITE       ; write data to LCD
                incx
                cpx     #$20
                bne     write_scr2      ; second row
                rts



**************************************************************************
* MESSAGE1 and MESSAGE2 send greetings to LCD at startup                 *
*                                                                        *
**************************************************************************

MESSAGE1        lda	#$84            ;addr = $04 row1 col4
		jsr     LCD_ADDR        ;send addr to LCD
                clrx
L3              lda     MSG1,X          ;load AccA w/char from msg
                beq     OUTMSG1         ;end of msg?
                jsr     LCD_WRITE       ;write data to LCD
                incx
                bra     L3              ;loop to finish msg
OUTMSG1         rts



MESSAGE2        lda     #$C0            ;addr = $C0 row1 col0
                jsr     LCD_ADDR        ;send addr to LCD
                clrx
L5              lda     MSG2,X          ;load AccA w/char from msg
                beq     OUTMSG2         ;end of msg?
                jsr     LCD_WRITE       ;write data to LCD
                incx
                bra     L5              ;loop to finish msg
OUTMSG2         rts

**************************************************************************
* end of LCD routines                                                    *
**************************************************************************

**************************************************************************
* raminit                                                                *
*                                                                        *
* initialize RAM memory                                                  *
*                                                                        *
**************************************************************************

raminit:
                clr TIME
                clr row
                clr col
                clr inptr
                clr outptr
                clr parm1
                clr parm2
                ldx #0
                lda #0
raminit1:
                sta data,x
                incx
                cpx #$20
                bne raminit1
                ldx #0
                lda #' '
raminit2:
                sta screen,x
                incx
                cpx #$20
                bne raminit2
                rts

**************************************************************************
* getchar                                                                *
*                                                                        *
* gets the next character from the input buffer and returns it in A      *
*                                                                        *
**************************************************************************

getchar:

                pshx                ; save x-reg
getch1
                ldx  outptr         ; load output pointer to receivebuffer
                cpx  inptr          ; compare with inputpointer
                beq  getch1         ; both equal then there is no character
                lda  data,x         ; else load character in A
                incx                ; increment output pointer
                stx  outptr         ; and save it
                cpx  #20            ; end of buffer ?
                bne  getchend       ; no ok
                clr  outptr         ; else set output pointer to the begin of the buffer
getchend:
                pulx                ; get x-reg back
                rts                 ; and done

**************************************************************************
* vt_command                                                             *
*                                                                        *
* do vt100 commands                                                      *
*                                                                        *
*                                                                        *
*                                                                        *
*  force Cursor Position is a little messy                               *
*  should be done in a sepearate subroutine with a better structure      *
*                                                                        *
**************************************************************************

vt_command:
                jsr getchar         ; get first character following escape
                cmp #'c'            ; Reset Device ?
                bne vt_comm_1       ; else go on
                jsr LCD_init        ; reset LCD
                jsr raminit         ; initialize RAM
                jsr LCD_home        ; set cursor
                jmp vt_comm_end     ; andleave
vt_comm_1:
                cmp #'M'            ; Scroll Up ?
                bne vt_comm2        ; els go on
                jsr LCD_scroll      ; Scroll
                jmp vt_comm_end     ; and leave
vt_comm2:
                cmp #'['            ; everything else begins with [
                bne vt_err          ; or leave in error
                jsr getchar         ; get next character
                cmp #'H'            ; Cursor Home ?
                bne vt_comm3        ; else go on
                jsr LCD_home        ; Cursor Home
vt_err:         jmp vt_comm_end     ; and leave

vt_comm3:
                cmp #'K'            ; Clear to end of Line
                bne vt_comm4        ; else go on
                jsr LCD_clreol      ; clear to end of line
                jmp vt_comm_end     ; and leave
vt_comm4:
                cmp #'2'            ; here are more the one possibilities
                bne vt_comm6        ; else go on
                sta parm1           ; save as parameter
                jsr getchar         ; next character
                cmp #'K'            ; K is erase line
                bne vt_comm5        ; else go on
                clr col             ; set col to 0
                jsr LCD_clreol      ; and clear to end of line
                jmp vt_comm_end     ; and leave
vt_comm5:
                cmp #'J'            ; J is Clear Screen
                bne vt_comm6        ; else go on
                jsr LCD_clear
                jmp vt_comm_end     ; and leave

vt_comm6:
                cmp #'1'             ; Force Cursor position
                bne vt_comm7         ; uses only two lines 0 and 1
                sub #30              ; make it binary
                sta parm1            ; and save it. There will be another parameter
                bra force
vt_comm7:
                cmp #'0'             ; row 0 ?
                bne vt_comm10
                sub #30              ; convert to binary
                sta parm1            ; and save it
force:
                jsr getchar          ; get next character
                cmp #';'             ; should be ;
                bne vt_comm10
                jsr getchar          ; get next character
                cmp #'2'             ; max 16 characters per line, first character < 2
                bhi vt_comm8         ; or < 10
                sub #30              ; convert to binary
                nsa                  ; multiply by 16
                sta parm2            ; and save it
                jsr getchar          ; get next character
vt_comm8:

                cmp #'f'             ; end, first character is coloumn
                bne vt_comm9         ; col is two digits long
                lda parm2            ; divide by 16
                nsa                  ;
                sta parm2            ; and save it
                lda parm1            ; get row parameter
                sta row              ; and save it
                clc                  ; clear carry to rotate zeroes
                rora                 ; until at the right position
                rora
                rora
                add #$80             ; add $80 makes it 80 or C0
                add parm2            ; add coloumn
                jsr LCD_ADDR         ; set adress
                lda parm2            ; and save cursor position
                sta col
                jmp vt_comm_end      ; done
vt_comm9:
                cmp #'9'             ; second character can´t be > 9
                bhi vt_comm10        ; else leave in error
                sub $30              ; convert to binary
                add parm2            ; and add it
                cmp #$0A             ; > 10 ?
                blo force2
                sub #$06             ; subtract 6 because we multiplied thee 1  by 16

force2:
                sta parm2            ; save it
                lda parm1            ; get row
                sta row              ; and save cursor position
                clc                  ; claer carry to sgift zeroes
                rora                 ; shift to the rigth position
                rora
                rora
                add #$80             ; add $80 gives 80 or C0
                add parm2            ; add coloumn
                jsr LCD_ADDR         ; set adress
                lda parm2            ; and save cursor position
                sta col
                jsr getchar          ; should be f
                jmp vt_comm_end
vt_comm10:
                                     ; here is the place where error processing could be
vt_comm_end:
                rts

**************************************************************************
* Keyboard Interrupt service Routine                                     *
*                                                                        *
* called at falling edge on PTA0                                         *
* calls GetByte and saves received data in the receive buffer            *
* wastes ca 20 clocks until getbyte begins. Getbyte triggers to the      *
* middle of a bit time. So we are 20 clocks out of synch.                *
* at 9600 baud a bit takes approx. 330 clocks, so there is enough time   *
* left. The stopbit is ignored so there is enough time for post          *
* processing before next character can arrive                            *

**************************************************************************
KbdIsr:

               pshh                     ;2 save H-reg
               sei                      ;2 disable other interrupts
               mov  #%00000010,KBIER    ;4 KB-Int disablen
               bclr PTA0,PTA            ;4 initialize PTA0 for serial comms
               jsr  GetByte             ;  receive RS232 Byte
               ldx  inptr               ; loud pointer to receive buffer
               sta  data,x              ; and save character
               incx                     ; increment pointer
               cpx  #$20                ; end of buffer ?
               bne  kbd1                ; no go on
               clrx                     ; else set to begin of buffer
kbd1:
               stx  inptr               ; save input pointer
               mov  #%00000100,KBSCR    ; write ACK to clear pending interrupts
               mov  #%00000001,KBIER    ; enable ints on PTA0
               cli                      ; enable ints global
               pulh                     ; restore H-Reg
               rti

*** MESSAGE STORAGE ******************************************************

                ORG     MSG_STORAGE

MSG1            db      'NITRON'
                db       0
MSG2            db      'LCD Terminal'
                db       0


*** Belegung der Vektoren ************************************************

    org AltRESET
    JMP $EE00
    org AltKBD
    JMP KbdIsr









