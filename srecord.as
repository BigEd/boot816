        ;; ---------------------------------------------------------
        ;;
        ;; srecord
        ;;
        ;; Srecord loader by Ed Spittles
        ;;
        ;; bugs: some redundant code copied in from boot816 rom
        ;; bugs: using page 6 for string buffer without reservation
        ;;
        ;; ---------------------------------------------------------
        ;; (C) 2009 Ed Spittles
        ;; some hex handling routines (c) Gordon Horsington from his Module09 file
        ;; ---------------------------------------------------------
        ;;
        ;; $Author:$
        ;; $Id:$
        ;; $Rev: 2 $
        ;;
.ifndef TOPLEVEL
        TOPLEVEL=1
        .SETCPU "65816"

        .ifndef BASE
        .error  "BASE is not defined - invoke ca65 with e.g.  -D BASE=0x2000 "
        .endif
        .ORG BASE

        .DEFINE OSRDCH  $FFE0
        .DEFINE OSASCI  $FFE3
        .DEFINE OSNEWL  $FFE7
        .DEFINE OSWRCH  $FFEE
        .DEFINE OSWORD  $FFF1
        .DEFINE OSBYTE  $FFF4

        ;; zero page usage:
        ;; &F6, &F7  - officially ptr into paged ROM, used here by mem testers and prntstr
        ;; &A8-&AF   - free for 'os commands' - might be overwritten by OSASCI used by prntstr?
        ;;           - up to 4 bytes will be used by hipeek and hipoke

        .MACRO MAC_MODE816
        SEI                     ; disable interrupts and enter 816 mode
        CLC
        XCE
        .ENDMACRO

        .MACRO MAC_MODE02
        SEC                     ; enter emulation mode and reenable interrupts
        XCE
        NOP                     ; delay before re-enabling interrupts
        NOP
        CLI
        .ENDMACRO
.endif

        ;; ---------------------------------------------------------
        ;; Load a chunk of data in srecord format: see
        ;;    man srec_motorola
        ;; Only rudimentary error handling and consistency checking
        ;; 
        ;; May or may not implement the RUN type of record
        ;; (we begin with the minimum: S1 records)
        ;; 
        ;; Typical minimal srecord input:
        ;;   S123204065640DEA204F20446F6E65210DEA606885F66885F79848A000F00320E3FFE6F617
        ;;   S1122060D002E6F7B1F6C9EAD0F168A86CF60031
        ;; (lines up to 74 characters, 10 chars of overhead, that's max 32 bytes of payload)
        ;;  
        ;; S1 Format is a data packet with a 16-bit address
        ;;   <whitespace>
        ;;   <line end>
        ;;   Sn     where n=1
        ;;   LL     two hex chars of length: number of bytes in remainder of this record
        ;;   AAAA   four hex chars of address
        ;;   BB<*>  many pairs of hex chars, the data to be stored
        ;;   CC     two hex chars, a checksum
        ;;   <line end>
        ;;   <whitespace>
        ;; 
        ;; ---------------------------------------------------------


	.DEFINE		PZ_SREC_BUFFLO	$A8
	.DEFINE		PZ_SREC_BUFFHI	$A9
	.DEFINE		PZ_SREC_LEN	$AA  ;; the max length, and then the actual length
	.DEFINE		PZ_SREC_MINASC  $AB
	.DEFINE		PZ_SREC_MAXASC	$AC

	.DEFINE		SREC_BUFF	$1600	;; $0600 is use by basic as a string buffer

SRECORD:
        JSR PRNTSTR
        .BYTE "Waiting for srecord input...", $0D
        NOP

SRECORDonemore:
	LDA #<SREC_BUFF
	STA PZ_SREC_BUFFLO
	LDA #>SREC_BUFF
	STA PZ_SREC_BUFFHI
	LDA #76
	STA PZ_SREC_LEN
	LDA #'0'
	STA PZ_SREC_MINASC
	LDA #'S'
	STA PZ_SREC_MAXASC

        LDA #0
	LDX #PZ_SREC_BUFFLO
	LDY #0
	JSR OSWORD            ;; read a line into $0600

	BCC notescape

        JSR PRNTSTR
	.BYTE "Escape", $0D
        NOP
        RTS


notescape:
	STY PZ_SREC_LEN		;;
	LDY #0			;; Y indexes into the srecord in the buffer

	LDA #'S'
	CMP SREC_BUFF,Y
        BNE badformat
	INY
	LDA #'1'
	CMP SREC_BUFF,Y
        BNE badformat
	INY			;; advance to the first char of the length

	LDX #0			;; X indexes into the decoded data

read3bytes:
	JSR read2hex
	BCS badformat
	STA SREC_BUFF,X		;; re-use the buffer for the binary data
	INX
	CPX #3			;; we wanted to read LL and AAAA
	BNE read3bytes
	
	LDA SREC_BUFF-3,X	;; the number of bytes left according to the srecord
	INC
	INC
	ASL
	CMP PZ_SREC_LEN		;; the line length according to OSWORD
	BEQ readNbytes

badformat:
        JSR PRNTSTR
	.BYTE "Bad format", $0D
        NOP
        RTS


readNbytes:
	JSR read2hex
	BCS badformat
	STA SREC_BUFF,X		;; re-use the buffer for the binary data
	INX
	CPY PZ_SREC_LEN		;; compare to end of string
	BNE readNbytes

	;; we need to verify the checksum
	STX PZ_SREC_LEN	     	 ;; this length is now a byte count
	LDY #0
	LDA #0
	CLC
checknext:
	ADC SREC_BUFF,Y
	INY
	CPY PZ_SREC_LEN
	BNE checknext
	INC
	BEQ blockmove

        JSR PRNTSTR
	.BYTE "Bad checksum", $0D
        NOP
        RTS


	;; finally, we block move the valid data to the target location
	;; there are assumptions here that addresses are 2 bytes
	;; so this can't be re-used for S2 records with 24-bit addresses
blockmove:
	LDA SREC_BUFF+2
	STA PZ_SREC_BUFFLO
	LDA SREC_BUFF+1
	STA PZ_SREC_BUFFHI
	DEY			;; Y was pointing just past the checksum byte
	DEY
	DEY
	DEY
	DEY
blockmovenext:
	LDA SREC_BUFF+3,Y
	STA (PZ_SREC_BUFFLO),Y
	DEY
	BPL blockmovenext	;; we know the max Y is never more than 32

	JMP SRECORDonemore
	; done - the only exit is to quit or read a bad record
	; which gets fixed when we deal with S5 or S6 count records, or S8 or S9 execute records

        .ifndef PRNTSTR
        ;; ----------------------------------------------------------------------
        ;; Print inline text up to NOP
        ;; zero page usage: F6 and F7
        ;; ----------------------------------------------------------------------
PRNTSTR:
        PLA
        STA $F6
        PLA
        STA $F7                 ;Pop return address to &F6/7
        TYA
        PHA
        LDY #$00
        BEQ PRNTSTRBGN
PRNTSTRNXT:
        JSR OSASCI
PRNTSTRBGN:
        INC $F6
        BNE PRNTSTRSKP
        INC $F7                 ; Increment address
PRNTSTRSKP:
        LDA ($F6),Y             ; Get character
        CMP #$EA
        BNE PRNTSTRNXT          ; If not 'NOP' opcode, loop to print it
        PLA
        TAY
        JMP ($00F6)             ; Pop Y and jump back to code
        .endif

        ;; Read two hex bytes from a string, return in A
        ;;   derived from code to parse hex args (c) Gordon Horsington from his Module09 file
	;; C=0 for success, C=1 for failure


read2hex:
	LDA SREC_BUFF,Y
	JSR hexread1
	BCC readmore
	RTS
readmore:
	ASL
	ASL
	ASL
	ASL
	STA SREC_BUFF,Y	;; temp reuse the buffer for upper nibble
	INY
	LDA SREC_BUFF,Y
	JSR hexread1
	BCC readmore2
	RTS

readmore2:
	ORA SREC_BUFF-1,Y	;; result in A
	INY		;; point to the next char to be read
	RTS

hexread1:
        CMP #$3A      ; ASC("9")+1
        BCS hexletters
        CMP #$30      ; ASC("0")
        BMI hexerr
        AND #$F
        CLC
        RTS
hexletters:
        SBC #$37
        CMP #$A
        BMI hexerr
        CMP #$10
	AND #$F
        RTS		; carry will be set if out of range
hexerr:
        SEC           ; indicate failure to caller
        RTS


