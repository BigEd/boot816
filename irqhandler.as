        ;;
        ;; irqhandler.as
        ;;
        ;; save full state of 816 and call a 6502-mode irq handler
	;;
	;; 816 irq is vectored to (ffee) which on a beeb os1.20 is 0e6c
	;; 
	;; build with:
	;;   make -B BASE=0x4000 irqhandler.srec
	;; 
	;; results in run65816 demonstrates IRQs are handled without disaster:
	;; >CALL&4000
	;; start
	;; in 816 mode, allowing interrupts, expect fun and games
	;; in 816 mode, still alive
	;; aaa
	;; back in 6502 mode, done
	;; 
	;; 
	;; 


        .setcpu "65816"
	.listbytes      unlimited
	.code
	
	;; .DEFINE OSASCI  $FFE3   ; commented out so I can overload it>
        .DEFINE OSBYTE  $FFF4
        .DEFINE OSWRCH  $FFEE
        .DEFINE OSRDCH  $FFE0

        .MACRO MAC_MODE816
        SEI                     ; disable interrupts and enter 816 mode
        CLC
        XCE
	.A8
	.I8
	SEP #$30		; force 8-bit for A and M bits
        .ENDMACRO

        .MACRO MAC_MODE02
        SEC                     ; enter emulation mode and reenable interrupts  
        XCE
        NOP			;  NOP for safety as we switch back
        NOP			;  NOP for safety as we switch back
        NOP			;  NOP for safety as we switch back
        NOP			;  NOP for safety as we switch back
	CLI
        .ENDMACRO

	.MACRO MAC_PUTS string
	JSR PRNTSTR
	.BYTE $0d,string,$0d
	NOP
	.ENDMACRO
	
	.MACRO MAC_PUTC_NOINT char
	PHP
	PHA
	LDA &0250 		;  OS shadow of ACIA control register ($96 = %10010110)
	;; work in progress
	LDA #char
	JSR OSASCI
	PLA
	PLP
	.ENDMACRO

	.MACRO MAC_PUTC char	; 
	PHP			; preserve the flags
	PHA
	LDA #char
	JSR OSASCI
	PLA
	PLP
	.ENDMACRO
	
.define  irqvector816  $FFEE
.define  irqvector02   $FFFE

.ORG $4000
testprog:	;;
	JMP testprog_more

irqcounter:	;; PRINT ~!&2003
	.BYTE $0,$0,$0,$0

loopcounter:	;; PRINT ~!&2007
	.BYTE $0,$0,$3,$0	;  about a second
	
irqstacks:	;; PRINT ~!&200B
	.BYTE $0,$0,$0,$0	;
	
testprog_more:	;; 
	MAC_PUTS "start"
	MAC_MODE816
	
	LDA #$4C			; save a JMP at 0E6C - OS1.20 sends 816-IRQ there
	STA $E6C
	LDA # .lobyte(irqhandler816)
	STA $FE0000 + irqvector816
	STA $E6D
	LDA # .hibyte(irqhandler816)
	STA $FE0000 + irqvector816 + 1
	STA $E6E
	
	MAC_PUTS "in 816 mode, allowing interrupts, expect fun and games"
 	CLI
	MAC_PUTS "in 816 mode, still alive"
	NOP
tightloop:	
	NOP
	NOP
	DEC loopcounter		
	BNE tightloop
	DEC loopcounter	+ 1
	BNE tightloop
	MAC_PUTC 'a'
	DEC loopcounter	+ 2
	BNE tightloop
	MAC_MODE02

	MAC_PUTS "back in 6502 mode, done"

	RTS
	

irqhandler816x:
	;; a trivial handler. It can't clear the irq source
	;; so it has to disable interrupts in order not to be
	;; invoked endlessly.  (Disable them in the saved flags register)
	;; (the P as we enter the handler has SEI already)
	PHA
	PHX
	INC irqcounter ; we want to leave a record
	TSX
	LDA $0103,X		; PLP would be wrong - would re-enable interrupts!
	ORA #%00000100		;  make this a one-shot by setting saved SEI bit. status flags are nvmxdizc
	STA $0103,X		; again, PHP would be wrong, as the B bit is not real in P
	;; place a sentinel and take a copy of the stack
takecopy:	
	TSX
	TXA
	PHA			; stack pointer
	LDA #$ED
	PHA			; sentinel 1
	PHA			; sentinel 2
	PLA			; sentinel 2
	PLA			; sentinel 1
	PLA			; stack pointer
	
 	LDX #$FF
	LDA irqcounter
	CMP #1
	BNE stackcopy3
stackcopy2:	
	LDA $0100,X
	STA $2400,X
	DEX
	BNE stackcopy2
stackrestore:	
	PLX			; x
	PLA			; a
	RTI
stackcopy3:	
	LDA $0100,X
	STA $2600,X
	DEX
	BNE stackcopy3
	BEQ stackrestore
	
irqhandler816:	
	;; irq handling code derived from http://cerebro.xu.edu/~ryanr/atari/65816.html
	;; this is called from the 816 irq vector, so only called in 816 mode
	;; a beeb-hosted 816 must switch to 6502 mode to use the OS handler
	;; or it could handle it itself
	;; note that the machine will already have pushed P and done SEI
        PHD    		; Save DBR and Direct
        PHB
        PEA $0000     	; Clear Direct...
        PLD           	;
	PHK		; ... and with a single zero byte ...
        PLB		; ... clear the DBR
        PHX           	; X&Y saved at present width, because resetting them
        PHY           	;  to 8 bits would destroy the upper half contents.
	PHP  		; push 816 P reg for reg width info (I was set as IRQ vector fetched)

	;; push a fake interrupt frame so we can call the 6502 host interrupt service
	PER IRETURN   	; push return address (then status) for the RTI
	SEP #%00110000 	; Set 8 bit regs
	LDA #%00000100  ; we want I set and B clear for the 6502 irq handler
	PHA           	; saving for sake of 6502 handler and RTI

	;; everything is safe
	;; switch to 6502 mode for the host interrupt service routine
	SEC
	XCE
	NOP		; residual doubt about our clock switching?

	;;        now jump to the appropriate interrupt vector, such as...
	JMP (irqvector02) ; Its RTI will return to IRETURN

IRETURN:
	CLC		; beeb special:	return to 816-mode (we're in the 816-mode handler!)
	XCE
	PLP 		; recover unmodified 816 status byte - for reg widths
			;  we know this saved P has SEI
	                ; we don't worry about N and Z because the next RTI will pull a real P
	PLY           	;  now restore X&Y at whatever width they were saved at
        PLX           	;
        PLB           	; Restore DBR and Direct
        PLD
        RTI           	; Return to main program (pulling genuine user-mode P then 3 PC bytes)


	;; notes on the beeb's irq handling:
	;; FFFE  6502-mode irq vector = DC1C
	;; DC1C  save A in &FC; check PLP for B bit
	;; DC27  BRK-handler
	;; (0204)  IRQ1V = DC93 - pre-handler for highest priority interception
	;; DC93  start checking ACIA status
	;; check all other expected hardware sources
	;; (0206)  IRQ2V = DE89 - post-handler for unhandled sources
	
	;; ----------------------------------------------------------------------
        ;; Print inline text up to NOP
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


OSASCI:				; polling-mode serial putc, needs no OS or interrupts
	CMP #$0D
	BNE OSASCI_PUTC
	LDA #$0A
	JSR OSASCI_PUTC
	LDA #$0D
OSASCI_PUTC:	
	PHA
OSASCI_BUSY:	
	LDA #2
	AND $FE08
	BEQ OSASCI_BUSY
	PLA
	STA $FE09
	RTS

