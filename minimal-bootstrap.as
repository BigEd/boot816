	;; sketch of a minimal bootstrap
	;; suitable to be toggled into an eeprom
        ;;    or somehow loaded via CPLD on a romless tube parasite
	;; will load a bigger second-stage bootstrap from serial port
	;;    or over the tube
	;; up to 256 bytes, will be located in zero page
	;; a typical second stage might load a monitor in srecord format
	;;
	;;     PATH=$PATH:bin make -B minimal-bootstrap.srec BASE=0xFFE5
	;;

        .ORG $FFE5 ; place the bootstrap up against the reset vector

	;; for info on 6850 see http://www.electronics.dit.ie/staff/tscarff/6800/6850acia/6850.htm
        .DEFINE         SERIAL_STATUS_REG  $FE08
        .DEFINE         SERIAL_DATA_REG    $FE09

	.DEFINE         EOF		   $EA   ; NOP opcode to indicate end of program
					   	 ; (must avoid this value even as a data byte)

bootstrap:
	; master-reset the UART - if we're using it!
	LDA #$03
	STA SERIAL_STATUS_REG

	LDX #0			; ideally X is zeroed by reset?
readbyte:
	LDA SERIAL_STATUS_REG	; bit 0 tells us if there is a byte to read
	    			; would be a tad easier if it was bit 7 or 6
	ROR
	BCC readbyte
	LDA SERIAL_DATA_REG
	CMP #EOF    ; check for end of second stage - or we could do it by byte count
	BEQ 65536   ; jump to second stage at bottom of zero page
	STA 0,X
	INX
	BNE readbyte

        .word bootstrap

;; that's 27 bytes as written
;; less 5 if we don't init the uart (or tube)
;; less 2 if X is zero at reset time (beginning to think it isn't)
;; less 2 if we always load a full page instead of having an end byte
;; less 1 if the non-empty-receive flag is in bit 7 or 6
;; so probably 19 bytes minimum: ideally can synth that as a ROM, but
;;    if it has to be on mass storage we might as well go to second stage directly.

