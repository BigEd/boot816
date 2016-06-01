	;; clear host mem
	;;
	;;     PATH=$PATH:bin make -B clearhostmem.srec BASE=0x7000
	;;
        .SETCPU "65816"
        .ORG $7000  ; must live in video shadow, outside of area to be cleared



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


        .DEFINE         CPLD_MAPREG    $B00003
        .DEFINE         CPLD_MAPMASK   $10      ; NB bits 0..3 are clock control bits and not to be disturbed

clearhostmem:
	PHP
	SEI

        MAC_MODE816   ; also sets interrupt mask

        ; switch out the memory mapping by clearing the control bit
        LDA CPLD_MAPREG
        AND #($ff-CPLD_MAPMASK)
        STA CPLD_MAPREG

        ;; use block move with overlapping range to fill
        REP #%00110000        ; 16 bit index registers on
        .I16
        .A16
        ;; MVN <destbank> <srcbank> with Y as dest addr, X as source addr, A as bytecount-1
        LDX #0
	STX 0
        LDY #1
        LDA #$6ffe
        MVN 0, 0
        SEP #%00110000 ; Back to 8b registers
        .I8
        .A8

        ; switch in the memory mapping using 24-bit addressing
        LDA CPLD_MAPREG
        ORA #CPLD_MAPMASK
        STA CPLD_MAPREG

        MAC_MODE02 ; also re-enables interrupts

	PLP
	RTS
