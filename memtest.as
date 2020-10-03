        ;; ---------------------------------------------------------
        ;;
        ;; memtest816
        ;;
        ;; Memory test code by Rich Evans, extracted from Boot816 ROM
        ;;
        ;; ---------------------------------------------------------
        ;; (C) 2008,2009 Ed Spittles, Richard Evans
        ;; ---------------------------------------------------------
        ;;
        ;; $Author:$
        ;; $Id:$
        ;; $Rev: 2 $
        ;;
        .SETCPU "65816"
        .ORG $2000
        .DEFINE OSASCI  $FFE3
        .DEFINE OSBYTE  $FFF4
        .DEFINE OSNEWL  $FFE7
        .DEFINE OSWRCH  $FFEE
        .DEFINE OSRDCH  $FFE0

        ;; zero page usage:
        ;; &F2, &F3  - official OS text pointer (our command line, also used for gsinit/gsread)
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

        ;; ---------------------------------------------------------
        ;; Run multiple iterations of testhimem and hitesthimem
        ;; ---------------------------------------------------------

BIST20:
        LDA #20
BIST:   PHA
        JSR TESTHIMEM
        JSR HITESTHIMEM
        PLA
        DEC
        BNE BIST
        RTS


        ;; ----------------------------------------------------------------------
        ;; TESTHIMEM (*TESTHIMEM)
        ;;
        ;; Very simple test of a 64K byte area of HIMEM.
        ;;
        ;; Fill the area with one pattern, then read it back and compare it.
        ;; Repeat with the inverted version of the pattern
        ;;
        ;; Temporarily disables interrupts to switch to 816 mode but returns with
        ;; the CPU back in emulation mode and interrupts enabled
        ;;
        ;; Uses location $F6 of page zero for temporary storage
        ;;
        ;; Uses the X index register in 16bit mode to point to addresses during
        ;; the test.
        ;;
        ;; Need to detect presence of 816 CPU first, and then presence of HIMEM
        ;; before running the test. ie Don't crash a BBC or a BBC with a L0 '816
        ;; board when running this command.
        ;;
        ;; ----------------------------------------------------------------------
TESTHIMEM:
        .DEFINE         MEMTOP          $FFFF
        .DEFINE         MEMBOT          $0000
        .DEFINE         MEMTESTAREA     $FF0000
;       .DEFINE         MEMTOP          $1FFF
;       .DEFINE         MEMBOT          $0000
;       .DEFINE         MEMTESTAREA     $4000

        .DEFINE         PZ_BKG0         $F6
        .DEFINE         PZ_BKG1         PZ_BKG0 + 1
        .DEFINE         HIMARCHSTART    $F25000
        .DEFINE         HI_WrDown       HIMARCHSTART + WrDown - MARCHSTART
        .DEFINE         HI_RdWrUp       HIMARCHSTART + RdWrUp - MARCHSTART
        .DEFINE         HI_RdWrDown     HIMARCHSTART + RdWrDown - MARCHSTART
        .DEFINE         CODESIZE        MARCHEND-MARCHSTART

        ;; Check 65816 is fitted first
        JSR DieIfNot65816

        ;; Check HiMem is fitted next before testing it
;        JSR DieIfNoHiMem

        JSR PRNTSTR
        .BYTE "Starting TESTHIMEM"
        NOP

        JSR PRNTSTR
        .BYTE $0D,"March0: >wr(0)      .."
        NOP

        LDA #$55
        MAC_MODE816
        JSL WrDown
        MAC_MODE02

        JSR PrintDoneandAnnounceMarch1

        LDA #$55
        MAC_MODE816
        JSL RdWrDown
        MAC_MODE02

        JSR ReportAndAnnounceMarch2   ; result is in Y

        LDA #$AA
        MAC_MODE816
        JSL RdWrUp
        MAC_MODE02

        JSR ReportAndAnnounceMarch3   ; result is in Y

        LDA #$55
        MAC_MODE816
        JSL RdWrUp
        MAC_MODE02
        JMP ReportResult        ; result is in Y
        ; done

ReportResult:
        CPY #04
        BNE Report03
        JSR PRNTSTR
        .BYTE " Fail",$0D
        NOP
        RTS
Report03:
        CPY #03
        BNE Report02
FailNo65816:
        JSR PRNTSTR
        .BYTE $0D,"65816 not fitted",$0D
        NOP
        RTS
Report02:
        CPY #$02
        BNE ReportNext
FailNoHIMEM:
        JSR PRNTSTR             ; Print inline text up to NOP
        .BYTE " Fail - HIMEM aliassed to LOMEM",$0D
        NOP
        RTS
ReportNext:
        CPY #$01
        BNE ReportPass
        JSR PRNTSTR             ; Print inline text up to NOP
        .BYTE " Fail - data mismatch",$0D
        NOP
        RTS
ReportPass:
        JSR PRNTSTR             ; must be successful if we got here
        .BYTE " Pass",$0D
        NOP
        RTS

        ;; ------------------------------------------------------------
        ;; March Elements for the RAM test
        ;;
        ;; All are relocatable, and end with RTL so MUST be called via JSL
        ;;
        ;; WrDown   - \WR(Acc) - write value of acc to mem in desc. order
        ;; RdWrDown - \RD(Acc)WR(~Acc)
        ;; RdWrUp   - /RD(Acc)WR(~Acc)
        ;;
        ;; Makes temporary use of page0 location $F6-9
        ;;
        ;; Entry A = expected data
        ;; Exit  Y = 0 (pass) or non-zero (fail)
        ;;
        ;; ------------------------------------------------------------
MARCHSTART:

        ;;\ RD(0)Wr(1)
RdWrDown:
        STA     PZ_BKG0      ; init expected data reg
        EOR     #$FF         ; invert it
        XBA                  ; save it in B register
        LDY     #$00         ; y = 0 is a pass

        REP     #%00010000        ; 16 bit index registers on
        .I16
        LDX     #MEMTOP     ;
RdWrDown_top:
        LDA     MEMTESTAREA,X   ; read back data
        XBA                     ; save it and get inverted data in A
        STA     MEMTESTAREA,X   ; write inverted data back
        XBA                     ; restore read data and put inverted back in B
        CMP     PZ_BKG0         ; does it match expected value?
        BEQ     RdWrDown_cont   ; if result is zero then all ok
        LDY     #0004           ; .. else set sticky fail value
RdWrDown_cont:
        CPX     #MEMBOT         ; reached membot yet?
        BEQ     RdWrDown_exit
        DEX
        BRA     RdWrDown_top
RdWrDown_exit:
        SEP     #%00010000
        .I8
        RTL

        ;;/ RD(0)Wr(1)
RdWrUp:
        STA     PZ_BKG0      ; init expected data reg
        EOR     #$FF         ; invert it
        XBA                  ; save it in B register
        LDY     #$00         ; y = 0 is a pass
        REP     #%00010000        ; 16 bit index registers on
        .I16
        LDX     #MEMBOT     ;
RdWrUp_top:
        LDA     MEMTESTAREA,X   ; read back data
        XBA                     ; save it  and get inverted bkg from B
        STA     MEMTESTAREA,X   ; write inverted data back
        XBA                     ; restore read data and put inverted data bkg back in B
        CMP     PZ_BKG0         ; does it match expected value?
        BEQ     RdWrUp_cont     ; if result is zero then all ok
        LDY     #0004           ; .. else set sticky fail value
RdWrUp_cont:
        CPX     #MEMTOP         ; reached memtop yet?
        BEQ     RdWrUp_exit
        INX
        BRA     RdWrUp_top
RdWrUp_exit:
        SEP     #%00010000
        .I8
        RTL

        ;; \WR(0)
WrDown:
        REP     #%00010000
        .I16
        LDX     #MEMTOP
WrDown_top:
        STA     MEMTESTAREA,X       ; write inverted data back
        CPX     #MEMBOT             ; reached membot yet?
        BEQ     WrDown_exit
        DEX
        BRA     WrDown_top
WrDown_exit:
        SEP     #%00010000
        .I8
        RTL
        NOP
MARCHEND:

        ;; ----------------------------------------------------------------------
        ;; HITESTHIMEM (HITESTHIMEM)
        ;;
        ;; Very simple test of a 64K byte area of HIMEM. Identical
        ;; to TESTHIMEM, except that the march test code is first
        ;; relocated to himemory (with all RTS opcode replaced by
        ;; RTL) and then called using JSL.
        ;;
        ;; Test operation, including checking first for CPU type etc
        ;; is totally identical to the TESTHIMEM code.
        ;;
        ;; ----------------------------------------------------------------------
        ;; Check 65816 is fitted first
HITESTHIMEM:
        JSR DieIfNot65816

        ;; Check HiMem is fitted next before testing it
;        JSR DieIfNoHiMem

        JSR MSG_BLOCKCPY
        MAC_MODE816

BlockCopy:
        REP #%00110000        ; 16 bit index registers on
        .I16
        .A16
        LDX #MARCHSTART
        LDY #(HIMARCHSTART & $0FFFF)
        LDA #CODESIZE
        PHB                     ; save DBR
        ;; Opcode should be <MVN> <dest> <src>
        MVN (HIMARCHSTART>>16) & $FF, (MARCHSTART & $FF0000) >> 16
        PLB                    ; restore DBR
        SEP #%00110000 ; Back to 8b registers
        .I8
        .A8

BlockCpyEnd:

        MAC_MODE02
        JSR PRNTSTR
        .BYTE " Done",$0D,"March0: >wr(0)      .."
        NOP

        LDA #$55
        ;; NB have to be in 816 mode before calling hi memory via JSL
        MAC_MODE816
        JSL HI_WrDown
        MAC_MODE02

        JSR PrintDoneandAnnounceMarch1

        LDA #$55
        MAC_MODE816
        JSL HI_RdWrDown
        MAC_MODE02

        JSR ReportAndAnnounceMarch2   ; result is in Y

        LDA #$AA
        MAC_MODE816
        JSL HI_RdWrUp
        MAC_MODE02

        JSR ReportAndAnnounceMarch3   ; result is in Y

        LDA #$55
        MAC_MODE816
        JSL HI_RdWrUp
        MAC_MODE02

        JMP ReportResult        ; result is in Y
        ; done

PrintDoneandAnnounceMarch1:
        JSR PRNTSTR
        .BYTE " Done",$0D,"March1: >rd(0)wr(1) .."
        NOP
        RTS

ReportAndAnnounceMarch2:
        JSR ReportResult        ; result is in Y
        JSR PRNTSTR
        .BYTE "March2: <rd(1)wr(0) .."
        NOP
        RTS

ReportAndAnnounceMarch3:
        JSR ReportResult        ; result is in Y
        JSR PRNTSTR
        .BYTE "March3: <rd(0)wr(1) .."
        NOP
        RTS

MSG_BLOCKCPY:
        JSR PRNTSTR
        .BYTE $0D,"Copying March code to himem .."
        NOP
        RTS

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


DieIfNot65816:

        ; detect the CPU type
	; code is from
	;   comp.sys.apple2
	;   Re: Enhanced //e with a 6503??!
	;   David Empson  Sun, 2 Apr 2006
	;
	; return in A
	;    0 6502
	;    1 Standard 65C02
	;    2 Rockwell R65C02
	;    3 65802 or 65816
	;

	LDY #$00
	SED
	LDA #$99
	CLC
	ADC #$01
	CLD
	BMI DetectionDone  	; 6502 N flag not affected by decimal add
	LDY #$03
	LDX #$00
	.BYTE $BB               ; TYX - 65802 instruction, NOP on all 65C02s
	BNE DetectionDone  	; Branch only on 65802/816
	LDX $F6          	; non-destructive use of location $F6 (should maybe disable interrupts first)
	DEY
	STY $F6
	.BYTE $17
        .BYTE $EA
 	; RMB1 $F6       	; Rockwell R65C02 instruction
	CPY $F6          	; Location $F6 unaffected on other 65C02
	STX $F6
	BNE DetectionDone      	; Branch only on Rockwell R65C02 (test CPY)
	DEY
DetectionDone:
	TYA

        CMP #$03
        BEQ DieReturnsOK
        PLA          ; discard the return address
        PLA
        LDY #$03
        JMP ReportResult
        ; done


DieIfNoHiMem:
        MAC_MODE816
        LDA #$AA                ; Store checkerboard to aliased location
        STA $F6
        LDA #$55
        STA $F200F6             ; Store inverted checkerboard to himem
        LDA $F6                 ; retrieve checkerboard from lo mem
        CMP #$AA                ; compare with expected value
        BEQ DETNXT1             ; ..skip fail code if it's ok
        LDY #02                 ; return fail code 2 : aliassing
        MAC_MODE02              ; back into 6502 mode
        RTS
DETNXT1: LDA $F200F6            ; retrieve inverted checkerboard from hi mem
        MAC_MODE02              ; back into 6502 mode
        CMP #$55                ; compare with expected value
        BEQ DETNXT2             ; ..skip fail code it it's ok
        LDY #01                 ; return fail code 1 : mismatch
        RTS
DETNXT2:LDY #00                 ; return code 0 : pass

        CPY #$00
        BEQ DieReturnsOK
        PLA                    ; discard our caller's return address
        PLA
        JSR PRNTSTR
        .BYTE $0D,"No Himem found - aborting",$0D
        NOP

DieReturnsOK:
        RTS

