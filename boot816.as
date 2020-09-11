        ;; ---------------------------------------------------------
        ;;
        ;; Boot816
        ;;
        ;; Boot ROM based on Ed's code to detect CPU type and
        ;; redirect IO to the serial port on startup. Added
        ;; new * extension commands for testing various
        ;; 65816 configurations.
        ;;
        ;; ---------------------------------------------------------
        ;; (C) 2008-2016 Ed Spittles, Richard Evans
        ;; ---------------------------------------------------------
        ;;
        .SETCPU "65816"
        .ORG $8000

        .DEFINE ROMLATCH	$FE30
        .DEFINE ROMLATCHCOPY	  $F4
	.DEFINE		CPLD_MAPREG    $800003      ; CPLD MAP and clock control register location
	.DEFINE		CPLD_RAM_MAPMASK   $10      ; NB bits 0..3 are clock control bits and not to be disturbed
	.DEFINE		CPLD_ROM_MAPMASK   $20      ; NB bits 0..3 are clock control bits and not to be disturbed        

	.DEFINE OSRDRM  $FFB9 rom number in Y, address in &F6/7, X and Y not preserved
        .DEFINE GSINIT  $FFC2
        .DEFINE GSREAD  $FFC5
        .DEFINE OSASCI  $FFE3
        .DEFINE OSNEWL  $FFE7
        .DEFINE OSWRCH  $FFEE
        .DEFINE OSRDCH  $FFE0
        .DEFINE OSWORD  $FFF1
        .DEFINE OSBYTE  $FFF4

        ;; zero page usage:
	;; &70 - &8F - unused by OS, granted to user by BASIC
        ;;           - up to 4 bytes will be used by hipeek and hipoke
        ;; &A8-&AF   - free for 'os commands' - might be overwritten by OSASCI used by prntstr?
  	;; &B0-&CF   - available to a ROM, but it is supposed to claim and release it
        ;; &F2, &F3  - official OS text pointer (our command line, also used for gsinit/gsread)
        ;; &F6, &F7  - officially ptr into paged ROM, used here by mem testers and prntstr

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

	.MACRO MAC_SWAP	AA, BB ;  there's a clever way with XOR...
        LDA AA
	XBA
        LDA BB
	STA AA
	XBA
        STA BB
	.ENDMACRO


LANG:   .BYTE $00,$00,$00       ; no language entry
SERV:   JMP CHECK               ; service entry
TYPE:   .BYTE $82               ; ROM type=Serv+6502
OFST:   .BYTE COPYRT-LANG
VERNO:  .BYTE $02
TITLE:  .BYTE "BOOT816 - 65816 support",$0D
        .BYTE $00
TITLE2: .BYTE "2008-16"
COPYRT: .BYTE $00
        .BYTE "(C) Rich Evans & Ed Spittles",$0D
        .BYTE $00
CHECK:  CMP #$04                ; is it a command?
        BNE HPCH                ; ..no, is it help?
        JMP COMCH               ; ..yes, check command list
HPCH:   CMP #$09                ; is it a help rqst?
        BNE SRVCCH              ; ..no, is it service rqst?
        JMP HLPCH               ; ..yes, check if for this ROM
SRVCCH: CMP #$03                ; is it a service3 ?
        BNE SRVC27CH            ; ..no, is it srvc27?
        JMP Service3Reset       ; ..yes, handle a service 3
SRVC27CH: CMP #$27              ; is it a service27?
        BNE OUT                 ; ..no, bug out
        JMP Service27Reset      ; ..yes
OUT:    RTS
HLPCH:  PHP                     ; save registers
        PHA
        TXA
        PHA
        TYA
        PHA
        LDX #$00
        ;; Y as passed to us is part of the pointer
BLOOP:  LDA ($F2),Y
        CMP TITLE,X             ; check request against
        BEQ NXLET               ; title, cater for lower
        SEC                     ; case letters
        SBC #$20
        CMP TITLE,X
        BNE SMALL               ; not this ROM, print
NXLET:  INX                     ; name only
        INY
        LDA ($F2),Y
        CMP #$2E                ; Cater for abbreviations
        BEQ LARGE
        CPX #$05
        BCC BLOOP
LARGE:  LDX #$00
LLOOP:  LDA BTEXT,X             ; Print full help
        CMP #$00
        BEQ BOUT
        JSR OSASCI
        INX
        JMP LLOOP
SMALL:  LDX #$00
SLOOP:  LDA LTEXT,X             ; print small help
        CMP #$00
        BEQ LOUT
        JSR OSASCI
        INX
        JMP SLOOP
BOUT:   PLA
        TAY
        PLA
        TAX
        PLA
        PLP
        LDA #$00                ; prevent other ROMs responding by setting a 0
        RTS
LOUT:   PLA
        TAY
        PLA
        TAX
        PLA
        PLP
        RTS                     ; offer help rqst to other roms by not altering A
BTEXT:  .BYTE $0D
        .BYTE "BOOT816 ROM - new * commands",$0D
        .BYTE "  HITESTHIMEM",$0D
        .BYTE "  REPORTCPU",$0D
        .BYTE "  REPORTHIMEM",$0D
        .BYTE "  SETSERIALREDIRECT",$0D
.ifdef SRECORD_D
        .BYTE "  SRECORD", $0D
.endif
        .BYTE "  TEST816",$0D
        .BYTE "  TESTHIMEM",$0D
.ifdef IRQINSTALL_D
        .BYTE "  IRQINSTALL",$0D
.endif
        .BYTE "  HIPEEK",$0D
        .BYTE "  HIPOKE",$0D
        .BYTE "  HEXDUMP",$0D
        .BYTE "  OVERLAYON",$0D
        .BYTE "  OVERLAYOFF",$0D
        .BYTE "  ROMCOPY",$0D
        .BYTE $00
LTEXT:  .BYTE $0D
        .BYTE "BOOT816 $Rev: 381 $",$0D
        .BYTE $00
LOCHK:  LDA COMLIST,X           ; routine to cater for lower case letters
        CLC
        ADC #$20
        CMP ($F2),Y
        BEQ CLOOP
        JMP BACKUP
COMCH:  PHA                     ; COMMAND CHECK
        TXA
        PHA
        TYA
        PHA
        STY $0A02               ; SAVE Y OFFSET
        LDX #$FF
GRABY:  LDY $0A02
        DEY
CLOOP:  INY
        INX
        LDA COMLIST,X           ; check command list
        BMI NEG                 ; is it an address?
        CMP ($F2),Y
        BNE LOCHK               ; is it lower case
        JMP CLOOP
BACKUP: INX
        LDA COMLIST,X
        BPL BACKUP
        INX
        LDA ($F2),Y
        CMP #$2E                ; cater for abbreviations
        BNE GRABY
        INY
        DEX
        LDA COMLIST,X
        BNE GRABADD
NEG:    CMP #$FF                ; bottom of command list
        BEQ BYE                 ; yes, so exit
GRABADD: STA $0A01
        LDA COMLIST+1,X         ; ELSE LOAD ADDRESS
        STA $0A00
        JSR EXEC                ; and goto command code
        CLC
BYE:    PLA
        TAY
        PLA
        TAX
        PLA
        BCS HOP                 ; if command performed then
        LDA #$00                ; stop other ROMS responding
HOP:    RTS
EXEC:   JMP ($0A00)
        ;; Table of command names and jump targets
COMLIST:
.ifdef IRQINSTALL_D
        .BYTE "IRQINSTALL"
        .BYTE >IRQINSTALL
        .BYTE <IRQINSTALL
.endif
        .BYTE "HITESTHIMEM"
        .BYTE >HITESTHIMEM
        .BYTE <HITESTHIMEM
        .BYTE "REPORTCPU"
        .BYTE >REPORTCPU
        .BYTE <REPORTCPU
        .BYTE "REPORTHIMEM"
        .BYTE >REPORTHIMEM
        .BYTE <REPORTHIMEM
        .BYTE "SETSERIALREDIRECT"
        .BYTE >SetSerialRedirect
        .BYTE <SetSerialRedirect
.ifdef SRECORD_D
        .BYTE "SRECORD"
        .BYTE >SRECORD
        .BYTE <SRECORD
.endif
        .BYTE "TEST816"
        .BYTE >TST816
        .BYTE <TST816
        .BYTE "TESTHIMEM"
        .BYTE >TESTHIMEM
        .BYTE <TESTHIMEM
        .BYTE "HEXDUMP"
        .BYTE >HEXDUMP
        .BYTE <HEXDUMP
        .BYTE "HIPEEK"
        .BYTE >HIPEEK
        .BYTE <HIPEEK
        .BYTE "HIPOKE"
        .BYTE >HIPOKE
        .BYTE <HIPOKE
        .BYTE "OVERLAYON"
        .BYTE >OVERLAYON
        .BYTE <OVERLAYON
        .BYTE "OVERLAYOFF"
        .BYTE >OVERLAYOFF
        .BYTE <OVERLAYOFF
        .BYTE "ROMCOPY"
        .BYTE >ROMCOPY
        .BYTE <ROMCOPY
        .BYTE $FF
        ;; ------------------------------------------------------------------------
        ;; (*)TEST816
        ;;
        ;; Skip into and back out of out of 816 native mode after first
        ;; detecting that the 65816 is present.
        ;; ------------------------------------------------------------------------

TST816:
        JSR DieIfNot65816
        MAC_MODE816
        REP #%00010000          ; enable 16b index registers!
        SEP #%00010000          ; disable 16b index registers!
        MAC_MODE02
        LDY #00
        JMP ReportResult
        ; done

        ;; ------------------------------------------------------------------------
        ;; REPORTHIMEM (*REPORTHIMEM)
        ;;
        ;; Detect HIMEM by writing and retrieving a single data byte and checking
        ;; that the operation is not aliased to a location in the normal memory map
        ;;
        ;;
        ;; Checks that an '816 is fitted before attempting to check himem but 
        ;; don't need to go into native mode for the test.
        ;; ------------------------------------------------------------------------


REPORTHIMEM:
        JSR DieIfNot65816
        JSR DetectHiMem         ; result returned in Y
        JMP ReportResult        ; Use Y value to report result
        ; done

DetectHiMem:
        LDA #$AA                ; Store checkerboard to aliased location
        STA $F6
        LDA #$55
        STA $FF00F6             ; Store inverted checkerboard to himem
        LDA $F6                 ; retrieve checkerboard from lo mem
        CMP #$AA                ; compare with expected value
        BEQ DETNXT1             ; ..skip fail code if it's ok
        LDY #02                 ; return fail code 2 : aliassing
        RTS
DETNXT1: LDA $FF00F6            ; retrieve inverted checkerboard from hi mem
        CMP #$55                ; compare with expected value
        BEQ DETNXT2             ; ..skip fail code it it's ok
        LDY #01                 ; return fail code 1 : mismatch
        RTS
DETNXT2:LDY #00                 ; return code 0 : pass
DieReturnsOK:
        RTS

DieIfNot65816:
        JSR DetectCPUType
        CMP #$03
        BEQ DieReturnsOK
        PLA          ; discard the return address
        PLA
        LDY #$03
        JMP ReportResult
        ; done

DieIfNoHiMem:
        JSR DetectHiMem
        CPY #$00
        BEQ DieReturnsOK
        PLA                    ; discard our caller's return address
        PLA
        JMP MSG_NOHIMEM
        ; done


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
        .DEFINE         PZ_BKG0         $F6
        .DEFINE         PZ_BKG1         PZ_BKG0 + 1
        .DEFINE         HIMARCHSTART    $FE5000
        .DEFINE         HI_WrDown       HIMARCHSTART + WrDown - MARCHSTART
        .DEFINE         HI_RdWrUp       HIMARCHSTART + RdWrUp - MARCHSTART
        .DEFINE         HI_RdWrDown     HIMARCHSTART + RdWrDown - MARCHSTART
        .DEFINE         CODESIZE        MARCHEND-MARCHSTART

        ;; Check 65816 is fitted first
        JSR DieIfNot65816

        ;; Check HiMem is fitted next before testing it
        JSR DieIfNoHiMem

        JSR PRNTSTR
        .BYTE $0D,"March0: >wr(0)      .."
        NOP

        LDA #$55
        MAC_MODE816  ; will use 16-bit index mode (but the JSL should be fine)
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
        JSR DieIfNoHiMem

        JSR MSG_BLOCKCPY
        MAC_MODE816  ; switch to Native mode for 16-bit index registers

BlockCopy:
        REP #%00110000        ; 16 bit index registers on
        .I16
        .A16
        LDX #MARCHSTART
        LDY #(HIMARCHSTART & $0FFFF)
        LDA #CODESIZE
        PHB                     ; save DBR
        ;; Opcode should be <MVN> <dest> <src>
        MVN ^HIMARCHSTART, ^MARCHSTART
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
        ;; actually, need 816 mode for 16-bit index registers
        ;;   and interrupts disabled because we have no 816 IRQ handler
        ;;   But: we could call into hi memory using JSL in Emulated mode,
        ;;   provided we had disabled interrupts or had a PBK safe copy
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

        ;; ----------------------------------------------------------------------
        ;; (*)REPORTCPU
        ;;
        ;; Detect and display CPU type
        ;; ----------------------------------------------------------------------
REPORTCPU:
        JSR DetectCPUType
        PHA
        JSR PRNTSTR             	; Print inline text up to NOP
        .BYTE "- Detected "
        NOP
        PLA
        CMP #$00
        BNE DetectionNot6502
        JSR PRNTSTR             	; Print inline text up to NOP
        .BYTE "original NMOS 6502",$0D
        NOP
        RTS
DetectionNot6502:
        CMP #$01
        BNE DetectionNotCMOS6502
        JSR PRNTSTR                     ; Print inline text up to NOP
        .BYTE "Standard 65C02",$0D
        NOP
        RTS
DetectionNotCMOS6502:
        CMP #$02
	BNE DetectionNotRockwell
	JSR PRNTSTR              	; Print inline text up to NOP
	.BYTE "Rockwell R65C02",$0D
	.BYTE 13
	NOP
	RTS
DetectionNotRockwell:
	JSR PRNTSTR              	; Print inline text up to NOP
	.BYTE "65802 or 65816",$0D
	NOP
	RTS	
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
DetectCPUType:
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
	RTS

        ;; ----------------------------------------------------------------------
        ;; (*)SetSerialRedirect
        ;;
        ;; setup 19200 baud and redirect both in and out.
        ;; ----------------------------------------------------------------------
SetSerialRedirect:
        LDA #181
        LDX #0
        LDY #0
        JSR OSBYTE
        ;; Second entry point skips the *FX 181 call which seems to
        ;; hang the machine if used at boot time. So, the *Setser.. command
        ;; enters at the top and the call from BootFS enters below.
SetSerialRedirect2:
        LDA #7
        LDX #8
        JSR OSBYTE
        LDA #8
        LDX #8
        JSR OSBYTE
        LDA #3
        LDX #5
        JSR OSBYTE
        LDA #2
        LDX #1
        JSR OSBYTE
        ;; VDU 15 to turn off page mode
        LDA #15
        JMP OSWRCH
        ; done

        ;; ----------------------------------------------------------
        ;; BootFS - initialize the ROM at boot time
        ;; ----------------------------------------------------------
BootFS:
        JSR PrintRomTitle
        JSR REPORTCPU
        JSR DetectCPUType
        CMP #$03
        BNE DetectKey
;        JSR DetectHiMem
;        CPY #$00
;        BNE DetectKey
;        JSR ROMCOPY
;        JSR OVERLAYON
DetectKey:
        ;; detect any key pressed
        LDA#$7A
        JSR OSBYTE
        CPX #$FF
        BNE BootFSSkip1
        ;; redirect the serial if no key was pressed
        JSR PRNTSTR
        .BYTE "- Setting serial IO, 19200 baud", $0D,"(hold any key during boot to disable)", $0D
        .BYTE 13
        NOP
        JSR SetSerialRedirect2
BootFSSkip1:
        RTS

        ;; ----------------------------------------------------------
        ;; SERVICE 3 - BOOT FS
        ;; ----------------------------------------------------------
Service3Reset:
        PHA                     ; we should not claim this service
;;         JSR PRNTSTR             ; Print inline text up to NOP
;;         .BYTE $0D,"BOOT816 saw a service 3",$0D
;;         NOP
        JSR BootFS
        PLA
        RTS

        ;; ----------------------------------------------------------
        ;; SERVICE &27 - RESET
        ;;
        ;; We have never seen this on a BBC but it seems to be how
        ;; the ROM is initialized on a Master128
        ;; ----------------------------------------------------------
Service27Reset:
        PHA                 ;we should not claim this service
;;        JSR PRNTSTR         ;Print inline text up to NOP
;;        .BYTE 13,"Boot816 saw a service 27",13
;;        NOP
        JSR BootFS
        PLA
        RTS

        ;; ---------------------------------------------------------
        ;; Print ROM title
        ;; ---------------------------------------------------------
PrintRomTitle:
        LDX #$FF
        LDA #$0D                ; Prepare to print newline
PrintRomTitleNext:
        JSR OSASCI
        INX
        LDA TITLE,X
        BNE PrintRomTitleNext
        RTS

        ;; ---------------------------------------------------------
        ;; Stop the machine, copy some host RAM into HIRAM
        ;; then enable the remapping in CPLD, allow machine to continue
        ;; ---------------------------------------------------------
        .DEFINE         PZ_NMI         $F6
        .DEFINE         NMI_HANDLER    $0D00
	.DEFINE		LOMEM_MAPSTART $0000
	.DEFINE		HIMEM_MAPSTART $FE0000
	.DEFINE		HIMEM_MAPLEN   $8000    ; overlay covers 32K now

OVERLAYON:
	JSR DieIfNot65816
	MAC_MODE816   ; also sets interrupt mask
	; nobble the NMI handler temporarily
	LDA NMI_HANDLER
	STA PZ_NMI
        LDA #$40  ; RTI
	STA NMI_HANDLER

        PHB                     ; save DBR because block moves change it

	;; copy low (host) memory into high (fast) memory
	;; block copy routine - ought really to re-use this code
        REP #%00110000        ; 16 bit index registers on
        .I16
        .A16
        ;; MVN <destbank> <srcbank> with Y as dest addr, X as source addr, A as bytecount-1
        LDX #LOMEM_MAPSTART            ;; lower 16 bits of source
        LDY #(HIMEM_MAPSTART & $ffff)  ;; lower 16 bits of destination
        LDA #HIMEM_MAPLEN-1
        MVN ^HIMEM_MAPSTART, ^LOMEM_MAPSTART
        SEP #%00110000 ; Back to 8b registers
        .I8
        .A8

        ; switch in the memory mapping using 24-bit addressing
	LDA CPLD_MAPREG
	ORA #CPLD_RAM_MAPMASK
	STA CPLD_MAPREG

        PLB                     ; restore DBR (from the stack in the mapped state)

        ; restore the NMI handler
        LDA PZ_NMI
        STA NMI_HANDLER

	MAC_MODE02 ; also re-enables interrupts
	RTS

OVERLAYOFF:
	JSR DieIfNot65816
	MAC_MODE816   ; also sets interrupt mask
	; nobble the NMI handler temporarily
	LDA NMI_HANDLER
	STA PZ_NMI
        LDA #$40  ; RTI
	STA NMI_HANDLER

        PHB                     ; save DBR because block moves change it
				; take care to save in the mapped state
				; so when we copy down, it's there to retrieve

        ; switch out the memory mapping by clearing the control bit
	LDA CPLD_MAPREG
	AND #($ff-CPLD_RAM_MAPMASK)
	STA CPLD_MAPREG

	;; block copy routine - ought really to re-use this code
        REP #%00110000        ; 16 bit index registers on
        .I16
        .A16
        ;; MVN <destbank> <srcbank> with Y as dest addr, X as source addr, A as bytecount-1
        LDX #(HIMEM_MAPSTART & $ffff)  ;; lower 16 bits of source
        LDY #LOMEM_MAPSTART            ;; lower 16 bits of destination
        LDA #HIMEM_MAPLEN-1
        MVN ^LOMEM_MAPSTART, ^HIMEM_MAPSTART
        PLB                     ; restore DBR
        SEP #%00110000 ; Back to 8b registers
        .I8
        .A8

        ; restore the NMI handler
        LDA PZ_NMI
        STA NMI_HANDLER

	MAC_MODE02 ; also re-enables interrupts
	RTS

        ;; ---------------------------------------------------------
        ;; ROMCOPY - call both the OSCOPY and COPY8ROMS functions
        ;;           to make HIMEM copies of the firmware
        ;; *ROMCOPY
        ;; ---------------------------------------------------------
        
ROMCOPY:
       JSR DieIfNot65816
       JSR PRNTSTR
       .BYTE "Copying MOS to high memory ..."
       NOP
       JSR OSCOPY
       JSR PRNTSTR
       .BYTE "DONE.", $0D,"Copying 8 ROMs to high memory ..."
       NOP
       JSR COPY8ROMS
       JSR PRNTSTR
       .BYTE "DONE.", $0D
       NOP
       ;; Set the ROM copied mapping bit at the end
       ;; LDA CPLD_MAPREG
       ;; ORA #CPLD_ROM_MAPMASK
       ;; STA CPLD_MAPREG
       RTS
       

        ;; ---------------------------------------------------------
        ;; OS-ROM-copy: copy 15k and 256bytes from bank0 to himem
        ;; *OSCOPY
        ;; ---------------------------------------------------------
        .DEFINE         OSCOPY1_SRC	      $C000
        .DEFINE         OSCOPY1_DST         $EEC000
        .DEFINE         COPY1_LEN             $3C00  ; 15k
        .DEFINE         OSCOPY2_SRC	      $FF00
        .DEFINE         OSCOPY2_DST         $EEFF00
        .DEFINE         COPY2_LEN              $100  ; 256 bytes

OSCOPY:
        MAC_MODE816   ; also sets interrupt mask
        PHB                     ; save DBR because block moves change it
        REP #%00110000        ; 16 bit index registers on
        .I16
        .A16
        ;; MVN <destbank> <srcbank> with Y as dest addr, X as source addr, A as bytecount-1

        LDX #OSCOPY1_SRC            ;; lower 16 bits of source
        LDY #(OSCOPY1_DST & $ffff)  ;; lower 16 bits of destination
        LDA #(COPY1_LEN-1)
        MVN ^OSCOPY1_DST, $0
        LDX #OSCOPY2_SRC            ;; lower 16 bits of source
        LDY #(OSCOPY2_DST & $ffff)  ;; lower 16 bits of destination
        LDA #(COPY2_LEN-1)
        MVN ^OSCOPY2_DST, $0
        SEP #%00110000 ; Back to 8b registers
        .I8
        .A8
        PLB                     ; restore DBR
        MAC_MODE02 ; also re-enables interrupts
	RTS


        ;; ---------------------------------------------------------
        ;; a fixed copy of 16k of SWROM 15 from bank 0 to FE
        ;; *MEMCOPY  (unimplemented)
	;; note that variable banks means patching the code
	;; note that the code must be in RAM anyway to read ROM
	;;   possible but slower to use OSRDRM to read a byte at a time
	;;
	;; we might one day want a generic MEMCOPY with parameters
	;; perhaps this should be called ROMCOPY
        ;; ---------------------------------------------------------

COPY8ROMS:
         ;; we need about 50 bytes, somewhere in RAM, to do the copying
         ; first we copy our master routine, then patch it and then use it
         ; it would be clever to allocate on the stack
         ; but a bit too clever
         ; so let's just use some spare RAM from somewhere
         ; we could allocate ROM workspace officially
         ; or just pick somewhere which looks unimportant
         .DEFINE SCRATCHRAM $7F80  ; in screen memory, for now!

         JSR INSTALLMEMCOPY

         ; copy the 4 RAMish ROMs, 4 to 7, to bank EC
         LDA #$EC
         STA MEMCOPY_PATCH_MVN - MEMCOPYCODE+SCRATCHRAM +1
         LDY #4
NEXTROM1:
         TYA
         STA MEMCOPY_PATCH_ROM - MEMCOPYCODE+SCRATCHRAM +1
         ROR        ;; move ROM index into top bits for destination
         ROR
         ROR
         AND #$C0
         STA MEMCOPY_PATCH_DEST - MEMCOPYCODE+SCRATCHRAM +2
         PHY
         JSR SCRATCHRAM
         PLY
         INY
         CPY #8
         BNE NEXTROM1

         ; copy the top 4 ROMs, 12 to 15, to bank ED
         LDA #$ED
         STA MEMCOPY_PATCH_MVN - MEMCOPYCODE+SCRATCHRAM +1
         LDY #12
NEXTROM2:
         TYA
         STA MEMCOPY_PATCH_ROM - MEMCOPYCODE+SCRATCHRAM +1
         ROR        ;; move ROM index into top bits for destination
         ROR
         ROR
         AND #$C0
         STA MEMCOPY_PATCH_DEST - MEMCOPYCODE+SCRATCHRAM +2
         PHY
         JSR SCRATCHRAM
         PLY
         INY
         CPY #16
         BNE NEXTROM2

         RTS


        ;; we've allocated ourselves two bytes in page zero
        ;; rather boldly and unofficially
	.DEFINE		ZP_PTR_1	         $70

INSTALLMEMCOPY:
	; init our pointer to the destination area
	LDA #<SCRATCHRAM
	STA ZP_PTR_1
	LDA #>SCRATCHRAM
	STA ZP_PTR_1+1

	LDY #(MEMCOPYCODE_END - MEMCOPYCODE)
MEMCOPY_BACK_1:
	LDA MEMCOPYCODE,Y
	STA (ZP_PTR_1),Y
	DEY
	BPL MEMCOPY_BACK_1	; we copy fewer than 127 bytes

	RTS

        ;; ---------------------------------------------------------
        ;; Code to be copied into RAM so it can copy SWROM
	;; Has extra labels so it can be patched in various places
	;; 43 bytes ($2B bytes) approx - can fit in stack
        ;; ---------------------------------------------------------
        .DEFINE         MEMCOPY_SRC	       $8000
	.DEFINE		MEMCOPY_SOURCE_ROM_DUMMY $0F  ; default to copying with ROM 15 in the map
        .DEFINE         MEMCOPY_DST          $EDC000  ; for testing: $018000 
        .DEFINE         MEMCOPY_LEN            $4000  ; just copy one ROM's worth

MEMCOPYCODE:

	LDA ROMLATCHCOPY ; take a safe copy of the current ROM
	PHA

MEMCOPY_PATCH_ROM:
	LDA #MEMCOPY_SOURCE_ROM_DUMMY

	STA ROMLATCHCOPY
	STA ROMLATCH

        MAC_MODE816   ; also sets interrupt mask
	;; note that the PHB/PLB may have to be external to the block copy
        ;; to cope with the overlay case
        PHB                     ; save DBR because block moves change it

        ;; block copy routine - ought really to re-use this code
        REP #%00110000        ; 16 bit index registers on
        .I16
        .A16
        ;; MVN <destbank> <srcbank> with Y as dest addr, X as source addr, A as bytecount-1

MEMCOPY_PATCH_SRC:
        LDX #MEMCOPY_SRC            ;; lower 16 bits of source
MEMCOPY_PATCH_DEST:
        LDY #(MEMCOPY_DST & $ffff)  ;; lower 16 bits of destination
MEMCOPY_PATCH_LEN:
        LDA #(MEMCOPY_LEN-1)
MEMCOPY_PATCH_MVN:
        MVN ^MEMCOPY_DST, $0
        SEP #%00110000 ; Back to 8b registers
        .I8
        .A8

        PLB                     ; restore DBR

        MAC_MODE02 ; also re-enables interrupts

	;; re-select the original ROM
	PLA
	STA ROMLATCHCOPY
	STA ROMLATCH

MEMCOPYCODE_END:
        RTS


        ;; ---------------------------------------------------------
        ;; Take a 24-bit address and print the byte found there
        ;; *HIPEEK FFFFE
        ;; ---------------------------------------------------------
HIPEEK:
        TYA                    ;; Y is part of our command-line pointer
        PHA
        JSR DieIfNot65816
        PLA
        TAY

        LDA #6                 ;; read up to 6 hex digits
        JSR argparse           ;; place a 24-bit address in 70/71/72
        BCS HIPEEKPOKEFAIL

        JSR print24bits
        JSR OSNEWL

HIPEEK_peek:
        ; we're using 24-bit addressing in emulated mode: that's OK
        LDA [$70]
        JSR printhex1byte
        JMP OSNEWL
        ; done

        ;; ---------------------------------------------------------
        ;; Write a given byte to a given 24-bit address
        ;; *HIPOKE 80000 ED
        ;; grubby in part because the arg parser has a poor interface
        ;; ---------------------------------------------------------
HIPOKE:
        TYA                    ;; Y is part of our command-line pointer
        PHA
        JSR DieIfNot65816
        PLA
        TAY

        LDA #6                 ;; read up to 6 hex digits
        JSR argparse           ;; place a 24-bit address in 70/71/72
        BCS HIPEEKPOKEFAIL

        JSR print24bits
        JSR OSNEWL

        LDA $70                ;; save the address - we're about to overwrite
        PHA
        LDA $71
        PHA
        LDA $72
        PHA

        LDA #2                 ;; read a single byte of hex
        JSR argparse           ;; return a one-byte datum in A8
        BCS HIPEEKPOKEFAIL3POP

        JSR print8bits
        JSR OSNEWL

        LDA $70                ;; save the data byte
        TAX                  
        PLA                    ;; now recover the 3-byte address we had
        STA $72
        PLA
        STA $71
        PLA
        STA $70
        TXA
        ; we're using 24-bit addressing in emulated mode: that's OK
        STA [$70]
        JMP HIPEEK_peek        ;; re-read the value and print it

HIPEEKPOKEFAIL3POP:
        PLA
        PLA
        PLA
HIPEEKPOKEFAIL:
        LDY #4
        JMP ReportResult
        ; done

        ;; ---------------------------------------------------------
        ;; Dump a region of memory in hex
        ;; *HEXDUMP AAAAAA BBBBBB
        ;; A is the starting address, B is the end address
        ;; ---------------------------------------------------------
HEXDUMP:
        TYA                    ;; Y is part of our command-line pointer
        PHA
        JSR DieIfNot65816
        PLA
        TAY

        LDA #6                 ;; read up to 6 hex digits
        JSR argparse           ;; place a 24-bit address in 70/71/72
        BCS HIPEEKPOKEFAIL

        LDA $70                ;; save the address in 73/74/75
        STA $73
        LDA $71
        STA $74
        LDA $72
        STA $75

        LDA #6                 ;; read up to 6 hex digits
        JSR argparse           ;; place a 24-bit address in 70/71/72
        BCS HIPEEKPOKEFAIL

	; the addresses are the wrong way around - I really need indirection
	MAC_SWAP $70, $73
	MAC_SWAP $71, $74
	MAC_SWAP $72, $75

HEXDUMP1LINE:
        JSR print24bits
	LDX #16
HEXDUMPNEXTBYTE:
	LDA #' '
	JSR OSWRCH
	LDA [$70]
        JSR printhex1byte
	INC $70
	BNE HD_SKIP_1
	INC $71
	BNE HD_SKIP_1
	INC $72
HD_SKIP_1:
	DEX
	BNE HEXDUMPNEXTBYTE
        JSR OSNEWL

	LDA $70  ;; check to see if we're done
	CMP $73
	LDA $71
	SBC $74
	LDA $72
	SBC $75
	BCC HEXDUMP1LINE
	RTS

print24bits:
        LDA $72
        JSR printhex1byte
        LDA $71
        JSR printhex1byte
print8bits:
        LDA $70
        JMP printhex1byte
        ; done


.ifdef IRQINSTALL_D
        ;; ---------------------------------------------------------
        ;; Install an 816-mode IRQ handler in high memory
	;;
	;; tested only in emulation. No test routine in ROM.
	;;
        ;; ---------------------------------------------------------

.define  irqvector816  $FFEE
.define  irqvector02   $FFFE

IRQINSTALL:
        JSR DieIfNot65816

        ;; Check HiMem is fitted next before testing it
        JSR DieIfNoHiMem

	SEI
	LDA # .lobyte(IRQHANDLER)
	STA $FE0000 + irqvector816
	LDA # .hibyte(IRQHANDLER)
	STA $FE0000 + irqvector816 + 1
	CLI
	RTS


IRQHANDLER:
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
        ; done


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

.endif

MSG_BLOCKCPY:
        JSR PRNTSTR
        .BYTE $0D,"Copying March code to himem .."
        NOP
        RTS

MSG_NOHIMEM:
        JSR PRNTSTR
        .BYTE $0D,"No Himem found - aborting",$0D
        NOP
        RTS

        ;;
        ;; Some code to parse hex args (c) Gordon Horsington from his Module09 file
        ;; uses gsinit/gsread to remove quotes and other escape chars
        ;; parses the OS string at (&F2),Y
        ;; allows up to 6 digits, will zero-extend short input
        ;; results returned in 70,71,72 (lsb to msb)
        ;; fixme: max number of bytes accepted should be a parameter
        ;; fixme: zero page addresses not symbolic
        ;; fixme: should be looping not straightlined
        ;; fixme: reverses the bytes in order to store lsb first: better to shift into a result

argparse:
        STA $77       ; save the parameter
        CLC           ; terminate with space, return or "
        JSR GSINIT    ; initialise argument with Gsinit
        TSX
        STX $76       ; save the stack pointer so we can error out
        LDX #0        ; initial result = $000000
        STX $70       ; least sig. byte of result
        STX $71       ; 
        STX $72       ; most sig. byte of result
        DEX           ; X will count number of nybbles - 1
argloop:
        JSR GSREAD    ; read character from argument with Gsread
        BCS argend    ; branch if termination character
        JSR argconv   ; convert nybble into binary
        PHA           ; push each nibble on stack
        INX           ; X is a running count of nibbles
        BPL argloop   ; branch for next character (limit of 127 chars!)
argend:
        CPX $77       ; more than expected number of characters?
        BCS argerr    ; reject if too many

        PLA           ; pull a low nibble
        STA $70       ; store in appropriate place

        DEX           ; 
        BMI argdone   ; branch if we're done

        PLA           ; pull a high nibble
        ASL A         ; shift
        ASL A         ; left
        ASL A         ; four
        ASL A         ; bits
        ORA $70       ; OR into appropriate byte
        STA $70       ; and store

        DEX           ; 
        BMI argdone   ; branch if we're done

        PLA           ; pull a low nibble
        STA $71       ; store in appropriate place

        DEX           ; 
        BMI argdone   ; branch if we're done

        PLA           ; pull a high nibble
        ASL A         ; shift
        ASL A         ; left
        ASL A         ; four
        ASL A         ; bits
        ORA $71       ; OR into appropriate byte
        STA $71       ; and store

        DEX           ; 
        BMI argdone   ; branch if we're done

        PLA           ; pull a low nibble
        STA $72       ; store in appropriate place

        DEX           ; 
        BMI argdone   ; branch if we're done

        PLA           ; pull a high nibble
        ASL A         ; shift
        ASL A         ; left
        ASL A         ; four
        ASL A         ; bits
        ORA $72       ; OR into appropriate byte
        STA $72       ; and store

argdone:
        CLC           ; indicate success
        RTS

argconv:
        CMP #$3A      ; ASC("9")+1
        BCS argletters
        CMP #$30      ; ASC("0")
        BMI argerr
        AND #$F
        CLC
        RTS
argletters:
        AND #$DF      ; downcase the letters for convenience
        SBC #$37
        CMP #$A
        BMI argerr
        CMP #$10
        BCS argerr
        RTS

argerr:
        LDX $76       ; recover the stack pointer
        TXS 
        SEC           ; indicate failure to caller
        RTS

        ;; other cribs for handling hex found in os1.2 disassembly:
        ;;   E2AD  shift a hex string into osfile control block at 02ee,X
        ;;   E08F  check for hex digit
        ;;   F97A  print ASCII equivalent of hex byte


        ;; print ASCII equivalent of hex byte
        ;; copied from bbc os1.20

printhex1byte:
        PHA         ;save A on stack
        LSR         ;/16 to put high nybble in lo
        LSR         ;
        LSR         ;
        LSR         ;
        JSR printhex1nibble  ; print the high nibble first
        PLA         ;get back A
    
printhex1nibble:
        CLC         ;clear carry flag
        AND #$0F    ;clear high nybble
        ADC #$30    ;Add &30 to convert 0-9 to ASCII A-F to : ; < = > ?
        CMP #$3A    ;if A< ASC(':')
        BCC printhex1char
        ADC #$06    ;else add 7 to convert : ; < = > ? to A B C D E F
printhex1char:
        JMP OSWRCH  ;print character and return
        ; done

.ifndef TOPLEVEL
TOPLEVEL=1
.endif
.ifdef SRECORD_D
.include "srecord.as"
.endif
