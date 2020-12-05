        ;; ---------------------------------------------------------
        ;;
        ;; XLOAD
        ;;
        ;; High memory file loader
        ;;
        ;; This started life as XLOAD by Jonathan Harston:
        ;; http://mdfs.net/Software/CommandSrc/Utils/Memory/XLoad.src.bas
        ;; but is now almost entirely different!
        ;;
        ;; ---------------------------------------------------------
        ;; (C) 2008-2020 Ed Spittles, Richard Evans, David Banks
        ;; ---------------------------------------------------------
        ;;

; Zero page

.DEFINE addr   $70          ; used by argparse in boot816.as
.DEFINE num    $A8          ; transient command space
.DEFINE handle $A9          ; transient command space
.DEFINE lptr   $F2          ; string pointer used by GSINIT/GSREAD


.DEFINE gbpb   $2EE         ; OSGBPB ctrl block
.DEFINE buffer $900         ; OSGBPB buffer (one page)
.DEFINE ctrl   $900         ; OSFIND ctrl block (can overlap with OSGBPB buffer)

; This is what we are clobbering:
; &02EE-&02FF MOS OSFILE control block for *LOAD, *SAVE, *CREATE and *DELETE
; &0900-&09BF RS423 output buffer
; &09C0-&09FF Speech buffer
; &0900-&09FF CFS/RFS BPUT sequential output buffer (nb, *not* used by SAVE)
; &0900-&09BF ENVELOPES 5 to 16

XLOAD:

        JSR SkipSpace
        BEQ errSyntax

        TYA                 ; copy address of filename to OSFIND block
        CLC
        ADC lptr
        STA ctrl
        LDA lptr+1
        ADC #0
        STA ctrl+1

        CLC                 ; terminate with space, return or "
        JSR GSINIT          ; initialise argument with Gsinit
        BNE SkipName

errSyntax:
        ;; TODO - should copy error to &100,X and JMP to it
        JSR PRNTSTR
        .BYTE "Syntax: XLOAD <afsp> <address>", $0D
        NOP
        RTS

SkipName:
        JSR GSREAD
        BCC SkipName        ; Step past the filename

        JSR SkipSpace
        BEQ errSyntax

        LDA #6              ; Parse the 24-bit address
        JSR argparse        ; to addr (&70-72, LSB first)

        LDX ctrl
        LDY ctrl+1          ; XY=>filename
        LDA #$40
        JSR OSFIND          ; Open the file for readinf
        TAY
        STY handle
        BNE CopyData

errNotFound:
        ;; TODO - should copy error to &100,X and JMP to it
        JSR PRNTSTR
        .BYTE "File not found", $0D

        NOP
        RTS

CopyData:                   ; Use OSGBPB to read 256 bytes into buffer
        LDX #1
        STX gbpb+6          ; count b15-b7
        DEX
        STX gbpb+5          ; count b0-b7
        STX gbpb+7
        STX gbpb+8          ; count=$00000100
        DEX
        STX gbpb+4
        STX gbpb+3          ; addr=$FFFFxxxx
        LDA #>buffer
        STA gbpb+2
        LDA #<buffer
        STA gbpb+1          ; addr=$FFFFbuffer
        LDA handle
        STA gbpb            ; handle

        LDA #4
        LDX #<gbpb
        LDY #>gbpb
        JSR OSGBPB          ; Read 256 bytes from the file

        LDA gbpb+6          ; gbpb+5/gbpb+6 have been decremented by the number of bytes transferred
        BNE Close           ; End-of-File, and no bytes to copy
        LDA #0
        SEC
        SBC gbpb+5
        TAY                 ; Y = number of bytes transferred (or zero a full page transferred)

        PHP                 ; Z=0 if this a partial last block

CopyBytes:                  ; Copy number of bytes transferred
        DEY
        LDA buffer, Y
        STA [addr], Y
        CPY #0
        BNE CopyBytes

        PLP
        BNE Close

;; Increment the address by a page
        CLC
        LDA addr+1
        ADC #1
        STA addr+1
        LDA addr+2
        ADC #0
        STA addr+2
        BRA CopyData

Close:
        LDY handle
        LDA #0
        JMP OSFIND

SkipSpace1:
        INY
SkipSpace:
        LDA (lptr), Y
        CMP #' '
        BEQ SkipSpace1
        CMP #13
        RTS
