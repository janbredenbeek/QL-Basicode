* BASICODE-3 for the Sinclair QL
* Part 1: initialisation & Basic proc & fns 
* Copyright (C) 1986, 1987, 2017 by Jan Bredenbeek
* Released under GNU Public License in 2017

* Changelog:
* 20170501: Added PX_ASPRT command to adjust graphics scaling for pixel aspect
*           ratio (SMSQ/E only).
* 20170313: Allow for ROM/RAM versions using QMAC conditional assembly dir's
*           NOTE: This file should now be INCLUDEd from the BSCRAM_ASM or
*           BSCROM_ASM respectively. BSCROM should be set to 1 if assembling 
*           the ROM version and 0 for the LRESPR'ed version
*
* 19860118: Start of work...

          INCLUDE   QDOS_IN_MAC         Standard QDOS definitions

version   SETSTR    2.0

* Some definitions

BV_NTBAS  EQU       $18
BV_NTP    EQU       $1C
BV_NLBAS  EQU       $20
BV_CHBAS  EQU       $30
BV_CHP    EQU       $34
BV_RIP    EQU       $58

BUFLEN    EQU       8
IOSTAT    EQU       $0C
BREAKDIS  EQU       $10
SCHD_LEN  EQU       $12

* Macro for calling vectors

VECTOR    MACRO     A,B
[.LAB]    MOVE.W    [A],[B]
          JSR       ([B])
          ENDM

          XDEF      BUFLEN,GET_NAME,GET_CHAN,NO_PARAM,GET1INT,PRINTMSG
          XDEF      STO_INT,RECSTRG,GET_SCHD

          SECTION   CODE

* Initialisation code - Entry point when CALLed

          GENIF     BSCROM = 0

          SUBA.L    A0,A0               ensure A0 is zero when CALLed
          LEA       ROMSTART,A3          .. and set A3 to start of code
          BRA.S     ROMINIT             Jump to common init
          
          ENDGEN
          
* start of ROM header - but also included in LRESPR version for signon
          
ROMSTART  DC.L      $4AFB0001           ROM signature
          DC.W      0                   no proc/fn, we'll handle this..
          DC.W      ROMINIT-ROMSTART    .. in our init routine
          DC.W      0                   Do not print name, init does this.
ROMNAME   DC.W      ROMN_END-ROMNAME-2
          DC.B      'BASICODE-3 V[version]  2017 Jan Bredenbeek',10
ROMN_END  EQU       *

ROMINIT   MOVE.L    A0,-(A7)            Entry point for ROM version
          MOVE.L    A3,A5               Start address of ROM
          CMPA.L    #$CC000,A5          
          BNE.S     INIT_2
          LEA       SCHDTASK,A1         if $CC000, check for double init
          MOVEQ     #0,D0
          MOVE.W    A1,D0
          MOVE.L    D0,A1
          BSR       GETSCHD1            Are we already linked in?
          BEQ.S     INITEND             yes, so skip
INIT_2    MOVE.L    (A7),A0
          LEA       ROMNAME,A1
          BSR.S     PRINTMSG            Now print name
          MOVEQ     #SCHD_LEN,D1
          MOVEQ     #0,D2
          QDOS      MT.ALCHP            Allocate room for scheduler task
          TST.L     D0
          BNE.S     INITEND
          LEA       SCHDTASK,A1
          MOVE.L    A1,4(A0)
          MOVE.L    #$8000,BUFLEN(A0)   Initialise buffer size to 32K
          QDOS      MT.LSCHD
          
          GENIF     BSCROM = 1          RAM version has no CAS device
          
          LEA       CAS_DEF,A4
          BSR.S     DEVINIT             Initialise CAS device
          
          ENDGEN

          LEA       BOOT_DEF,A4         Initialise BCBOOT device
          BSR.S     DEVINIT             
          LEA       PROCS,A1            Now initialise procs&fn's
          VECTOR    BP_INIT,A2
INITEND   MOVE.L    A5,A3
          MOVE.L    (A7)+,A0
          RTS
PRINTMSG  MOVE.W    UT_MTEXT,A2
          JMP       (A2)
          
* Initialise CAS and BCBOOT devices          
                    
DEVINIT   MOVEQ     #34,D1              allocate physical def block
          MOVEQ     #0,D2
          QDOS      MT.ALCHP
          TST.L     D0
          BNE.S     BSCINIT2
          LEA       8(A0),A2
          BSR.S     GETADDR             open routine
          BSR.S     GETADDR             close routine
          MOVE.L    A2,4(A0)            physical I/O routine
          MOVE.W    #$4EB8,(A2)+        JMP absolute short
          MOVE.W    IO_SERIO,(A2)+      to IO_SERIO
          BSR.S     GETADDR             pending I/O routine
          BSR.S     GETADDR             fetch character
          BSR.S     GETADDR             send character
          MOVE.W    #$4E75,(A2)         RTS
          QDOS      MT.LIOD             Link in driver
BSCINIT2  RTS

* Calculate and store the absolute addresses required for IO_SERIO

GETADDR   MOVE.W    (A4)+,A1
          ADDA.L    A5,A1               A5 = ROM start address
          MOVE.L    A1,(A2)+
          RTS
          
* The following commands are only implemented in the ROM version          
          
          GENIF     BSCROM = 1

          XREF      BUFFER
          XREF      CLOAD,CSAVE
          XREF      BCLOAD_T
          XREF      BCSAVE

* and so is the CAS device
          
          XREF      CAS_OPEN,CAS_CLOSE,CAS_PEND,CAS_FETCH,CAS_SEND

* Table for physical I/O subroutines          
          
CAS_DEF   DC.W      CAS_OPEN-ROMSTART
          DC.W      CAS_CLOSE-ROMSTART
          DC.W      CAS_PEND-ROMSTART
          DC.W      CAS_FETCH-ROMSTART
          DC.W      CAS_SEND-ROMSTART
          
          ENDGEN
          
* BCBOOT device for loading standard subroutines
          
BOOT_DEF  DC.W      BOOT_OP-ROMSTART    ; OPEN routine
          DC.W      BOOT_CL-ROMSTART    ; CLOSE routine
          DC.W      BOOT_PD-ROMSTART    ; Pending input
          DC.W      BOOT_FT-ROMSTART    ; Fetch character
          DC.W      BOOT_BP-ROMSTART    ; Send char (gives ERR.BP)

          XREF      BASPROG             ; ptr to stored BASIC subroutines
          
* BCBOOT Open: check name against 'BCBOOT' and create channel if matched.          
          
BOOT_OP   CMPI.L    #$00064243,(A0)
          BNE.S     BOOT_NF
          CMPI.L    #'BOOT',4(A0)
          BNE.S     BOOT_NF
          MOVEQ     #$1C,D1              
          MOVE.W    MM_ALCHP,A2
          JSR       (A2)
          BNE.S     BO_RTS
          LEA       BASPROG,A1
          MOVE.L    A1,$18(A0)
BO_RTS    RTS
BOOT_NF   MOVEQ     #ERR.NF,D0
          RTS

* Close: simply release channel def block          
          
BOOT_CL   MOVE.W    MM_RECHP,A2
          JMP       (A2)
          
* Pending input: check for EOF          
          
BOOT_PD   MOVE.L    $18(A0),A1          ptr to next character
          MOVE.B    (A1)+,D1            null char means EOF
          BEQ.S     BOOT_EOF
          MOVEQ     #0,D0
          RTS
BOOT_EOF  MOVEQ     #ERR.EF,D0
          RTS

* Fetch next character
          
BOOT_FT   BSR       BOOT_PD             Pending routine returns next char
          BNE       BO_RTS
          MOVE.L    A1,$18(A0)          If OK, advance pointer
          RTS

* Send character: returns ERR.BP since this is a read-only device
          
BOOT_BP   MOVEQ     #ERR.BP,D0
          RTS
          
* The following commands are NOT implemented in the RAM version
* but in order to keep the definition table consistent we'll define them here
* to give ERR.NI

          GENIF     BSCROM = 0
          
BUFFER:
CLOAD:
CSAVE:
BCLOAD_T:
BCSAVE:
          MOVEQ     #ERR.NI,D0
          RTS
          
          ENDGEN
                    
* BCLOAD and BCMERGE are in another module          
          
          XREF      BCLOAD
          XREF      BCMERGE          
 
* Procedure and function definitions
 
PROCS     DC.W      16                  number of procs
          DC.W      BUFFER-*
          DC.B      6,'BUFFER'
          DC.W      CLOAD-*
          DC.B      5,'CLOAD'
          DC.W      CSAVE-*
          DC.B      5,'CSAVE'
          DC.W      BCLOAD_T-*
          DC.B      8,'BCLOAD_T'
          DC.W      BCLOAD-*
          DC.B      6,'BCLOAD'
          DC.W      BCMERGE-*
          DC.B      7,'BCMERGE'
          DC.W      BCSAVE-*
          DC.B      6,'BCSAVE'
          DC.W      BCBOOT-*
          DC.B      6,'BCBOOT'
          DC.W      CURSON-*
          DC.B      7,'CURS_ON'
          DC.W      CURSOFF-*
          DC.B      8,'CURS_OFF'
          DC.W      BREAK_ON-*
          DC.B      8,'BREAK_ON'
          DC.W      BREAK_OFF-*
          DC.B      9,'BREAK_OFF'
          DC.W      SOUND-*
          DC.B      7,'BCSOUND'
          DC.W      GETLINE-*
          DC.B      7,'GETLINE'
          DC.W      PUTLINE-*
          DC.B      7,'PUTLINE'
          DC.W      PX_ASPRT-*
          DC.B      8,'PX_ASPRT'
          DC.W      0                   end of procs
          DC.W      22                  number of fns
          DC.W      WWIDTH-*
          DC.B      6,'WWIDTH'
          DC.W      WHEIGHT-*
          DC.B      7,'WHEIGHT'
          DC.W      HSIZE-*
          DC.B      5,'HSIZE'
          DC.W      VSIZE-*
          DC.B      5,'VSIZE'
          DC.W      HPOS-*
          DC.B      4,'HPOS'
          DC.W      VPOS-*
          DC.B      4,'VPOS'
          DC.W      VAL-*
          DC.B      3,'VAL'
          DC.W      SGN-*
          DC.B      3,'SGN'
          DC.W      LEFT$-*
          DC.B      5,'LEFT$'
          DC.W      MID$-*
          DC.B      4,'MID$'
          DC.W      RIGHT$-*
          DC.B      6,'RIGHT$'
          DC.W      ASC-*
          DC.B      3,'ASC'
          DC.W      ATN-*
          DC.B      3,'ATN'
          DC.W      LOG-*
          DC.B      3,'LOG'
          DC.W      SQR-*
          DC.B      3,'SQR'
          DC.W      USING$-*
          DC.B      6,'USING$'
          DC.W      BSC_CODE-*
          DC.B      8,'BSC_CODE'
          DC.W      SHIFT$-*
          DC.B      6,'SHIFT$'
          DC.W      SCREEN$-*
          DC.B      7,'SCREEN$'
          DC.W      FOPENIN-*
          DC.B      8,'FOPEN_IN'
          DC.W      FOPENNEW-*
          DC.B      9,'FOPEN_NEW'
          DC.W      IOSTATUS-*
          DC.B      8,'IOSTATUS'
          DC.W      0                   end of fns

* BCBOOT command loads standard subroutines
* and is equivalent to LOAD 'BCBOOT'.          
          
BCBOOT    LEA       BOOTNAME,A0
          MOVEQ     #-1,D1
          QDOS      IO.OPEN
          TST.L     D0
          BNE.S     BCB_RTS
          MOVE.L    A0,$84(A6)
          CLR.W     $88(A6)
          CLR.B     $8A(A6)
          MOVE.W    #10,$8C(A6)
          SF        $6D(A6)
BCB_RTS   RTS

          SECTION   MSG
BOOTNAME  STRING$   {'BCBOOT'}
          SECTION   CODE
          
* Get a file name from S*BASIC (null, string or SB name)
* Entry: A3, A5 pointer to parameters
* Exit: A0,A1 ptr to string on RI stack, D0 error code

GET_NAME  CMPA.L    A3,A5
          BNE.S     GTFNAM_1
          MOVE.L    BV_RIP(A6),A1
          SUBQ.W    #2,A1
          CLR.W     0(A6,A1.L)
          BRA.S     GTFNAM_E
GTFNAM_1  MOVEQ     #$0F,D0
          AND.B     1(A6,A3.L),D0
          SUBQ.B    #1,D0
          BNE.S     GTFNAM_2
          MOVE.L    A5,-(A7)
          LEA       8(A3),A5
          MOVE.W    CA_GTSTR,A2
          JSR       (A2)
          MOVE.L    (A7)+,A5
          TST.L     D0
          BNE.S     GTFNAM_R
          BRA.S     GTFNAM_E
GTFNAM_2  MOVEQ     #0,D0
          MOVE.W    2(A6,A3.L),D0
          BLT.S     ERR_BN
          LSL.L     #3,D0
          MOVE.L    BV_NTBAS(A6),A0
          ADDA.L    D0,A0
          MOVE.W    2(A6,A0.L),A0
          ADDA.L    BV_NLBAS(A6),A0
          MOVEQ     #3,D1
          ADD.B     0(A6,A0.L),D1
          BCLR      #0,D1
          MOVE.W    D1,-(A7)
          MOVE.W    BV_CHRIX,A2
          JSR       (A2)
          MOVE.L    BV_RIP(A6),A1
          SUBA.W    (A7)+,A1
          MOVE.L    A1,A2
          MOVEQ     #0,D1
          MOVE.B    0(A6,A0.L),D1
          MOVE.W    D1,0(A6,A2.L)
GTFNAMLP  MOVE.B    1(A6,A0.L),2(A6,A2.L)
          ADDQ.W    #1,A0
          ADDQ.W    #1,A2
          SUBQ.B    #1,D1
          BNE.S     GTFNAMLP
GTFNAM_E  MOVE.L    A1,BV_RIP(A6)
          MOVE.L    A1,A0
          MOVEQ     #0,D0
GTFNAM_R  RTS
ERR_BN    MOVEQ     #ERR.BN,D0
          RTS

* Get a channel parameter, use default #1          
          
GET_CHAN  MOVEQ     #1,D0
          CMPA.L    A3,A5
          BEQ.S     GC_DEFLT
          TST.B     1(A6,A3.L)
          BPL.S     GC_DEFLT
          MOVE.L    A5,-(A7)
          LEA       8(A3),A5
          VECTOR    CA_GTINT,A2
          MOVE.L    (A7)+,A5
          BNE.S     GC_END
          ADDQ.W    #8,A3
          MOVE.W    0(A6,A1.L),D0
          ADDQ.L    #2,BV_RIP(A6)
GC_DEFLT  MOVE.L    BV_CHBAS(A6),A1
          MULU      #$28,D0
          ADDA.L    D0,A1
          CMPA.L    BV_CHP(A6),A1
          BHS.S     GC_NOTF
          MOVE.L    0(A6,A1.L),A0
          MOVE.W    A0,D0
          BLT.S     GC_NOTF
          MOVEQ     #0,D0
GC_END    RTS
GC_NOTF   MOVEQ     #ERR.NO,D0
          RTS

* Called from proc/fns which require no parameters, exits with ERR.BP otherwise          
          
NO_PARAM  CMPA.L    A3,A5
          BEQ.S     OP_OK
          ADDQ.W    #4,A7               NOTE: Exits from calling routine!
          MOVEQ     #ERR.BP,D0
OP_OK     RTS

* Get 1 integer, float or string parameter

GET1INT   MOVE.W    CA_GTINT,A2
          BRA.S     GT_JSR
GET1FP    MOVE.W    CA_GTFP,A2
          BRA.S     GT_JSR
GET1STR   MOVE.W    CA_GTSTR,A2
GT_JSR    JSR       (A2)
          BNE.S     GT_END
          SUBQ.W    #1,D3
          BEQ.S     GT_END
          MOVEQ     #ERR.BP,D0
GT_END    RTS

* Store an integer in D1 on the RI stack

STO_INT   MOVE.W    D1,-(A7)
          MOVEQ     #2,D1
          VECTOR    BV_CHRIX,A2
          MOVE.L    BV_RIP(A6),A1
          SUBQ.W    #2,A1
          MOVE.W    (A7)+,(A6,A1.L)
          MOVE.L    A1,BV_RIP(A6)
          MOVEQ     #3,D4
          MOVEQ     #0,D0
          RTS

* Reclaim a string from the RI stack          
          
RECSTRG   MOVE.L    BV_RIP(A6),A1
          MOVE.L    D0,-(A7)
          MOVEQ     #3,D0
          ADD.W     (A6,A1.L),D0
          BCLR      #0,D0
          ADD.L     D0,BV_RIP(A6)
          MOVE.L    (A7)+,D0
          RTS

* HSIZE, VSIZE, HPOS, VPOS functions return current values for text position
* 20170322: Added WWIDTH and WHEIGHT functions which return window size in 
* pixels, so HG and VG variables can be set dynamically in GOTO 20 sub

WWIDTH    MOVEQ     #0,D7               D7 = value in buffer to fetch
          BRA.S     PXENQ
WHEIGHT   MOVEQ     #2,D7
PXENQ     MOVEQ     #SD.PXENQ&$FF,D6    D6 = SD.PXENQ or SD.CHENQ operation
          BRA.S     GET_ENQ
HSIZE     MOVEQ     #0,D7
          BRA.S     CHENQ
VSIZE     MOVEQ     #2,D7
          BRA.S     CHENQ
HPOS      MOVEQ     #4,D7
          BRA.S     CHENQ
VPOS      MOVEQ     #6,D7
CHENQ     MOVEQ     #SD.CHENQ&$FF,D6
GET_ENQ   BSR       GET_CHAN
          BNE.S     ENQ_RTS             get channel, default #1
          BSR       NO_PARAM            no further parameters
          MOVE.L    (A6),A1             20170417: Use SB buffer not stack!
          MOVEQ     #-1,D3
          TRAP      #4                  .. to avoid absolute pointers!
          MOVE.L    D6,D0               D6 holds operation to perform
          TRAP      #3
          TST.L     D0
          BNE.S     ENQ_RTS
          MOVE.L    (A6),A1             base of buffer
          ADDA.W    D7,A1               add offset for result
          MOVE.W    (A6,A1.L),D1        get return value from buffer
          BSR       STO_INT             and store it on RI stack
ENQ_RTS   RTS

* CURS_ON and CURS_OFF functions

CURSON    MOVEQ     #SD.CURE&$FF,D7
          BRA.S     CURS_PRM
CURSOFF   MOVEQ     #SD.CURS&$FF,D7
CURS_PRM  BSR       GET_CHAN
          BSR       NO_PARAM
          MOVEQ     #-1,D3
CURS_AGN  MOVE.B    D7,D0
          TRAP      #3
          CMPI.L    #ERR.OR,D0          Trap ERR.OR (pending newline)
          BNE.S     CURS_RTS
          MOVE.L    (A6),A1             buffer for SD.CHENQ
          TRAP      #4
          QDOS      SD.CHENQ            This activates pending newline
          MOVE.L    (A6),A1
          MOVEM.W   (A6,A1.L),D1-D2     position cursor
          QDOS      SD.POS
          TST.L     D0
          BEQ       CURS_AGN            Loop back for another try
CURS_RTS  RTS

* VAL function - returns numeric value of string
* This simply evaluates the argument as an FP value, returning 0 if this fails.

VAL       BSR       GET1FP
          BEQ.S     VAL_END
          CMPI.L    #ERR.XP,D0
          BNE.S     VAL_END
          MOVE.L    BV_RIP(A6),A1
          SUBQ.W    #6,A1
          CLR.W     0(A6,A1.L)          if ERR.XP, simply return zero.
          CLR.L     2(A6,A1.L)
          MOVEQ     #0,D0
VAL_END   MOVE.L    A1,BV_RIP(A6)
          MOVEQ     #2,D4
          RTS

* The SGN function (why isn't this built-in?)          
          
SGN       BSR       GET1FP
          BNE.S     SGN_END
          MOVEQ     #-1,D1
          TST.L     2(A6,A1.L)
          BLT.S     SGN_3
          BEQ.S     SGN_2
          ADDQ.W    #1,D1
SGN_2     ADDQ.W    #1,D1
SGN_3     MOVE.W    D1,4(A6,A1.L)
          ADDQ.L    #4,BV_RIP(A6)
          MOVEQ     #3,D4
SGN_END   RTS

* Get parameters for LEFT$, MID$ and RIGHT$ functions
* Evaluates string and then one or two numeric arguments
* Returns: A1 pointing to start of string, D1 value of second argument
*          D3 holds 2 for one numeric argument, 4 for two numeric args

GET_PARAM CMPA.L    A3,A5
          BEQ.S     ERR_BP1
          MOVE.L    A5,-(A7)
          LEA       8(A3),A5
          VECTOR    CA_GTSTR,A2         First argument is a string
          MOVE.L    (A7)+,A5
          BNE.S     GP_END
          MOVE.W    0(A6,A1.L),D7       Get string length
          ADDQ.W    #8,A3               
          VECTOR    CA_GTINT,A2         Next argument(s) are numeric
          BNE.S     GP_END
          CMPI.W    #2,D3               ... at most two!
          BGT.S     ERR_BP1
          LSL.W     #1,D3               ... and at least one
          BEQ.S     ERR_BP1
          MOVE.W    0(A6,A1.L),D1       ... and must not be negative
          BLT.S     ERR_OR
          ADDA.W    D3,A1               Skip past 2nd and possibly 3rd arg
          ADDQ.W    #2,A1               Point to start of string (past length)
          MOVEQ     #3,D0
          ADD.W     D7,D0
          BCLR      #0,D0               String size, including length word
          ADD.W     D3,D0               Add size of numeric args
          ADD.L     D0,BV_RIP(A6)       ... and reclaim this from RI stack
          MOVEQ     #0,D0
GP_END    RTS
ERR_OR    MOVEQ     #-4,D0
          RTS

* LEFT$(a$,n) returns n chars from start of string, but never more than length
          
LEFT$     BSR.S     GET_PARAM
          BNE.S     LS_END
          SUBQ.W    #2,D3               Only one numeric paramater allowed
          BEQ.S     STK_STR
ERR_BP1   MOVEQ     #-15,D0
LS_END    RTS

* MID$(a$,m,n) returns n characters starting at m
* if n is omitted, returns substring from m to end of string

MID$      BSR.S     GET_PARAM
          BNE.S     LS_END
          SUBQ.W    #1,D1               m must be >0
          BLT.S     ERR_OR
          SUB.W     D1,D7               String length will be shortened by m
          BGE.S     MS_SECND
          MOVEQ     #0,D7               if m>LEN(a$), result will be empty
          MOVE.W    -2(A6,A1.L),D1      .. in which case m=LEN(a$)
MS_SECND  MOVE.W    #$7FFF,D2           if n omitted, assume 32767
          SUBQ.W    #2,D3               (we'll adjust this later)
          BEQ.S     MS_STRING
          MOVE.W    -4(A6,A1.L),D2      Else, get value of n
          BLT.S     ERR_OR              .. which must be >0
MS_STRING ADDA.W    D1,A1               Point to start of substring
          MOVE.W    D2,D1
STK_STR   CMP.W     D7,D1               D7 = max len of resulting substring
          BLE.S     SS_DEST
          MOVE.W    D7,D1               ensure n is within substring length
          BRA.S     SS_DEST
          
* RIGHT$(a$,n) returns n characters from end of string
          
RIGHT$    BSR       GET_PARAM
          BNE.S     LS_END
          SUBQ.W    #2,D3
          BNE.S     ERR_BP1
          ADDA.W    D7,A1               Point past string
          CMP.W     D7,D1
          BLE.S     RS_DEST
          MOVE.W    D7,D1               ensure n is within string length
RS_DEST   SUBA.W    D1,A1               Point to first char of substring

* Finish up - shuffle string at (A6,A1.L) into right position on RI stack

SS_DEST   LEA       0(A1,D1.W),A2       A2 now points past string
          MOVE.L    BV_RIP(A6),A1       Get top of RI stack
          MOVE.W    D1,D2               Copy string length to D2
          BTST      #0,D1               
          BEQ.S     SS_COPY1            if length is even, go ahead
          SUBQ.W    #1,A1               else, one extra byte required
          
* This handles the case of RIGHT$ where original length was even but new length is odd
* In this case, we have to move the string down rather than up!
          
          CMPA.L    A1,A2               Old end is above new end?
          BLE.S     SS_COPY1            No, do copy from end down (moves string up)
          SUBA.W    D1,A1               else, point to start of old and new string
          SUBA.W    D1,A2
          BRA.S     SS_COPY2            .. and start copy from start up
SS_LOOP   SUBQ.W    #1,A1               
          SUBQ.W    #1,A2
          MOVE.B    0(A6,A2.L),0(A6,A1.L)
SS_COPY1  DBF       D1,SS_LOOP
          BRA.S     SS_END
SS_LOOP2  MOVE.B    0(A6,A2.L),0(A6,A1.L)
          ADDQ.W    #1,A1
          ADDQ.W    #1,A2
SS_COPY2  DBF       D1,SS_LOOP2
          SUBA.W    D2,A1
SS_END    SUBQ.W    #2,A1
          MOVE.W    D2,0(A6,A1.L)       store length of string
          MOVE.L    A1,BV_RIP(A6)
          MOVEQ     #1,D4               signal 'string result'
          MOVEQ     #0,D0
          RTS

* ATN, LOG and SQR functions (equivalent to ATAN, LN and SQRT)          
          
ATN       MOVEQ     #$24,D7
          BRA.S     DO_FP
LOG       MOVEQ     #$2A,D7
          BRA.S     DO_FP
SQR       MOVEQ     #$28,D7
DO_FP     BSR       GET1FP
          BNE.S     FP_END
          MOVE.W    D7,D0
          MOVEQ     #0,D7
          VECTOR    RI_EXEC,A2
          MOVE.L    A1,BV_RIP(A6)
          MOVEQ     #2,D4
FP_END    RTS

* ASC function (equiv. CODE)

ASC       BSR       GET1STR
          BNE.S     FP_END
          MOVE.W    (A6,A1.L),D1
          BEQ.S     ST_ASC
          MOVEQ     #0,D1
          MOVE.B    2(A6,A1.L),D1
ST_ASC    BSR       RECSTRG
          BRA       STO_INT

* Locate the scheduler task and storage area     
          
SV.SHLST  EQU       $40          
          
GET_SCHD  LEA       SCHDTASK,A1         
GETSCHD1  QDOS      MT.INF
          LEA       SV.SHLST(A0),A0     scheduler linked list
GS_LOOP   MOVE.L    (A0),D0
          BEQ.S     GS_NOTF
          MOVE.L    D0,A0
          CMPA.L    4(A0),A1
          BNE       GS_LOOP             loop until our entry found
          MOVEQ     #0,D0
          RTS
GS_NOTF   MOVEQ     #ERR.NF,D0          if not, return error
          RTS

* BREAK_OFF disables BREAK as required by subroutine 280 - BREAK_ON re-enables         
          
BREAK_OFF MOVEQ     #-1,D7              
          BRA.S     BRK_1
BREAK_ON  MOVEQ     #0,D7
BRK_1     BSR.S     GET_SCHD
          BNE.S     BRK_END
          MOVE.B    D7,BREAKDIS(A0)     just store the flag in storage area
          MOVEQ     #0,D0
BRK_END   RTS

* This scheduler task does the actual work of disabling BREAK
* NOTE: The original code assumed that SuperBASIC's BV area immediately
* followed its job header, which could be found at SV.BASIC... 
* ...which is no longer true for SBASIC! Hence, the wrong location was set!
* It is now avoided by looking up the header of job 0, peeking the value of A6
* ...which always points to the BV area.
* There is currently one flaw: with MultiBASICs, the routine can't tell which
* interpreter's BREAK should be disabled! So currently it only works on job 0.

SV.JBBAS  EQU       $68
BV.BRK    EQU       $8F
JB.STAT   EQU       $14
JB.A6     EQU       $58

SCHDTASK  TST.B     BREAKDIS+$10(A3)    A3 = start of storage area -$10
          BEQ.S     RTS1                leave BREAK alone
          MOVE.L    SV.JBBAS(A6),A0     get pointer to start of job table
          MOVE.L    (A0),A0             get start of job 0's header
          MOVE.L    JB.A6(A0),A1        ... and value for A6
          TAS       BV.BRK(A1)          reset any BREAK flag
          TST.W     JB.STAT(A0)         job suspended?
          BGE.S     RTS1                yes, exit      
          MOVE.W    #-2,JB.STAT(A0)     ensure interrupt code can't break I/O
RTS1      RTS

ERR_BP2   MOVEQ     #ERR.BP,D0
          RTS

* USING$(CT,CN,SR) returns formatted representation of SR 
* using CN decimals and total of CT characters
          
USING$    MOVEQ     #$18,D7
          ADD.L     A3,D7
          SUB.L     A5,D7
          BNE       ERR_BP2
          SUBQ.W    #8,A5
          VECTOR    CA_GTINT,A2
          ADDQ.W    #8,A5
          BNE.S     RTS1
          LEA       16(A3),A3
          VECTOR    CA_GTFP,A2
          BNE.S     RTS1
          MOVEQ     #0,D1
          MOVE.W    6(A6,A1.L),D1
          BLT.S     ERR_OR2
          VECTOR    BV_CHRIX,A2
          MOVE.L    BV_RIP(A6),A1
          MOVEQ     #0,D4
          MOVE.W    0(A6,A1.L),D1
          MOVE.L    2(A6,A1.L),D2
          MOVEM.W   6(A6,A1.L),D4-D5
          LEA       10(A1),A4
          MOVEQ     #2,D0
          SUB.L     D4,D0
          BCLR      #0,D0
          BEQ.S     SET_RIP
          SUBQ.W    #1,A4
SET_RIP   ADDA.L    D0,A1
          MOVE.W    D1,0(A6,A1.L)
          MOVE.L    D2,2(A6,A1.L)
          MOVE.W    D4,6(A6,A1.L)
          MOVE.W    D5,D0
          BEQ.S     U_SPACES
ERR_OR2   BLT       ERR_OR
          SUB.W     D0,D4
          SUBQ.W    #1,D4
          BLE       U_FILL
U_ZEROS   SUBQ.W    #1,A4
          MOVE.B    #'0',0(A6,A4.L)
          DBF       D0,U_ZEROS
          MOVE.B    #'.',0(A6,A4.L)
          MOVE.L    A4,A5
U_SPACES  MOVE.L    A4,A0
          MOVE.W    D4,D0
          BEQ.S     TST_SIGN
U_SPC_LP  SUBQ.W    #1,A0
          MOVE.B    #$20,0(A6,A0.L)
          SUBQ.W    #1,D0
          BNE.S     U_SPC_LP
TST_SIGN  MOVEQ     #0,D6
          TST.L     D2
          BGE.S     U_POS
          MOVEQ     #$14,D0
          VECTOR    RI_EXEC,A2
          MOVEQ     #-1,D6
U_POS     MOVE.W    D5,D1
          NEG.W     D1
          SUBQ.W    #8,A1
          MOVE.L    #$08045000,2(A6,A1.L)
          CLR.W     6(A6,A1.L)
          MOVE.W    D1,0(A6,A1.L)
          MOVE.W    RI_EXEC,A2
          MOVEQ     #8,D0
          JSR       (A2)
          MOVEQ     #$30,D0
          JSR       (A2)
          TST.W     0(A6,A1.L)
          BEQ.S     U_ROUND
          SUBQ.W    #1,0(A6,A1.L)
U_ROUND   MOVEQ     #$0A,D0
          JSR       (A2)
          MOVE.W    #$081F,D0
          SUB.W     0(A6,A1.L),D0
          BLT       U_FILL
          SUBQ.W    #6,A1
          MOVE.W    6(A6,A1.L),0(A6,A1.L)
          MOVE.L    8(A6,A1.L),D2
          CMPI.W    #$20,D0
          BLT.S     U_SHIFT
          MOVEQ     #0,D2
U_SHIFT   ASR.L     D0,D2
          MOVE.L    D2,D1
          MOVE.L    D2,D3
          ASL.L     D0,D2
          BNE.S     U_TRUNC
          CLR.W     0(A6,A1.L)
U_TRUNC   MOVE.L    D2,2(A6,A1.L)
          MOVEQ     #$0C,D0
          JSR       (A2)
          SUBQ.W    #1,D4
          BLT       U_FILL
U_INT_LP  MOVEQ     #0,D0
          SWAP      D1
          MOVE.W    D1,D0
          DIVU      #10,D0
          SWAP      D0
          MOVE.W    D0,D1
          SWAP      D1
          DIVU      #10,D1
          MOVE.W    D1,D0
          SWAP      D1
          EXG       D0,D1
          ADDI.B    #$30,D0
          SUBQ.W    #1,A4
          MOVE.B    D0,$00(A6,A4.L)
U_INT_P   TST.L     D1
          DBEQ      D4,U_INT_LP
          BNE.S     U_FILL
          MOVE.L    $02(A6,A1.L),D1
          LSL.L     #1,D1
          MOVE.W    #$0800,D0
          SUB.W     $00(A6,A1.L),D0
          CMPI.W    #$0020,D0
          BCS.S     U_FR_SHI
          MOVEQ     #$00,D1
U_FR_SHI  LSR.L     D0,D1
          BRA.S     U_FRAC_P
U_FRAC_LP MOVE.W    D1,D0
          MULU      #10,D0
          SWAP      D1
          MULU      #10,D1
          SWAP      D0
          ADD.W     D0,D1
          SWAP      D0
          SWAP      D1
          MOVEQ     #0,D2
          ADDX.B    D1,D2
          BEQ.S     MAKE_ASC
          ST        D3
MAKE_ASC  ADDI.B    #'0',D2
          ADDQ.W    #1,A5
          MOVE.B    D2,0(A6,A5.L)
          MOVE.W    D0,D1
U_FRAC_P  DBF       D5,U_FRAC_LP
          AND.L     D3,D6
          BEQ.S     USING_END
          TST.W     D4
          BLE.S     U_FILL
          MOVE.B    #'-',-1(A6,A4.L)
USING_END ADDQ.W    #6,A1
          MOVE.L    A1,BV_RIP(A6)
          MOVEQ     #1,D4
          MOVEQ     #0,D0
          RTS
U_FILL    LEA       8(A1),A4
          MOVE.W    -2(A6,A4.L),D0
          BEQ.S     USING_END
U_FILL_LP MOVE.B    #'*',0(A6,A4.L)
          ADDQ.W    #1,A4
          SUBQ.W    #1,D0
          BNE.S     U_FILL_LP
          BRA.S     USING_END

* BSC_CODE(a$) returns BASICODE numeric code of string
* This is basically the same as ASC(a$) but caters for special codes
* (notably cursor codes and CR/LF swap)
* Also returns uppercase ASCII codes for lowercase chars         
          
BSC_CODE  BSR       ASC
          BNE.S     CODE_END
          MOVE.W    (A6,A1.L),D1
          LEA       KEY_TABLE,A0
KEY_LOOP  MOVE.W    (A0)+,D2
          BEQ.S     KEY_UP
          CMP.B     D1,D2
          BNE.S     KEY_LOOP
          LSR.W     #8,D2
          MOVE.B    D2,D1
          EXT.W     D1
KEY_UP    CMPI.W    #$60,D1
          BLT.S     KEY_END
          CMPI.W    #$7F,D1
          BGE.S     KEY_END
          SUBI.W    #$20,D1
KEY_END   MOVE.W    D1,0(A6,A1.L)
CODE_END  RTS

* Key translation table

KEY_TABLE DC.B      13,10,28,$C0,29,$C8,30,$D8,31,$D0,127,$C2,127,$CA
          DC.B      -1,$E8,-2,$EC,-3,$F0,-4,$F4,-5,$F8 ; F1-F5
          DC.B      -6,$EA,-7,$EE,-8,$F2,-9,$F6,-10,$FA,0,0 ; F1-F5 shifted

* SHIFT$(a$) returns uppercase representation of a$

SHIFT$    BSR       GET1STR
          BNE.S     CODE_END
          MOVE.W    0(A6,A1.L),D2
          BEQ.S     SH_END
          ADDQ.W    #2,A1
SH_LOOP   MOVE.B    0(A6,A1.L),D1
          CMPI.B    #$60,D1
          BCS.S     SH_NEXT
          CMPI.B    #$7F,D1
          BCC.S     SH_NEXT
          SUBI.B    #$20,D1
          MOVE.B    D1,0(A6,A1.L)
SH_NEXT   ADDQ.W    #1,A1
          SUBQ.W    #1,D2
          BNE.S     SH_LOOP
SH_END    MOVEQ     #1,D4
          RTS

* BCSOUND duration,pitch          
          
SOUND     VECTOR    CA_GTINT,A2
          BNE.S     SOUND_END
          SUBQ.W    #2,D3
          BNE.S     ERR_BP3
          ADDQ.L    #4,BV_RIP(A6)
          MOVE.W    0(A6,A1.L),D3
          BLT.S     SOUND_END
          MOVE.W    2(A6,A1.L),D1
TST_LOWER CMPI.W    #29,D1
          BGE.S     TST_UPPER
          ADDI.W    #12,D1
          BRA.S     TST_LOWER
TST_UPPER CMPI.W    #88,D1
          BLE.S     DO_BEEP
          SUBI.W    #12,D1
          BRA.S     TST_UPPER
DO_BEEP   MOVE.W    #$0100,-(A7)
          CLR.L     -(A7)
          CLR.L     -(A7)
          MOVE.B    BEEP_TBL-29(PC,D1.W),(A7)
          MOVE.B    (A7),1(A7)
          MOVE.L    #$5555AAAA,-(A7)
          MOVE.W    #$0A08,-(A7)
          MOVE.L    A7,A3
          QDOS      MT.IPCOM
          MOVEQ     #-1,D1
          SUBA.L    A1,A1
          QDOS      MT.SUSJB
          LEA       NOSOUND,A3
          QDOS      MT.IPCOM
          ADDA.W    #16,A7
          MOVEQ     #0,D0
SOUND_END RTS
ERR_BP3   MOVEQ     #-15,D0
          RTS
BEEP_TBL  DC.B      255,240,226,213,201,189,178,169,158,148,140,131,123
          DC.B      116,109,102,96,91,85,80,75,70,66,62,58,54,51,48,44,41
          DC.B      38,36,34,31,29,27,25,23,21,20,18,17,15,14,13,12,11,10
          DC.B      9,8,7,6,5,4,4,3,2,2,1,1
          DS.W      0
NOSOUND   DC.B      $0B,0,0,0,0,0,1

* SCREEN$ returns character at given position on screen
* NOTE: This currently works ONLY with screen in QL mode!

SCREEN$   BSR       GET_CHAN
          BNE.S     SCR_END
          VECTOR    CA_GTINT,A2
          BNE.S     SCR_END
          SUBQ.W    #2,D3
          BNE.S     ERR_BP3
          ADDQ.L    #4,BV_RIP(A6)
          MOVEM.W   0(A6,A1.L),D1-D2
          LEA       GET_SCR,A2
          MOVEQ     #-1,D3
          QDOS      SD.EXTOP
          MOVE.L    BV_RIP(A6),A1
          SUBQ.W    #2,A1
          MOVEQ     #0,D2
          TST.B     D1
          BEQ.S     SCR_STK
          SUBQ.W    #2,A1
          MOVEQ     #1,D2
          MOVE.B    D1,2(A6,A1.L)
SCR_STK   MOVE.W    D2,0(A6,A1.L)
          MOVE.L    A1,BV_RIP(A6)
          MOVEQ     #1,D4
SCR_END   RTS
GET_SCR   MOVEM.W   $26(A0),D4-D5
          MULU      D4,D1
          MULU      D5,D2
          MOVE.W    $1C(A0),D0
          SUB.W     D4,D0
          CMP.W     D0,D1
          BHI.S     SCR_EMPTY
          MOVE.W    $1E(A0),D0
          SUB.W     D5,D0
          CMP.W     D0,D2
SCR_EMPTY BHI       SCR_EMP2
          ADD.W     $18(A0),D1
          ADD.W     $1A(A0),D2
          LEA       $20000,A4
          LSL.L     #7,D2
          ADDA.L    D2,A4
          MOVEQ     #7,D7
          AND.B     D1,D7
          LSR.W     #3,D1
          ADD.W     D1,D1
          ADDA.W    D1,A4
          SUBA.W    #40,A7
          MOVE.L    A7,A5
          MOVE.W    #10,-(A7)
GET_ROW   MOVE.B    (A4)+,D0
          MOVE.B    (A4)+,D1
          MOVE.B    (A4)+,D2
          MOVE.B    (A4)+,D3
          MOVE.B    (A4)+,D4
          MOVE.B    (A4),D5
          MOVE.B    D7,D6
          BEQ.S     ST_ROW
SH_ROW    LSL.B     #1,D4
          ROXL.B    #1,D2
          ROXL.B    #1,D0
          LSL.B     #1,D5
          ROXL.B    #1,D3
          ROXL.B    #1,D1
          SUBQ.B    #1,D6
          BNE.S     SH_ROW
ST_ROW    MOVE.B    D0,(A5)+
          MOVE.B    D1,(A5)+
          MOVE.B    D2,(A5)+
          MOVE.B    D3,(A5)+
          ADDA.W    #123,A4
          BTST      #4,$42(A0)
          BEQ.S     GTROW_NXT
          ADDA.W    #128,A4
GTROW_NXT SUBQ.W    #1,(A7)
          BNE.S     GET_ROW
          ADDQ.W    #2,A7
          MOVEQ     #$60,D0
          AND.B     $42(A0),D0
          LSR.B     #3,D0
          MOVE.L    MASKTAB(PC,D0.W),D5
          MOVE.L    $2A(A0),A4
          BSR.S     CMP_CHRS
          TST.B     D1
          BNE.S     SCR_RTS
          MOVE.L    $2E(A0),A4
          BSR.S     CMP_CHRS
SCR_RTS   ADDA.W    #40,A7
          RTS
MASKTAB   DC.L      $FCFC0000,$FFFF0000,$FFFFF0F0,$FFFFFFFF
CMP_CHRS  MOVE.B    (A4)+,D1
          MOVE.B    (A4)+,D7
          ADD.B     D1,D7
CMP_LOOP  LEA       8(A7),A5
          MOVE.L    A4,A3
          BSR.S     CMP_ROW
          BEQ.S     NXT_ROWS
          CMP.L     D5,D0
          BNE.S     NXT_CHR
NXT_ROWS  MOVE.L    D0,D4
          MOVEQ     #7,D6
CMP_LP2   BSR.S     CMP_ROW
          EOR.L     D4,D0
          BNE.S     NXT_CHR
          DBF       D6,CMP_LP2
          RTS
NXT_CHR   ADDA.W    #9,A4
          ADDQ.B    #1,D1
          CMP.B     D7,D1
          BLS.S     CMP_LOOP
SCR_EMP2  MOVEQ     #0,D1
          MOVEQ     #0,D0
          RTS
CMP_ROW   MOVE.B    (A3)+,D0
          BTST      #6,$42(A0)
          BNE.S     DBL_WIDTH
          MOVE.B    D0,D2
          LSL.W     #8,D0
          MOVE.B    D2,D0
          SWAP      D0
          BRA.S     DO_COLOR
DBL_WIDTH MOVEQ     #7,D3
DBL_SHIFT BTST      D3,D0
          SNE       D2
          LSL.L     #2,D2
          DBF       D3,DBL_SHIFT
          MOVE.W    D2,D0
          LSR.W     #8,D2
          MOVE.B    D2,D0
          SWAP      D0
          SWAP      D2
          MOVE.B    D2,D0
          LSL.W     #8,D0
          MOVE.B    D2,D0
          SWAP      D0
DO_COLOR  MOVE.L    $3A(A0),D2
          MOVE.L    $3E(A0),D3
          EOR.L     D2,D3
          AND.L     D0,D3
          EOR.L     D2,D3
          MOVE.L    (A5)+,D0
          EOR.L     D3,D0
          AND.L     D5,D0
          RTS

* FOPEN_IN, FOPEN_NEW do OPEN_IN and OPEN_NEW as functions
* (of course TK2 has equivalent functions, but this was designed to run without it)
          
FOP_BP    MOVEQ     #ERR.BP,D0
FOP_RTS   RTS
FOPENIN   MOVEQ     #1,D7
          BRA.S     FOP_1
FOPENNEW  MOVEQ     #2,D7
FOP_1     MOVEQ     #16,D0
          ADD.L     A3,D0
          SUB.L     A5,D0
          BNE.S     FOP_BP
          TST.B     1(A6,A3.L)
          BPL.S     FOP_BP
          MOVEQ     #$28,D4
          BSR       GET_CHAN
          MOVE.L    A1,A4
          BEQ.S     FOP_CLOS
          CMPI.L    #ERR.NO,D0
          BNE       FOP_RTS
          MOVE.L    A4,D5
          SUB.L     BV_CHP(A6),D5
          BLT.S     FOP_2
          ADD.L     D4,D5
          MOVE.L    D5,D1
          MOVE.W    BV_CHRIX,A2
          JSR       $2C(A2)
          MOVE.L    BV_CHP(A6),A0
          MOVEQ     #-1,D0
FOP_LOOP  MOVE.L    D0,(A6,A0.L)
          ADDA.L    D4,A0
          SUB.L     D4,D5
          BGT       FOP_LOOP
          MOVE.L    A0,BV_CHP(A6)
          BRA.S     FOP_2
FOP_CLOS  QDOS      IO.CLOSE
          MOVE.L    #-1,(A6,A4.L)
FOP_2     BSR       GET_NAME
          BNE       FOP_RTS
          BSR       RECSTRG
          MOVEQ     #-1,D1
          MOVE.B    D7,D3
          TRAP      #4
          QDOS      IO.OPEN
          TST.L     D0
          BNE.S     FOP_FAIL
          MOVE.L    A4,A1
FOP_CLCH  CLR.L     (A6,A1.L)
          ADDQ.W    #4,A1
          SUBQ.W    #4,D4
          BGT       FOP_CLCH
          MOVE.L    A0,(A6,A4.L)
          MOVE.W    #80,$22(A6,A4.L)
          MOVEQ     #0,D1
          BRA.S     FOP_STK
FOP_FAIL  MOVEQ     #-1,D1
FOP_STK   BRA       STO_INT

* GETLINE$(#c,a$) gets line from file and assigns to a$, returning error status

GETLINE   BSR       GET_CHAN
          BNE       GTL_RTS
          MOVEQ     #8,D0
          ADD.L     A3,D0
          SUB.L     A5,D0
          BNE.S     GTL_BP
          SUBA.L    BV_NTBAS(A6),A3
          MOVEQ     #-1,D3
          MOVE.L    (A6),A1
GTL_LP    MOVE.L    8(A6),D2
          SUB.L     A1,D2
          TRAP      #4
          QDOS      IO.FLINE
          CMPI.L    #ERR.BO,D0
          BNE.S     GTL_TST
          MOVE.L    A1,4(A6)
          MOVEQ     #$7E,D1
          MOVE.W    BV_CHRIX,A2
          JSR       $1C(A2)
          BRA       GTL_LP
GTL_TST   MOVE.L    A1,-(A7)
          BSR.S     ST_STAT
          MOVE.L    (A7)+,A1
          TST.L     D4
          BEQ.S     GTL_TST2
          MOVE.L    (A6),A1
          ADDQ.W    #1,A1
GTL_TST2  MOVE.L    A1,D4
          SUB.L     (A6),D4
          SUBQ.W    #1,D4
          MOVEQ     #3,D1
          ADD.L     D4,D1
          BCLR      #0,D1
          MOVE.L    D1,-(A7)
          VECTOR    BV_CHRIX,A2
          MOVE.L    (A7)+,D1
          MOVE.L    BV_RIP(A6),A1
          SUBA.L    D1,A1
          MOVE.L    A1,BV_RIP(A6)
          MOVE.W    D4,(A6,A1.L)
          MOVE.L    (A6),A0
          BRA.S     GTL_COP1
GTL_COPY  MOVE.W    (A6,A0.L),(A6,A1.L)
          ADDQ.W    #2,A0
GTL_COP1  ADDQ.W    #2,A1
          SUBQ.W    #2,D1
          BGT       GTL_COPY
          ADDA.L    BV_NTBAS(A6),A3
          VECTOR    BP_LET,A2
GTL_RTS   RTS
GTL_BP    MOVEQ     #ERR.BP,D0
          RTS

ST_STAT   MOVE.L    D0,D4
          BSR       GET_SCHD
          BNE.S     STAT_END
          MOVE.L    D4,IOSTAT(A0)
STAT_END  RTS

* PUTLINE(#c,a$) puts line to channel c, returns error status

PUTLINE   BSR       GET_CHAN
          BNE.S     PTL_RTS
          BSR       GET1STR
          BNE.S     PTL_RTS
          BSR       RECSTRG
          MOVEQ     #-1,D3
          MOVE.W    (A6,A1.L),D2
          ADDQ.W    #2,A1
          TRAP      #4
          QDOS      IO.SSTRG
          TST.L     D0
          BNE.S     PTL_1
          MOVEQ     #$0A,D1
          QDOS      IO.SBYTE
PTL_1     BSR       ST_STAT
          MOVEQ     #0,D0
PTL_RTS   RTS

* IOSTATUS returns last error status

IOSTATUS  BSR       NO_PARAM
          BSR       GET_SCHD
          BNE.S     PTL_RTS
          MOVEQ     #-1,D1
          MOVE.L    IOSTAT(A0),D0
          BEQ.S     STAT_1
          CMPI.L    #ERR.EF,D0
          BNE.S     STK_STAT
          ADDQ.W    #1,D1
STAT_1    ADDQ.W    #1,D1
STK_STAT  BRA       STO_INT

* PX_ASPRT changes pixel aspect ratio on SMSQ/E (for graphics routines)
* This is for emulated environments with non-native screen resolutions
* Parameter is the pixel height/width ratio - 1.355 on native QL screen
* On QPC2 with modern resolutions you'll probably have square pixels so
* it should be set to 1.
* This should ONLY be called on SMSQ/E v3.00 or higher!

SYS_CLNK  EQU       $C4             pointer to CON linkage block
PT_ASPRT  EQU       $14A            FP value of pixel h/w ratio

PX_ASPRT  BSR       GET1FP          Get exactly 1 float
          BNE.S     PXA_RTS
          QDOS      MT.INF
          ROL.L     #8,D2
          CMPI.B    #'3',D2         check major version - should be >=3
          BLT.S     PXA_NI
          MOVE.L    SYS_CLNK(A0),D0 get ptr to CON linkage block
          BEQ.S     PXA_NI          ..just to be sure..
          MOVE.L    D0,A0
          MOVE.L    BV_RIP(A6),A1   now set PT_ASPRT
          MOVE.W    (A6,A1.L),PT_ASPRT(A0)
          MOVE.L    2(A6,A1.L),PT_ASPRT+2(A0)
          ADDQ.L    #6,BV_RIP(A6)   clean up RI stack
          MOVEQ     #0,D0
PXA_RTS   RTS
PXA_NI    MOVEQ     #ERR.NI,D0      sorry folks - this only works on SMSQ/E
          RTS
