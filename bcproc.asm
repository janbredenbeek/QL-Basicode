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
          
BCBOOT    LEA       BOOTNAME,A0         The device name
          MOVEQ     #-1,D1
          QDOS      IO.OPEN             Open a channel
          TST.L     D0
          BNE.S     BCB_RTS             ..oops..
          MOVE.L    A0,$84(A6)          Set BV.COMCH to new input
          CLR.W     $88(A6)             Clear BV.NXLIN
          CLR.B     $8A(A6)             .. and BV.NXSTM
          MOVE.W    #10,$8C(A6)         Signal 'LOAD' to BV.STOPN
          SF        $6D(A6)             .. and stop the program.
BCB_RTS   RTS

          SECTION   MSG
BOOTNAME  STRING$   {'BCBOOT'}
          SECTION   CODE
          
* Get a file name from S*BASIC (null, string or SB name)
* Entry: A3, A5 pointer to parameters
* Exit: A0,A1 ptr to string on RI stack, D0 error code

GET_NAME  CMPA.L    A3,A5               Any parameters?
          BNE.S     GTFNAM_1            Yes, evaluate them
          MOVE.L    BV_RIP(A6),A1       Else, put a null string on the RI stack
          SUBQ.W    #2,A1               (nasty: should have tested for space
          CLR.W     0(A6,A1.L)          first!)
          BRA.S     GTFNAM_E            Exit
GTFNAM_1  MOVEQ     #$0F,D0             Mask off the lower 4 bits of the
          AND.B     1(A6,A3.L),D0       argument type
          SUBQ.B    #1,D0               '1' means a string argument
          BNE.S     GTFNAM_2            Jump if not a string
          MOVE.L    A5,-(A7)            Save last argument's ptr
          LEA       8(A3),A5            We only want to get 1 string argument,
          MOVE.W    CA_GTSTR,A2         go get it
          JSR       (A2)
          MOVE.L    (A7)+,A5            Restore ptr past last argument
          TST.L     D0
          BNE.S     GTFNAM_R            Exit with any error
          BRA.S     GTFNAM_E            Else, finish up and return

* Not a string so it should be a name (or a numeric expression, but we'll
* exclude that by testing it's name list pointer)

GTFNAM_2  MOVEQ     #0,D0
          MOVE.W    2(A6,A3.L),D0       Get index into name table
          BLT.S     ERR_BN              It should have a name, else throw error
          LSL.L     #3,D0               Multiply by 8
          MOVE.L    BV_NTBAS(A6),A0
          ADDA.L    D0,A0               Point to the 'real' NT entry
          MOVE.W    2(A6,A0.L),A0       Offset into name list
          ADDA.L    BV_NLBAS(A6),A0     A0 now points to name
          MOVEQ     #3,D1               We need 2+length bytes for the name
          ADD.B     0(A6,A0.L),D1
          BCLR      #0,D1               ..but rounded up to an even number
          MOVE.W    D1,-(A7)            Save length
          MOVE.W    BV_CHRIX,A2         Reserve this space on the RI stack
          JSR       (A2)
          MOVE.L    BV_RIP(A6),A1       Get RI stack pointer
          SUBA.W    (A7)+,A1            Move down
          MOVE.L    A1,A2               Use A2 as running pointer
          MOVEQ     #0,D1               The name list entry has 1 byte for the
          MOVE.B    0(A6,A0.L),D1       length, get this into D1
          MOVE.W    D1,0(A6,A2.L)       Store this as a word on the RI stack
GTFNAMLP  MOVE.B    1(A6,A0.L),2(A6,A2.L) Now copy the name's characters
          ADDQ.W    #1,A0               (past the length byte/word)
          ADDQ.W    #1,A2
          SUBQ.B    #1,D1
          BNE.S     GTFNAMLP
GTFNAM_E  MOVE.L    A1,BV_RIP(A6)       Set new RI stack pointer
          MOVE.L    A1,A0               Leave the name pointer also at (A6,A0)
          MOVEQ     #0,D0               Succesful return
GTFNAM_R  RTS
ERR_BN    MOVEQ     #ERR.BN,D0          'Bad parameter' return
          RTS

* Get a channel parameter, use default #1          
          
GET_CHAN  MOVEQ     #1,D0               Use channel 1 by default
          CMPA.L    A3,A5               Any arguments?
          BEQ.S     GC_DEFLT            No, go use default
          TST.B     1(A6,A3.L)          Bit 7 is set with '#n' argument
          BPL.S     GC_DEFLT            if not, skip
          MOVE.L    A5,-(A7)
          LEA       8(A3),A5            Get next (and only that) number
          VECTOR    CA_GTINT,A2
          MOVE.L    (A7)+,A5
          BNE.S     GC_END              Skip if error
          ADDQ.W    #8,A3               Advance to next argument
          MOVE.W    0(A6,A1.L),D0       Get specified channel number
          ADDQ.L    #2,BV_RIP(A6)       Tidyup RI stack

* Now index into the channel table

GC_DEFLT  MOVE.L    BV_CHBAS(A6),A1     Base of channel table
          MULU      #$28,D0             Each entry = $28 bytes
          ADDA.L    D0,A1               Point to entry
          CMPA.L    BV_CHP(A6),A1       ..but first check if it's really there
          BHS.S     GC_NOTF
          MOVE.L    0(A6,A1.L),A0       Get channel ID (or -1 if closed)
          MOVE.W    A0,D0               Only check lower 16 bits of channel id!
          BLT.S     GC_NOTF             (because tag can be > 32767!)
          MOVEQ     #0,D0               Looks OK
GC_END    RTS
GC_NOTF   MOVEQ     #ERR.NO,D0          'channel not open'
          RTS

* Called from proc/fns which require no parameters, exits with ERR.BP otherwise          
          
NO_PARAM  CMPA.L    A3,A5
          BEQ.S     OP_OK
          ADDQ.W    #4,A7               NOTE: Exits from calling routine!
          MOVEQ     #ERR.BP,D0          'bad parameter'
OP_OK     RTS

* Get 1 integer, float or string parameter

GET1INT   MOVE.W    CA_GTINT,A2         Get integer
          BRA.S     GT_JSR
GET1FP    MOVE.W    CA_GTFP,A2          Get float
          BRA.S     GT_JSR
GET1STR   MOVE.W    CA_GTSTR,A2         Get string
GT_JSR    JSR       (A2)
          BNE.S     GT_END              If error, exit early
          SUBQ.W    #1,D3               There should be exactly 1 argument
          BEQ.S     GT_END
          MOVEQ     #ERR.BP,D0          else, 'bad parameter'.
GT_END    RTS

* Store an integer in D1 on the RI stack

STO_INT   MOVE.W    D1,-(A7)            Save value
          MOVEQ     #2,D1               Okay, we'll probably have enough space
          VECTOR    BV_CHRIX,A2         for two bytes, but just to be sure...
          MOVE.L    BV_RIP(A6),A1
          SUBQ.W    #2,A1
          MOVE.W    (A7)+,(A6,A1.L)     Now store the value
          MOVE.L    A1,BV_RIP(A6)       and set the RI stack pointer
          MOVEQ     #3,D4               Signal 'integer result'
          MOVEQ     #0,D0               Return OK
          RTS

* Reclaim a string from the RI stack
* NB: Leaves A1 pointing to the reclaimed string
          
RECSTRG   MOVE.L    BV_RIP(A6),A1       Get RI stack pointer
          MOVE.L    D0,-(A7)            Save D0
          MOVEQ     #3,D0               We'll need to reclaim 'length+2' bytes,
          ADD.W     (A6,A1.L),D0        or 'length+3' if length is odd.
          BCLR      #0,D0               Ensure even (total) length
          ADD.L     D0,BV_RIP(A6)       Reclaim this number of bytes
          MOVE.L    (A7)+,D0            Restore D0 on exit
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

CURSON    MOVEQ     #SD.CURE&$FF,D7     Cursor on
          BRA.S     CURS_PRM
CURSOFF   MOVEQ     #SD.CURS&$FF,D7     Cursor off
CURS_PRM  BSR       GET_CHAN            Get channel, default #1
          BSR       NO_PARAM            No further parameters
          MOVEQ     #-1,D3              Timeout
CURS_AGN  MOVE.B    D7,D0               Operation code
          TRAP      #3
          CMPI.L    #ERR.OR,D0          Trap ERR.OR (pending newline)
          BNE.S     CURS_RTS
          MOVE.L    (A6),A1             buffer for SD.CHENQ
          TRAP      #4
          QDOS      SD.CHENQ            This activates pending newline
          MOVE.L    (A6),A1
          MOVEM.W   4(A6,A1.L),D1-D2    position cursor
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
          
SGN       BSR       GET1FP              Get float
          BNE.S     SGN_END
          MOVEQ     #-1,D1              Assume -1 as result
          TST.L     2(A6,A1.L)          Test mantissa of argument
          BLT.S     SGN_3               Return -1 if negative
          BEQ.S     SGN_2               If zero, return 0
          ADDQ.W    #1,D1               Else, return 1
SGN_2     ADDQ.W    #1,D1
SGN_3     MOVE.W    D1,4(A6,A1.L)       Store result (overwriting float
          ADDQ.L    #4,BV_RIP(A6)       argument so reclaim 4 bytes)
          MOVEQ     #3,D4               Signal 'integer result'
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
          
ATN       MOVEQ     #$24,D7             FP calculator operation code to D7
          BRA.S     DO_FP
LOG       MOVEQ     #$2A,D7
          BRA.S     DO_FP
SQR       MOVEQ     #$28,D7
DO_FP     BSR       GET1FP              Get argument
          BNE.S     FP_END
          MOVE.W    D7,D0               FP operation code goes to D0
          MOVEQ     #0,D7               Signal 'single operation'
          VECTOR    RI_EXEC,A2          ..and do the math!
          MOVE.L    A1,BV_RIP(A6)       Set RI pointer
          MOVEQ     #2,D4               Signal 'float result'
FP_END    RTS

* ASC function (equiv. CODE)

ASC       BSR       GET1STR             Get string argument
          BNE.S     FP_END
          MOVE.W    (A6,A1.L),D1        ..and its length
          BEQ.S     ST_ASC              Null string returns zero as result
          MOVEQ     #0,D1
          MOVE.B    2(A6,A1.L),D1       Else, get first character's code
ST_ASC    BSR       RECSTRG             Clean up the stack
          BRA       STO_INT             And store the code

* Locate the scheduler task and storage area     
          
SV.SHLST  EQU       $40                 Offset from base of SV area
          
GET_SCHD  LEA       SCHDTASK,A1         Pointer to our scheduler's task
GETSCHD1  QDOS      MT.INF
          LEA       SV.SHLST(A0),A0     Pointer scheduler linked list
GS_LOOP   MOVE.L    (A0),D0             Get next entry
          BEQ.S     GS_NOTF             If at the end, we lost it somehow...
          MOVE.L    D0,A0
          CMPA.L    4(A0),A1            Is this ours?
          BNE       GS_LOOP             No, try next
          MOVEQ     #0,D0               We've found it.
          RTS
GS_NOTF   MOVEQ     #ERR.NF,D0          Oops, we lost it (Can happen?)
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

ERR_BP2   MOVEQ     #ERR.BP,D0          Another 'bad parameter' return
          RTS

* USING$(CT,CN,SR) returns a formatted representation of SR using CN decimals
* and a total of CT characters. If needed, the number will be padded to the
* left with spaces and padded to the right with zeros.
          
USING$    MOVEQ     #$18,D7             We need exactly 3 arguments
          ADD.L     A3,D7
          SUB.L     A5,D7
          BNE       ERR_BP2
          SUBQ.W    #8,A5               First, only evaluate the first two
          VECTOR    CA_GTINT,A2         .. as integer
          ADDQ.W    #8,A5
          BNE.S     RTS1
          LEA       16(A3),A3           Now get the third argument
          VECTOR    CA_GTFP,A2          .. as float
          BNE.S     RTS1
          MOVEQ     #0,D1               Get CT (total length of string result)
          MOVE.W    6(A6,A1.L),D1       (it's next after SR on the RI stack)
          BLT.S     ERR_OR2             .. and must be >=0
          VECTOR    BV_CHRIX,A2         Reserve this space on the RI stack
          MOVE.L    BV_RIP(A6),A1       Get RI stack pointer again

* At this point, the RI stack contains:
* 0(A6,A1.L): SR (float)
* 6(A6,A1.L): CT (int)
* 8(A6,A1.L): CN (int)
* We need to return a string result CT bytes long and reclaim these 10 bytes.
* Effectively, this means a reservation of CT-8 bytes, rounded up to the next
* even value. Since the stack expands downwards, this can also be written as
* adding an offset to A1 of '8-CT' bytes, rounded down to an even value.
* However, we do need an extra temporary space of 6 bytes on the stack, so
* we'll use '2-CT', rounded down.

          MOVEQ     #0,D4               Clear out D4.L
          MOVE.W    0(A6,A1.L),D1       Exponent of SR to D1
          MOVE.L    2(A6,A1.L),D2       Mantissa of SR to D2
          MOVEM.W   6(A6,A1.L),D4-D5    CT to D4, CN to D5
          LEA       10(A1),A4           A4 points past string result
          MOVEQ     #2,D0               
          SUB.L     D4,D0               D0 is set to 2-CT
          BCLR      #0,D0               .. rounded down to an even value ..
          BEQ.S     SET_RIP             .. and if it was odd, A4 is adjusted so
          SUBQ.W    #1,A4               .. it remains pointing past the string
SET_RIP   ADDA.L    D0,A1               Now get A1 in the right position
          MOVE.W    D1,0(A6,A1.L)       Store mantissa and exponend of SR again
          MOVE.L    D2,2(A6,A1.L)
          MOVE.W    D4,6(A6,A1.L)       Set length of result string
          MOVE.W    D5,D0               Get CN (number of decimals)
          BEQ.S     U_SPACES            Skip this if no decimals required
ERR_OR2   BLT       ERR_OR              .. but reject negative values
          SUB.W     D0,D4               Discount this from total length
          SUBQ.W    #1,D4               .. and include the decimal point
          BLE       U_FILL              CN>CT is silly so return asterisks
U_ZEROS   SUBQ.W    #1,A4
          MOVE.B    #'0',0(A6,A4.L)     Pre-set the fractional part to zeros
          DBF       D0,U_ZEROS
          MOVE.B    #'.',0(A6,A4.L)     Enter the decimal point
          MOVE.L    A4,A5               Save its position in A5
U_SPACES  MOVE.L    A4,A0               The rest of the result is to be filled
          MOVE.W    D4,D0               with D4 number of spaces
          BEQ.S     TST_SIGN            If nothing to fill, skip this
U_SPC_LP  SUBQ.W    #1,A0
          MOVE.B    #$20,0(A6,A0.L)     Pre-set the integer part to all spaces
          SUBQ.W    #1,D0
          BNE.S     U_SPC_LP
TST_SIGN  MOVEQ     #0,D6               Preset D6 to zero for 'positive number'
          TST.L     D2                  Now test the sign of the mantissa
          BGE.S     U_POS               Branch if indeed positive (or zero)
          MOVEQ     #$14,D0             Else, negate the value
          VECTOR    RI_EXEC,A2
          MOVEQ     #-1,D6              .. and set D6 to -1 as a flag for later

* We need to round up correctly before ASCIIfying
* Note: an extra call to BV_CHRIX would probably be nice here, just in case...

U_POS     MOVE.W    D5,D1               Get CN (number of decimals)
          NEG.W     D1                  Negate this
          SUBQ.W    #8,A1               Eight bytes required on the stack
          MOVE.L    #$08045000,2(A6,A1.L)
          CLR.W     6(A6,A1.L)          This puts the FP value 10 on the stack
          MOVE.W    D1,0(A6,A1.L)       .. followed by -CN
          MOVE.W    RI_EXEC,A2
          MOVEQ     #8,D0               RI.FLOAT to convert -CN to float
          JSR       (A2)
          MOVEQ     #$30,D0             Now TOS = 10^-CN
          JSR       (A2)
          TST.W     0(A6,A1.L)          These 3 lines do a quick division by 2
          BEQ.S     U_ROUND             .. unless it was already zero.
          SUBQ.W    #1,0(A6,A1.L)       .. so it's now (10^-CN)/2
U_ROUND   MOVEQ     #$0A,D0             Add this to SR to round up
          JSR       (A2)

* We now need to split the integer and fractional part of SR. But first, check
* if the integer part will fit into the number of digits allocated (in D4).
* Since the mantissa is normalised to 32 bits and the exponent is offset by
* $81F, the 'binary point' will be at bit ($81F-exponent). This also means that
* any number with exponent > $81F can no longer be represented as an integer,
* and we would have to resort to using E-format. In that case, and in case
* the integer part would be too large to fit in CT-CN digits (also including
* any decimal point and minus sign), we'll return a string of asterisks.
* The largest number that can be accurately represented as an integer is 2^31-1
* or 2147483647, a 10-digit number. This function will happily handle that, 
* unlike the standard QDOS/SMS conversion routines which already start using 
* E-format at 7 digits!

          MOVE.W    #$081F,D0           Find '$81F-exponent' in D0 - this
          SUB.W     0(A6,A1.L),D0       yields the position of the binary point
          BLT       U_FILL              If past bit 0, bail out
          SUBQ.W    #6,A1               Make room for another float
          MOVE.W    6(A6,A1.L),0(A6,A1.L) Copy the exponent
          MOVE.L    8(A6,A1.L),D2       Get mantissa in D2
          CMPI.W    #$20,D0             A 'shift number' of 32 or more means
          BLT.S     U_SHIFT             the integer part will be zero,
          MOVEQ     #0,D2               so set the mantissa to zero too
U_SHIFT   ASR.L     D0,D2               Now shift the mantissa D0 times right,
          MOVE.L    D2,D1               and copy the integer part to D1 and D3
          MOVE.L    D2,D3               (as a long integer)
          ASL.L     D0,D2               Shift the mantissa bits back so it's
          BNE.S     U_TRUNC             normalised again but with the fraction
          CLR.W     0(A6,A1.L)          bits discarded, also set the exponent
U_TRUNC   MOVE.L    D2,2(A6,A1.L)       to zero if the mantissa yields zero.
          MOVEQ     #$0C,D0             Find (SR-INT(SR)) and leave this on the
          JSR       (A2)                RI stack for later
          SUBQ.W    #1,D4               Decrement D4 for the DBEQ loop - but
          BLT       U_FILL              bail out if already 'out of space'

* The following loop converts the integer part of SR to ASCII, by dividing D1
* by 10 on each pass and storing the ASCIIfied remainder, until it either
* becomes zero or we run out of available space (in D4). We need to use
* 32 to 32-bit division, which requires two (32 to 16-bit) DIVU instructions
* and some fiddling with low and high words (yes it works on the 68000 too!)

U_INT_LP  MOVEQ     #0,D0               Clear out D0
          SWAP      D1
          MOVE.W    D1,D0               High word to D0
          DIVU      #10,D0              .. and divide this by 10
          SWAP      D0                  Put quotient into high word of D0
          MOVE.W    D0,D1               The remainder goes to high word of D1,
          SWAP      D1                  so we can divide this and the low word
          DIVU      #10,D1              of D1 to 16 bits without overflow
          MOVE.W    D1,D0               Complete the 32-bit quotient in D0
          SWAP      D1                  Now low word of D1 holds the remainder
          EXG       D0,D1               .. which is swapped with quotient in D0
          ADDI.B    #$30,D0             This is the next digit so ASCIIfy it
          SUBQ.W    #1,A4               Go left one location and store it
          MOVE.B    D0,$00(A6,A4.L)
U_INT_P   TST.L     D1                  Test the remaining value for zero
          DBEQ      D4,U_INT_LP         Loop until zero or D4 exhausted
          BNE.S     U_FILL              If still nonzero, it won't fit so exit
          MOVE.L    $02(A6,A1.L),D1     Get the mantissa of the fractional part
          LSL.L     #1,D1               It's now between $80000000-$FFFFFFFF
          MOVE.W    #$0800,D0           Exponent will be $800 for 0.5<=f<1,
          SUB.W     $00(A6,A1.L),D0     $7FF for 0.25<=f<0.5 and so on.
          CMPI.W    #$0020,D0           Now D0 will be 0 for 0.5<=f<1, 1 for
          BCS.S     U_FR_SHI            0.25<=f<0.5, and so on, until 31 for 
          MOVEQ     #$00,D1             f<2^-31, and treat smaller values as 0
U_FR_SHI  LSR.L     D0,D1               Shift right according to exponent
          BRA.S     U_FRAC_P

* the following loop shifts out the fractional digits by multiplying D1 by 10
* on each pass; the resulting overflow becomes the value of the next decimal.

U_FRAC_LP MOVE.W    D1,D0               Get LSW from D1
          MULU      #10,D0              .. and multiply LSW by 10
          SWAP      D1                  Get MSW from D1 ready
          MULU      #10,D1              .. and multiply MSW by 10
          SWAP      D0
          ADD.W     D0,D1               Add the overflow from the LSW
          SWAP      D0                  Swap D0 (LSW of result) back
          SWAP      D1                  Overflow from MSW now to D1.W
          MOVEQ     #0,D2               Transfer this to D2, including possible
          ADDX.B    D1,D2               overflow bit from the addition
          BEQ.S     MAKE_ASC            Unless this digit is zero, set D3 to
          ST        D3                  signal a nonzero digit encountered
MAKE_ASC  ADDI.B    #'0',D2             ASCIIfy this digit
          ADDQ.W    #1,A5               Go right one position
          MOVE.B    D2,0(A6,A5.L)       .. and store the new digit
          MOVE.W    D0,D1               Complete the result in D1 for next pass
U_FRAC_P  DBF       D5,U_FRAC_LP        Loop for all fractional digits
          AND.L     D3,D6               D3 will be nonzero with any nonzero
          BEQ.S     USING_END           digits; AND this with the negative flag
          TST.W     D4                  (this avoids results like -0.00). If no
          BLE.S     U_FILL              room for the sign, bail out (finally)
          MOVE.B    #'-',-1(A6,A4.L)    Enter the minus sign
USING_END ADDQ.W    #6,A1               Finish up by setting the RI stack
          MOVE.L    A1,BV_RIP(A6)       pointer to the resulting string
          MOVEQ     #1,D4               Signal 'string result'
          MOVEQ     #0,D0               No error; done
          RTS

* If SR won't fit into the format specified by CN and CT, just return asterisks

U_FILL    LEA       8(A1),A4            Point to string result (past length)
          MOVE.W    -2(A6,A4.L),D0      Get length word but exit with D0=0 when
          BEQ.S     USING_END           the string has zero length
U_FILL_LP MOVE.B    #'*',0(A6,A4.L)     Fill the whole string with asterisks
          ADDQ.W    #1,A4
          SUBQ.W    #1,D0
          BNE.S     U_FILL_LP
          BRA.S     USING_END           Exit with D0 zero

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

* BCSOUND duration,pitch (used at GOSUB 250 and 400)
* Duration is in 1/50ths of a second (synchronous with the frame interrupt)
* Pitch is in semitones with 60 equal to middle C (much like the Spectrum's
* BEEP command, except it's offset by 60).
* Unfortunately, the QL's sound generator has a quite limited range of tones,
* corresponding to pitch values 29 to 88. To make lower or higher tones sound
* more recognisable, we'll play them as the corresponding semitone which falls
* into the next playable higher or lower octave.
* Furthermore, the tones generated do not exactly correspond to the frequencies
* required by the pitch values, resulting in a sound slightly 'out of tune'.
* (Of course, nowadays there are better sound sources around such as SMSQ/E's 
* SSSS, you can implement this at subroutine 400 if you're very ambitious...)
          
SOUND     VECTOR    CA_GTINT,A2         Get 2 integers
          BNE.S     SOUND_END           Bail out if error
          SUBQ.W    #2,D3               Two arguments must be given
          BNE.S     ERR_BP3
          ADDQ.L    #4,BV_RIP(A6)       Tidyup stack
          MOVE.W    0(A6,A1.L),D3       Get duration
          BLT.S     SOUND_END           .. which must be >=0
          MOVE.W    2(A6,A1.L),D1       Get pitch
TST_LOWER CMPI.W    #29,D1              .. which must be in range 29 to 88
          BGE.S     TST_UPPER           If lower, just go up one octave
          ADDI.W    #12,D1              .. until it fits
          BRA.S     TST_LOWER
TST_UPPER CMPI.W    #88,D1              Test against upper limit
          BLE.S     DO_BEEP
          SUBI.W    #12,D1              If too high, go down one octave
          BRA.S     TST_UPPER           .. and loop back

* An IPC command is now built up onto the machine stack and sent to the 8049
* Since the command should wait for the tone to finish, we'll suspend our job
* for the required duration and then issue a 'stop sound' command.

DO_BEEP   MOVE.W    #$0100,-(A7)        
          CLR.L     -(A7)
          CLR.L     -(A7)
          MOVE.B    BEEP_TBL-29(PC,D1.W),(A7)     the 'pitch'
          MOVE.B    (A7),1(A7)                    also 'pitch2'
          MOVE.L    #$5555AAAA,-(A7)    other params are zero
          MOVE.W    #$0A08,-(A7)        'make sound' command
          MOVE.L    A7,A3               pass pointer to A3 as parameter

* Note: we really shouldn't pass a pointer to S*BASIC's stack as absolute!
* Unfortunately there is no 'TRAP #4' mechanism for MT.IPCOM :-(.
* To do it right, we should reserve memory using ALCHP and pass this as pointer
* but fortunately, the chance of S*BASIC moving in between these two
* instructions will be very remote...

          QDOS      MT.IPCOM            Make the sound
          MOVEQ     #-1,D1
          SUBA.L    A1,A1
          QDOS      MT.SUSJB            Now wait 'duration' frames
          LEA       NOSOUND,A3
          QDOS      MT.IPCOM            Silence again now...
          ADDA.W    #16,A7              Clean up the stack
          MOVEQ     #0,D0
SOUND_END RTS
ERR_BP3   MOVEQ     #-15,D0
          RTS

* The pitch values for MT.IPCOM, corresponding to SP values 29 to 88.

BEEP_TBL  DC.B      255,240,226,213,201,189,178,169,158,148,140,131,123
          DC.B      116,109,102,96,91,85,80,75,70,66,62,58,54,51,48,44,41
          DC.B      38,36,34,31,29,27,25,23,21,20,18,17,15,14,13,12,11,10
          DC.B      9,8,7,6,5,4,4,3,2,2,1,1
          DS.W      0

* IPC command for 'kill all sound'.

NOSOUND   DC.B      $0B,0,0,0,0,0,1

* SCREEN$ returns character at given position on screen
* Note: This currently works ONLY with screen in QL mode, and the current STRIP
*       and INK should be either the same as when the character was PRINTed, or
*       both reversed (this will detect characters PRINTed using GOSUB 150).

SCREEN$   BSR       GET_CHAN            Get channel ID
          BNE.S     SCR_END
          VECTOR    CA_GTINT,A2         Get character position
          BNE.S     SCR_END
          SUBQ.W    #2,D3               Two arguments must be given
          BNE.S     ERR_BP3
          ADDQ.L    #4,BV_RIP(A6)       Tidyup stack
          MOVEM.W   0(A6,A1.L),D1-D2    X to D1, Y to D2
          LEA       GET_SCR,A2          Address of EXTOP routine
          MOVEQ     #-1,D3
          QDOS      SD.EXTOP            Call it
          MOVE.L    BV_RIP(A6),A1       Get RI stack pointer
          SUBQ.W    #2,A1               Assume null string for now
          MOVEQ     #0,D2
          TST.B     D1                  Test returned code
          BEQ.S     SCR_STK             If zero, stack null string
          SUBQ.W    #2,A1               Else, two extra bytes needed
          MOVEQ     #1,D2               .. and length is 1
          MOVE.B    D1,2(A6,A1.L)       Stack the 1-byte string
SCR_STK   MOVE.W    D2,0(A6,A1.L)
          MOVE.L    A1,BV_RIP(A6)       Set RI stack pointer
          MOVEQ     #1,D4               Signal 'string result'
SCR_END   RTS

* The following subroutine does the actual work. It is called from SD.EXTOP
* in supervisor mode, hence A0 points to the start of the channel definition
* block (possibly offset by $30 if using Pointer Interface or SMSQ/E)
* On entry, D1 and D2 hold the X and Y position to be scanned for a character.
* Returns D1.B holding the code of the character found or zero if not found.

GET_SCR   MOVEM.W   $26(A0),D4-D5       Get SD.XINC, SD.YINC (character size)
          MULU      D4,D1               Multiply to get pixel coordinates
          MULU      D5,D2
          MOVE.W    $1C(A0),D0          SD.XSIZE
          SUB.W     D4,D0               Subtract one character position
          CMP.W     D0,D1               Is X coordinate beyond start of last
          BHI.S     SCR_EMPTY           column? Return empty string if so
          MOVE.W    $1E(A0),D0          SD.YSIZE
          SUB.W     D5,D0               Same check for Y coordinate
          CMP.W     D0,D2
SCR_EMPTY BHI       SCR_EMP2
          ADD.W     $18(A0),D1          Now add SD.XMIN and SD.YMIN to get
          ADD.W     $1A(A0),D2          absolute position on screen
          LEA       $20000,A4           Base of screen

* Note: The above instruction should be replaced by MOVE.L SD.SCRB(A0),A4! 
* Also, the following code should be adapted for modern screen resolutions...

          LSL.L     #7,D2               Multiply Y by 128 (bytes per line)
          ADDA.L    D2,A4               A4 now base of screen line
          MOVEQ     #7,D7               D7 = X position MOD 8
          AND.B     D1,D7
          LSR.W     #3,D1               D1 = X position DIV 8
          ADD.W     D1,D1               Double this as there are 8 pixels in
          ADDA.W    D1,A4               one word (both MODE 4 and 8)

* A buffer is now allocated to store the bit pattern of the next 10 rows - but
* only the relevant 32 bits to be matched up with the font pattern. These
* need to be shifted first before being matched up, since they may be
* positioned anywhere in the current word and possibly the two next words.
* These 32 bits correspond to 16 pixels on the screen which is the widest
* possible character size in both modes.

          SUBA.W    #40,A7              Make room on the stack for 10*4 bytes
          MOVE.L    A7,A5               A5 points to the buffer
          MOVE.W    #10,-(A7)           Count 10 rows

* The following loop fetches three words for every row in turn.
* D0, D2, D4: even byte (green for MODE 4, green/flash for MODE 8)
* D1, D3, D5: odd byte (red for MODE 4, red/blue for MODE 8).
* We'll count the pixels from left to right.

GET_ROW   MOVE.B    (A4)+,D0            Pixels G0-G7 or G0F0-G3F3
          MOVE.B    (A4)+,D1            Pixels R0-R7 or R0B0-R3B3
          MOVE.B    (A4)+,D2            Pixels G8-G15 or G4F4-G7F7
          MOVE.B    (A4)+,D3            Pixels R8-R15 or R4B4-R7B7
          MOVE.B    (A4)+,D4            Pixels G16-G23 or G8F8-G11F11
          MOVE.B    (A4),D5             Pixels R16-R23 or R8B8-R11B11
          MOVE.B    D7,D6               Position of first pixel in word (0-7)
          BEQ.S     ST_ROW              Skip if already in leftmost position

* The following loop shifts all pixel bits left until they are in the right
* position, at most 7 times. This ensures that all pixels to be matched up are
* held in byte-sized registers D0 to D3, and are stored as one long word.

SH_ROW    LSL.B     #1,D4               Shift out G16-G23 or G8F8-G11F11
          ROXL.B    #1,D2               .. into G8-G15 or G4F4-G7F7
          ROXL.B    #1,D0               .. and into G0-G7 or G0F0-G3F3
          LSL.B     #1,D5               Shift out R16-R23 or R8B8-R11B11
          ROXL.B    #1,D3               .. into R8-R15 or R4B4-R7B7
          ROXL.B    #1,D1               .. and into R0-R7 or R0B0-R3B3
          SUBQ.B    #1,D6               Decrement position counter
          BNE.S     SH_ROW              Loop back until zero
ST_ROW    MOVE.B    D0,(A5)+            Store G0-G7 or G0F0-G3F3
          MOVE.B    D1,(A5)+            Store R0-R7 or R0B0-R3B3
          MOVE.B    D2,(A5)+            Store G8-G15 or G4F4-G7F7
          MOVE.B    D3,(A5)+            Store R8-R15 or R4B4-R7B7
          ADDA.W    #123,A4             Move to next line in display
          BTST      #4,$42(A0)          Do we have double height characters?
          BEQ.S     GTROW_NXT           No, skip
          ADDA.W    #128,A4             Else, go down one extra line
GTROW_NXT SUBQ.W    #1,(A7)             Loop for 10 pixel rows
          BNE.S     GET_ROW
          ADDQ.W    #2,A7               Remove row counter
          MOVEQ     #$60,D0             Bits 5 and 6 of SD.CATTR - extended
          AND.B     $42(A0),D0          width and double width
          LSR.B     #3,D0               Shift them into bits 2 and 3
          MOVE.L    MASKTAB(PC,D0.W),D5 Pick up the appropriate bit mask
          MOVE.L    $2A(A0),A4          Pointer to fount 1
          BSR.S     CMP_CHRS            Now match up against fount 1
          TST.B     D1                  Have we got a match?
          BNE.S     SCR_RTS             Yes, exit
          MOVE.L    $2E(A0),A4          Else, match up against fount 2
          BSR.S     CMP_CHRS
SCR_RTS   ADDA.W    #40,A7              Remove buffer
          RTS                           Done

* The following table holds the masks for the four possible character widths
* Sizes in pixels are 6, 8, 12 and 16 respectively

MASKTAB   DC.L      $FCFC0000,$FFFF0000,$FFFFF0F0,$FFFFFFFF

* This subroutine matches up the pixel pattern collected above against the
* pattern of each character in a fount pointed to by A4.
* Each character row is converted to a pixel pattern, taking into account the 
* strip and ink colour and possible double and extended width and matched up.
* However a match is also possible when 'strip' and 'ink' are reversed so as to
* allow for recognition of 'highlighted' text printed using GOSUB 150.

* This is a quite elaborate process that could be optimised considerably if the
* calling routine would be changed to match up the pixel pattern collected from
* the display against the strip/ink colour, storing rows containing only bits
* signalling strip or ink (and skipping if neither of these two!). This way, a
* simple cmp or xor instruction would be sufficient to match a character row
* rather than the elaborate bit pattern manipulation now done in CMP_ROW.
* (a search in the standard ASCII set alone takes up to 864 calls to CMP_ROW!)

CMP_CHRS  MOVE.B    (A4)+,D1            Get first character code in fount
          MOVE.B    (A4)+,D7            Number of characters in fount minus one
          ADD.B     D1,D7               D7 now holds the code of the last char

* Note: In Minerva, the second fount was extended to hold character codes $80
* to $1F (yes, wrap around 0!). Using this fount, the above addition will
* overflow, leaving D1.B=$1F, resulting in just one character being matched up!
* This could be avoided by making D1 and D7 word-sized (if the 'CMP.B D7,D1'
* would be replaced by a DBF on D7, the above ADD could be removed too).

CMP_LOOP  LEA       8(A7),A5            Point to the SECOND row of pixels!
          MOVE.L    A4,A3               Pointer to current character pattern
          BSR.S     CMP_ROW             Match this up against screen row
          BEQ.S     NXT_ROWS            Jump if we have a direct match
          CMP.L     D5,D0               Now look for an 'inverse' match where
          BNE.S     NXT_CHR             all relevant bits are 1
NXT_ROWS  MOVE.L    D0,D4               Save the resultant bits in D4
          MOVEQ     #7,D6               Match up the next eight rows
CMP_LP2   BSR.S     CMP_ROW             The resultant bits in D0 should be all
          EOR.L     D4,D0               0 or 1, match these against the mask
          BNE.S     NXT_CHR             Skip if no match
          DBF       D6,CMP_LP2          Loop for a maximum of eight rows
          RTS
NXT_CHR   ADDA.W    #9,A4               No match - bump fount pointer by nine
          ADDQ.B    #1,D1               Increment character code
          CMP.B     D7,D1               Loop for all characters in the fount
          BLS.S     CMP_LOOP            (see notes above!)
SCR_EMP2  MOVEQ     #0,D1               Nothing found - return NULL char (this
          MOVEQ     #0,D0               could better be changed to -1).
          RTS

* This routine does the actual match for each character pixel row.
* Entry: D5.L mask for character width 
*        A3   pointer to fount pattern for this row (updated)
*        A5   pointer to screen pattern for this row (rotated in place) (upd)
* Exit:  D0.L match result: 0 if true match, D0=D5 if inverse match (strip and
*             ink reversed)

* We'll start by converting the bit pattern in a fount character row to
* something we can match up against the pixels collected at (A5)

CMP_ROW   MOVE.B    (A3)+,D0            Get pattern for character row
          BTST      #6,$42(A0)          Test the 'double width' flag
          BNE.S     DBL_WIDTH           Jump with double width (also MODE 8)
          MOVE.B    D0,D2
          LSL.W     #8,D0               Replicate the byte to bits 8-15
          MOVE.B    D2,D0
          SWAP      D0                  Swap it into leftmost 16 bits
          BRA.S     DO_COLOR            Do the match.
DBL_WIDTH MOVEQ     #7,D3               Double width - we need to replicate
DBL_SHIFT BTST      D3,D0               each bit into two bits
          SNE       D2                  Set D2 to 00 or FF according to bit in
          LSL.L     #2,D2               pattern and shift left into bits 8-23
          DBF       D3,DBL_SHIFT        Loop for 8 bits (Note: in practice,
          MOVE.W    D2,D0               only 6 bits are really used!)
          LSR.W     #8,D2               Replicate bits 8-15 to bits 0-7 in D0
          MOVE.B    D2,D0
          SWAP      D0                  Now prepare leftmost half of D0
          SWAP      D2
          MOVE.B    D2,D0               Copy bits 16 to 23 to bits 0-7
          LSL.W     #8,D0               .. and replicate to bits 8-15
          MOVE.B    D2,D0
          SWAP      D0                  Swap back, now D0 holds bits
*                                       77665544776655443322110033221100

* These instructions match the pixels from the screen row against the fount
* pattern row. It's much like the PRINT operation, except we do not print the
* bits but XOR them against what is on the screen. If a match occurs, the 
* result will be either all zeros or ones (when it was printed in inverse
* colours).

DO_COLOR  MOVE.L    $3A(A0),D2          Strip colour mask
          MOVE.L    $3E(A0),D3          Ink colour mask
          EOR.L     D2,D3               Mask off the ink bits against the bits
          AND.L     D0,D3               in the fount pattern; a 0 bit will set
          EOR.L     D2,D3               strip bits, a 1 bit will set ink bits
          MOVE.L    (A5)+,D0            Get stored screen bits from the buffer
          EOR.L     D3,D0               This will leave all relevant bits 
*                                       either 0 or 1 when a match occurs
          AND.L     D5,D0               Mask off the unwanted bits
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