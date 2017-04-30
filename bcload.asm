* BASICODE 3 for the Sinclair QL
* Part 2: BASICODE to S*BASIC conversion routines
* Copyright (C) 1986, 1987, 2017 by Jan Bredenbeek
* Released under the GNU Public License, 2017

* Revision history:

* 20170425: Deleting lines above 1000 doesn't work in SMSQ/E so replaced this
*           code by an error message. Corrected line number check and changed
*           trailing space check after tokens
* 20170417: Cleaned up and commented BCLOAD code, should also correctly delete
*           lines >= 1000 on SMSQ/E now
* 20170313: Re-instated direct LOADing from BCLOAD thanks to input from
*           Marcel Kilgus (thanks Marcel!)
* 20170115: Added OPENFILE subroutine, GET_NAME routine is now in BSC1_ASM 
*           (regression from v1.0!)
* 20160224: Reworked code to write translated program to file instead of 
*           LOADing it (which doesn't work on SBASIC)
* 19860723: Start of work... (v1.0 - 1.2)

          INCLUDE   QDOS_IN_MAC

BV_RIP    EQU       $58                 arithmetic stack pointer
P2..ON    EQU       0                   bit flag - ON statement
P2..FOR   EQU       1                   bit flag - FOR statement
P2..REM   EQU       2                   bit flag - REM statement
P2..DIM   EQU       3                   bit flag - DIM statement
P2..SAR   EQU       4                   bit flag - DIM'ing a string array

VECTOR    MACRO     A,B
[.LAB]    MOVE.W    [A],[B]
          JSR       ([B])
          ENDM

          XDEF      BCLOAD,BCMERGE,TOK_TBL,UNDO_RT

          XREF      GET_NAME,RECSTRG

          SECTION   CODE

* BASICODE token table
* These are temporary tokens used during translation

TOK_TBL   DC.B      $80,'ABS'
          DC.B      $81,'ASC'
          DC.B      $82,'ATN'
          DC.B      $83,'CHR$'
          DC.B      $84,'COS'
          DC.B      $85,'DEFFN'
          DC.B      $86,'END'
          DC.B      $87,'EXP'
          DC.B      $88,'FN'
          DC.B      $89,'INT'
          DC.B      $8A,'LEFT$'
          DC.B      $8B,'LEN'
          DC.B      $8C,'LOG'
          DC.B      $8D,'MID$'
          DC.B      $8E,'RESTORE'
          DC.B      $8F,'RETURN'
          DC.B      $90,'RIGHT$'
          DC.B      $91,'RUN'
          DC.B      $92,'SGN'
          DC.B      $93,'SIN'
          DC.B      $94,'SQR'
          DC.B      $95,'STOP'
          DC.B      $96,'TAB'
          DC.B      $97,'TAN'
          DC.B      $98,'VAL'
          DC.B      $99,'DATA'
          DC.B      $9A,'DIM'
          DC.B      $9B,'FOR'
          DC.B      $9C,'GOSUB'
          DC.B      $9D,'GOTO'
          DC.B      $9E,'IF'
          DC.B      $9F,'INPUT'
          DC.B      $A0,'LET'
          DC.B      $A1,'NEXT'
          DC.B      $A2,'NOT'
          DC.B      $A3,'ON'
          DC.B      $A4,'PRINT'
          DC.B      $A5,'READ'
          DC.B      $A6,'REM'
          DC.B      $A7,'AND'
          DC.B      $A8,'OR'
          DC.B      $A9,'STEP'
          DC.B      $AA,'THEN'
          DC.B      $AB,'TO'
          DC.B      $FF

* Substitution token table for SBasic equivalent keywords - searched first

TK_2      DC.B      $85,'DEFine FuNction _' DEFFN - rarely used in B'code
          DC.B      $86,'STOP' ; allowed in BC2 but not in BC3!
          DC.B      $88,'_'    ; change FN to _ so we get _fn_(parm)
          DC.B      $96,'TO'   ; TAB in PRINT statements
          
* The following marked DIM statements as 'bad line'. This is because we need to
* manually adjust DIMs of string arrays to add the length DIMension
* since we have this now automated, it's obsolete
          
*          DC.B      $9A,'MISTake DIM'
          DC.B      $FF

*** Obsoleted by GET_NAME in BSC1_ASM          
* Get filename (string or name) and open file

* GET_NAME  MOVEQ     #$0F,D0
*          AND.B     1(A6,A3.L),D0
*          SUBQ.B    #1,D0
*          BNE.S     GET_NAM1
*          MOVE.L    A5,-(A7)
*          LEA       8(A3),A5
*          VECTOR    CA_GTSTR,A2
*          MOVE.L    (A7)+,A5
*          BNE.S     GTNAM_END
*          MOVEQ     #3,D0
*          ADD.W     0(A6,A1.L),D0
*          BCLR      #0,D0
*          ADD.L     D0,$58(A6)
*          BRA.S     OP_FILE
*GET_NAM1  MOVE.W    2(A6,A3.L),D0
*          BLT.S     ERR_BN
*          LSL.L     #3,D0
*          MOVE.L    $18(A6),A0
*          ADDA.L    D0,A0
*          MOVE.W    2(A6,A0.L),A0
*          ADDA.L    $20(A6),A0
*          MOVEQ     #0,D0
*          MOVE.B    0(A6,A0.L),D0
*          ADDQ.W    #1,A0
*          MOVEQ     #3,D1
*          ADD.W     D0,D1
*          BCLR      #0,D1
*          MOVE.L    $58(A6),A1
*          SUBA.W    D1,A1
*          LEA       2(A1),A2
*          MOVE.W    D0,0(A6,A1.L)
*GTNAM_LP  MOVE.B    0(A6,A0.L),0(A6,A2.L)
*          ADDQ.W    #1,A0
*          ADDQ.W    #1,A2
*          SUBQ.B    #1,D0
*          BNE.S     GTNAM_LP
*OP_FILE   MOVE.L    A1,A0
*          MOVEQ     #-1,D1
*          MOVE.L    D5,D3
*          TRAP      #4
*          QDOS      IO.OPEN
*          TST.L     D0
*GTNAM_END RTS
*ERR_BN    MOVEQ     #-12,D0
*          RTS

* Undo return stack when doing BCLOAD/BCMERGE in a clause

BV.RTBAS  EQU       $38
BV.RTP    EQU       $3C
BV.SING   EQU       $6C
BV.UNDO   EQU       $B8

UNDO_RT   CMPI.W    #1000,BV.LINUM(A6)
          BGE.S     ERR_NI              don't let BCLOAD kill itself...
          MOVE.L    BV.RTP(A6),D0       are we in a clause?
          SUB.L     BV.RTBAS(A6),D0
          BEQ.S     UNDO_RTS            no, return
          TST.B     BV.SING(A6)         single line?
          BEQ.S     ERR_NI              no, error NI
          ST        BV.UNDO(A6)         signal 'unravel stack'
ERR_NI    ADDQ.W    #4,A7               ..and return from caller
          MOVEQ     #ERR.NI,D0          You can't BCLOAD or BCMERGE in a proc
UNDO_RTS  RTS

* Open file (with default dir if necessary)
* Entry: A0 filename (rel. A6), D3 access key
* Exit: D0 status, A0 channel ID
* Uses S*Basic buffer

OPENFILE  MOVEQ     #-1,D1
          TRAP      #4
          QDOS      IO.OPEN
          TST.L     D0
          BEQ.S     OF_RTS
          MOVE.L    D0,-(A7)
          QDOS      MT.INF
          MOVE.L    (A7)+,D0
          TST.L     $B0(A0)             ; ptr to DATA_USE default
          BEQ.S     OF_RTS
          MOVE.L    $B0(A0),A1
          MOVEQ     #0,D1
          MOVE.W    (A1),D1
          MOVE.L    BV_RIP(A6),A0
          ADD.W     (A6,A0.L),D1
          MOVEM.L   D1/D3,-(A7)
          MOVE.W    BV_CHRIX,A2
          JSR       $1C(A2)
          MOVEM.L   (A7)+,D1/D3
          MOVE.L    (A6),A0
          MOVE.W    D1,(A6,A0.L)
          MOVE.W    (A1)+,D0
          BRA.S     OF_L1_E
OF_L1_L   MOVE.B    (A1)+,2(A6,A0.L)
          ADDQ.L    #1,A0
OF_L1_E   DBF       D0,OF_L1_L
          MOVE.L    BV_RIP(A6),A1
          MOVE.W    (A6,A1.L),D0
          BRA.S     OF_L2_E
OF_L2_L   MOVE.B    2(A6,A1.L),2(A6,A0.L)
          ADDQ.L    #1,A1
          ADDQ.L    #1,A0
OF_L2_E   DBF       D0,OF_L2_L
          MOVE.L    (A6),A0
          MOVEQ     #-1,D1
          TRAP      #4
          QDOS      IO.OPEN
OF_RTS    TST.L     D0
          RTS

ERR_BP    MOVEQ     #-15,D0
          RTS

* MERGE or LOAD a file in BASICODE format

* Some S*BASIC system variables

BV.PFBAS  EQU       $10             program file start
BV.PFP    EQU       $14             program file end ptr
BV.LINUM  EQU       $68             current line number
BV.STMNT  EQU       $6C             current statement
BV.CONT   EQU       $6D             stop or continue processing
BV.NXLIN  EQU       $88             line to be jumped too
BV.NXSTM  EQU       $8A             ... and statement
BV.STOPN  EQU       $8C             stop number
BV.EDIT   EQU       $8E             program edited flag

SB.STOP   EQU       4               signal: stop program
SB.RUN    EQU       6               signal: run program

* Offset of variables in workspace

BCL.CHAN  EQU       0               output channel (-1 if none)
BCL.LINR  EQU       4               current line number
BCL.LINE  EQU       6               start of original BASICODE line
BCL.TKLN  EQU       256             start of tokenised BASICODE line
BCL.LMAX  EQU       250             max len of line (b'code only allows 60...)
BCL.WKSZ  EQU       512             size of workspace in common heap

EOL       EQU       $0A

BCMERGE   MOVEQ     #0,D7
          BRA.S     BCLOAD_1
BCLOAD    MOVEQ     #-1,D7
BCLOAD_1  BSR       UNDO_RT         unravel proc/fn if needed
          CMPA.L    A3,A5
          BEQ.S     ERR_BP
          BSR       GET_NAME
          BNE.S     OF_RTS
          MOVEQ     #1,D3           Open for read
          BSR       OPENFILE
          BSR       RECSTRG
          BNE.S     OF_RTS                           
          MOVE.L    A0,A4
          MOVE.L    #-1,A0
          ADDQ.L    #8,A3
          CMPA.L    A3,A5
          BEQ.S     BCL_OUTF
          BSR       GET_NAME
          BNE       OF_RTS
          MOVEQ     #2,D3           Open for write
          BSR       OPENFILE
          BSR       RECSTRG
          MOVE.L    D0,D4
          BNE       BCL_CLOS
          MOVEQ     #0,D7           BCLOAD f1,f2 behaves like BCMERGE
BCL_OUTF  MOVE.L    A0,A5           Save output file # (or -1)
          MOVE.L    #BCL.WKSZ,D1
          MOVEQ     #-1,D2
          QDOS      MT.ALCHP
          MOVE.L    D0,D4
          BNE       BCL_CLO2
          EXG       A0,A4           A0 = Channel ID, A4 = buffer ptr
          MOVEM.L   A0/A4,-(A7)
          MOVE.L    A5,(A4)         Save output # in first longword of buffer
          MOVE.W    A5,D0           sending to a file?
          BPL       NEW_LINE        yes, skip tests below
          MOVE.W    #999,BCL.LINR(A4) don't allow entering lines below 1000
          SF        BV.CONT(A6)     signal 'stop program'
          TAS       BV.EDIT(A6)     and 'program edited & new names set'
          MOVE.W    #SB.STOP,BV.STOPN(A6)
          TST.B     BV.SING(A6)     single line?
          BNE.S     BCL_SING        yes
          MOVE.W    #SB.RUN,BV.STOPN(A6) else, signal 'run from current line'
          MOVE.W    BV.LINUM(A6),BV.NXLIN(A6)
          MOVE.B    BV.STMNT(A6),BV.NXSTM(A6)

* We now need to test for BCLOAD/BCMERGE. If BCLOAD, all lines from 1000
* onwards need to be deleted. In QDOS and Minerva, this could be done by
* calling the SuperBASIC vector at $138 with D4=1000, D6=32767 and D7=-1.
* Unfortunately, under SMSQ/E this vector no longer works and at the moment
* I don't know of any way to do this without crashing SBASIC...
* So we'll just give an error message instructing the user to clean up the
* program manually (the easiest way being a 'BCBOOT' command).

BCL_SING  TST.B     D7              D7 is 0 for BCMERGE, -1 for BCLOAD
          BEQ.S     NEW_LINE
          QDOS      MT.INF
          CMPI.L    #'2.00',D2      assume V2 or higher is SMSQ/E
          BLO.S     BCL_DEL

* Come here with SMSQ/E. Check if there are any lines above 1000, give
* appropriate error message if so.

          MOVE.L    BV.PFBAS(A6),A4  Get start of program file
          MOVEQ     #2,D1            init. running line len (skips 1st word)
BCL_CKLP  CMPA.L    BV.PFP(A6),A4    Already past last line?
          BHS.S     NEW_LINE         yes, gonna be OK
          CMPI.W    #1000,4(A6,A4.L) at this point there should be a line #
          BGE.S     BCL_SETP         if 1000 or higher, raise error
          ADD.W     (A6,A4.L),D1     else, get running line length
          ADDA.W    D1,A4            and skip to next
          BRA       BCL_CKLP

* We're out of luck. Explain the situation on #0 and return with ERR.NI

BCL_SETP  SUBA.L    A0,A0
          PRINT     {'Cannot delete BASICODE program, use BCBOOT first',$0A}
          MOVEQ     #ERR.NI,D0
          BRA.S     BCLOAD_E

* Here we have plain old QDOS or Minerva so calling pf_liste is safe

BCL_DEL   MOVE.L    BV.NXLIN(A6),-(A7)
          MOVE.W    BV.STOPN(A6),-(A7)
          MOVE.W    #1000,D4        Delete lines 1000 to end
          MOVE.W    #$7FFF,D6
          MOVE.W    $138,A2
          JSR       $4000(A2)
          MOVE.W    (A7)+,BV.NXLIN(A6)
          MOVE.L    (A7)+,BV.STOPN(A6)

* Main translation loop. Each line is read in from input until EOF reached.

NEW_LINE  MOVEM.L   (A7),A0/A4      Get chan id in A0, buffer addr in A4
          LEA       BCL.LINE(A4),A1
          MOVE.W    #BCL.LMAX,D2
          MOVEQ     #-1,D3
          QDOS      IO.FLINE        get next BASICODE line
          TST.L     D0
          BEQ.S     TRLINE          no error, go translate the line
          CMPI.L    #ERR.EF,D0      EOF?
          BNE.S     BCLOAD_E        if other error, report and finish up
          MOVEQ     #0,D0           EOF means no error
BCLOAD_E  MOVE.L    D0,D4
          MOVEM.L   (A7)+,A0/A4     get chan id and buffer addr
          MOVE.L    (A4),A5         any output chan id to a5
          EXG       A0,A4           now release buffer space
          QDOS      MT.RECHP
BCL_CLO2  MOVE.L    A5,A0
          MOVE.W    A0,D0           A0 = output chan id or -1 if none
          BMI.S     BCL_CLOS
          QDOS      IO.CLOSE        close output if any
BCL_CLOS  EXG       A0,A4
          QDOS      IO.CLOSE        now close input chan
          MOVE.L    D4,D0           any error
SKP_RTS   RTS

* Skip spaces, control codes and hi-bit characters

TR_SKPSP  CMPI.B    #' ',(A1)
          BGT.S     SKP_RTS         Return NE on printable non-space chars
          CMPI.B    #EOL,(A1)
          BEQ.S     SKP_RTS         ... but EQ when EOL reached
          ADDQ.L    #1,A1
          BRA       TR_SKPSP        skip anything else

* Now start program translation
* First pass: tokenise all BASICODE keywords to temporary codes
          
TRLINE    LEA       BCL.TKLN(A4),A5     We'll copy from (a4) to (a5) as we go
          LEA       BCL.LINE(A4),A1     point to start of line
          BSR       TR_SKPSP            Skip any leading spaces
          BEQ       NEW_LINE            ignore empty lines..
          
* Now read the line number. If current line # is lower than previous line,
* use last line # +1 to avoid creating lines out of order
          
TR_LINUM  MOVEQ     #0,D1
          MOVEQ     #0,D0
LNUM_LP   MOVE.B    (A1),D0
          SUBI.B    #'0',D0
          CMPI.B    #9,D0
          BHI.S     CMP_LNO
          MULU      #10,D1
          ADD.W     D0,D1
          ADDQ.W    #1,A1
          BRA.S     LNUM_LP
CMP_LNO   BSR       TR_SKPSP
          BEQ       NEW_LINE            if just a line number, ignore line...
          CMP.W     BCL.LINR(A4),D1
          BGT.S     UPD_LNO
          MOVE.W    BCL.LINR(A4),D1
          ADDQ.W    #1,D1
UPD_LNO   MOVE.W    D1,BCL.LINR(A4)

* Pass 1 for each line
* This mainly does keyword tokenising to temporary tokens so we can do more
* analysis in pass 2.
* Note that BASICODE does NOT require spaces around keywords (yes, it was made
* in 1981 when you paid a month's salary for 16K, you know...)

TRLOOP    BSR       TR_SKPSP        skip any spaces
          BEQ       PASS_2          if EOL reached
          MOVE.B    (A1)+,D4        get next char
          CMPI.B    #$41,D4         alphabetic?
          BLT.S     TST_QUOTE
          LEA       TOK_TBL,A3      try matching tokens
TK_NEXT   MOVE.B    (A3)+,D5        get next token code
          BPL.S     TK_NEXT
          CMPI.B    #$FF,D5         end of table reached?
          BEQ.S     TST_QUOTE       yes
          LEA       -1(A1),A2       point to first char
TK_CMP    CMPM.B    (A2)+,(A3)+     try matching
          BNE.S     TK_NEXT         no match (we don't have to consider case)
          TST.B     (A3)            end of token reached?
          BPL.S     TK_CMP          no, loop back
          MOVE.L    A2,A1           we found a match so reset a1 to end
          MOVE.B    D5,(A5)+        and copy token code
          CMPI.B    #$A6,D5         test for REM
          BEQ.S     COPY_REST       if REM, copy rest of line verbatim
          CMPI.B    #$AA,D5         test for THEN
          BNE       TRLOOP          no, loop back

* We now have to consider 'THEN line number' which must be changed to
* 'THEN GOTO line number'

          BSR       TR_SKPSP        skip any spaces
          CMPI.B    #'0',(A1)       check for number
          BLT       TRLOOP
          CMPI.B    #'9',(A1)
          BGT       TRLOOP
          MOVE.B    #$9D,(A5)+      number found, insert GOTO token
          BRA       TRLOOP

* Check for literal strings in quotes - copy them verbatim

TST_QUOTE MOVE.B    D4,(A5)+            
          CMPI.B    #$22,D4
          BNE       TRLOOP
QUOTE_LP  MOVE.B    (A1)+,D4
          CMPI.B    #$0A,D4
          BNE.S     QUOTE_NXT       oops - we've found EOL in a string
          MOVE.B    #$22,(A5)+      (M$ BASIC allows this abomination)
          BRA.S     PASS_2          - so we correct it!
QUOTE_NXT CMPI.B    #$20,D4
          BLT       QUOTE_LP        any control chars are skipped
          MOVE.B    D4,(A5)+        rest is copied
          CMPI.B    #$22,D4         ...until end of string reached
          BNE.S     QUOTE_LP
          BRA       TRLOOP

* Handle 'REM', copy rest of line unaltered          
          
COPY_REST MOVE.B    (A1)+,D4
          CMPI.B    #$0A,D4
          BEQ.S     PASS_2
          CMPI.B    #$20,D4
          BLT       COPY_REST
          MOVE.B    D4,(A5)+
          BRA.S     COPY_REST

* Pass 2: Tokenised line is analysed and copied to the buffer area in a form
* suitable for SBasic, then entered into program or output chan

PASS_2    MOVE.B    #$0A,(A5)       set EOL on tokenised line
*          MOVE.L    4(A7),A4       shouldn't be needed now!
          LEA       BCL.TKLN(A4),A5 Point to start of tokenised BASICODE line
P2_1      MOVE.L    (A6),A0         Start of SBASIC buffer
          MOVE.L    BV_RIP(A6),A1
          SUBQ.W    #2,A1
          MOVE.W    BCL.LINR(A4),(A6,A1.L) Line number
          VECTOR    CN_ITOD,A2

* 20160224: Inserted extra space after line number

          MOVE.B    #' ',(A6,A0.L)
          ADDQ.L    #1,A0
          MOVE.L    A0,4(A6)
          MOVEQ     #0,D6     flags reg
          MOVEQ     #0,D7     quote counter
          
* Loop for pass 2
* Note: D6 is a flag register:
*       bit 0: set if in 'ON' clause
*       bit 1: set if in 'FOR' clause
*       bit 2: set if in 'REM' clause
*       bit 3: set if in 'DIM' clause
*       bit 4: set if DIM'ing a string array
          
P2_LOOP   MOVE.B    (A5)+,D4
          BMI       P2_TOKENS
          BTST      #P2..REM,D6         in "REM"?
          BNE.S     P2_BAS2             yes, copy verbatim
          CMPI.B    #$22,D4
          BNE.S     TST_QFLAG           count quotes in D7
          ADDQ.B    #1,D7
TST_QFLAG BTST      #0,D7               in quotes?
          BNE.S     P2_BAS2             yes, copy verbatim
          CMPI.B    #'$',D4             string variable?
          BNE.S     P2_BRAC
          CMPI.B    #'(',(A5)           ..and string array or function?
          BNE.S     P2_BAS2             no, skip
          BTST      #P2..DIM,D6         are we in a DIM statement?
          BEQ.S     P2_UNDER            no
          BSET      #P2..SAR,D6         signal 'DIM string-array'
          BRA.S     P2_UNDER            else, consider arrays/fns
P2_BRAC   CMPI.B    #'(',D4             check for arrays
          BNE.S     P2_PLUS
          MOVE.B    -2(A5),D1           get char before bracket
          SUBI.B    #'0',D1
          CMPI.B    #'9'-'0',D1
          BLS.S     P2_UNDER
          SUBI.B    #'A'-'0',D1
          CMPI.B    #'Z'-'A',D1         if A-Z or 0-9, we have an array name
          BHI.S     P2_BAS2
          
* These lines change array names by appending an underscore after their names
* to distinguish them from ordinary variables. This is necessary, as arrays
* and ordinary variables share the same name space in S*Basic, unlike BASICODE!
          
P2_UNDER  MOVE.B    D4,D5               save current character
          MOVEQ     #'_',D4
          BSR       IN_BAS              and insert underscore
          MOVE.B    D5,D4
P2_BAS2   BRA       P2_BAS

* String concatenation: change "+" in "&"

P2_PLUS   CMPI.B    #'+',D4
          BNE.S     P2_SAR
          MOVE.B    -2(A5),D1
          SUBI.B    #$22,D1             check for quote or '$' before '+'
          BEQ.S     P2_AMP
          SUBQ.B    #2,D1
          BEQ.S     P2_AMP
          MOVE.B    (A5),D1
          CMPI.B    #$22,D1             .. and after '+'
          BEQ.S     P2_AMP
          SUBI.B    #$41,D1
          CMPI.B    #25,D1
          BHI.S     P2_P_1
          MOVEQ     #'$',D0
          CMP.B     1(A5),D0
          BEQ.S     P2_AMP
          CMP.B     2(A5),D0
          BEQ.S     P2_AMP
          BRA.S     P2_BAS
P2_P_1    SUBI.B    #$42,D1             Now check for string functions
          BEQ.S     P2_AMP              CHR$
          SUBQ.B    #7,D1
          BEQ.S     P2_AMP              LEFT$
          SUBQ.B    #3,D1
          BEQ.S     P2_AMP              MID$
          SUBQ.B    #3,D1
          BNE.S     P2_BAS              RIGHT$
P2_AMP    MOVEQ     #'&',D4             We have string concat so insert '&'
          BRA.S     P2_BAS

* The following code adds an extra length dimension for string arrays
* (which BASICODE doesn't require, but S*BASIC does)
* We use 255 - the maximum allowed length in BASICODE
* This is usually overkill, but even on a standard QL we have plenty of RAM...

P2_SAR    CMPI.B    #')',D4             check for closing bracket
          BNE.S     P2_COL
          BCLR      #P2..SAR,D6         if DIM'ing a string array, bit is set
          BEQ.S     P2_COL
          MOVEQ     #',',D4
          BSR.S     IN_BAS
          MOVEQ     #'2',D4
          BSR.S     IN_BAS
          MOVEQ     #'5',D4
          BSR.S     IN_BAS
          BSR.S     IN_BAS
          MOVEQ     #')',D4
          BRA.S     P2_BAS

* Now check for single-line FOR clauses
* We HAVE to split them, else strange things may happen!

P2_COL    CMPI.B    #':',D4             end of statement?
          BNE.S     P2_BAS
          BTST      #1,D6               Are we in a FOR-clause?
          BEQ.S     P2_EOS              ..no
          MOVEQ     #$0A,D4             ..yes, split line
          BSR.S     IN_BAS
          BSR       P2_ENT
          ADDQ.W    #1,BCL.LINR(A4)     ..and bump next line number
          BRA       P2_1                loop for next line
P2_EOS    MOVEQ     #0,D6               end of statement, clear flags

* All conversions done so copy character to buffer and check for EOL

P2_BAS    BSR.S     IN_BAS
          CMPI.B    #$0A,D4             End of line?
          BNE       P2_LOOP             no, loop for next char
          BSR       P2_ENT              Enter line into BASIC or write to file
          BRA       NEW_LINE            and loop for next line

* Subroutine adds character in D4 to BASIC buffer area and updates pointer
* Note that this calls an offset from BV.CHRIX vector which is rather dirty...

IN_BAS    MOVE.L    4(A6),A0
          CMPA.L    8(A6),A0        still room in buffer?
          BLT.S     IN_BAS2
          MOVEQ     #$7E,D1         Make room for another 126 bytes
          MOVE.W    $11A,A2         BV.CHRIX
          JSR       $1C(A2)
IN_BAS2   MOVE.B    D4,0(A6,A0.L)
          ADDQ.W    #1,A0
          MOVE.L    A0,4(A6)        set BV.BFP
          RTS

SEARCHTK  MOVE.B    (A3)+,D5
          BPL.S     SEARCHTK
          CMP.B     D4,D5
          BEQ.S     STK_END
          ADDQ.B    #1,D5
          BNE.S     SEARCHTK
          SUBQ.B    #1,D5
STK_END   RTS

* Come here for tokens

P2_TOKENS LEA       TK_2,A3             try alternative tokens first
          BSR.S     SEARCHTK
          BEQ.S     TK_COPY
          LEA       TOK_TBL,A3          else, try standard tokens
          BSR.S     SEARCHTK
TK_COPY   CMPI.B    #$9C,D5             GOTO?
          BEQ.S     GOTO
          CMPI.B    #$9D,D5             GOSUB?
          BNE.S     TST_LSP
GOTO      BCLR      #0,D6               AFTER "ON"?
          BNE.S     LEAD_SP
TST_LSP   CMPI.B    #$A7,D5             AND, OR, STEP, TO, THEN..
          BCS.S     COPY_TK             .. should have a space in front
LEAD_SP   MOVEQ     #$20,D4             else, we put in a leading space
          BSR.S     IN_BAS
COPY_TK   MOVE.B    (A3)+,D4            now copy all token characters
          BMI.S     TST_TRSP            ..until we reach the end
          BSR.S     IN_BAS
          BRA.S     COPY_TK

* 20170425: changed test for trailing spaces. We now consider the character
*           after the token and add a trailing space if it's alphanumeric.

* TST_TRSP  CMPI.B    #$99,D5           old test: $80-$98 got no trailing space

TST_TRSP  MOVE.B    (A5),D4             get character after token
          BMI.S     P2_TRSP             always add space between tokens
          CMPI.B    #'.',D4             a dot is also numeric...
          BEQ.S     P2_TRSP             ...else we break SBASIC names!
          SUBI.B    #'0',D4
          CMPI.B    #9,D4               numeric?
          BLS.S     P2_TRSP             yes, add space
          SUBI.B    #'A'-'0',D4
          CMPI.B    #'Z'-'A',D4         alphabetic?
          BHI.S     P2_TK2              no
P2_TRSP   MOVEQ     #$20,D4             else, add trailing space
          BSR       IN_BAS
P2_TK2    CMPI.B    #$A3,D5             test for 'ON'
          BNE.S     NOT_ON
          BSET      #0,D6               if 'ON', set bit 0 for later
NOT_ON    CMPI.B    #$9B,D5
          BNE.S     NOT_FOR
          BSET      #1,D6               if 'FOR', set bit 1 for later
NOT_FOR   CMPI.B    #$A6,D5
          BNE.S     NOT_REM
          BSET      #2,D6               if 'REM', set bit 2
NOT_REM   CMPI.B    #$9A,D5
          BNE.S     P2_LOOP2
          BSET      #P2..DIM,D6         if 'DIM', set bit 3
P2_LOOP2  BRA       P2_LOOP

* Enter line into program, or write it to output file
* if output file has been specified, (A4) holds channel ID
* if (A4) contains -1, we just LOAD or MERGE the translated program

P2_ENT    MOVE.L    (A4),A0
          MOVE.W    A0,D0
          BMI.S     P2_PARSE
          MOVE.L    (A6),A1
          MOVE.L    4(A6),D2
          SUB.L     A1,D2
          MOVEQ     #-1,D3
          TRAP      #4
          QDOS      IO.SSTRG
          RTS

* Enter line into S*Basic system
* uses rather obscure vector calls (credits to TK2's ED command)

P2_PARSE  MOVEM.L   A4-A5,-(A7)         must save A4 too on SMSQ/E!
          QDOS      MT.INF
          MOVE.W    $13A,A2             vector for pa_init routine
          CMPI.L    #'1.03',D2          QDOS version
          BHI.S     P2_E_2              if >1.03, routine is vectored
          MOVE.W    $12C,A2             ... but on <=1.03, it's not vectored
          ADDA.W    #$138,A2            so we have to find it using pa_graph
P2_E_2    JSR       $4000(A2)           initialise parser work areas
          MOVE.W    $12E,A2             parser's syntax table
          ADDA.W    #$4000,A2           add offset
          
* NEW: Test for JMP instruction at location of table
* This is silly, but necessary as SMSQ/E's SBASIC has the vector to the real
* location stored after the JMP (it's a table, not a subroutine!)

          CMPI.W    #$4EF9,(A2)         opcode for JMP.L?
          BNE.S     P2_E_2A             No, it's the real table
          MOVE.L    2(A2),A2            else, get table from JMP destination
P2_E_2A   MOVE.W    $12C,A0
          JSR       $4000(A0)           parse the line
          BEQ.S     P2_E_3              syntax ok? branch if yes
          MOVE.W    $134,A2             
          JSR       $4000(A2)           else, insert 'MISTake'
P2_E_3    MOVE.W    $132,A2             
          JSR       $4000(A2)           remove forced spaces
          MOVE.W    $136,A2
          JSR       $4000(A2)           and enter line into system
          NOP                           error return - ignored
          MOVEM.L   (A7)+,A4-A5
          RTS

          END
