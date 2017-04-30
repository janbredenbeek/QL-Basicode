* BASICODE-3 CASSETTE LOAD/SAVE ROUTINES
* MAY 10, 1987

         INCLUDE  MACRO_LIB
         INCLUDE  QDOS_IN_MAC

         XDEF     BUFFER,CAS_OPEN,CAS_CLOSE,CAS_PEND,CAS_FETCH
         XDEF     CAS_SEND,BCLOAD_T,SAVBLOCK,REPLY
         XDEF     CLOAD,CSAVE,SAV_KEY,STOPTAPE

         XREF.S   BUFLEN
         XREF     GET1INT,PRINTMSG,GET_SCHD,BASPROG,GET_NAME,RECSTRG

         SECTION  CODE

BUFFER   BSR      GET1INT
         BNE.S    BUF_RTS
         ADDQ.L   #2,$58(A6)
         MOVEQ    #0,D7
         MOVE.W   (A6,A1.L),D7
         BLE.S    ERR_BP
         BSR      GET_SCHD
         BNE.S    BUF_RTS
         SWAP     D7
         LSR.L    #6,D7
         MOVE.L   D7,BUFLEN(A0)
         MOVEQ    #0,D0
BUF_RTS  RTS
ERR_BP   MOVEQ    #ERR.BP,D0
         RTS

REPLY    LEA      YNMSG,A1
         BSR      PRINTMSG
         SECTION  MSG
YNMSG    STRING$  {'(Y/N) '}
         SECTION  CODE
         MOVEQ    #0,D3
         REPEAT
            QDOS     IO.FBYTE
            TST.L    D0
         UNTIL NE
         MOVEQ    #-1,D3
         QDOS     SD.CURE
         REPEAT
            QDOS     IO.FBYTE
            IF D1 EQ.B #$1B THEN
               MOVEQ    #'N',D1
            ENDIF
            ANDI.B   #$DF,D1
         UNTIL D1 EQ.B #'Y' OR D1 EQ.B #'N'
         MOVE.B   D1,D2
         QDOS     SD.CURS
         MOVE.B   D2,D1
         QDOS     IO.SBYTE
         MOVEQ    #10,D1
         QDOS     IO.SBYTE
         CMPI.B   #'Y',D2
REP_RTS  RTS

BCLOAD_T BSR      GET_SCHD
         BNE      REP_RTS
         MOVE.L   BUFLEN(A0),D1
         MOVEQ    #-1,D2
         QDOS     MT.ALCHP
         TST.L    D0
         BNE.S    REP_RTS
         MOVE.L   A0,A5
         MOVE.L   D1,-(A7)
         SUBA.L   A0,A0
         LEA      STARTMSG,A1
         BSR      PRINTMSG
         MOVEQ    #10,D1
         QDOS     IO.SBYTE
         MOVE.L   A5,A1
         MOVEQ    #-$10,D2
         ADD.L    (A7)+,D2
         TRAP     #0
         BSR      LDBLOCK
         ANDI     #$DFFF,SR
         MOVE.L   D0,-(A7)
         ADDQ.L   #1,D0
         IF NE THEN
            BSR      STOPTAPE
         ENDIF
         MOVE.L   (A7)+,D0
         MOVE.L   A1,D5
         SUB.L    A5,D5
         MOVE.L   A5,A2
CRTOLF   CMPI.B   #$0D,(A2)
         BNE.S    CRTOLF_1
         SUBQ.B   #3,(A2)
CRTOLF_1 ADDQ.W   #1,A2
         CMPA.L   A1,A2
         BLO      CRTOLF
SAV_ERR  MOVE.W   UT_ERR0,A2
         JSR      (A2)
ASKFN    SUBA.L   A0,A0
         LEA      OFMSG,A1
         BSR      PRINTMSG
         SECTION  MSG
OFMSG    STRING$  {'Output file? '}
         SECTION  CODE
         MOVE.L   (A6),A1
         ADDQ.W   #2,A1
         MOVEQ    #44,D2
         MOVEQ    #-1,D3
         TRAP     #4
         QDOS     IO.FLINE
         ADDQ.L   #1,D0
         BEQ.S    QLOAD
         SUBA.W   D1,A1
         SUBQ.W   #1,D1
         BEQ.S    QLOAD
         MOVE.W   D1,-2(A6,A1.L)
         LEA      -2(A1),A0
         BSR.S    OPEN_OUT
         BNE      ASKFN
         MOVE.L   D5,D2
         MOVE.L   A5,A1
         MOVEQ    #-1,D3
         QDOS     FS.SAVE
         MOVEQ    #ERR.BP,D1
         CMP.L    D1,D0
         IF EQ THEN
            QDOS     IO.SSTRG
         ENDIF
         MOVE.L   D0,D4
         QDOS     IO.CLOSE
         MOVE.L   D4,D0
         BNE      SAV_ERR
QLOAD    MOVE.L   A5,A0
         QDOS     MT.RECHP
         MOVEQ    #0,D0
         RTS

OPEN_OUT MOVE.L   A0,-(A7)
ASKFN_2  MOVE.L   (A7),A0
         MOVEQ    #-1,D1
         MOVEQ    #2,D3
         TRAP     #4
         QDOS     IO.OPEN
         TST.L    D0
         BEQ.S    OO_OK
         MOVEQ    #ERR.EX,D1
         CMP.L    D1,D0
         BNE.S    OO_ERR
         SUBA.L   A0,A0
         MOVE.L   (A7),A1
         MOVE.W   0(A6,A1.L),D2
         ADDQ.W   #2,A1
         TRAP     #4
         QDOS     IO.SSTRG
         LEA      EXMSG,A1
         BSR      PRINTMSG
         SECTION  MSG
EXMSG    STRING$  {' exists, overwrite? '}
         SECTION  CODE
         BSR      REPLY
         BNE.S    OO_FAIL
         MOVE.L   (A7),A0
         MOVEQ    #-1,D1
         TRAP     #4
         QDOS     IO.DELET
         BRA      ASKFN_2
OO_ERR   MOVE.W   UT_ERR0,A2
         JSR      (A2)
OO_FAIL  MOVE.L   (A7)+,A0
         MOVEQ    #-1,D0
         RTS
OO_OK    ADDQ.W   #4,A7
         RTS

ITOD_L   MOVEM.L  D0-D1,-(A7)
         IF D1 HS.L #10 THEN
            MOVEQ    #0,D0
            SWAP     D1
            MOVE.W   D1,D0
            DIVU     #10,D0
            SWAP     D0
            MOVE.W   D0,D1
            SWAP     D1
            DIVU     #10,D1
            MOVE.W   D1,D0
            SWAP     D1
            EXG      D0,D1
            BSR      ITOD_L
            MOVE.B   D0,D1
         ENDIF
         ADDI.B   #'0',D1
         MOVE.B   D1,0(A6,A1.L)
         ADDQ.W   #1,A1
         ADDQ.W   #1,D2
         MOVEM.L  (A7)+,D0-D1
ITOD_RTS RTS

CLOAD    BSR      GET_NAME
         BNE      ITOD_RTS
         SUBA.L   A0,A0
         LEA      STARTMSG,A1
         BSR      PRINTMSG
         MOVEQ    #10,D1
         QDOS     IO.SBYTE
         MOVE.L   (A6),A4
         ADDQ.W   #2,A4
LD_LOOKH MOVEQ    #65,D2
         LEA      -1(A4),A1
         TRAP     #0
         ADDA.L   A6,A1
         BSR      LDBLOCK
         ANDI.W   #$DFFF,SR
         CMPI.W   #ERR.NC,D0
         BEQ      RECSTRG
         TST.L    D0
         BNE.S    LD_LOOKH
         ADDQ.B   #1,-1(A6,A4.L)
         BNE.S    LD_LOOKH
         SUBA.L   A0,A0
         MOVEQ    #-1,D3
         LEA      $10(A4),A1
         MOVE.W   $0E(A6,A4.L),D2
         TRAP     #4
         QDOS     IO.SSTRG
         MOVEQ    #10,D1
         QDOS     IO.SBYTE
         LEA      64(A4),A1
         MOVE.L   0(A6,A4.L),D1
         MOVEQ    #0,D2
         BSR      ITOD_L
         MOVEQ    #$20,D0
         MOVE.B   D0,0(A6,A1.L)
         MOVE.B   D0,1(A6,A1.L)
         SUBA.W   D2,A1
         ADDQ.W   #2,D2
         SUBA.L   A0,A0
         TRAP     #4
         QDOS     IO.SSTRG
         MOVEQ    #32,D1
         MOVE.W   BV_CHRIX,A2
         JSR      (A2)
         MOVE.L   $58(A6),A1
         MOVE.L   $3C(A6,A4.L),D1
         MOVE.W   CN_DATE,A2
         JSR      (A2)
         MOVE.W   0(A6,A1.L),D2
         ADDQ.W   #2,A1
         TRAP     #4
         QDOS     IO.SSTRG
         MOVEQ    #10,D1
         QDOS     IO.SBYTE
         MOVE.L   $58(A6),A1
         TST.W    0(A6,A1.L)
         IF NE THEN
            LEA      $0E(A4),A0
            MOVEQ    #1,D0
            MOVE.W   UT_CSTR,A2
            JSR      (A2)
            BNE      LD_LOOKH
         ENDIF
         MOVE.L   0(A6,A4.L),D1
         ADDQ.L   #2,D1
         MOVEQ    #-1,D2
         QDOS     MT.ALCHP
         TST.L    D0
         BNE      RECSTRG
         MOVE.L   A0,A5
         SUBA.L   A0,A0
         PRINT    {'Loading..',10}
         MOVE.L   0(A6,A4.L),D2
         ADDQ.L   #1,D2
         LEA      1(A5),A1
         TRAP     #0
         BSR      LDBLOCK
         ANDI.W   #$DFFF,SR
         MOVE.W   UT_ERR0,A2
         JSR      (A2)
         BSR      STOPTAPE
ASKFN_3  SUBA.L   A0,A0
         LEA      OFMSG,A1
         BSR      PRINTMSG
         LEA      $10(A4),A1
         MOVEQ    #0,D1
         MOVE.W   $0E(A6,A4.L),D1
         ADDA.W   D1,A1
         MOVEQ    #48,D2
         MOVEQ    #-1,D3
         TRAP     #4
         QDOS     IO.EDLIN
         CMPI.W   #ERR.NC,D0
         BEQ.S    LDQUIT
         SUBQ.W   #1,D1
         BEQ.S    LDQUIT
         MOVE.W   D1,$0E(A6,A4.L)
         LEA      $0E(A4),A0
         BSR      OPEN_OUT
         BNE      ASKFN_3
         MOVE.L   A4,A1
         MOVEQ    #-1,D3
         TRAP     #4
         QDOS     FS.HEADS
         MOVE.L   0(A6,A4.L),D2
         LEA      2(A5),A1
         QDOS     FS.SAVE
         MOVE.L   D0,D4
         QDOS     IO.CLOSE
         MOVE.L   D4,D0
         BEQ.S    LDQUIT
         MOVE.W   UT_ERR0,A2
         JSR      (A2)
         BRA      ASKFN_3
LDQUIT   MOVE.L   D0,D4
         MOVE.L   A5,A0
         QDOS     MT.RECHP
         MOVE.L   D4,D0
         BRA      RECSTRG

CSAVE    BSR      GET_NAME
         BNE      SAVRTS
         MOVEQ    #-1,D1
         MOVEQ    #1,D3
         TRAP     #4
         QDOS     IO.OPEN
         TST.L    D0
         BNE      RECSTRG
         MOVE.L   (A6),A4
         ADDQ.W   #2,A4
         MOVE.L   A4,A1
         MOVEQ    #64,D2
         MOVEQ    #-1,D3
         TRAP     #4
         QDOS     FS.HEADR
         TST.L    D0
         BNE.S    SAVCLOS
         MOVE.L   A0,A5
         MOVE.L   0(A6,A4.L),D1
         ADDQ.L   #2,D1
         MOVEQ    #-1,D2
         QDOS     MT.ALCHP
         EXG      A0,A5
         TST.L    D0
         BNE.S    SAVCLOS
         LEA      2(A5),A1
         MOVE.L   0(A6,A4.L),D2
         MOVEQ    #-1,D3
         QDOS     FS.LOAD
         TST.L    D0
         BNE.S    RECBUF
         MOVE.L   A0,-(A7)
         BSR.S    SAV_KEY
         QDOS     MT.RCLCK
         MOVE.L   D1,$3C(A6,A4.L)
         ST       -1(A6,A4.L)
         MOVEQ    #65,D2
         LEA      -1(A4),A1
         TRAP     #0
         ADDA.L   A6,A1
         BSR      SAVBLOCK
         MOVE.L   0(A6,A4.L),D2
         ADDQ.L   #1,D2
         LEA      1(A5),A1
         BSR      SAVBLOCK
         ANDI.W   #$DFFF,SR
         BSR.S    STOPTAPE
         MOVE.L   (A7)+,A0
         MOVEQ    #0,D0
RECBUF   MOVE.L   D0,D4
         EXG      A0,A5
         QDOS     MT.RECHP
         EXG      A0,A5
         MOVE.L   D4,D0
SAVCLOS  MOVE.L   D0,D4
         QDOS     IO.CLOSE
         MOVE.L   D4,D0
         BRA      RECSTRG
SAVRTS   RTS

SAV_KEY  SUBA.L   A0,A0
         LEA      PLRECMSG,A1
         BSR      PRINTMSG
         MOVEQ    #-1,D3
         QDOS     SD.CURE
         QDOS     IO.FBYTE
         QDOS     SD.CURS
         MOVEQ    #10,D1
         QDOS     IO.SBYTE
         LEA      SAVMSG,A1
         BSR      PRINTMSG
         MOVEQ    #10,D1
         QDOS     IO.SBYTE
         RTS

STOPTAPE LEA      IPC_PAR,A3
         QDOS     MT.IPCOM
         RTS
IPC_PAR  DC.B     $0A,8,$55,$55,$AA,$AA,5,5,0,0,144,13,0,0,1


* CASSETTE I/O DEVICE DRIVER

CAS_DIR  EQU      $18   DIRECTION: <=0 INPUT, >0 OUTPUT
CAS_BLK  EQU      $19   BLOCK NUMBER
CAS_BPOS EQU      $1A   CURRENT POSITION IN BUFFER
CAS_BSOH EQU      $1C   START OF SENT DATABLOCK (CONTAINS SOH CHR)
CAS_BBLK EQU      $1D   BUFFER BLOCK NUMBER
CAS_BDAT EQU      $1E   START OF 1024-BYTE DATABUFFER
CAS_BETX EQU      $41E  END OF DATABUFFER - CONTAINS ETX CHR
CAS_LEN  EQU      $420  LENGTH OF CAS CHANNEL

CAS_OPEN SUBQ.W   #2,A7
         MOVE.L   A7,A3
         MOVE.W   IO_NAME,A2
         JSR      (A2)
         BRA.S    OP_RTS
         BRA.S    OP_RTS
         BRA.S    OP_OK
         STRING$  {'CAS'}
         DC.W     1
         DC.W     2
         DC.B     'IO'
OP_OK    MOVE.L   #CAS_LEN,D1
         MOVE.W   MM_ALCHP,A2
         JSR      (A2)
         BNE.S    OP_RTS
         MOVE.B   1(A7),CAS_DIR(A0)
         ADDQ.B   #1,CAS_BSOH(A0)
         ADDQ.B   #3,CAS_BETX(A0)
         SUBQ.B   #1,CAS_DIR(A0)
         IF LE THEN
            BSR      FETCHBUF
         ENDIF
OP_RTS   ADDQ.W   #2,A7
         RTS

CAS_CLOSE:
         TST.B    CAS_DIR(A0)
         IF GT THEN
            MOVE.W   CAS_BPOS(A0),D0
            REPEAT
               MOVE.B   #4,CAS_BDAT(A0,D0.W)
               ADDQ.W   #1,D0
            UNTIL    D0 GE.W #$400
            BSR.S    SENDBUF
         ENDIF
BOOT_CL  MOVE.W   MM_RECHP,A2
         JMP      (A2)

CAS_PEND TST.B    CAS_DIR(A0)
         BGT.S    CAS_BP
         MOVE.W   CAS_BPOS(A0),D0
         MOVE.B   CAS_BDAT(A0,D0.W),D1
         IF D0 GE.W #$400 THEN
            BSR      FETCHBUF
            BNE.S    CAS_RTS
            BRA      CAS_PEND
         ENDIF
         IF D1 EQ.B #$0D THEN
            MOVEQ    #$0A,D1
         ENDIF
         CMPI.B   #4,D1
         BEQ.S    CAS_EOF
CAS_OK   MOVEQ    #0,D0
CAS_RTS  RTS
CAS_EOF  MOVEQ    #ERR.EF,D0
         RTS
CAS_BP   MOVEQ    #ERR.BP,D0
         RTS

CAS_FETCH:
         BSR      CAS_PEND
         IF EQ THEN
            ADDQ.W   #1,CAS_BPOS(A0)
         ENDIF
         RTS
CAS_SEND TST.B    CAS_DIR(A0)
         BLE.S    CAS_BP
         MOVE.W   CAS_BPOS(A0),D0
         IF D1 EQ.B #$0A THEN
            MOVEQ    #$0D,D1
         ENDIF
         MOVE.B   D1,CAS_BDAT(A0,D0.W)
         ADDQ.W   #1,D0
         MOVE.W   D0,CAS_BPOS(A0)
         CMPI.W   #$400,D0
         BLT      CAS_OK
SENDBUF  MOVE.B   CAS_BLK(A0),CAS_BBLK(A0)
         ADDQ.B   #1,CAS_BLK(A0)
         CLR.W    CAS_BPOS(A0)
         LEA      PLRECMSG,A1
         BSR      PRMSG
         MOVEQ    #0,D3
         MOVE.L   A0,-(A7)
         MOVE.L   D3,A0
         REPEAT
            QDOS     IO.FBYTE
            TST.L    D0
         UNTIL    EQ
         MOVE.L   (A7)+,A0
         LEA      SAVMSG,A1
         BSR      PRMSG
         MOVE.L   #1027,D2
         LEA      CAS_BSOH(A0),A1
         BSR.S    SAVBLOCK
         LEA      STOPMSG,A1
         BSR      PRMSG
         BSR.S    WAIT2S
         BRA      CLRLN

WAIT2S   MOVE.L   A0,-(A7)
         QDOS     MT.INF
         CLR.W    $30(A0)
         REPEAT
         UNTIL    $30(A0) GE.W #100
         MOVE.L   (A7)+,A0
         RTS

* Save block of bytes
* Entry: D2 length of block, A1 start of block

SAVBLOCK MOVEM.L  D2-D7/A0/A2-A4,-(A7)
         BSR      XMITWAIT
         MOVE.L   (A7),D2
         MOVEQ    #0,D5
         MOVEQ    #7,D6
         MOVE.B   $A0(A0),D7        SV.TMOD
         BSET     D6,D7             Bit 7 = NET line
         MOVE.W   #5999,D3
         BSR.S    LEADER
         MOVEQ    #0,D3             Init. checksum
         MOVEQ    #62,D0            Timing constant
SA_LOOP  MOVE.B   (A1)+,D1
         MOVE.B   D1,(A4)
         BCHG     D6,D1
         EOR.B    D1,D3
         BSR.S    SA_BYTE
         MOVEQ    #61,D0
         SUBQ.L   #1,D2
         BHI.S    SA_LOOP
         MOVE.B   D3,D1
         MOVEQ    #63,D0
         BSR.S    SA_BYTE
         MOVE.W   #1199,D3
         BSR.S    LEADER
         MOVEQ    #0,D3
         BRA      LDEXIT

* Produce leader or trailer tone (2400Hz, all 1s)

LEADER   MOVEQ    #65,D0
         MOVEQ    #1,D1
LEAD_1   BSR.S    PERIOD
         MOVEQ    #71,D0
         DBF      D3,LEAD_1
         RTS

* Save byte in D1 (+ 2 stopbits)

SA_BYTE  ORI.W    #$0300,D1
         LSL.W    #1,D1             Allow for 1 startbit
SA_BYT1  BSR.S    PERIOD
         MOVEQ    #70,D0
         LSR.W    #1,D1
         BNE.S    SA_BYT1
         RTS

* Generate 1 period (of 2 if bit is 1)

PERIOD   BSR.S    HALFPER
         BTST     D5,D1
         BEQ.S    P_END
         MOVEQ    #76,D0
         BSR.S    HALFPER
         BTST     D5,D1
         BRA.S    P_END
HALFPER  DBF      D0,HALFPER
         BTST     D5,D1
         BNE.S    SA_OUT
         MOVEQ    #85,D0
SA_WAIT  DBF      D0,SA_WAIT
SA_OUT   MOVE.B   D7,(A2)
         MOVEQ    #81,D0
         BCHG     D6,D7
         BEQ.S    HALFPER
P_END    RTS

PRMSG    MOVEM.L  D1-D3/A0,-(A7)
         MOVE.L   A1,-(A7)
         SUBA.L   A0,A0
         MOVEQ    #0,D3
         SUBQ.W   #8,A7
         MOVE.L   A7,A1
         QDOS     SD.CHENQ
         MOVE.W   2(A7),D2
         ADDQ.W   #8,A7
         SUBQ.W   #1,D2
         MOVEQ    #0,D1
         QDOS     SD.POS
         QDOS     SD.CLRLN
         MOVE.L   (A7)+,A1
         BSR      PRINTMSG
         MOVEM.L  (A7)+,D1-D3/A0
         RTS

         SECTION  MSG
PLRECMSG STRING$  {'Press <PLAY> & <REC> then any key..'}
SAVMSG   STRING$  {'Saving..'}
STARTMSG STRING$  {'Start tape..'}
STOPMSG  STRING$  {'STOP THE TAPE'}
LOBLKMSG STRING$  {'LOW BLOCK - IGNORED'}
HIBLKMSG STRING$  {'HIGH BLOCK - REWIND TAPE'}
LDERRMSG STRING$  {'LOAD ERROR - REWIND TAPE'}
BDATMSG  STRING$  {'WARNING - BLOCK CONTAINS BAD DATA'}
NCMSG    STRING$  {'not complete',10}
         SECTION  CODE

F_BRK    LEA      NCMSG,A2
         MOVE.L   A2,D0
         BSET     #31,D0
         TST.L    D0
         RTS
FETCHBUF LEA      STARTMSG,A1
         BSR      PRMSG
F_AGAIN  MOVEQ    #3,D4
F_ERROR  MOVE.L   #1027,D2
         LEA      CAS_BSOH(A0),A1
         BSR      LDBLOCK
         BSR.S    CLRLN
         ADDQ.L   #1,D0
         BEQ.S    F_BRK
         SUBQ.L   #1,D0
         IF NE OR CAS_BSOH(A0) NE.B #1 THEN
            SUBQ.B   #1,D4
            BLT.S    F_BDATA
            LEA      LDERRMSG,A1
            BSR      PRMSG
            BRA      F_ERROR
         ENDIF
         MOVE.B   CAS_BBLK(A0),D0
         CMP.B    CAS_BLK(A0),D0
         BEQ.S    F_OK
         BHI.S    F_HIBLK
         LEA      LOBLKMSG,A1
         BRA.S    F_PRMSG
F_HIBLK  LEA      HIBLKMSG,A1
F_PRMSG  BSR      PRMSG
         BRA      F_AGAIN
F_BDATA  LEA      BDATMSG,A1
         BSR      PRMSG
         BSR      WAIT2S
F_OK     ADDQ.B   #1,CAS_BLK(A0)
         CLR.W    CAS_BPOS(A0)
         LEA      CAS_BDAT(A0),A1
         MOVE.W   #$400,D0
         REPEAT
            CMPI.B   #4,(A1)
            BEQ.S    F_NXTCHR
            CMPI.B   #$0D,(A1)
            BEQ.S    F_NXTCHR
            CMPI.B   #$20,(A1)
            BGE.S    F_NXTCHR
            MOVE.B   #'#',(A1)
F_NXTCHR    ADDQ.W   #1,A1
            SUBQ.W   #1,D0
         UNTIL    EQ
         MOVEQ    #0,D0
CLRLN    MOVEM.L  D0-D3/A0-A1,-(A7)
         MOVEQ    #0,D3
         MOVE.L   D3,A0
         QDOS     SD.CLRLN
         MOVEQ    #0,D1
         QDOS     SD.TAB
         MOVEM.L  (A7)+,D0-D3/A0-A1
         TST.L    D0
         RTS

XMITWAIT QDOS     MT.INF
         REPEAT
            TST.B    $EE(A0)
         UNTIL    EQ
         REPEAT
            SUBQ.W   #1,$A6(A0)
            EXITIF   LT
            MOVE.W   #$208B,D0
WAIT30MS    DBF      D0,WAIT30MS
         FOREVER
         CLR.W    $A6(A0)
         ORI.B    #$18,$A0(A0)
         LEA      $18020,A3
         LEA      -$1E(A3),A2
         MOVE.B   $A0(A0),(A2)
         MOVEQ    #-1,D1
         MOVEQ    #-1,D2
         QDOS     MT.DMODE
         LEA      $27FC0,A4
         ST       (A4)
         TST.B    D1
         IF NE THEN
            MOVE.B   #$AA,(A4)
         ENDIF
         ADDQ.W   #1,A4
         ORI      #$0700,SR
         RTS

LDBLOCK  MOVEM.L  D2-D7/A0/A2-A4,-(A7)
         BSR      XMITWAIT
         MOVE.L   (A7),D2
         MOVEQ    #59,D5
         MOVEQ    #1,D6
         MOVE.B   (A3),D7
LDBRK    BSR.S    BRKTST
         BCS.S    EXBRK
         MOVEQ    #0,D1
         BSR      EDGE1
         BEQ      LDBRK
         MOVE.W   #2400,D4
LD_HEADR BSR      EDGE2
         BEQ      LDBRK
         CMPI.B   #34,D1
         BLO      LDBRK
         CMP.B    D5,D1
         BHI      LDBRK
         DBF      D4,LD_HEADR
         MOVEQ    #0,D3
         BSR.S    LDSYNC
         BRA.S    LDSTORE
LDNEXT   BSR.S    LDBYTE
         BEQ.S    TSTCHK
LDSTORE  SUBQ.L   #1,D2
         BLO.S    LDCHK
         BCHG     #7,D4
         MOVE.B   D4,(A4)
         MOVE.B   D4,(A1)+
         BRA      LDNEXT
LDCHK    BSR.S    LDBYTE
         BEQ.S    TSTCHK
         MOVEQ    #ERR.BO,D0
         BRA.S    LDEXIT
EXBRK    MOVEQ    #ERR.NC,D0
         BRA.S    LDEXIT
TSTCHK   MOVE.L   D3,D0
         BEQ.S    LDEXIT
         LEA      CKERRMSG,A2
         MOVE.L   A2,D0
         BSET     #31,D0
LDEXIT   ANDI.B   #$E7,$A0(A0)
         ANDI     #$F8FF,SR
         CLR.W    -1(A4)
         MOVEM.L  (A7)+,D2-D7/A0/A2-A4
         TST.L    D0
         RTS

         SECTION  MSG
CKERRMSG STRING$  {'checksum error',10}
         SECTION  CODE

BRKTST   MOVEM.L  D5/D7/A3,-(A7)
         LEA      CMD_09,A3
         QDOS     MT.IPCOM
         MOVEM.L  (A7)+,D5/D7/A3
         LSR.B    #4,D1
         RTS
CMD_09   DC.B     9,1,0,0,0,0,1,2
LDBRK2   BSR      BRKTST
         BCC.S    LDSYNC
         ADDQ.W   #4,A7
         BRA      EXBRK
LDSYNC   MOVEQ    #1,D4
LDSYNC2  MOVEQ    #187,D1
         BSR.S    EDGE1
         BEQ      LDBRK2
         CMPI.B   #221,D1
         BLO      LDSYNC
         DBF      D4,LDSYNC2
         BRA.S    LDMARK
LDBYTE   MOVEQ    #0,D4
LDBYTE1  SUBQ.B   #1,D4
         BEQ.S    LDBYTRTS
         BSR.S    EDGE2
         BEQ      LDBRK2
         CMP.B    D5,D1
         BLO      LDBYTE1
LDMARK   MOVEQ    #$80,D4
LDBIT    BSR.S    EDGE2
         BEQ      LDBRK2
         SUB.B    D5,D1
         BCC.S    LD_0
         BSR.S    EDGE2
         BEQ      LDBRK2
         SUB.B    D5,D1
LD_0     ROXR.B   #1,D4
         BCC      LDBIT
         EOR.B    D4,D3
         MOVEQ    #1,D0
LDBYTRTS RTS

EDGE2    MOVEQ    #0,D1
         BSR.S    EDGE1
         BEQ.S    LD_ED_RT
EDGE1    ADDQ.B   #1,D1
         BEQ.S    LD_ED_RT
         MOVE.B   (A3),D0
         EOR.B    D7,D0
         AND.B    D6,D0
         BEQ      EDGE1
         EOR.B    D6,D7
         MOVEQ    #1,D0
LD_ED_RT RTS

         END
