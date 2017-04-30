* BASICODE-3 PART 3
* CONVERT-TO-BASICODE ROUTINE

         INCLUDE  MACRO_LIB
         INCLUDE  QDOS_IN_MAC

         XDEF     BCSAVE
         XREF     TOK_TBL,GET_CHAN,REPLY,SAVBLOCK,GET1INT,PRINTMSG
         XREF     GET_SCHD,SAV_KEY,STOPTAPE
         XREF.S   BUFLEN

.CODE    SETSTR   {}

         SECTION CODE

KEYTBL   DC.B     KEND-KEYTBL
         DC.B     KFOR-KEYTBL
         DC.B     KIF-KEYTBL
         DC.B     KREPEAT-KEYTBL
         DC.B     KSELECT-KEYTBL
         DC.B     KWHEN-KEYTBL
         DC.B     KDEFINE-KEYTBL
         DC.B     KPROC-KEYTBL
         DC.B     KFN-KEYTBL
         DC.B     KGO-KEYTBL
         DC.B     KTO-KEYTBL
         DC.B     KSUB-KEYTBL
         DC.B     0
         DC.B     KERROR-KEYTBL
         DC.B     0,0
         DC.B     KRESTORE-KEYTBL
         DC.B     KNEXT-KEYTBL
         DC.B     KEXIT-KEYTBL
         DC.B     KELSE-KEYTBL
         DC.B     KON-KEYTBL
         DC.B     KRETURN-KEYTBL
         DC.B     KREMAIN-KEYTBL
         DC.B     KDATA-KEYTBL
         DC.B     KDIM-KEYTBL
         DC.B     KLOCAL-KEYTBL
         DC.B     KLET-KEYTBL
         DC.B     KTHEN-KEYTBL
         DC.B     KSTEP-KEYTBL
         DC.B     KREMARK-KEYTBL
         DC.B     KMISTAKE-KEYTBL
KEND     DC.B     $83,'END'
KFOR     DC.B     3,'FOR'
KIF      DC.B     2,'IF'
KREPEAT  DC.B     $86,'REPEAT'
KSELECT  DC.B     $86,'SELECT'
KWHEN    DC.B     $84,'WHEN'
KDEFINE  DC.B     $83,'DEF'
KPROC    DC.B     $84,'PROC'
KFN      DC.B     $82,'FN'
KGO      DC.B     3,' GO'
KTO      DC.B     3,' TO'
KSUB     DC.B     3,'SUB'
KERROR   DC.B     $85,'ERROR'
KRESTORE DC.B     7,'RESTORE'
KNEXT    DC.B     4,'NEXT'
KEXIT    DC.B     $84,'EXIT'
KELSE    DC.B     $84,'ELSE'
KON      DC.B     2,'ON'
KRETURN  DC.B     6,'RETURN'
KREMAIN  DC.B     $89,'REMAINDER'
KDATA    DC.B     4,'DATA'
KDIM     DC.B     3,'DIM'
KLOCAL   DC.B     $85,'LOCAL'
KLET     DC.B     3,'LET'
KTHEN    DC.B     5,' THEN'
KSTEP    DC.B     5,' STEP'
KREMARK  DC.B     4,'REM '
KMISTAKE DC.B     0

SYMTBL   DC.B     '='
         DC.B     ':'
         DC.B     '#'+$80
         DC.B     ','
         DC.B     '('
         DC.B     ')'
         DC.B     '{'+$80
         DC.B     '}'+$80
         DC.B     ' '
         DC.B     $0D

BINOPTBL DC.B     BPLUS-BINOPTBL
         DC.B     BMIN-BINOPTBL
         DC.B     BAST-BINOPTBL
         DC.B     BSLASH-BINOPTBL
         DC.B     BGE-BINOPTBL
         DC.B     BGT-BINOPTBL
         DC.B     BEQ2-BINOPTBL
         DC.B     BEQ1-BINOPTBL
         DC.B     BNE-BINOPTBL
         DC.B     BLE-BINOPTBL
         DC.B     BLT-BINOPTBL
         DC.B     BBOR-BINOPTBL
         DC.B     BBAND-BINOPTBL
         DC.B     BBXOR-BINOPTBL
         DC.B     BPOWER-BINOPTBL
         DC.B     BCONCAT-BINOPTBL
         DC.B     BOR-BINOPTBL
         DC.B     BAND-BINOPTBL
         DC.B     BXOR-BINOPTBL
         DC.B     BMOD-BINOPTBL
         DC.B     BDIV-BINOPTBL
         DC.B     BINSTR-BINOPTBL
BPLUS    DC.B     1,'+'
BMIN     DC.B     1,'-'
BAST     DC.B     1,'*'
BSLASH   DC.B     1,'/'
BGE      DC.B     2,'>='
BGT      DC.B     1,'>'
BEQ2     DC.B     $82,'=='
BEQ1     DC.B     1,'='
BNE      DC.B     2,'<>'
BLE      DC.B     2,'<='
BLT      DC.B     1,'<'
BBOR     DC.B     $82,'||'
BBAND    DC.B     $82,'&&'
BBXOR    DC.B     $82,'^^'
BPOWER   DC.B     1,'^'
BCONCAT  DC.B     1,'+'
BOR      DC.B     2,'OR'
BAND     DC.B     3,'AND'
BXOR     DC.B     $83,'XOR'
BMOD     DC.B     $83,'MOD'
BDIV     DC.B     $83,'DIV'
BINSTR   DC.B     $85,'INSTR'

MONOPTBL DC.B     MMIN-MONOPTBL
         DC.B     MPLUS-MONOPTBL
         DC.B     MBNOT-MONOPTBL
         DC.B     MNOT-MONOPTBL
MMIN     DC.B     1,'-'
MPLUS    DC.B     1,'+'
MBNOT    DC.B     $82,'~~'
MNOT     DC.B     3,'NOT'

SEPTBL   DC.B     SCOMMA-SEPTBL
         DC.B     SSEMI-SEPTBL
         DC.B     SBSL-SEPTBL
         DC.B     SEXCL-SEPTBL
         DC.B     STO-SEPTBL
SCOMMA   DC.B     1,','
SSEMI    DC.B     1,';'
SBSL     DC.B     $81,'\'
SEXCL    DC.B     $81,'!'
STO      DC.B     3,'TAB'
         DC.W     0

VARTBL   DC.B     'ASATDIEIFNGRLNPISQSTTI'

         SECTION  MSG
ERRTBL   DC.W     WARNMSG-ERRTBL
         DC.W     LN2LONG-ERRTBL
         DC.W     ILLMSG-ERRTBL
         DC.W     CHRERR-ERRTBL
         DC.W     KEYERR-ERRTBL
         DC.W     SYMERR-ERRTBL
         DC.W     OPERR-ERRTBL
         DC.W     SEPERR-ERRTBL
         DC.W     PROCERR-ERRTBL
         DC.W     FNERR-ERRTBL
         DC.W     VARERR-ERRTBL
         DC.W     TWOLF-ERRTBL
WARNMSG  STRING$  {'*** WARNING : '}
LN2LONG  STRING$  {'Line longer than 60 characters'}
ILLMSG   STRING$  {'Illegal '}
CHRERR   STRING$  {'character'}
KEYERR   STRING$  {'keyword'}
SYMERR   STRING$  {'symbol'}
OPERR    STRING$  {'operator'}
SEPERR   STRING$  {'separator'}
PROCERR  STRING$  {'procedure name'}
FNERR    STRING$  {'function name'}
VARERR   STRING$  {'variable name'}
TWOLF    STRING$  {10,10}
         SECTION  CODE

ERR.WR   EQU      0
ERR.LN   EQU      2
ERR.IL   EQU      4
ERR.CH   EQU      6
ERR.KW   EQU      8
ERR.SM   EQU      10
ERR.OP   EQU      12
ERR.SP   EQU      14
ERR.PR   EQU      16
ERR.FN   EQU      18
ERR.VR   EQU      20
ERR.LF   EQU      22

BCSAVE   BSR      GET_CHAN
         BNE.S    BCSAV_RT
         MOVE.W   #1000,D4
         CMPA.L   A3,A5
         BEQ.S    BCSAVE_1
         MOVE.L   A0,-(A7)
         BSR      GET1INT
         MOVE.L   (A7)+,A0
         BNE.S    BCSAV_RT
         MOVE.W   0(A6,A1.L),D4
         ADDQ.L   #2,$58(A6)
BCSAVE_1 MOVE.L   A0,-(A7)
         BSR      GET_SCHD
         MOVE.L   A0,A3
         MOVE.L   (A7)+,A0
         BNE.S    BCSAV_RT
         MOVE.L   A0,-(A7)
         MOVE.L   BUFLEN(A3),D1
         MOVEQ    #-1,D2
         QDOS     MT.ALCHP
         MOVE.L   (A7)+,A3
         TST.L    D0
         BNE.S    BCSAV_RT
         MOVEQ    #-32,D6
         ADD.L    D1,D6
         MOVE.L   A0,A5
         MOVE.W   #$020D,(A0)+
         MOVEQ    #0,D7
         MOVE.L   $10(A6),A4
         MOVEQ    #0,D1
FIND1000 CMPA.L   $14(A6),A4
         BHS.S    BCSAV_EN
         CMP.W    4(A6,A4.L),D4
         BLE.S    TRPROG
         ADD.W    0(A6,A4.L),D1
         ADDQ.W   #2,A4
         ADDA.W   D1,A4
         BRA      FIND1000
TRPROG   MOVEQ    #$10,D1
         MOVE.W   BV_CHRIX,A2
         JSR      (A2)
         BSR.S    TRPROG1
BCSAV_EN MOVE.L   A5,A0
         MOVE.L   D0,D4
         QDOS     MT.RECHP
         MOVE.L   D4,D0
BCSAV_RT RTS

TRPROG1  CMPA.L   $14(A6),A4
         BHS      GOSAVE
         ADDQ.W   #2,A4
         SF       D3
         MOVE.W   2(A6,A4.L),D4
         MOVE.L   D6,D5
TRNEXTCH CMPI.W   #$840A,0(A6,A4.L)
         IF EQ THEN
            BSR.S    DO_TOKEN
            SUB.L    D6,D5
            IF D5 HI.W #61 THEN
               MOVEQ    #ERR.LN,D0
               BSR      WARNING
            ENDIF
            BRA      TRPROG1
         ENDIF
         BSR.S    DO_TOKEN
         BRA      TRNEXTCH

DO_TOKEN MOVEQ    #0,D0
         MOVEQ    #0,D1
         MOVE.B   0(A6,A4.L),D0
         SUBI.B   #$80,D0
         CMPI.B   #$70,D0
         BGE.S    TK_FP
         MOVE.B   1(A6,A4.L),D1
         ADDQ.W   #2,A4
         LSL.W    #1,D0
         MOVE.W   TOKTABLE(PC,D0.W),D0
         JMP      TOKTABLE(PC,D0.W)

ERR_BO   ADDQ.W   #4,A7
         MOVEQ    #ERR.BO,D0
         RTS

TK_FP    MOVE.L   $58(A6),A1
         SUBQ.W   #6,A1
         MOVE.W   0(A6,A4.L),0(A6,A1.L)
         ANDI.W   #$0FFF,0(A6,A1.L)
         MOVE.L   2(A6,A4.L),2(A6,A1.L)
         ADDQ.W   #6,A4
         TRAP     #0
         MOVE.L   A6,-(A7)
         ADDA.L   A6,A1
         SUBA.L   A6,A6
         MOVE.W   CN_FTOD,A2
         JSR      (A2)
         MOVE.L   (A7)+,A6
         ANDI.W   #$DFFF,SR
         SUB.L    D1,D6
         BLT      ERR_BO
         RTS

TOKTABLE DC.W     TK_SP-TOKTABLE
         DC.W     TK_KEY-TOKTABLE
         DC.W     TK_SP-TOKTABLE
         DC.W     TK_SP-TOKTABLE
         DC.W     TK_SYM-TOKTABLE
         DC.W     TK_BINOP-TOKTABLE
         DC.W     TK_MONOP-TOKTABLE
         DC.W     TK_SP-TOKTABLE
         DC.W     TK_NAME-TOKTABLE
         DC.W     TK_SP-TOKTABLE
         DC.W     TK_SP-TOKTABLE
         DC.W     TK_STR-TOKTABLE
         DC.W     TK_TXT-TOKTABLE
         DC.W     TK_LINUM-TOKTABLE
         DC.W     TK_SEP-TOKTABLE

TK_SP    RTS

TK_SYM   LEA      SYMTBL,A1
         MOVE.B   -1(A1,D1.W),D1
         IF MI THEN
            MOVEQ    #ERR.SM,D0
            BSR      WARNING
            ANDI.B   #$7F,D1
         ENDIF
         SUBQ.L   #1,D6
         BLT      ERR_BO
         MOVE.B   D1,(A0)+
         RTS

TK_STR   SUBQ.L   #2,D6
         BLT      ERR_BO
         MOVE.B   #'"',(A0)+
         BSR.S    TK_TXT1
         MOVE.B   #'"',(A0)+
         RTS

TK_TXT   BSR.S    TK_TXT1
         RTS
TK_TXT1  MOVE.W   0(A6,A4.L),D1
         SUB.L    D1,D6
         IF LT THEN
            ADDQ.W   #4,A7
            BRA      ERR_BO
         ENDIF
         LEA      2(A4),A1
         MOVEQ    #0,D0
         BRA.S    TK_TXT3
TK_TXT2  MOVE.B   0(A6,A1.L),D2
         IF D2 LT.B #$20 OR D2 HI.B #$7E THEN
            MOVEQ    #ERR.CH,D0
         ENDIF
         MOVE.B   D2,(A0)+
         ADDQ.W   #1,A1
TK_TXT3  DBF      D1,TK_TXT2
         TST.B    D0
         IF NE THEN
            BSR      WARNING
         ENDIF
         MOVEQ    #3,D1
         ADD.W    0(A6,A4.L),D1
         BCLR     #0,D1
         ADDA.W   D1,A4
         RTS

TK_LINUM MOVE.L   $58(A6),A1
         SUBQ.W   #2,A1
         MOVE.W   0(A6,A4.L),0(A6,A1.L)
         ADDQ.W   #2,A4
         TRAP     #0
         MOVE.L   A6,-(A7)
         ADDA.L   A6,A1
         SUBA.L   A6,A6
         MOVE.W   CN_ITOD,A2
         JSR      (A2)
         MOVE.L   (A7)+,A6
         ANDI.W   #$DFFF,SR
         MOVE.B   #' ',(A0)+
         ADDQ.W   #1,D1
         SUB.L    D1,D6
         BLT      ERR_BO
         RTS

TK_KEY   MOVEQ    #ERR.KW,D0
         LEA      KEYTBL,A1
         BRA.S    COPY_TK
TK_BINOP LEA      BINOPTBL,A1
         BRA.S    COPY_OP
TK_MONOP LEA      MONOPTBL,A1
COPY_OP  MOVEQ    #ERR.OP,D0
         BRA.S    COPY_TK
TK_SEP   MOVEQ    #ERR.SP,D0
         LEA      SEPTBL,A1
COPY_TK  MOVE.B   -1(A1,D1.W),D1
         ADDA.W   D1,A1
         MOVE.B   (A1)+,D1
         IF MI THEN
            BSR      WARNING
            ANDI.B   #$7F,D1
         ENDIF
         MOVE.W   -2(A6,A4.L),D0
         IF D0 EQ.W #$810A THEN
            BSET     #0,D3
            BCLR     #1,D3
            IF EQ THEN
               SUBQ.W   #1,D1
               ADDQ.W   #1,A1
            ENDIF
         ENDIF
         IF D0 EQ.W #$810B THEN
            BCLR     #0,D3
            IF NE THEN
               SUBQ.W   #1,D1
               ADDQ.W   #1,A1
            ENDIF
         ENDIF
         SUB.L    D1,D6
         BLT      ERR_BO
         BRA.S    COPY_TK3
COPY_TK2 MOVE.B   (A1)+,(A0)+
COPY_TK3 DBF      D1,COPY_TK2
         IF D0 EQ.W #$811C AND {0(A6,A4.L)} EQ.L #$810A810B THEN
            ADDQ.W   #4,A4
         ENDIF
         IF D0 EQ.W #$8115 THEN
            BSET     #1,D3
         ENDIF
         IF D0 EQ.W #$8402 THEN
            SF       D3
         ENDIF
         RTS

TK_NAME  MOVE.W   0(A6,A4.L),D1
         ADDQ.W   #2,A4
         LSL.L    #3,D1
         MOVE.L   $18(A6),A1
         ADDA.L   D1,A1
         MOVE.W   2(A6,A1.L),A2
         ADDA.L   $20(A6),A2
         MOVE.B   0(A6,A2.L),D1
         MOVEQ    #0,D2
TKNAM_LP ADDQ.W   #1,A2
         MOVE.B   0(A6,A2.L),D0
         CMPI.B   #'_',D0
         IF NE THEN
            IF GE THEN
               SUBI.B   #$20,D0
            ENDIF
            MOVE.B   D0,(A0)+
            ADDQ.W   #1,D2
         ELSE
            TST.W    D2
            IF EQ THEN
               SUBQ.L   #2,D6
               MOVE.B   #'F',(A0)+
               MOVE.B   #'N',(A0)+
            ENDIF
         ENDIF
         SUBQ.B   #1,D1
         BNE      TKNAM_LP
         SUB.L    D2,D6
         BLT      ERR_BO
         MOVE.L   A0,A2
         SUBA.W   D2,A2
         MOVE.B   0(A6,A1.L),D1
         SUBQ.B   #4,D1
         BEQ      BADPROC
         SUBQ.B   #4,D1
         BGE.S    TSTPROC
         MOVE.W   D2,D1
         CMPI.B   #'%',D0
         BEQ.S    BADVAR
         IF LE THEN
            SUBQ.W   #1,D1
         ENDIF
         TST.W    D2
         BEQ.S    BADVAR
         CMPI.B   #'O',(A2)
         BEQ.S    BADVAR
         SUBQ.W   #2,D1
         BGT.S    BADVAR
         IF EQ THEN
            MOVE.B   (A2)+,D1
            LSL.W    #8,D1
            MOVE.B   (A2),D1
            IF D0 EQ.B #'$' THEN
               CMPI.W   #'TI',D1
               BEQ.S    BADVAR
            ELSE
               LEA      VARTBL,A1
               MOVEQ    #11,D0
               REPEAT
                  CMP.W    (A1)+,D1
                  BEQ.S    BADVAR
                  SUBQ.B   #1,D0
               UNTIL EQ
            ENDIF
         ENDIF
         RTS
BADVAR   MOVEQ    #ERR.VR,D0
         BRA.S    WARNING
TSTPROC  LEA      TOK_TBL,A1
         SUBQ.W   #1,D2
NXTTOKEN MOVE.B   (A1)+,D0
         BPL      NXTTOKEN
         ADDQ.B   #1,D0
         BEQ.S    BADNAME
         MOVE.W   D2,D1
         MOVE.L   A2,-(A7)
TSTNAMLP CMPM.B   (A1)+,(A2)+
         DBNE     D1,TSTNAMLP
         MOVE.L   (A7)+,A2
         BNE      NXTTOKEN
         TST.B    (A1)+
         BPL      NXTTOKEN
         CMPI.B   #$92,D0     TEST FOR "RUN"
         BEQ.S    BADNAME
         CMPI.B   #$96,D0     TEST FOR "STOP"
         BEQ.S    BADNAME
         RTS
BADNAME  MOVEQ    #ERR.FN,D0
         TST.B    D1
         BGT.S    WARNING
BADPROC  MOVEQ    #ERR.PR,D0

WARNING  SUBA.L   $10(A6),A4
         MOVEM.L  D0-D7/A0-A5,-(A7)
         MOVE.W   D4,D6
         MOVEQ    #0,D7
         MOVE.L   A3,A0
         ST       $AB(A6)
         MOVE.W   $138,A2
         JSR      $4000(A2)
         MOVEQ    #ERR.WR,D0
         BSR.S    PRMSG
         MOVE.L   (A7),D0
         SUBQ.W   #ERR.LN,D0
         IF NE THEN
            MOVEQ    #ERR.IL,D0
            BSR.S    PRMSG
         ENDIF
         MOVE.L   (A7),D0
         BSR.S    PRMSG
         MOVEQ    #ERR.LF,D0
         BSR.S    PRMSG
         MOVEM.L  (A7)+,D0-D7/A0-A5
         ADDA.L   $10(A6),A4
         ADDQ.W   #1,D7
         RTS
PRMSG    LEA      ERRTBL,A1
         MOVE.W   0(A1,D0.W),D0
         ADDA.W   D0,A1
         BRA      PRINTMSG

GOSAVE   MOVE.B   #3,(A0)+
         MOVE.W   D7,D1
         MOVE.L   A0,-(A7)
         SUBA.L   A0,A0
         MOVE.W   UT_MINT,A2
         JSR      (A2)
         LEA      SAVMSG,A1
         BSR      PRINTMSG
         SECTION  MSG
SAVMSG   STRING$  {' Warnings, Save? '}
         SECTION  CODE
         BSR      REPLY
         MOVE.L   (A7)+,A4
         BNE.S    SAVRTS
         BSR      SAV_KEY
         MOVE.L   A4,D2
         SUB.L    A5,D2
         MOVE.L   A5,A1
         TRAP     #0
         BSR      SAVBLOCK
         ANDI     #$DFFF,SR
         BSR      STOPTAPE
SAVRTS   RTS

         END
