 
 XDEF BASPROG
 SECTION BASIC

BASPROG:
 DC.B '1 :',10
 DC.B '2 REMark BASICODE-3 Standard Subroutines for Sinclair QL',10
 DC.B '3 REMark Copyright (C) 1987, 1988, 2017 by Jan Bredenbeek',10
 DC.B '4 REMark Released under GNU Public License v3 (2017)',10
 DC.B '7 :',10
 DC.B '8 REMark Start: Jump to line 1000',10
 DC.B '9 :',10
 DC.B '10 GO TO 1000',10
 DC.B '11 :',10
 DC.B '15 REMark GOTO 20: Initialisation of windows, variables etc.',10
 DC.B '16 REMark NOTE: You may need to adjust this for non-native environments, e.g. emulators',10
 DC.B '17 REMark The default uses a text screen of 24 x 40 and graphic screen of 480 x 240',10
 DC.B '18 REMark BASICODE assumes an aspect ratio of 4/3 on square pixels - not always the case here!',10
 DC.B '19 :',10
 DC.B '20 SCALE 1,-(1.48-4/3)/2,-1',10
 DC.B '21 REMark BASICODE-3C colour initialisation',10
 DC.B '22 CLEAR:DIM CC_(1):CC_(0)=7:CC_(1)=0:GO SUB 100',10
 DC.B '23 REMark the variables HO, VE, HG, and VG are set by m/c functions according to window size',10
 DC.B '24 REMark HO,VE: Text window size; HG,VG: Graphics resolution',10
 DC.B '25 OPEN#3,ser1c:HO=HSIZE-1:VE=VSIZE-1:SV=35:HG=WWIDTH:VG=WHEIGHT:oAspect=4/3',10
 DC.B '30 IN$="":IN=0:RANDOMISE:GO TO 1010',10
 DC.B '95 :',10
 DC.B '96 REMark GOSUB 100: Clear the screen (or in case of QL the standard window), also selects text mode',10
 DC.B '98 :',10
 DC.B '100 oFG=CC_(0):oBG=CC_(1):INK oFG:PAPER oBG:CLS:RETurn',10
 DC.B '105 :',10
 DC.B '106 REMark GOSUB 110: Position the text cursor at position HO, VE',10
 DC.B '107 :',10
 DC.B '110 AT INT(VE) MOD VSIZE,INT(HO) MOD HSIZE:RETurn',10
 DC.B '115 :',10
 DC.B '116 REMark GOSUB 120: Return current text cursor position in HO and VE (uses HPOS and VPOS extensions)',10
 DC.B '117 :',10
 DC.B '120 HO=HPOS:VE=VPOS:RETurn',10
 DC.B '145 :',10
 DC.B '146 REMark GOSUB 150: Print string SR$ using emphasis (inverse)',10
 DC.B '147 :',10
 DC.B '150 PRINT " ";:STRIP CC_(0):INK CC_(1):PRINT "  ";SR$;"  ";:STRIP oBG:INK oFG:PRINT " ";:RETurn',10
 DC.B '195 :',10
 DC.B '196 REMark GOSUB 200: Return key currently being pressed in IN$ and code in IN',10
 DC.B '197 :',10
 DC.B '200 IN$=INKEY$',10
 DC.B '201 IN=BSC_CODE(IN$):IF IN<0:IN$=""',10
 DC.B '202 IF IN=13:IN$=CHR$(13)',10
 DC.B '203 RETurn',10
 DC.B '205 :',10
 DC.B '206 REMark GOSUB 210: Wait for keypress and return key code in IN and IN$',10
 DC.B '207 :',10
 DC.B '210 CURS_ON:IN$=INKEY$(#1,-1):CURS_OFF:GO TO 201',10
 DC.B '215 :',10
 DC.B '216 REMark GOSUB 220: Return code of character at position (HO,VE) in IN',10
 DC.B '217 REMark NOTE: This will probably fail on environments with non-QL screen layout',10
 DC.B '218 :',10
 DC.B '220 OI$=SCREEN$(INT(HO),INT(VE)):IN=BSC_CODE(OI$):CN=CODE(OI$)-IN:RETurn',10
 DC.B '245 :',10
 DC.B '246 REMark GOSUB 250: Generate a BEEP',10
 DC.B '248 :',10
 DC.B '250 BCSOUND 12,81:RETurn',10
 DC.B '255 :',10
 DC.B '256 REMark GOSUB 260: Return a random number in RV; 0<=RV<1',10
 DC.B '258 :',10
 DC.B '260 RV=RND:RETurn',10
 DC.B '265 :',10
 DC.B '266 REMark GOSUB 270: Return amount of free memory in FR',10
 DC.B '267 REMark NOTE: On TK2 equipped QLs (and SMSQ) you may want to change this to FR=FREE_MEM',10
 DC.B '268 :',10
 DC.B '270 FR=PEEK_L(163856)-PEEK_L(163852):RETurn',10
 DC.B '275 :',10
 DC.B '276 REMark GOSUB 280: Enable or disable BREAK as indicated in FR',10
 DC.B '277 :',10
 DC.B '280 IF FR=1:BREAK_OFF:ELSE BREAK_ON',10
 DC.B '281 RETurn',10
 DC.B '295 :',10
 DC.B '296 REMark GOSUB 300: Return string representation of number SR in SR$',10
 DC.B '297 :',10
 DC.B '300 SR$=SR:RETurn',10
 DC.B '305 :',10
 DC.B '306 REMark GOSUB 310: Return string representation of SR in SR$, formatted',10
 DC.B '307 REMark CT=total length, CN=number of decimals',10
 DC.B '308 :',10
 DC.B '310 SR$=USING$(CT,CN,SR):RETurn',10
 DC.B '325 :',10
 DC.B '326 REMark GOSUB 330: Convert SR$ to uppercase',10
 DC.B '328 :',10
 DC.B '330 SR$=SHIFT$(SR$):RETurn',10
 DC.B '345 :',10
 DC.B '346 REMark GOSUB 350: Print SR$ to printer (NOTE: #3 should have been opened in line 20)',10
 DC.B '348 :',10
 DC.B '350 PRINT#3;SR$;:RETurn',10
 DC.B '355 :',10
 DC.B '356 REMark GOSUB 360: Print a CR/LF to printer',10
 DC.B '358 :',10
 DC.B '360 PRINT#3:RETurn',10
 DC.B '395 :',10
 DC.B '396 REMark GOSUB 400: Play a tone of SD*100 ms and pitch SP or silence if SV=0',10
 DC.B '398 :',10
 DC.B '400 IF SV:BCSOUND SD*5,SP:RETurn',10
 DC.B '401 OS=SD:OI$=IN$:OI=IN',10
 DC.B '402 GO SUB 450:IF SD:GO TO 402',10
 DC.B '403 SD=OS:IN$=OI$:IN=OI:RETurn',10
 DC.B '445 :',10
 DC.B '446 REMark GOSUB 450: Wait SD*100 ms or when a key is pressed',10
 DC.B '448 :',10
 DC.B '450 IN$=INKEY$',10
 DC.B '451 IF SD<=0:SD=0:GO TO 201',10
 DC.B '452 IF IN$<>"":GO TO 201',10
 DC.B '453 IN$=INKEY$(1):SD=SD-.2:GO TO 451',10
 DC.B '490 :',10
 DC.B '491 REMark GOSUB 500: Open channel NF to file NF$ for read (NF=even) or write (NF=odd)',10
 DC.B '492 REMark Uses cassette if NF is 0 to 3 or MDV/Disk storage if NF is 4 to 7',10
 DC.B '495 REMark NOTE: Cassette I/O is ONLY supported in the EPROM version, on native QLs!',10
 DC.B '496 REMark Feel free to adapt this subroutine to your own storage devices (flp, win etc.)',10
 DC.B '499 :',10
 DC.B '500 IF NF<4',10
 DC.B '505   IF NF MOD 2:OD$="CASO":ELSE OD$="CASI"',10
 DC.B '510 ELSE',10
 DC.B '515   IF NF<6:OD$="MDV1_"&NF$:ELSE OD$="MDV2_"&NF$',10
 DC.B '520 END IF',10
 DC.B '525 IF NF MOD 2:IN=FOPEN_NEW(#NF+4,OD$):ELSE IN=FOPEN_IN(#NF+4,OD$)',10
 DC.B '530 RETurn',10
 DC.B '535 :',10
 DC.B '537 REMark GOSUB 540: Read a line IN$ of data from channel NF, set IN to error status',10
 DC.B '538 :',10
 DC.B '540 GETLINE#NF+4;IN$:IN=IOSTATUS:IF NOT IN THEN IN=EOF(#NF+4)',10
 DC.B '550 RETurn',10
 DC.B '555 :',10
 DC.B '556 REMark GOSUB 560: Print string SR$ to channel NF, return error status in IN',10
 DC.B '558 :',10
 DC.B '560 PUTLINE#NF+4;SR$:IN=IOSTATUS:RETurn',10
 DC.B '575 :',10
 DC.B '576 REMark GOSUB 580: Close channel NF',10
 DC.B '578 :',10
 DC.B '580 CLOSE#NF+4:IN=0:RETurn',10
 DC.B '595 :',10
 DC.B '596 REMark GOSUB 600: Clear and initialise graphics screen',10
 DC.B '598 :',10
 DC.B '600 oBG=CC_(1):PAPER oBG:CLS:LINE 0,0:RETurn',10
 DC.B '615 :',10
 DC.B '616 REMark GOSUB 620: Plot a point on (HO,VE) in colour CN; 0<=HO<1 and 0<=VE<1',10
 DC.B '617 REMark Note aspect ratio: should always be 4/3!',10
 DC.B '618 :',10
 DC.B '620 IF NOT CN:INK CC_(0):ELSE INK oBG',10
 DC.B '622 POINT HO*oAspect,-VE:INK CC_(0):RETurn',10
 DC.B '625 :',10
 DC.B '626 REMark GOSUB 630: Draw a line to point (HO,VE) in colour CN',10
 DC.B '628 :',10
 DC.B '630 IF NOT CN:INK CC_(0):ELSE INK oBG',10
 DC.B '632 LINE TO HO*oAspect,-VE:INK CC_(0):RETurn',10
 DC.B '645 :',10
 DC.B '647 REMark GOSUB 650: Print string SR$ on graphics screen at (HO,VE) in colour CN',10
 DC.B '648 :',10
 DC.B '650 IF NOT CN:INK CC_(0):ELSE INK oBG',10
 DC.B '652 OVER 1:LINE HO*oAspect,-VE:CURSOR HO*oAspect,-VE,0,0:PRINT SR$;:OVER 0:INK CC_(0):RETurn',10
 DC.B '945 :',10
 DC.B '946 REMark GOTO 950: End program',10
 DC.B '948 :',10
 DC.B '950 CLEAR:STOP',10
 DC.B '995 :',10
 DC.B '997 REMark Application program starts at 1000',10
 DC.B '998 :',10
 DC.B 0

 END
