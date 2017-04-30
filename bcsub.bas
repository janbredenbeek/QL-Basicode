1 :
2 REMark BASICODE-3 Standard Subroutines for Sinclair QL
3 REMark Copyright (C) 1987, 1988, 2017 by Jan Bredenbeek
4 REMark Released under GNU Public License v3 (2017)
7 :
8 REMark Start: Jump to line 1000
9 :
10 GO TO 1000
11 :
15 REMark GOTO 20: Initialisation of windows, variables etc.
16 REMark NOTE: You may need to adjust this for non-native environments, e.g. emulators
17 REMark The default uses a text screen of 24 x 40 and graphic screen of 480 x 240
18 REMark BASICODE assumes an aspect ratio of 4/3 on square pixels - not always the case here!
19 :
20 SCALE 1,-(1.48-4/3)/2,-1
21 REMark BASICODE-3C colour initialisation
22 CLEAR:DIM CC_(1):CC_(0)=7:CC_(1)=0:GO SUB 100
23 REMark the variables HO, VE, HG, and VG are set by m/c functions according to window size
24 REMark HO,VE: Text window size; HG,VG: Graphics resolution
25 OPEN#3,ser1c:HO=HSIZE-1:VE=VSIZE-1:SV=35:HG=WWIDTH:VG=WHEIGHT:oAspect=4/3
30 IN$="":IN=0:RANDOMISE:GO TO 1010
95 :
96 REMark GOSUB 100: Clear the screen (or in case of QL the standard window), also selects text mode
98 :
100 oFG=CC_(0):oBG=CC_(1):INK oFG:PAPER oBG:CLS:RETurn
105 :
106 REMark GOSUB 110: Position the text cursor at position HO, VE
107 :
110 AT INT(VE) MOD VSIZE,INT(HO) MOD HSIZE:RETurn
115 :
116 REMark GOSUB 120: Return current text cursor position in HO and VE (uses HPOS and VPOS extensions)
117 :
120 HO=HPOS:VE=VPOS:RETurn
145 :
146 REMark GOSUB 150: Print string SR$ using emphasis (inverse)
147 :
150 PRINT " ";:STRIP CC_(0):INK CC_(1):PRINT "  ";SR$;"  ";:STRIP oBG:INK oFG:PRINT " ";:RETurn
195 :
196 REMark GOSUB 200: Return key currently being pressed in IN$ and code in IN
197 :
200 IN$=INKEY$
201 IN=BSC_CODE(IN$):IF IN<0:IN$=""
202 IF IN=13:IN$=CHR$(13)
203 RETurn
205 :
206 REMark GOSUB 210: Wait for keypress and return key code in IN and IN$
207 :
210 CURS_ON:IN$=INKEY$(#1,-1):CURS_OFF:GO TO 201
215 :
216 REMark GOSUB 220: Return code of character at position (HO,VE) in IN
217 REMark NOTE: This will probably fail on environments with non-QL screen layout
218 :
220 OI$=SCREEN$(INT(HO),INT(VE)):IN=BSC_CODE(OI$):CN=CODE(OI$)-IN:RETurn
245 :
246 REMark GOSUB 250: Generate a BEEP
248 :
250 BCSOUND 12,81:RETurn
255 :
256 REMark GOSUB 260: Return a random number in RV; 0<=RV<1
258 :
260 RV=RND:RETurn
265 :
266 REMark GOSUB 270: Return amount of free memory in FR
267 REMark NOTE: On TK2 equipped QLs (and SMSQ) you may want to change this to FR=FREE_MEM
268 :
270 FR=PEEK_L(163856)-PEEK_L(163852):RETurn
275 :
276 REMark GOSUB 280: Enable or disable BREAK as indicated in FR
277 :
280 IF FR=1:BREAK_OFF:ELSE BREAK_ON
281 RETurn
295 :
296 REMark GOSUB 300: Return string representation of number SR in SR$
297 :
300 SR$=SR:RETurn
305 :
306 REMark GOSUB 310: Return string representation of SR in SR$, formatted
307 REMark CT=total length, CN=number of decimals
308 :
310 SR$=USING$(CT,CN,SR):RETurn
325 :
326 REMark GOSUB 330: Convert SR$ to uppercase
328 :
330 SR$=SHIFT$(SR$):RETurn
345 :
346 REMark GOSUB 350: Print SR$ to printer (NOTE: #3 should have been opened in line 20)
348 :
350 PRINT#3;SR$;:RETurn
355 :
356 REMark GOSUB 360: Print a CR/LF to printer
358 :
360 PRINT#3:RETurn
395 :
396 REMark GOSUB 400: Play a tone of SD*100 ms and pitch SP or silence if SV=0
398 :
400 IF SV:BCSOUND SD*5,SP:RETurn
401 OS=SD:OI$=IN$:OI=IN
402 GO SUB 450:IF SD:GO TO 402
403 SD=OS:IN$=OI$:IN=OI:RETurn
445 :
446 REMark GOSUB 450: Wait SD*100 ms or when a key is pressed
448 :
450 IN$=INKEY$
451 IF SD<=0:SD=0:GO TO 201
452 IF IN$<>"":GO TO 201
453 IN$=INKEY$(1):SD=SD-.2:GO TO 451
490 :
491 REMark GOSUB 500: Open channel NF to file NF$ for read (NF=even) or write (NF=odd)
492 REMark Uses cassette if NF is 0 to 3 or MDV/Disk storage if NF is 4 to 7
495 REMark NOTE: Cassette I/O is ONLY supported in the EPROM version, on native QLs!
496 REMark Feel free to adapt this subroutine to your own storage devices (flp, win etc.)
499 :
500 IF NF<4
505   IF NF MOD 2:OD$="CASO":ELSE OD$="CASI"
510 ELSE
515   IF NF<6:OD$="MDV1_"&NF$:ELSE OD$="MDV2_"&NF$
520 END IF
525 IF NF MOD 2:IN=FOPEN_NEW(#NF+4,OD$):ELSE IN=FOPEN_IN(#NF+4,OD$)
530 RETurn
535 :
537 REMark GOSUB 540: Read a line IN$ of data from channel NF, set IN to error status
538 :
540 GETLINE#NF+4;IN$:IN=IOSTATUS:IF NOT IN THEN IN=EOF(#NF+4)
550 RETurn
555 :
556 REMark GOSUB 560: Print string SR$ to channel NF, return error status in IN
558 :
560 PUTLINE#NF+4;SR$:IN=IOSTATUS:RETurn
575 :
576 REMark GOSUB 580: Close channel NF
578 :
580 CLOSE#NF+4:IN=0:RETurn
595 :
596 REMark GOSUB 600: Clear and initialise graphics screen
598 :
600 oBG=CC_(1):PAPER oBG:CLS:LINE 0,0:RETurn
615 :
616 REMark GOSUB 620: Plot a point on (HO,VE) in colour CN; 0<=HO<1 and 0<=VE<1
617 REMark Note aspect ratio: should always be 4/3!
618 :
620 IF NOT CN:INK CC_(0):ELSE INK oBG
622 POINT HO*oAspect,-VE:INK CC_(0):RETurn
625 :
626 REMark GOSUB 630: Draw a line to point (HO,VE) in colour CN
628 :
630 IF NOT CN:INK CC_(0):ELSE INK oBG
632 LINE TO HO*oAspect,-VE:INK CC_(0):RETurn
645 :
647 REMark GOSUB 650: Print string SR$ on graphics screen at (HO,VE) in colour CN
648 :
650 IF NOT CN:INK CC_(0):ELSE INK oBG
652 OVER 1:LINE HO*oAspect,-VE:CURSOR HO*oAspect,-VE,0,0:PRINT SR$;:OVER 0:INK CC_(0):RETurn
945 :
946 REMark GOTO 950: End program
948 :
950 CLEAR:STOP
995 :
997 REMark Application program starts at 1000
998 :
