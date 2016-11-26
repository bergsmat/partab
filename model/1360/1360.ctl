$PROBLEM //like/1341//but/diagonal omega//
$INPUT C,ID,SEQ,TIME,TAD,TAFD,EVID,CMT,AMT,SS,II,LDOS,DV,MDV,BLQ,LLOQ,
OBST,IMPT,AGE,RACE,SEX,HGT,WGT,BILI,AST,ALT,KETO,TRAS,CAPE,TADG,STUD,
RTE,HV,LEGA

$DATA ../../data/derived/PUMA-PCS-101_ex3.csv IGNORE= (C=C, TADG=1)
$SUBROUTINE ADVAN4 TRANS4

$PK
 DD = THETA(12)**TRAS * THETA(13)**CAPE * THETA(15)**KETO
 LV = (BILI/10)**THETA(10) * (LOG(ALT)/3)**THETA(11)
 CL = THETA(1)*EXP(ETA(1)) * (WGT/70) **THETA(4) * (AGE/53)**THETA(9) * LV * DD
 V2 = THETA(2)*EXP(ETA(2)) * (WGT/70) **THETA(5) * (AGE/53)**THETA(14)
 KA = THETA(3)*EXP(ETA(3)) * (AGE/53)**THETA(16)
 Q  = THETA(6)             * (WGT/70) **THETA(4)
 V3 = THETA(7)             * (WGT/70) **THETA(5)
 S2 =V2/1000
 ALAG1 = THETA(8)
 
$ERROR
 Y=F*(1+ERR(1)) + ERR(2)
 IPRE=F

$THETA
; 
(0,181); CL(L/h)
(0,4520); V2 (L)
(0,0.404); KA
(0,0.253); WT on CL
(0,0.562); WT on V
(0,137); Q (L/h)
(0,1010); V3 (L)
(0,0.581);  alag1
1.07; age on CL
-0.0781; bili
0.0402; alt
1.53; tras
1.29; cap
1.11; age on V
0.938; keto
1; age on Ka
$OMEGA
.1 .1 .1
;.01 .1
;.01 .01 .1
;.01 .01 .01 .1
;.01 .01 .01 .01 .1


$SIGMA 0.1 0.1


$ESTIMATION MAXEVAL=9999 PRINT=5 NOABORT METHOD=1 INTER MSFO=./mod.msf
$COV PRINT=E
$TABLE NOPRINT FILE=./mod.tab ONEHEADER ID AMT TIME EVID PRED IPRE CWRESI CIWRESI
$TABLE NOPRINT FILE=./mod_par.tab ONEHEADER ID TIME CL Q V2 V3 KA ETA1 ETA2 ETA3

