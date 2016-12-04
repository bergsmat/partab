$PROBLEM //like/1001//but/2 CMT//
$INPUT C ID TIME SEQ=DROP EVID AMT DV SUBJ HOUR HEIGHT WT SEX AGE DOSE FED
$DATA ../../data/derived/phase1.csv IGNORE=C
$SUBROUTINE ADVAN4 TRANS4
$PK
 CL=THETA(1)*EXP(ETA(1)) * THETA(6)**SEX * (WT/70)**THETA(7)
 V2 =THETA(2)*EXP(ETA(2))
 KA=THETA(3)*EXP(ETA(3))
 Q  =THETA(4)
 V3=THETA(5)
 S2=V2
 
$ERROR
 Y=F*(1+ERR(1)) + ERR(2)
 IPRE=F

$THETA 
(0,10,50)     ;CL/F;clearance;L/h
(0,10,100)    ;Vc/F;central volume;L
(0,0.2, 5)    ;Ka;absorption rate constant;1/h
(0,10,50)     ;Q/F;intercompartmental clearance;L/h
(0,100,1000)  ;Vp/F;peripheral volume;L
(0,1,2)       ;MALE_CL;male effect on clearance;
(0,0.75,3)    ;WT_CL;weight effect on clearance;

$OMEGA BLOCK(3)
.1            ;IIV_CL;interindividual variability on clearance
.01           ;CL_V;interindividual clearance-volume covariance
.1            ;IIV_Vc;interindividual variability on central volume
.01           ;CL_Ka;interindividual clearance-Ka covariance
.01           ;Vc_Ka;interindividual volume-Ka covariance
.1            ;IIV_Ka;interindividual variability on Ka

$SIGMA 
0.1           ;ERR_PROP;proportional error
0.1           ;ERR_ADD;additive error

$ESTIMATION MAXEVAL=9999 PRINT=5 NOABORT METHOD=1 INTER MSFO=./1005.msf
$COV PRINT=E
$TABLE NOPRINT FILE=./1005.tab ONEHEADER ID AMT TIME EVID PRED IPRE CWRES
$TABLE NOPRINT FILE=./1005par.tab ONEHEADER ID TIME CL Q V2 V3 KA ETA1 ETA2 ETA3
