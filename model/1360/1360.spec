column	label	type	guide	required	derivation
C	nonmem comment	character	//C/commented//./not commented//	0	.
ID	nonmem identifier	integer	[1,644]	1	.
SEQ	secondary record sequence	integer	//0//1//	1	.
TIME	time	numeric	h [-694.75,4175.33]	1	.
TAD	time after dose	numeric	h [-24.07,1390.67]	0	.
TAFD	time after first dose	numeric	h [-1991.58,3577]	0	.
EVID	nonmem event identifier	integer	//0/observation//1/dose//	1	.
CMT	compartment identifier	integer	//1/depot//2/central//	1	.
AMT	drug amount	integer	mg [0,800]	0	.
SS	steady-state flag	integer	//0/not steady-state//1/steady-state//	0	.
II	interdose interval	integer	h [0,24]	0	.
LDOS	amount of most recent dose	integer	mg [0,800]	0	.
DV	observed neratinib plasma concentration	numeric	ng/mL [3,327]	0	.
MDV	missing dependent value	integer	//0/DV present//1/DV missing//	1	.
BLQ	below limit of quantitation	integer	//0/not BLQ//1/BLQ//	0	.
LLOQ	lower limit of quantitation	integer	ng/mL [3,3]	0	.
OBST	observation nominal time	numeric	h [-2,96]	0	.
IMPTIME	time was imputed	integer	//0/time not imputed//1/time imputed//	0	.
AGE	age	integer	y [18,90]	0	.
RACEN	race	integer	//1/WHITE//2/BLACK OR AFRICAN AMERICAN//3/HISPANIC//4/ASIAN//5/AMERICAN INDIAN OR ALASKA NATIVE//6/ARABIC//7/NATIVE HAWAIIAN OR OTHER PACIFIC ISLANDER//8/OTHER MULTI-RACIAL//9/UNKNOWN//	0	.
SEXN	sex	integer	//0/female//1/male//	0	.
HGT	height	numeric	cm [143,198.1]	0	.
WGT	weight	numeric	kg [39,130]	0	.
BILI	total bilirubin	numeric	umol/L [0.171,53.01]	1	.
AST	plasma AST	numeric	U/L [7,239]	0	.
ALT	SGPT_ALT	numeric	U/L [5,380]	0	.
KETO	concomitant ketoconazole	integer	//0/no concom. ketoconazole//1/concomitant ketoconazole//	1	.
TRAS	concomitant trastuzumab	integer	//0/no concom. trastuzumab//1/concomitant trastuzumab//	0	.
CAPE	concomitant capecitabine	integer	//0/no concom. capecitabine//1/concomitant capecitabine//	0	.
TADGT100	time after dose greater than 100 h	integer	//0/100 h after dose or less//1/more than 100 h after dose//	1	.
STUDY	study identifier	integer	//102/3144A1-102-US//2206/3144A1-2206-WW//3003/3144A2-3003-WW//104/3144A1-104-JA//201/3144A1-201-WW//105/3144A1-105-US//107/3144A1-107-US//1116/3144A1-1116-US//1117/3144A1-1117-US//1127/3144A1-1127//4201/NER-4201//	1	.
RATE	nonmem rate item	integer	//0/default//-2/estimate duration//	1	.
HV	subject type	integer	//0/patient//1/healthy volunteer//	1	.
LEGACY	study in original analysis	integer	//0/new study//1/original study//	1	.
LBW	lean body weight	numeric	kg [32.36,84.52]	0	.
IBW	ideal body weight	numeric	kg [45.5,91.4]	0	.
BMI	body mass index	numeric	kg/m2 [15.96,50.54]	0	.
STINT	dosing period	character	//0//1//C1D15 1 HR//C1D15 2 HR//C1D15 24 HR//C1D15 4 HR//C1D15 5 HR//C1D15 6 HR//C1D15 PREDOSE//C3D1 PREDOSE//C4D1 PREDOSE//Cycle 1 Day 1//Cycle 1 Day 14//Cycle 1 Day 15//Cycle 2 Day 1//Daily Dosing Period//FOLLOWUP//MEDICATION at C1D15//MEDICATION at C3D1//MEDICATION at C4D1//MONTH 1 WEEK 1//MONTH 2 WEEK 1//MONTH 3 WEEK 1//MONTH 4 WEEK 1//MONTH 5 WEEK 1//MONTH 6 WEEK 1//ON THERAPY//PERIOD 1//PERIOD 2//PERIOD 3//PERIOD 4//PERIOD 5//POSTSTUDY//PRESTUDY//Single Dose Period//	0	.
DATETIME	date and time	character	%Y-%m-%d %H:%M	0	.
RACE	race	character	//AMERICAN INDIAN OR ALASKA NATIVE//ASIAN//BLACK OR AFRICAN AMERICAN//HISPANIC//NATIVE HAWAIIAN OR OTHER PACIFIC ISLANDER//OTHER MULTI-RACIAL//UNKNOWN//WHITE//	0	.
SEX	sex	character	//M//F//	0	.
SUBJ	universal subject identifier	character	.	1	.
NADV	dependent value is missing	integer	//0/DV not missing//1/DV is missing//	1	.
ZEROAMT	amount is zero	integer	//0/amount not zero//1/amount is zero//	1	.
PREDOSE	observation is predose	integer	//0/not predose//1/predose//	1	.
NOPK	subject has no contributing PK	integer	//0/pk present//1/no pk//	1	.
HOUR	observed time	numeric	h [0,504.12]	0	.
ALP	alkaline phosphatase	numeric	U/L [28,1093]	0	.
ANTDIA	concomitant anti-diarrheal med use	integer	//0/no concom. antidiarrheal//1/concom. antidiarrheal//	0	.
ASTHI	AST normal high	integer	U/L [35,59]	0	.
ASTHIN	AST greater than normal high	integer	//0/AST not above normal//1/AST above normal//	0	.
BILIHI	BILI normal high	numeric	umol/L [17,23.94]	0	.
BILIHIN	BILI greater than normal high	integer	//0/bilirubin not above normal//1/bilirubin above normal//	0	.
BSA	body surface area	numeric	m2 [1.296,2.553]	0	.
CRCLBSA	BSA-indexed creatinine clearance	numeric	mL/min [30.57,212.7]	0	.
CRCLCAT	BSA-indexed creatinine clearance category	character	//A/[60,]//B/[40,60)//C/[20,40)//D/[,20)//	0	.
CRCLCATN	BSA-indexed creatinine clearance	integer	//1/[60,]//2/[40,60)//3/[20,40)//4/[,20)//	0	.
CREAT	creatinine	numeric	mg/dL [0.4,1.55]	0	.
HEPTCAT	hepatic dysfunction category	character	//A/normal bilirubin and AST//B/elevated AST or bilirubin up to 1.5 times normal high//C/bilirubin up to 3 times normal high//D/bilirubin more than 3 times normal high//	0	.
HEPTCATN	hepatic dysfunction numeric	integer	//1/normal bilirubin and AST//2/elevated AST or bilirubin up to 1.5 times normal high//3/bilirubin up to 3 times normal high//4/bilirubin more than 3 times normal high//	0	.
IMPHT	height was imputed	integer	//0/height observed//1/height imputed//	0	.
IMPWT	weight was imputed	integer	//0/weight observed//1/weight imputed//	0	.
INDUCER	concomitant inducer use	integer	//0/no concom. inducer//1/concom. inducer//	0	.
INHIBTR	concomitant inhibitor use	integer	//0/no concom. inhibitor//1/concom. inhibitor//	0	.
LOPERA	concomitant loperamide use	integer	//0/no concom. loperamide//1/concom. loperamide//	0	.
PPI	concomitant proton pump inhibitor use	integer	//0/no concom. proton pump inhibitor//1/concom. proton pump inhibitor//	0	.
PRIMTU	cancer type	character	.	0	.
PRIMTUN	cancer type	integer	//0/Healthy Volunteer//1/Breast Cancer//2/Colorectal Cancer//3/Gastric Cancer//4/Glioblastoma Cancer//5/Head and Neck - Squamous Cell Cancer//6/Non-Small Cell Lung Cancer//7/Ovarian Cancer//8/Pancreatic Cancer//9/Renal Cancer//10/Other: Adenocarcinoma of lung wt multifocal bronch//11/Other: Gall bladder//12/Other: Metastatic colon cancer to scrotum//13/Other: Oligoastrocytoma//14/Other: Synovial sarcoma right lung//15/Other: TCC of bladder and incidental prostate canc//16/Other: Unknown primary//	0	.
TEMSIRO	concomitant temsirolimus use	integer	//0/no concom. temsirolimus//1/concom. temsirolimus//	0	.
VISIBLE	record visible to NONMEM	integer	//0/no//1/yes//	1	.
TIME.1	copy of time	numeric	h [0,4175.3]	0	.
IPRE	indivual predicted neratinib conc.	numeric	ng/mL [0,225.18]	0	.
CWRESI	conditional weighted residual	numeric	[-3.723,8.7707]	0	.
CIWRESI	individualized conditional weighted residual	numeric	[-2.9818,8.7707]	0	.
PRED	population predicted neratinib conc	numeric	ng/mL [0,189.88]	0	.
RES	residual	numeric	ng/mL [-91.751,258.08]	0	.
WRES	weighted residual	numeric	[-6.2059,15.984]	0	.
CL	systemic clearance	numeric	L/h [17.484,769.23]	0	.
Q	intercompartmental clearance	numeric	L/h [41.612,65.796]	0	.
V2	central volume	numeric	L [192.79,33859]	0	.
V3	peripheral volume	numeric	[1098.5,1916]	0	.
KA	first-order absorption rate constant	numeric	1/h [0.060084,3.4451]	0	.
ETA1	IIV of systemic clearance	numeric	[-1.4047,1.367]	0	.
ETA2	IIV of central volume	numeric	[-2.5287,2.2928]	0	.
ETA3	IIV of absorption rate constant	numeric	[-1.7174,2.3356]	0	.
