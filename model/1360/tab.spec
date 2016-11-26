column	label	type	guide	required	derivation
VISIBLE	record visible to NONMEM	integer	//0/no//1/yes//	1	.
TIME.1	copy of time	numeric	h [0,0]	0	.
IPRE	indivual predicted neratinib conc.	numeric	ng/mL [0,0]	0	.
CWRESI	conditional weighted residual	numeric	[0,0]	0	.
CIWRESI	individualized conditional weighted residual	numeric	[0,0]	0	.
PRED	population predicted neratinib conc	numeric	ng/mL [0,0]	0	.
RES	residual	numeric	ng/mL [0,0]	0	.
WRES	weighted residual	numeric	[0,0]	0	.
CL	systemic clearance	numeric	L/h [0,0]	0	.
Q	intercompartmental clearance	numeric	L/h [0,0]	0	.
V2	central volume	numeric	L [0,0]	0	.
V3	peripheral volume	numeric	[0,0]	0	.
KA	first-order absorption rate constant	numeric	1/h [0,0]	0	.
ETA1	IIV of systemic clearance	numeric	[0,0]	0	.
ETA2	IIV of central volume  	numeric	[0,0]	0	.
ETA3	IIV of absorption rate constant	numeric	[0,0]	0	.
