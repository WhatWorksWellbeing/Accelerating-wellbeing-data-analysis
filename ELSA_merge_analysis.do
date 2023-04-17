*************ELSA Notes*****
**********We have two waves for ELSA in 2020. First one is elsa_cpvod_w1_eulv2 which has 7040 participants and the MCZs are CvMhOnsC CvMhOnsD CvMhOnsA CvMhOnsB. ************Second is elsa_cpvod_w2_eulv2 which has 6794 participants and the MCZs are CvMhOnsC CvMhOnsD CvMhOnsA CvMhOnsB.
cd "C:\Users\se0010\OneDrive - University of Surrey\Documents\What Works Wellbeing\Data\ELSA Covid 19 waves"


append using elsa_covid_w2_eulv2 , force
sort idauniq time 
xtset idauniq time
browse

sum CvMhOnsC CvMhOnsD CvMhOnsA CvMhOnsB

 


********************Perception of age********
sum CvMhcaspGridA_CvMhcasp1_q CvMhcaspGridA_CvMhcasp2_q /*
*/ CvMhcaspGridA_CvMhcasp3_q CvMhcaspGridA_CvMhcasp4_q CvMhcaspGridA_CvMhcasp5_q CvMhcaspGridA_CvMhcasp6_q CvMhcaspGridB_CvMhcasp7_q/*
*/ CvMhcaspGridB_CvMhcasp8_q CvMhcaspGridB_CvMhcasp9_q CvMhcaspGridB_CvMhcasp10_q CvMhcaspGridB_CvMhcasp11_q CvMhcaspGridB_CvMhcasp12_q nofvlabel if/*
*/ CvMhcaspGridA_CvMhcasp1_q>=1 & CvMhcaspGridA_CvMhcasp2_q>=1 &/*
*/ CvMhcaspGridA_CvMhcasp3_q>=1 & CvMhcaspGridA_CvMhcasp4_q>=1 & CvMhcaspGridA_CvMhcasp5_q>=1 & CvMhcaspGridA_CvMhcasp6_q>=1 & CvMhcaspGridB_CvMhcasp7_q>=1 &/*
*/ CvMhcaspGridB_CvMhcasp8_q>=1 & CvMhcaspGridB_CvMhcasp9_q>=1 & CvMhcaspGridB_CvMhcasp10_q>=1 & CvMhcaspGridB_CvMhcasp11_q>=1 & CvMhcaspGridB_CvMhcasp12_q>=1 


**********Generation of relevant socioeconomic variables**********



sum d_retired d_unemployed d_smoking d_single alcohol d_incomeless d_heill_2w d_helim_2w age_2w


gen d_retired=.
replace d_retired=0 if CvPstd==2|CvPstd==3 |CvPstd==4 |CvPstd==5
replace d_retired=1 if CvPstd==1


gen d_unemployed=.
replace d_unemployed=1 if CvPstd==6|CvPstd==7 |CvPstd==8 |CvPstd==5 
replace d_unemployed=0 if CvPstd==1| CvPstd==2|CvPstd==3 |CvPstd==4 

gen d_smoking=.
replace d_smoking=1 if CvHesmoke==1
replace d_smoking=0 if CvHesmoke==2

gen d_single=.
replace d_single=1 if RelStat==5 | RelStat==6 | RelStat==7 | RelStat==8
replace d_single=0 if RelStat==1 | RelStat==2 | RelStat==3 | RelStat==4


gen alcohol=.
replace alcohol=1 if CvHedrinkB==-1
replace alcohol=2 if CvHedrinkB==1
replace alcohol=3 if CvHedrinkB==2
replace alcohol=4 if CvHedrinkB==3

drop d_incomeless
gen d_incomeless=.
replace d_incomeless=1 if CvInc==3
replace d_incomeless=0 if CvInc==1 | CvInc==2


drop heill_2w
gen heill_2w=.
replace heill_2w = heill_updated 
replace heill_2w= HEILL if heill_2w==.

drop d_heill_2w
gen d_heill_2w=.
replace d_heill_2w=1 if heill_2w==1
replace d_heill_2w=0 if heill_2w==2

drop d_heill_2w
gen d_heill_2w=.
replace d_heill_2w = 1 if heill_2w==1
replace d_heill_2w=0 if heill_2w==2



gen helim_2w=.
replace helim_2w = helim_updated 
replace helim_2w= HELIM if helim_2w==.
gen d_helim_2w=.
replace d_helim_2w = 1 if helim_2w==1
replace d_helim_2w=0 if helim_2w==2



gen age_2w=.
replace age_2w = Age_Arch 
replace age_2w= Age_arch if age_2w==.

drop educ_2w
gen educ_2w=.
replace educ_2w = w9edqual 
replace educ_2w= w9edqual(time=2) if educ_2w==. 
gen d_helim_2w=.
replace d_helim_2w = 1 if helim_2w==1
replace d_helim_2w=0 if helim_2w==2

***************
sum CvMhOnsC CvMhOnsD CvMhOnsA CvMhOnsB 


***********************Distribution of the MCZs by frequency********

histogram  CvMhOnsC if CvMhOnsC<=10 & CvMhOnsC>=1, freq normal
histogram  CvMhOnsD if CvMhOnsC<=10 & CvMhOnsD>=1, freq normal
histogram  CvMhOnsA if CvMhOnsC<=10 & CvMhOnsA>=1, freq normal
histogram  CvMhOnsB if CvMhOnsC<=10 & CvMhOnsB>=1, freq normal
**********************
*************generation of the four MCZs*************
gen MCZ_1= CvMhOnsC if CvMhOnsC<=10 & CvMhOnsC>=0
gen MCZ_2=  CvMhOnsD if CvMhOnsD<=10 & CvMhOnsD>=0
gen MCZ_3=  CvMhOnsA if CvMhOnsA<=10 & CvMhOnsA>=0
gen MCZ_4= CvMhOnsB if CvMhOnsB<=10 & CvMhOnsB>=0

****************** generation of the 12 indicators of perception of ageing as likert scale value to idicate the value of 1 for answering often to 4 for answering never***********
gen CvMhcasp1_q =  CvMhcaspGridA_CvMhcasp1_q if CvMhcaspGridA_CvMhcasp1_q<=4 & CvMhcaspGridA_CvMhcasp1_q>=0
gen CvMhcasp2_q =  CvMhcaspGridA_CvMhcasp2_q if CvMhcaspGridA_CvMhcasp2_q<=4 & CvMhcaspGridA_CvMhcasp2_q>=0
gen CvMhcasp3_q =  CvMhcaspGridA_CvMhcasp3_q if CvMhcaspGridA_CvMhcasp3_q<=4 & CvMhcaspGridA_CvMhcasp3_q>=0
gen CvMhcasp4_q =  CvMhcaspGridA_CvMhcasp4_q if CvMhcaspGridA_CvMhcasp4_q<=4 & CvMhcaspGridA_CvMhcasp4_q>=0
gen CvMhcasp5_q =  CvMhcaspGridA_CvMhcasp5_q if CvMhcaspGridA_CvMhcasp5_q<=4 & CvMhcaspGridA_CvMhcasp5_q>=0
gen CvMhcasp6_q =  CvMhcaspGridA_CvMhcasp6_q if CvMhcaspGridA_CvMhcasp6_q<=4 & CvMhcaspGridA_CvMhcasp6_q>=0
gen CvMhcasp7_q =  CvMhcaspGridB_CvMhcasp7_q if CvMhcaspGridB_CvMhcasp7_q<=4 & CvMhcaspGridB_CvMhcasp7_q>=0
gen CvMhcasp8_q =  CvMhcaspGridB_CvMhcasp8_q if CvMhcaspGridB_CvMhcasp8_q<=4 & CvMhcaspGridB_CvMhcasp8_q>=0
gen CvMhcasp9_q =  CvMhcaspGridB_CvMhcasp9_q if CvMhcaspGridB_CvMhcasp9_q<=4 & CvMhcaspGridB_CvMhcasp9_q>=0
gen CvMhcasp10_q = CvMhcaspGridB_CvMhcasp10_q if CvMhcaspGridB_CvMhcasp10_q<=4 & CvMhcaspGridB_CvMhcasp10_q>=0
gen CvMhcasp11_q = CvMhcaspGridB_CvMhcasp11_q if CvMhcaspGridB_CvMhcasp11_q<=4 & CvMhcaspGridB_CvMhcasp11_q>=0
gen CvMhcasp12_q = CvMhcaspGridB_CvMhcasp12_q if CvMhcaspGridB_CvMhcasp12_q<=4 & CvMhcaspGridB_CvMhcasp12_q>=0


**************Descriptive Statistics and correlation matrix**************

sum MCZ_1 MCZ_2 MCZ_3 MCZ_4 CvMhcasp1_q CvMhcasp2_q CvMhcasp3_q CvMhcasp4_q CvMhcasp5_q CvMhcasp6_q/*
*/ CvMhcasp7_q CvMhcasp8_q CvMhcasp9_q CvMhcasp10_q CvMhcasp11_q CvMhcasp12_q/*
*/ d_unemployed d_smoking d_incomeless d_heill_2w age_2w Sex d_retired time

corr MCZ_1 MCZ_2 MCZ_3 MCZ_4 CvMhcasp1_q CvMhcasp2_q CvMhcasp3_q CvMhcasp4_q CvMhcasp5_q CvMhcasp6_q/*
*/ CvMhcasp7_q CvMhcasp8_q CvMhcasp9_q CvMhcasp10_q CvMhcasp11_q CvMhcasp12_q/*
*/ d_unemployed d_smoking d_incomeless d_heill_2w age_2w Sex d_retired time
 *************Pooled Estimation of the perception of ageing**************
 
 
reg MCZ_1 CvMhcasp1_q CvMhcasp2_q CvMhcasp3_q CvMhcasp4_q CvMhcasp5_q CvMhcasp6_q/*
*/ CvMhcasp7_q CvMhcasp8_q CvMhcasp9_q CvMhcasp10_q CvMhcasp11_q CvMhcasp12_q/*
*/ d_retired d_smoking d_incomeless d_heill_2w age_2w Sex time, robust
estimate store eq1
reg MCZ_2 CvMhcasp1_q CvMhcasp2_q CvMhcasp3_q CvMhcasp4_q CvMhcasp5_q CvMhcasp6_q/*
*/ CvMhcasp7_q CvMhcasp8_q CvMhcasp9_q CvMhcasp10_q CvMhcasp11_q CvMhcasp12_q/*
*/ d_retired d_smoking d_incomeless d_heill_2w age_2w Sex time, robust
estimate store eq2
reg MCZ_3 CvMhcasp1_q CvMhcasp2_q CvMhcasp3_q CvMhcasp4_q CvMhcasp5_q CvMhcasp6_q/*
*/ CvMhcasp7_q CvMhcasp8_q CvMhcasp9_q CvMhcasp10_q CvMhcasp11_q CvMhcasp12_q/*
*/ d_retired d_smoking d_incomeless d_heill_2w age_2w Sex time, robust
estimate store eq3
reg MCZ_4 CvMhcasp1_q CvMhcasp2_q CvMhcasp3_q CvMhcasp4_q CvMhcasp5_q CvMhcasp6_q/*
*/ CvMhcasp7_q CvMhcasp8_q CvMhcasp9_q CvMhcasp10_q CvMhcasp11_q CvMhcasp12_q/*
*/ d_retired d_smoking d_incomeless d_heill_2w age_2w Sex time, robust
estimate store eq4
esttab eq1 eq2 eq3 eq4, b(4) se(2) r2 star(* 0.10 ** 0.05 *** 0.01) stats(r2 N F p) title ("Estimation of the wellbeing effects using ELSA") mtitle("Satisfied" "Worthwhile" "Happy" "Anxious")


*******************Panel Analysis using FE and testing for using FE or RE using Hausman test***********

xtreg MCZ_1 CvMhcasp1_q CvMhcasp2_q CvMhcasp3_q CvMhcasp4_q CvMhcasp5_q CvMhcasp6_q/*
*/ CvMhcasp7_q CvMhcasp8_q CvMhcasp9_q CvMhcasp10_q CvMhcasp11_q CvMhcasp12_q/*
*/ d_unemployed d_smoking d_incomeless d_heill_2w age_2w i.time, fe robust
estimate store eq1
xtreg MCZ_2 CvMhcasp1_q CvMhcasp2_q CvMhcasp3_q CvMhcasp4_q CvMhcasp5_q CvMhcasp6_q/*
*/ CvMhcasp7_q CvMhcasp8_q CvMhcasp9_q CvMhcasp10_q CvMhcasp11_q CvMhcasp12_q/*
*/ d_retired d_smoking d_incomeless d_heill_2w age_2w Sex time, fe robust
estimate store eq2
xtreg MCZ_3 CvMhcasp1_q CvMhcasp2_q CvMhcasp3_q CvMhcasp4_q CvMhcasp5_q CvMhcasp6_q/*
*/ CvMhcasp7_q CvMhcasp8_q CvMhcasp9_q CvMhcasp10_q CvMhcasp11_q CvMhcasp12_q/*
*/ d_retired d_smoking d_incomeless d_heill_2w age_2w Sex time, fe robust
estimate store eq3
xtreg MCZ_4 CvMhcasp1_q CvMhcasp2_q CvMhcasp3_q CvMhcasp4_q CvMhcasp5_q CvMhcasp6_q/*
*/ CvMhcasp7_q CvMhcasp8_q CvMhcasp9_q CvMhcasp10_q CvMhcasp11_q CvMhcasp12_q/*
*/ d_retired d_smoking d_incomeless d_heill_2w age_2w Sex time, fe robust
estimate store eq4
esttab eq1 eq2 eq3 eq4, b(4) se(2) r2 star(* 0.10 ** 0.05 *** 0.01) stats(r2 N F p) title ("Estimation of the wellbeing effects using ELSA") mtitle("Satisfied" "Worthwhile" "Happy" "Anxious")

*************FE or RE test using Hauman statistic************

xtreg MCZ_1 CvMhcasp1_q CvMhcasp2_q CvMhcasp3_q CvMhcasp4_q CvMhcasp5_q CvMhcasp6_q/*
*/ CvMhcasp7_q CvMhcasp8_q CvMhcasp9_q CvMhcasp10_q CvMhcasp11_q CvMhcasp12_q/*
*/ d_unemployed d_smoking d_incomeless d_heill_2w age_2w i.time, fe
estimate store fe
xtreg MCZ_1 CvMhcasp1_q CvMhcasp2_q CvMhcasp3_q CvMhcasp4_q CvMhcasp5_q CvMhcasp6_q/*
*/ CvMhcasp7_q CvMhcasp8_q CvMhcasp9_q CvMhcasp10_q CvMhcasp11_q CvMhcasp12_q/*
*/ d_unemployed d_smoking d_incomeless d_heill_2w age_2w i.time, re
estimate store re
hausman fe re


xtreg MCZ_2 CvMhcasp1_q CvMhcasp2_q CvMhcasp3_q CvMhcasp4_q CvMhcasp5_q CvMhcasp6_q/*
*/ CvMhcasp7_q CvMhcasp8_q CvMhcasp9_q CvMhcasp10_q CvMhcasp11_q CvMhcasp12_q/*
*/ d_unemployed d_smoking d_incomeless d_heill_2w age_2w i.time, fe
estimate store fe
xtreg MCZ_2 CvMhcasp1_q CvMhcasp2_q CvMhcasp3_q CvMhcasp4_q CvMhcasp5_q CvMhcasp6_q/*
*/ CvMhcasp7_q CvMhcasp8_q CvMhcasp9_q CvMhcasp10_q CvMhcasp11_q CvMhcasp12_q/*
*/ d_unemployed d_smoking d_incomeless d_heill_2w age_2w i.time, re
estimate store re
hausman fe re



xtreg MCZ_3 CvMhcasp1_q CvMhcasp2_q CvMhcasp3_q CvMhcasp4_q CvMhcasp5_q CvMhcasp6_q/*
*/ CvMhcasp7_q CvMhcasp8_q CvMhcasp9_q CvMhcasp10_q CvMhcasp11_q CvMhcasp12_q/*
*/ d_unemployed d_smoking d_incomeless d_heill_2w age_2w i.time, fe
estimate store fe
xtreg MCZ_3 CvMhcasp1_q CvMhcasp2_q CvMhcasp3_q CvMhcasp4_q CvMhcasp5_q CvMhcasp6_q/*
*/ CvMhcasp7_q CvMhcasp8_q CvMhcasp9_q CvMhcasp10_q CvMhcasp11_q CvMhcasp12_q/*
*/ d_unemployed d_smoking d_incomeless d_heill_2w age_2w i.time, re
estimate store re
hausman fe re



xtreg MCZ_4 CvMhcasp1_q CvMhcasp2_q CvMhcasp3_q CvMhcasp4_q CvMhcasp5_q CvMhcasp6_q/*
*/ CvMhcasp7_q CvMhcasp8_q CvMhcasp9_q CvMhcasp10_q CvMhcasp11_q CvMhcasp12_q/*
*/ d_unemployed d_smoking d_incomeless d_heill_2w age_2w i.time, fe
estimate store fe
xtreg MCZ_4 CvMhcasp1_q CvMhcasp2_q CvMhcasp3_q CvMhcasp4_q CvMhcasp5_q CvMhcasp6_q/*
*/ CvMhcasp7_q CvMhcasp8_q CvMhcasp9_q CvMhcasp10_q CvMhcasp11_q CvMhcasp12_q/*
*/ d_unemployed d_smoking d_incomeless d_heill_2w age_2w i.time, re
estimate store re
hausman fe re

**********FE is the correct estimation method**************



***************Final Estimation Tables*************
*************Pooled Estimation of the perception of ageing**************
 
 
reg MCZ_1 CvMhcasp1_q CvMhcasp2_q CvMhcasp3_q CvMhcasp4_q CvMhcasp5_q CvMhcasp6_q/*
*/ CvMhcasp7_q CvMhcasp8_q CvMhcasp9_q CvMhcasp10_q CvMhcasp11_q CvMhcasp12_q/*
*/ d_retired d_smoking d_incomeless d_heill_2w age_2w Sex time, robust
estimate store eq1
reg MCZ_2 CvMhcasp1_q CvMhcasp2_q CvMhcasp3_q CvMhcasp4_q CvMhcasp5_q CvMhcasp6_q/*
*/ CvMhcasp7_q CvMhcasp8_q CvMhcasp9_q CvMhcasp10_q CvMhcasp11_q CvMhcasp12_q/*
*/ d_retired d_smoking d_incomeless d_heill_2w age_2w Sex time, robust
estimate store eq2
reg MCZ_3 CvMhcasp1_q CvMhcasp2_q CvMhcasp3_q CvMhcasp4_q CvMhcasp5_q CvMhcasp6_q/*
*/ CvMhcasp7_q CvMhcasp8_q CvMhcasp9_q CvMhcasp10_q CvMhcasp11_q CvMhcasp12_q/*
*/ d_retired d_smoking d_incomeless d_heill_2w age_2w Sex time, robust
estimate store eq3
reg MCZ_4 CvMhcasp1_q CvMhcasp2_q CvMhcasp3_q CvMhcasp4_q CvMhcasp5_q CvMhcasp6_q/*
*/ CvMhcasp7_q CvMhcasp8_q CvMhcasp9_q CvMhcasp10_q CvMhcasp11_q CvMhcasp12_q/*
*/ d_retired d_smoking d_incomeless d_heill_2w age_2w Sex time, robust
estimate store eq4
esttab eq1 eq2 eq3 eq4, b(4) se(2) r2 star(* 0.10 ** 0.05 *** 0.01) stats(r2 N F p) title ("Estimation of the wellbeing effects using ELSA") mtitle("Satisfied" "Worthwhile" "Happy" "Anxious")


*******************Panel Analysis using FE and testing for using FE or RE using Hausman test***********

reg MCZ_1 CvMhcasp1_q CvMhcasp2_q CvMhcasp3_q CvMhcasp4_q CvMhcasp5_q CvMhcasp6_q/*
*/ CvMhcasp7_q CvMhcasp8_q CvMhcasp9_q CvMhcasp10_q CvMhcasp11_q CvMhcasp12_q/*
*/ d_retired d_smoking d_incomeless d_heill_2w age_2w Sex i.time, robust
estimate store eq1
reg MCZ_2 CvMhcasp1_q CvMhcasp2_q CvMhcasp3_q CvMhcasp4_q CvMhcasp5_q CvMhcasp6_q/*
*/ CvMhcasp7_q CvMhcasp8_q CvMhcasp9_q CvMhcasp10_q CvMhcasp11_q CvMhcasp12_q/*
*/ d_retired d_smoking d_incomeless d_heill_2w age_2w Sex i.time, robust
estimate store eq2
reg MCZ_3 CvMhcasp1_q CvMhcasp2_q CvMhcasp3_q CvMhcasp4_q CvMhcasp5_q CvMhcasp6_q/*
*/ CvMhcasp7_q CvMhcasp8_q CvMhcasp9_q CvMhcasp10_q CvMhcasp11_q CvMhcasp12_q/*
*/ d_retired d_smoking d_incomeless d_heill_2w age_2w Sex time, robust
estimate store eq3
reg MCZ_4 CvMhcasp1_q CvMhcasp2_q CvMhcasp3_q CvMhcasp4_q CvMhcasp5_q CvMhcasp6_q/*
*/ CvMhcasp7_q CvMhcasp8_q CvMhcasp9_q CvMhcasp10_q CvMhcasp11_q CvMhcasp12_q/*
*/ d_retired d_smoking d_incomeless d_heill_2w age_2w Sex i.time, robust
estimate store eq4
xtreg MCZ_1 CvMhcasp1_q CvMhcasp2_q CvMhcasp3_q CvMhcasp4_q CvMhcasp5_q CvMhcasp6_q/*
*/ CvMhcasp7_q CvMhcasp8_q CvMhcasp9_q CvMhcasp10_q CvMhcasp11_q CvMhcasp12_q/*
*/ d_unemployed d_smoking d_incomeless d_heill_2w age_2w i.time, fe robust
estimate store eq5
xtreg MCZ_2 CvMhcasp1_q CvMhcasp2_q CvMhcasp3_q CvMhcasp4_q CvMhcasp5_q CvMhcasp6_q/*
*/ CvMhcasp7_q CvMhcasp8_q CvMhcasp9_q CvMhcasp10_q CvMhcasp11_q CvMhcasp12_q/*
*/ d_unemployed d_smoking d_incomeless d_heill_2w age_2w i.time, fe robust
estimate store eq6
xtreg MCZ_3 CvMhcasp1_q CvMhcasp2_q CvMhcasp3_q CvMhcasp4_q CvMhcasp5_q CvMhcasp6_q/*
*/ CvMhcasp7_q CvMhcasp8_q CvMhcasp9_q CvMhcasp10_q CvMhcasp11_q CvMhcasp12_q/*
*/ d_unemployed d_smoking d_incomeless d_heill_2w age_2w i.time, fe robust
estimate store eq7
xtreg MCZ_4 CvMhcasp1_q CvMhcasp2_q CvMhcasp3_q CvMhcasp4_q CvMhcasp5_q CvMhcasp6_q/*
*/ CvMhcasp7_q CvMhcasp8_q CvMhcasp9_q CvMhcasp10_q CvMhcasp11_q CvMhcasp12_q/*
*/ d_unemployed d_smoking d_incomeless d_heill_2w age_2w i.time, fe robust
estimate store eq8
esttab eq1 eq2 eq3 eq4 eq5 eq6 eq7 eq8 , b(4) se(2) r2 star(* 0.10 ** 0.05 *** 0.01) stats(r2 N F p) title ("Estimation of the wellbeing effects using ELSA:Perception of Ageing") mtitle("Satisfied-OLS" "Worthwhile-OLS" "Happy-OLS" "Anxious-OLS" "Satisfied-FE" "Worthwhile-FE" "Happy-FE" "Anxious-FE")



****************** generation of the 8 indicators of mental health as likert scale value to idicate the value of 1 for answering often to 4 for answering never***********
gen CvMhCed1_q =  CvMhCed_CvMhCed1_q if CvMhCed_CvMhCed1_q<=2 & CvMhCed_CvMhCed1_q>=0
gen CvMhCed2_q =  CvMhCed_CvMhCed2_q if CvMhCed_CvMhCed2_q<=2 & CvMhCed_CvMhCed2_q>=0
gen CvMhCed3_q =  CvMhCed_CvMhCed3_q if CvMhCed_CvMhCed3_q<=2 & CvMhCed_CvMhCed3_q>=0
gen CvMhCed4_q =  CvMhCed_CvMhCed4_q if CvMhCed_CvMhCed4_q<=2 & CvMhCed_CvMhCed4_q>=0
gen CvMhCed5_q =  CvMhCed_CvMhCed5_q if CvMhCed_CvMhCed5_q<=2 & CvMhCed_CvMhCed5_q>=0
gen CvMhCed6_q =  CvMhCed_CvMhCed6_q if CvMhCed_CvMhCed6_q<=2 & CvMhCed_CvMhCed6_q>=0
gen CvMhCed7_q =  CvMhCed_CvMhCed7_q if CvMhCed_CvMhCed7_q<=2 & CvMhCed_CvMhCed7_q>=0

***************************
sum CvMhCed1_q CvMhCed2_q CvMhCed3_q CvMhCed4_q CvMhCed5_q CvMhCed6_q CvMhCed7_q 
corr CvMhCed1_q CvMhCed2_q CvMhCed3_q CvMhCed4_q CvMhCed5_q CvMhCed6_q CvMhCed7_q
*****************Pooled Estimation and panel analysis *************

reg MCZ_1 CvMhCed1_q CvMhCed2_q CvMhCed3_q CvMhCed4_q CvMhCed5_q CvMhCed6_q CvMhCed7_q/*
*/ d_retired d_smoking d_incomeless d_heill_2w age_2w Sex i.time, robust
estimate store eq1
reg MCZ_2 CvMhCed1_q CvMhCed2_q CvMhCed3_q CvMhCed4_q CvMhCed5_q CvMhCed6_q CvMhCed7_q/*
*/ d_retired d_smoking d_incomeless d_heill_2w age_2w Sex i.time, robust
estimate store eq2
reg MCZ_3 CvMhCed1_q CvMhCed2_q CvMhCed3_q CvMhCed4_q CvMhCed5_q CvMhCed6_q CvMhCed7_q/*
*/ d_retired d_smoking d_incomeless d_heill_2w age_2w Sex i.time, robust
estimate store eq3
reg MCZ_4 CvMhCed1_q CvMhCed2_q CvMhCed3_q CvMhCed4_q CvMhCed5_q CvMhCed6_q CvMhCed7_q/*
*/ d_retired d_smoking d_incomeless d_heill_2w age_2w Sex i.time, robust
estimate store eq4
xtreg MCZ_1 CvMhCed1_q CvMhCed2_q CvMhCed3_q CvMhCed4_q CvMhCed5_q CvMhCed6_q CvMhCed7_q/*
*/ d_retired d_smoking d_incomeless d_heill_2w age_2w Sex i.time, fe robust
estimate store eq5
xtreg MCZ_2 CvMhCed1_q CvMhCed2_q CvMhCed3_q CvMhCed4_q CvMhCed5_q CvMhCed6_q CvMhCed7_q/*
*/ d_retired d_smoking d_incomeless d_heill_2w age_2w Sex i.time, fe robust
estimate store eq6
xtreg MCZ_3 CvMhCed1_q CvMhCed2_q CvMhCed3_q CvMhCed4_q CvMhCed5_q CvMhCed6_q CvMhCed7_q/*
*/ d_retired d_smoking d_incomeless d_heill_2w age_2w Sex i.time, fe robust
estimate store eq7
xtreg MCZ_4 CvMhCed1_q CvMhCed2_q CvMhCed3_q CvMhCed4_q CvMhCed5_q CvMhCed6_q CvMhCed7_q/*
*/ d_retired d_smoking d_incomeless d_heill_2w age_2w Sex i.time, fe robust
estimate store eq8
esttab eq1 eq2 eq3 eq4 eq5 eq6 eq7 eq8 , b(4) se(2) r2 star(* 0.10 ** 0.05 *** 0.01) stats(r2 N F p) title ("Estimation of the wellbeing effects using ELSA:Mental Health") mtitle("Satisfied-OLS" "Worthwhile-OLS" "Happy-OLS" "Anxious-OLS" "Satisfied-FE" "Worthwhile-FE" "Happy-FE" "Anxious-FE")







