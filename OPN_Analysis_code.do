
********************************************************************************************************************************
**************************************** Accelerating Wellbeing Analysis *****************************************
************************************************ Shimaa Elkomy ***************************************************************
******************************************** What Works Centre for Wellbeing ***************************************************
***********Time analysis of wellbeing trends , namely satisfcation, worthwhilness, happiness and anxiety diaggregated by gender and age groups*************
**********ANALYSIS PART ONE************
**Set working directory:
cd "C:\Users\se0010\OneDrive - University of Surrey\Documents\What Works Wellbeing\Data\Codes\Analysis"
**Import dataset**
import excel "Wellbeing Data 2020-2023.xlsx"
**generate time variable to examine time trend**.
gen x = _n
rename x Time

**Descriptive Satistics for the four MCZs (meanscore and percentage of people reporting they are feeling satisfied, worthwhile, happy and anxious)*************
*******pmcz_1 is Percentage of adults with low levels of life satisfaction (score 0 to 4 out of 10); 
******pmcz_2 is Percentage of adults with low levels of feeling that things done in life are worthwhile (score 0 to 4 out of 10); 
**********pmcz_3 is Percentage of adults with low levels of happiness (score 0 to 4 out of 10) and 
****pmcz_4 is Percentage of adults with high levels of anxiety (score 6 to 10 out of 10)***************
sum mcz_1 mcz_2 mcz_3 mcz_4
sum pmcz_1 pmcz_2 pmcz_3 pmcz_4
***Correlation analysis for the four MCZs(meanscore and percentage of people reporting they are feeling satisfied, worthwhile, happy and anxious)*************
corr mcz_1 mcz_2 mcz_3 mcz_4
corr pmcz_1 pmcz_2 pmcz_3 pmcz_4


sum mcz_1 mcz_1_men mcz_1_women mcz_1_age_16_29 mcz_1_age_30_49 mcz_1_age_50_69 mcz_1_age_70_ mcz_2 mcz_2_men mcz_2_women mcz_2_age_16_29 mcz_2_age_30_49 mcz_2_age_50_69 mcz_2_age_70_  mcz_3 mcz_3_men mcz_3_women mcz_3_age_16_29 mcz_3_age_30_49 mcz_3_age_50_69 mcz_3_age_70_ mcz_3   mcz_4 mcz_4_men mcz_4_women mcz_4_age_16_29 mcz_4_age_30_49 mcz_4_age_50_69 mcz_4_age_70_ mcz_4


**********Descriptive Satistics for the four MCZs by gender*************

sum mcz_1_men mcz_1_women
sum mcz_2_men mcz_2_women
sum mcz_3_men mcz_3_women
sum mcz_4_men mcz_4_women


sum pmcz_1_men pmcz_1_women
sum pmcz_2_men pmcz_2_women
sum pmcz_3_men pmcz_3_women
sum pmcz_4_men pmcz_4_women

***********Descriptive Satistics for the four MCZs by age group****************

sum mcz_1_age_16_29 mcz_1_age_30_49 mcz_1_age_50_69 mcz_1_age_70_
sum mcz_2_age_16_29 mcz_2_age_30_49 mcz_2_age_50_69 mcz_2_age_70_
sum mcz_3_age_16_29 mcz_3_age_30_49 mcz_3_age_50_69 mcz_3_age_70_
sum mcz_4_age_16_29 mcz_4_age_30_49 mcz_4_age_50_69 mcz_4_age_70_

sum pmcz_1_age_16_29 pmcz_1_age_30_49 pmcz_1_age_50_69 pmcz_1_age_70_
sum pmcz_2_age_16_29 pmcz_2_age_30_49 pmcz_2_age_50_69 pmcz_2_age_70_
sum pmcz_3_age_16_29 pmcz_3_age_30_49 pmcz_3_age_50_69 pmcz_3_age_70_
sum pmcz_4_age_16_29 pmcz_4_age_30_49 pmcz_4_age_50_69 pmcz_4_age_70_

************************************
*Time trend analysisfor the four MCZs (meanscore and percentage of people reporting they are feeling satisfied, worthwhile, happy and anxious)************* 

reg mcz_1 Time, robust
estimate store eq1
reg mcz_2 Time, robust
estimate store eq2
reg mcz_3 Time, robust
estimate store eq3
reg mcz_4 Time, robust
estimate store eq4
esttab eq1 eq2 eq3 eq4, b(4) se(2) r2 star(* 0.10 ** 0.05 *** 0.01) csv stats(r2 N F p) title ("Estimation of time trend of wellbeing from pandemic to post-pandemic time") mtitle("Satisfied" "Worthwhile" "Happy" "Anxious")

reg pmcz_1 Time, robust
estimate store eq1
reg pmcz_2 Time, robust
estimate store eq2
reg pmcz_3 Time, robust
estimate store eq3
reg pmcz_4 Time, robust
estimate store eq4
esttab eq1 eq2 eq3 eq4, b(4) se(2) r2 csv star(* 0.10 ** 0.05 *** 0.01) stats(r2 N F p) title ("Estimation of time trend of wellbeing from pandemic to post-pandemic time") mtitle("Dissatisfied" "Unworthwhile" "UnHappy" "Anxious")


*******Analysis of Satisfaction, Worthwhilness, Happiness and AnTimeiety by Age-Group(meanscore and percentage of people reporting they are feeling satisfied, worthwhile, happy and anxious)**********************

reg mcz_1_age_16_29 Time, robust
estimate store eq1
reg mcz_1_age_30_49 Time, robust
estimate store eq2
reg mcz_1_age_50_69 Time, robust
estimate store eq3
reg mcz_1_age_70_ Time, robust
estimate store eq4
esttab eq1 eq2 eq3 eq4, b(4) se(2) r2 csv star(* 0.10 ** 0.05 *** 0.01) stats(r2 N F p) title ("Table 6. Estimation of time trend of wellbeing from pandemic to post-pandemic time") mtitle("Satisfied_16_29" "Satisfied_30_49" "Satisfied_50_69" "Satisfied_70_")



reg mcz_2_age_16_29 Time, robust
estimate store eq1
reg mcz_2_age_30_49 Time, robust
estimate store eq2
reg mcz_2_age_50_69 Time, robust
estimate store eq3
reg mcz_2_age_70_ Time, robust
estimate store eq4
esttab eq1 eq2 eq3 eq4, b(4) se(2) r2 csv star(* 0.10 ** 0.05 *** 0.01) stats(r2 N F p) title ("Estimation of time trend of wellbeing from pandemic to post-pandemic time") mtitle("Worthwhile_16_29" "Worthwhile_30_49" "Worthwhile_50_69" "Worthwhile_70_")



reg mcz_3_age_16_29 Time, robust
estimate store eq1
reg mcz_3_age_30_49 Time, robust
estimate store eq2
reg mcz_3_age_50_69 Time, robust
estimate store eq3
reg mcz_3_age_70_ Time, robust
estimate store eq4
esttab eq1 eq2 eq3 eq4, b(4) se(2) r2 csv star(* 0.10 ** 0.05 *** 0.01) stats(r2 N F p) title ("Table 7. Estimation of time trend of wellbeing from pandemic to post-pandemic time") mtitle("Happy_16_29" "Happy_30_49" "Happy_50_69" "Happy_70_")



reg mcz_4_age_16_29 Time, robust
estimate store eq1
reg mcz_4_age_30_49 Time, robust
estimate store eq2
reg mcz_4_age_50_69 Time, robust
estimate store eq3
reg mcz_4_age_70_ Time, robust
estimate store eq4
esttab eq1 eq2 eq3 eq4, b(4) se(2) r2 csv star(* 0.10 ** 0.05 *** 0.01) stats(r2 N F p) title ("Estimation of time trend of wellbeing from pandemic to post-pandemic time") mtitle("Anxious_16_29" "Anxious_30_49" "Anxious_50_69" "Anxious_70_")



reg pmcz_1_age_16_29 Time, robust
estimate store eq1
reg pmcz_1_age_30_49 Time, robust
estimate store eq2
reg pmcz_1_age_50_69 Time, robust
estimate store eq3
reg pmcz_1_age_70_ Time, robust
estimate store eq4
esttab eq1 eq2 eq3 eq4, b(4) se(2) r2 csv star(* 0.10 ** 0.05 *** 0.01) stats(r2 N F p) title ("Table 8. Estimation of time trend of wellbeing from pandemic to post-pandemic time") mtitle("Dissatisfied_16_29" "Dissatisfied_30_49" "Dissatisfied_50_69" "Dissatisfied_70_")



reg pmcz_2_age_16_29 Time, robust
estimate store eq1
reg pmcz_2_age_30_49 Time, robust
estimate store eq2
reg pmcz_2_age_50_69 Time, robust
estimate store eq3
reg pmcz_2_age_70_ Time, robust
estimate store eq4
esttab eq1 eq2 eq3 eq4, b(4) se(2) r2 csv star(* 0.10 ** 0.05 *** 0.01) stats(r2 N F p) title ("Estimation of time trend of wellbeing from pandemic to post-pandemic time") mtitle("UnWorthwhile_16_29" "UnWorthwhile_30_49" "UnWorthwhile_50_69" "UnWorthwhile_70_")



reg pmcz_3_age_16_29 Time, robust
estimate store eq1
reg pmcz_3_age_30_49 Time, robust
estimate store eq2
reg pmcz_3_age_50_69 Time, robust
estimate store eq3
reg pmcz_3_age_70_ Time, robust
estimate store eq4
esttab eq1 eq2 eq3 eq4, b(4) se(2) r2 csv star(* 0.10 ** 0.05 *** 0.01) stats(r2 N F p) title ("Table 9. Estimation of time trend of wellbeing from pandemic to post-pandemic time") mtitle("UnHappy_16_29" "UnHappy_30_49" "UnHappy_50_69" "UnHappy_70_")



reg pmcz_4_age_16_29 Time, robust
estimate store eq1
reg pmcz_4_age_30_49 Time, robust
estimate store eq2
reg pmcz_4_age_50_69 Time, robust
estimate store eq3
reg pmcz_4_age_70_ Time, robust
estimate store eq4
esttab eq1 eq2 eq3 eq4, b(4) se(2) r2 csv star(* 0.10 ** 0.05 *** 0.01) stats(r2 N F p) title ("Estimation of time trend of wellbeing from pandemic to post-pandemic time") mtitle("Anxious_16_29" "Anxious_30_49" "Anxious_50_69" "Anxious_70_")



********Analysis of Satisfaction, Worthwhilness, Happiness and AnTimeiety by Gender (meanscore and percentage of people reporting they are feeling satisfied, worthwhile, happy and anxious)**********************

reg mcz_1_men Time, robust
estimate store eq1
reg mcz_1_women Time, robust
estimate store eq2
esttab eq1 eq2, b(4) se(2) r2 csv star(* 0.10 ** 0.05 *** 0.01) stats(r2 N F p)title ("Estimation of time trend of wellbeing from pandemic to post-pandemic time") mtitle("Satisfied_men" "Satisfied_women")



reg mcz_2_men Time, robust
estimate store eq1
reg mcz_2_women Time, robust
estimate store eq2
esttab eq1 eq2, b(4) se(2) r2 csv star(* 0.10 ** 0.05 *** 0.01) stats(r2 N F p) title ("Estimation of time trend of wellbeing from pandemic to post-pandemic time") mtitle("Worthwhile_men" "Worthwhile_women")



reg mcz_3_men Time, robust
estimate store eq1
reg mcz_3_women Time, robust
estimate store eq2
esttab eq1 eq2, b(4) se(2) r2 csv star(* 0.10 ** 0.05 *** 0.01) stats(r2 N F p) title ("Estimation of time trend of wellbeing from pandemic to post-pandemic time") mtitle("Happy_men" "Happy_women")



reg mcz_4_men Time, robust
estimate store eq1
reg mcz_4_women Time, robust
estimate store eq2
esttab eq1 eq2, b(4) se(2) r2 csv star(* 0.10 ** 0.05 *** 0.01) stats(r2 N F p) title ("Estimation of time trend of wellbeing from pandemic to post-pandemic time") mtitle("Anxious_men" "Anxious_women")



reg pmcz_1_men Time, robust
estimate store eq1
reg pmcz_1_women Time, robust
estimate store eq2
esttab eq1 eq2, b(4) se(2) r2 csv star(* 0.10 ** 0.05 *** 0.01) stats(r2 N F p) title ("Table 5 Estimation of time trend of wellbeing from pandemic to post-pandemic time by gender") mtitle("Dissatisfied_men" "Dissatisfied_women")



reg pmcz_2_men Time, robust
estimate store eq1
reg pmcz_2_women Time, robust
estimate store eq2
esttab eq1 eq2, b(4) se(2) r2 csv star(* 0.10 ** 0.05 *** 0.01) stats(r2 N F p) title ("Estimation of time trend of wellbeing from pandemic to post-pandemic time") mtitle("Unworthwhile_men" "Unworthwhile_women")


reg pmcz_3_men Time, robust
estimate store eq1
reg pmcz_3_women Time, robust
estimate store eq2
esttab eq1 eq2, b(4) se(2) r2 csv star(* 0.10 ** 0.05 *** 0.01) stats(r2 N F p) title ("Estimation of time trend of wellbeing from pandemic to post-pandemic time") mtitle("Unhappy_men" "Unhappy_women")



reg pmcz_4_men Time, robust
estimate store eq1
reg pmcz_4_women Time, robust
estimate store eq2
esttab eq1 eq2, b(4) se(2) r2 csv star(* 0.10 ** 0.05 *** 0.01) stats(r2 N F p) title ("Estimation of time trend of wellbeing from pandemic to post-pandemic time") mtitle("Anxious_men" "Anxious_women")



****Analysis of structural breakevens during the peak of the outbreak, lockdowns, vaccination based on oxford stringency index (Blavatnik School of Government Working Paper Series) ****************

gen d_2020_21=.
replace d_2020_21 = 1 if year==2020 | year==2021
replace d_2020_21 = 0 if year==2022 | year==2023


gen d_2022_23=.
replace d_2022_23 = 1 if year==2022 | year==2023
replace d_2022_23 = 0 if year==2020 | year==2021


**************Structural breakeven*********
reg mcz_1 Time d_2022_23, robust
estimate store eq1
reg mcz_2 Time d_2022_23, robust
estimate store eq2
reg mcz_3 Time d_2022_23, robust
estimate store eq3
reg mcz_4 Time d_2022_23, robust
estimate store eq4
esttab eq1 eq2 eq3 eq4, b(4) se(2) r2 star(* 0.10 ** 0.05 *** 0.01) stats(r2 N F p) title ("Estimation of time trend of wellbeing from pandemic to post-pandemic time") mtitle("Satisfied" "Worthwhile" "Happy" "Anxious")

reg pmcz_1 Time d_2022_23, robust
estimate store eq1
reg pmcz_2 Time d_2022_23, robust
estimate store eq2
reg pmcz_3 Time d_2022_23, robust
estimate store eq3
reg pmcz_4 Time d_2022_23, robust
estimate store eq4
esttab eq1 eq2 eq3 eq4, b(4) se(2) r2 star(* 0.10 ** 0.05 *** 0.01) stats(r2 N F p) title ("Estimation of time trend of wellbeing from pandemic to post-pandemic time") mtitle("Dissatisfied" "Unorthwhile" "UnHappy" "Anxious")




*********pandemic analysis*********
reg mcz_1 d_2020_21, robust
estimate store eq1
reg mcz_2 d_2020_21, robust
estimate store eq2
reg mcz_3 d_2020_21, robust
estimate store eq3
reg mcz_4 d_2020_21, robust
estimate store eq4
esttab eq1 eq2 eq3 eq4, b(4) se(2) r2 star(* 0.10 ** 0.05 *** 0.01) stats(r2 N F p) title ("Estimation of time trend of wellbeing from pandemic to post-pandemic time") mtitle("Satisfied" "Worthwhile" "Happy" "Anxious")

reg pmcz_1 d_2020_21, robust
estimate store eq1
reg pmcz_2 d_2020_21, robust
estimate store eq2
reg pmcz_3 d_2020_21, robust
estimate store eq3
reg pmcz_4 d_2020_21, robust
estimate store eq4
esttab eq1 eq2 eq3 eq4, b(4) se(2) r2 star(* 0.10 ** 0.05 *** 0.01) stats(r2 N F p) title ("Estimation of time trend of wellbeing from pandemic to post-pandemic time") mtitle("Dissatisfied" "Unorthwhile" "UnHappy" "Anxious")

***post-pandemic analysis***********

reg mcz_1 d_2022_23, robust
estimate store eq1
reg mcz_2 d_2022_23, robust
estimate store eq2
reg mcz_3 d_2022_23, robust
estimate store eq3
reg mcz_4 d_2022_23, robust
estimate store eq4
esttab eq1 eq2 eq3 eq4, b(4) se(2) r2 star(* 0.10 ** 0.05 *** 0.01) stats(r2 N F p) title ("Estimation of time trend of wellbeing from pandemic to post-pandemic time") mtitle("Satisfied" "Worthwhile" "Happy" "Anxious")

reg pmcz_1 d_2022_23, robust
estimate store eq1
reg pmcz_2 d_2022_23, robust
estimate store eq2
reg pmcz_3 d_2022_23, robust
estimate store eq3
reg pmcz_4 d_2022_23, robust
estimate store eq4
esttab eq1 eq2 eq3 eq4, b(4) se(2) r2 star(* 0.10 ** 0.05 *** 0.01) stats(r2 N F p) title ("Estimation of time trend of wellbeing from pandemic to post-pandemic time") mtitle("Dissatisfied" "Unorthwhile" "UnHappy" "Anxious")









***************Graphical Representation of wellbeing by time (2020, 2021, 2022-23)
************We want to covert the "time" variable which is the week where the survey was collected to a format of a date
**please be noted that the variabe "time" is not the standard time format for STATA as it has range of time for example 15 December 2021 to 3 January 2022****
**********Generation of four years of analysis which are 2020, 2021, 2022 and 2023*************
drop year
gen year=.
replace year=2020 if Time<=39
replace year=2021 if Time>39 & Time<=81
replace year=2022 if Time>81 & Time<=107
replace year=2023 if Time>107

**********Generation of 12 months for the four years of analysis which are 2020, 2021, 2022 and 2023*************
drop month
gen month=.
replace month =12 if strmatch(time, "*December*")
replace month =11 if strmatch(time, "*November *")
replace month =10 if strmatch(time, "*October*")
replace month =9 if strmatch(time, "*September*")
replace month =8 if strmatch(time, "*August*")
replace month =7 if strmatch(time, "*July*")
replace month =6 if strmatch(time, "*June*")
replace month =5 if strmatch(time, "*May*")
replace month =4 if strmatch(time, "*April*")
replace month =3 if strmatch(time, "*March*")
replace month =2 if strmatch(time, "*February*")
replace month =1 if strmatch(time, "*January*")

**********Generation of 31 days of 12 months for the four years of analysis which are 2020, 2021, 2022 and 2023*************
drop day
gen day=.
replace day =31 if strmatch(time, "31 *")
replace day =30 if strmatch(time, "30 *")
replace day =29 if strmatch(time, "29 *")
replace day =28 if strmatch(time, "28 *")
replace day =27 if strmatch(time, "27 *")
replace day =26 if strmatch(time, "26 *")
replace day =25 if strmatch(time, "25 *")
replace day =24 if strmatch(time, "24 *")
replace day =23 if strmatch(time, "23 *")
replace day =22 if strmatch(time, "22 *")
replace day =21 if strmatch(time, "21 *")
replace day =20 if strmatch(time, "20 *")
replace day =19 if strmatch(time, "19 *")
replace day =18 if strmatch(time, "18 *")
replace day =17 if strmatch(time, "17 *")
replace day =16 if strmatch(time, "16 *")
replace day =15 if strmatch(time, "15 *")
replace day =14 if strmatch(time, "14 *")
replace day =13 if strmatch(time, "13 *")
replace day =12 if strmatch(time, "12 *")
replace day =11 if strmatch(time, "11 *")
replace day =10 if strmatch(time, "10 *")
replace day =9 if strmatch(time, "9 *")
replace day =8 if strmatch(time, "8 *")
replace day =7 if strmatch(time, "7 *")
replace day =6 if strmatch(time, "6 *")
replace day =5 if strmatch(time, "5 *")
replace day =4 if strmatch(time, "4 *")
replace day =3 if strmatch(time, "3 *")
replace day =2 if strmatch(time, "2 *")
replace day =1 if strmatch(time, "1 *")

**********Generation of date variable that has day, month and year format ***********************
gen date= mdy(month, day, year)
format date %td
*************************Graphical represepenatation********************
tsset date
gen mcz_1_feb_2020 = 7.3
gen mcz_2_feb_2020 = 7.6
gen mcz_3_feb_2020 = 7.2
gen mcz_4_feb_2020 = 3.5

label variable mcz_1_feb_2020 "MCZ_1_Feb_2020"
label variable mcz_2_feb_2020 "MCZ_2_Feb_2020"
label variable mcz_3_feb_2020 "MCZ_3_Feb_2020"
label variable mcz_4_feb_2020 "MCZ_4_Feb_2020"


*************Satisfcation by gender and age group************
graph twoway tsline mcz_1 mcz_1_feb_2020 if tin(01mar2020, 30jan2023), xlabel(, angle(90)) xlabel(#20) xmtick(#20) ymtick(#5) graphregion(color(white)) xtitle(Weekly OPN Survey - Covid-19 module) subtitle("Overall, how satisfied are you with your life nowadays?") ytitle("Satisfaction meanscore") note("Source: Opinion and Lifestyle Survey-Covid-19 module") legend(label(1 "Satisfaction") label(2 "Satisfaction_Feb_2020"))


graph twoway tsline mcz_1_men  mcz_1_women mcz_1_feb_2020 if tin(01mar2020, 30jan2023), xlabel(, angle(90)) xlabel(#20) xmtick(#20) ymtick(#5) graphregion(color(white)) xtitle(Weekly OPN Survey - Covid-19 module) subtitle("Overall, how satisfied are you with your life nowadays?") ytitle("Satisfaction meanscore") note("Source: Opinion and Lifestyle Survey-Covid-19 module") legend(label(1 "Satisfaction_Men") label(2 "Satisfaction_Women") label(3 "Satisfaction_Feb_2020"))

graph twoway tsline mcz_1_age_16_29 mcz_1_age_30_49 mcz_1_age_50_69 mcz_1_age_70_ mcz_1_feb_2020 if tin(01mar2020, 30jan2023), xlabel(, angle(90)) xlabel(#20) xmtick(#20) ymtick(#5) graphregion(color(white)) xtitle(Weekly OPN Survey - Covid-19 module) subtitle("Overall, how satisfied are you with your life nowadays?") ytitle("Satisfaction meanscore") note("Source: Opinion and Lifestyle Survey-Covid-19 module") legend(label(1 "Satisfaction_age_16_29") label(2 "Satisfaction_age_30_49") label(3 "Satisfaction_age_50_69") label(4 "Satisfaction_age_70_") label(5 "Satisfaction_Feb_2020"))

*************************


*************Worthwhilness by gender and age group************
graph twoway tsline mcz_2 mcz_2_feb_2020 if tin(01mar2020, 30jan2023), xlabel(, angle(90)) xlabel(#20) xmtick(#20) ymtick(#5) graphregion(color(white)) xtitle(Weekly OPN Survey - Covid-19 module) subtitle("Overall, to what extent do you feel that the things you do in your life are worthwhile?") ytitle("Worthwhile meanscore") note("Source: Opinion and Lifestyle Survey-Covid-19 module") legend(label(1 "Worthwhile") label(2 "Worthwhile_Feb_2020"))


graph twoway tsline mcz_2_men  mcz_2_women mcz_2_feb_2020 if tin(01mar2020, 30jan2023), xlabel(, angle(90)) xlabel(#20) xmtick(#20) ymtick(#5) graphregion(color(white)) xtitle(Weekly OPN Survey - Covid-19 module) subtitle("Overall, to what extent do you feel that the things you do in your life are worthwhile?") ytitle("Worthwhile meanscore") note("Source: Opinion and Lifestyle Survey-Covid-19 module") legend(label(1 "Worthwhile_Men") label(2 "Worthwhile_Women") label(3 "Worthwhile_Feb_2020"))

graph twoway tsline mcz_2_age_16_29 mcz_2_age_30_49 mcz_2_age_50_69 mcz_2_age_70_ mcz_2_feb_2020 if tin(01mar2020, 30jan2023), xlabel(, angle(90)) xlabel(#20) xmtick(#20) ymtick(#5) graphregion(color(white)) xtitle(Weekly OPN Survey - Covid-19 module) subtitle("Overall, to what extent do you feel that the things you do in your life are worthwhile? ") ytitle("Worthwhile meanscore") note("Source: Opinion and Lifestyle Survey-Covid-19 module") legend(label(1 "Worthwhile_age_16_29") label(2 "Worthwhile_age_30_49") label(3 "Worthwhile_age_50_69") label(4 "Worthwhile_age_70_") label(5 "Worthwhile_Feb_2020"))


*************************

*************Happiness by gender and age group************
graph twoway tsline mcz_3 mcz_3_feb_2020 if tin(01mar2020, 30jan2023), xlabel(, angle(90)) xlabel(#20) xmtick(#20) ymtick(#5) graphregion(color(white)) xtitle(Weekly OPN Survey - Covid-19 module) subtitle("Overall, how happy did you feel yesterday?") ytitle("Happy meanscore") note("Source: Opinion and Lifestyle Survey-Covid-19 module") legend(label(1 "Happy") label(2 "Happy_Feb_2020"))


graph twoway tsline mcz_3_men  mcz_3_women mcz_3_feb_2020 if tin(01mar2020, 30jan2023), xlabel(, angle(90)) xlabel(#20) xmtick(#20) ymtick(#5) graphregion(color(white)) xtitle(Weekly OPN Survey - Covid-19 module) subtitle("Overall, how happy did you feel yesterday?") ytitle("Happy meanscore") note("Source: Opinion and Lifestyle Survey-Covid-19 module") legend(label(1 "Happy_Men") label(2 "Happy_Women") label(3 "Happy_Feb_2020"))

graph twoway tsline mcz_3_age_16_29 mcz_3_age_30_49 mcz_3_age_50_69 mcz_3_age_70_ mcz_3_feb_2020 if tin(01mar2020, 30jan2023), xlabel(, angle(90)) xlabel(#20) xmtick(#20) ymtick(#5) graphregion(color(white)) xtitle(Weekly OPN Survey - Covid-19 module) subtitle("Overall, how happy did you feel yesterday?") ytitle("Happy meanscore") note("Source: Opinion and Lifestyle Survey-Covid-19 module") legend(label(1 "Happy_age_16_29") label(2 "Happy_age_30_49") label(3 "Happy_age_50_69") label(4 "Happy_age_70_") label(5 "Happy_Feb_2020"))

**************************

*************Anxiety by gender and age group************
graph twoway tsline mcz_4 mcz_4_feb_2020 if tin(01mar2020, 30jan2023), xlabel(, angle(90)) xlabel(#20) xmtick(#20) ymtick(#5) graphregion(color(white)) xtitle(Weekly OPN Survey - Covid-19 module) subtitle("Overall, how Anxious did you feel yesterday?") ytitle("Anxious meanscore") note("Source: Opinion and Lifestyle Survey-Covid-19 module") legend(label(1 "Anxious") label(2 "Anxious_Feb_2020"))


graph twoway tsline mcz_4_men  mcz_4_women mcz_4_feb_2020 if tin(01mar2020, 30jan2023), xlabel(, angle(90)) xlabel(#20) xmtick(#20) ymtick(#5) graphregion(color(white)) xtitle(Weekly OPN Survey - Covid-19 module) subtitle("Overall, how Anxious did you feel yesterday?") ytitle("Anxious meanscore") note("Source: Opinion and Lifestyle Survey-Covid-19 module") legend(label(1 "Anxious_Men") label(2 "Anxious_Women") label(3 "Anxious_Feb_2020"))

graph twoway tsline mcz_4_age_16_29 mcz_4_age_30_49 mcz_4_age_50_69 mcz_4_age_70_ mcz_4_feb_2020 if tin(01mar2020, 30jan2023), xlabel(, angle(90)) xlabel(#20) xmtick(#20) ymtick(#5) graphregion(color(white)) xtitle(Weekly OPN Survey - Covid-19 module) subtitle("Overall, how Anxious did you feel yesterday?") ytitle("Anxious meanscore") note("Source: Opinion and Lifestyle Survey-Covid-19 module") legend(label(1 "Anxious_age_16_29") label(2 "Anxious_age_30_49") label(3 "Anxious_age_50_69") label(4 "Anxious_age_70_") label(5 "Anxious_Feb_2020"))


*********************************************









graph twoway line mcz_2 date, graphregion(color(white)) xtitle(Weekly OPN Survey - Covid-19 module) ytitle(worthwhile (mean score) `x')
graph twoway line mcz_3 date, graphregion(color(white)) xtitle(Weekly OPN Survey - Covid-19 module) ytitle(Satisfaction (mean score) `x')
graph twoway line mcz_4 date, graphregion(color(white)) xtitle(Weekly OPN Survey - Covid-19 module) ytitle(Satisfaction (mean score) `x')








gen date1= mdy(day, month, year)

twoway lfitci mcz_1 Time, graphregion(color(white)) xtitle(Year) ytitle(Proportion with poor `x')









destring time, replace
format time %td


