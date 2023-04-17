****************************************************************************************************************************************
*************************************************What Works Centre for Wellbeing ******************************************************
**Analysis on Wellbeing over time, regions and by personal and housing circumstances using the English Housing Survey (EHS)2013-2020 ***
***************************************************Simona Tenaglia - March 2023**********************************************************
*****************************************************************************************************************************************
**Version 1.0  - Stata 14

**This do file run an analysis using  EHS Household Datasets for financial years from april 2013 to April 2020. We use the EHS Special License data,
**released through the UK Data Service. The analysis look at wellbeing variables over time, regions and by sociodemographic, households and housing characteristics.
**We do the following steps:
**1. for each year we merge the relevant datasets and save them  
**2. we append the  datasets
**3. we prepare the varables relevant for the analysis
**4. we look at wellbeing distributons
**5. we look at wellbeing over times, by sociodemographics, households and housing characteristics
**5. we analyse wellbeing over regions

**********************************************************The English Housing Survey (EHS) ******************************************************
**The EHS consist of 2 main elements: an initial interview survey of around 13,300 households with a follow up physical inspection of a sub-sample 
**of approximately 6,200 of these dwellings, including vacant dwellings.
**These data are available through the UK Data Service and  are released under a Special Licence or an End User Licence. The datasets released under the 
**different licences vary by the level of detail of the data and the measures of statistical disclosure control that have been applied

********************************************************** The English Housing Survey (EHS) datasets************************************************          
**In the EHS  two separate datasets are available:                                                                                           
** a) EHS Household Dataset – The dataset comprises the full sample primary ‘raw’ interview survey data (available via the Special Licence only),  
** plus associated derived variables (available via both the End User Licence and the Special Licence) for all cases where an interview has been  
**completed - approximately 13,300 households per annum. Datasets are provided for single financial years together with annual weights. This dataset
**should be used for any analysis where only information from the household interview is required.                                                 
                                                                                                                                                
**b) EHS Housing Stock Dataset – The Housing Stock Dataset should be used for any analysis requiring information relating to the 
**physical characteristics and energy efficiency of the housing stock. The dataset comprises the paired sample primary ‘raw’ interview 
**survey and physical survey data (available via the Special Licence only), plus associated derived variables (available via both the End 
**User Licence and the Special Licence) for all cases where a physical survey has been completed. Housing stock data on 
**occupied dwellings comprise of data from the household interview as well as data from the physical survey. For vacant properties,
**only data for the physical survey are provided. The housing stock data are made available for a two-year rolling sample with the appropriate 
**two-year weights.

******************************************************Datasets used in this analysis**************************************************************
** This file describe the analysis run on EHS Household Dataset. In the  file EHS_Github_stock.do we describe the analysis run on the EHS Housing Stock Dataset
**We use the following raw and derived dataset for all the years 2013-2020:

**RAW DATA files
** identity_sl_protect.dta 
**disability_sl_protect.dta
**people_sl_protect.dta
**attitudes_sl_protect.dta
**damp_sl_protected.dta  (available only from 2016-2017 onward)

**DERIVED DATA files
**generalfsXX_sl_protect.dta   (XX stands for the year)
**interviewfsXX_sl_protect.dta

****************************************************DATA***********************************************************
clear all
set more off, perm     /// not to pause or display a -more- message. The option perm means the -more- setting is remembered and become the default setting.

**Set working directory
cd 

**In the following lines we are going to:
**1. create a unique dataset for each year from 2013 to 2020 merging raw and derived datasets containing variables relevant for the analysis and save them separately. The dataset damp.dta is present only from 2016-2017 onward
**2. append the datasets created
**3. create unique variable for weights
 
**1. create one dataset for each year 2013-2020 merging relevant raw and derived datasets
**2013-2014
use "E:\WWCLAV\Housing\ONS data\2013-2014\UKDA-7801-stata\stata\stata11\derived\generalfs13.dta", clear
merge 1:1 aacode using "E:\WWCLAV\Housing\ONS data\2013-2014\UKDA-7801-stata\stata\stata11\derived\interviewfs13.dta"
drop _merge
merge m:m aacode using "E:\WWCLAV\Housing\ONS data\2013-2014\UKDA-7801-stata\stata\stata11\interview\identity.dta"
drop _merge
merge m:m aacode using "E:\WWCLAV\Housing\ONS data\2013-2014\UKDA-7801-stata\stata\stata11\interview\disability.dta"
drop _merge
merge m:m aacode using "E:\WWCLAV\Housing\ONS data\2013-2014\UKDA-7801-stata\stata\stata11\interview\people.dta"
drop _merge
merge m:m aacode using "E:\WWCLAV\Housing\ONS data\2013-2014\UKDA-7801-stata\stata\stata11\interview\attitudes.dta"
gen year = 2013
save "E:\WWCLAV\Housing\ONS data\merged data 2014-2020\2013-2014.dta", replace
**2014-2015
use "E:\WWCLAV\Housing\ONS data\2014-2015\8067stata_F13A6A934DEB70F65D314CB5303AF1E3_V1\UKDA-8067-stata\stata\stata11\derived\generalfs14_sl_protect.dta", clear
merge 1:1 aacode using "E:\WWCLAV\Housing\ONS data\2014-2015\8067stata_F13A6A934DEB70F65D314CB5303AF1E3_V1\UKDA-8067-stata\stata\stata11\derived\interviewfs14_sl_protect.dta"
drop _merge
merge 1:m aacode using "E:\WWCLAV\Housing\ONS data\2014-2015\8067stata_F13A6A934DEB70F65D314CB5303AF1E3_V1\UKDA-8067-stata\stata\stata11\interview\identity_sl_protect.dta"
drop _merge
merge m:m aacode using "E:\WWCLAV\Housing\ONS data\2014-2015\8067stata_F13A6A934DEB70F65D314CB5303AF1E3_V1\UKDA-8067-stata\stata\stata11\interview\disability_sl_protect.dta"
drop _merge
merge m:m aacode using "E:\WWCLAV\Housing\ONS data\2014-2015\8067stata_F13A6A934DEB70F65D314CB5303AF1E3_V1\UKDA-8067-stata\stata\stata11\interview\people_sl_protect.dta"
drop _merge
merge m:m aacode using "E:\WWCLAV\Housing\ONS data\2014-2015\8067stata_F13A6A934DEB70F65D314CB5303AF1E3_V1\UKDA-8067-stata\stata\stata11\interview\attitudes_sl_protect.dta"
gen year=2014
save "E:\WWCLAV\Housing\ONS data\merged data 2014-2020\2014-2015.dta", replace
**2015-2016
use "E:\WWCLAV\Housing\ONS data\2015-2016\8254stata_8EA749C75F63D65128576AEA483EE0DC_V1\UKDA-8254-stata\stata\stata11\derived\generalfs15_sl_protect.dta", clear
merge 1:1 serialanon using "E:\WWCLAV\Housing\ONS data\2015-2016\8254stata_8EA749C75F63D65128576AEA483EE0DC_V1\UKDA-8254-stata\stata\stata11\derived\interviewfs15_sl_protect.dta"
drop _merge
merge m:m serialanon using "E:\WWCLAV\Housing\ONS data\2015-2016\8254stata_8EA749C75F63D65128576AEA483EE0DC_V1\UKDA-8254-stata\stata\stata11\interview\identity_sl_protect.dta"
drop _merge
merge m:m serialanon using "E:\WWCLAV\Housing\ONS data\2015-2016\8254stata_8EA749C75F63D65128576AEA483EE0DC_V1\UKDA-8254-stata\stata\stata11\interview\disability_sl_protect.dta"
drop _merge
merge m:m serialanon using "E:\WWCLAV\Housing\ONS data\2015-2016\8254stata_8EA749C75F63D65128576AEA483EE0DC_V1\UKDA-8254-stata\stata\stata11\interview\people_sl_protect.dta"
drop _merge
merge m:m serialanon using "E:\WWCLAV\Housing\ONS data\2015-2016\8254stata_8EA749C75F63D65128576AEA483EE0DC_V1\UKDA-8254-stata\stata\stata11\interview\attitudes_sl_protect.dta"
gen year=2015
save "E:\WWCLAV\Housing\ONS data\merged data 2014-2020\2015-2016.dta", replace
**2016-2017
use "E:\WWCLAV\Housing\ONS data\2016-2017\8386stata_D1AFD7C3FC381A6744957CB5A830F4A4_V1\UKDA-8386-stata\stata\stata11\derived\generalfs16_sl_protect.dta", clear
merge 1:m serialanon using "E:\WWCLAV\Housing\ONS data\2016-2017\8386stata_D1AFD7C3FC381A6744957CB5A830F4A4_V1\UKDA-8386-stata\stata\stata11\derived\interviewfs16_sl_protect.dta"
drop _merge
merge m:m serialanon using "E:\WWCLAV\Housing\ONS data\2016-2017\8386stata_D1AFD7C3FC381A6744957CB5A830F4A4_V1\UKDA-8386-stata\stata\stata11\interview\identity_sl_protect.dta"
drop _merge
merge m:m serialanon using "E:\WWCLAV\Housing\ONS data\2016-2017\8386stata_D1AFD7C3FC381A6744957CB5A830F4A4_V1\UKDA-8386-stata\stata\stata11\interview\disability_sl_protect.dta"
drop _merge
merge m:m serialanon using"E:\WWCLAV\Housing\ONS data\2016-2017\8386stata_D1AFD7C3FC381A6744957CB5A830F4A4_V1\UKDA-8386-stata\stata\stata11\interview\damp_sl_protect.dta"
drop _merge
merge m:m  serialanon using "E:\WWCLAV\Housing\ONS data\2016-2017\8386stata_D1AFD7C3FC381A6744957CB5A830F4A4_V1\UKDA-8386-stata\stata\stata11\interview\people_sl_protect.dta"
drop _merge
merge m:m serialanon using "E:\WWCLAV\Housing\ONS data\2016-2017\8386stata_D1AFD7C3FC381A6744957CB5A830F4A4_V1\UKDA-8386-stata\stata\stata11\interview\attitudes_sl_protect.dta"
gen year=2016
save "E:\WWCLAV\Housing\ONS data\merged data 2014-2020\2016-2017.dta", replace
**2017-2018
use "E:\WWCLAV\Housing\ONS data\2017-2018\8545stata_13CA70FD980F883B47E9F67EA60682C0_V1\UKDA-8545-stata\stata\stata11\derived\generalfs17_sl_protected.dta", clear
merge 1:1 serialanon using "E:\WWCLAV\Housing\ONS data\2017-2018\8545stata_13CA70FD980F883B47E9F67EA60682C0_V1\UKDA-8545-stata\stata\stata11\derived\interviewfs17_sl_protected.dta"
drop _merge
merge m:m serialanon using "E:\WWCLAV\Housing\ONS data\2017-2018\8545stata_13CA70FD980F883B47E9F67EA60682C0_V1\UKDA-8545-stata\stata\stata11\interview\identity_sl_protected.dta"
drop _merge
merge m:m serialanon using "E:\WWCLAV\Housing\ONS data\2017-2018\8545stata_13CA70FD980F883B47E9F67EA60682C0_V1\UKDA-8545-stata\stata\stata11\interview\disability_sl_protected.dta"
drop _merge
merge m:m serialanon using "E:\WWCLAV\Housing\ONS data\2017-2018\8545stata_13CA70FD980F883B47E9F67EA60682C0_V1\UKDA-8545-stata\stata\stata11\interview\damp_sl_protected.dta"
drop _merge
merge m:m serialanon using "E:\WWCLAV\Housing\ONS data\2017-2018\8545stata_13CA70FD980F883B47E9F67EA60682C0_V1\UKDA-8545-stata\stata\stata11\interview\people_sl_protected.dta"
drop _merge
merge m:m serialanon using "E:\WWCLAV\Housing\ONS data\2017-2018\8545stata_13CA70FD980F883B47E9F67EA60682C0_V1\UKDA-8545-stata\stata\stata11\interview\attitudes_sl_protected.dta"
gen year=2017
save "E:\WWCLAV\Housing\ONS data\merged data 2014-2020\2017-2018.dta", replace
**2018-2019
use "E:\WWCLAV\Housing\ONS data\2018-2019\8719stata_FC3C7BC4388F1E58AEA842E7AEA57A6D_V1\UKDA-8719-stata\stata\stata13\derived\generalfs18_sl_protect.dta", clear
merge 1:1 serialanon using "E:\WWCLAV\Housing\ONS data\2018-2019\8719stata_FC3C7BC4388F1E58AEA842E7AEA57A6D_V1\UKDA-8719-stata\stata\stata13\derived\interviewfs18_sl_protect.dta"
drop _merge
merge m:m serialanon using "E:\WWCLAV\Housing\ONS data\2018-2019\8719stata_FC3C7BC4388F1E58AEA842E7AEA57A6D_V1\UKDA-8719-stata\stata\stata13\interview\identity_sl_protect.dta"
drop _merge
merge m:m serialanon using "E:\WWCLAV\Housing\ONS data\2018-2019\8719stata_FC3C7BC4388F1E58AEA842E7AEA57A6D_V1\UKDA-8719-stata\stata\stata13\interview\disability_sl_protect.dta"
drop _merge
merge m:m serialanon using "E:\WWCLAV\Housing\ONS data\2018-2019\8719stata_FC3C7BC4388F1E58AEA842E7AEA57A6D_V1\UKDA-8719-stata\stata\stata13\interview\damp_sl_protect.dta"
drop _merge
merge m:m serialanon using "E:\WWCLAV\Housing\ONS data\2018-2019\8719stata_FC3C7BC4388F1E58AEA842E7AEA57A6D_V1\UKDA-8719-stata\stata\stata13\interview\people_sl_protect.dta"
drop _merge
merge m:m serialanon using "E:\WWCLAV\Housing\ONS data\2018-2019\8719stata_FC3C7BC4388F1E58AEA842E7AEA57A6D_V1\UKDA-8719-stata\stata\stata13\interview\attitudes_sl_protect.dta"
gen year=2018
save "E:\WWCLAV\Housing\ONS data\merged data 2014-2020\2018-2019.dta", replace
**2019-2020
use  "E:\WWCLAV\Housing\ONS data\2019-2020\8921stata_43C8BD258FF85C2B2AF2B155750BB1208FBF1C9A449B266ED92E4C302C7AFCC1_V1\UKDA-8921-stata\stata\stata13\derived\generalfs_sl_protect.dta", clear
merge 1:1 serialanon using "E:\WWCLAV\Housing\ONS data\2019-2020\8921stata_43C8BD258FF85C2B2AF2B155750BB1208FBF1C9A449B266ED92E4C302C7AFCC1_V1\UKDA-8921-stata\stata\stata13\derived\interviewfs19_sl_protect.dta"
drop _merge
merge 1:m serialanon using "E:\WWCLAV\Housing\ONS data\2019-2020\8921stata_43C8BD258FF85C2B2AF2B155750BB1208FBF1C9A449B266ED92E4C302C7AFCC1_V1\UKDA-8921-stata\stata\stata13\identity_sl_protect.dta"
drop _merge
merge m:m serialanon using "E:\WWCLAV\Housing\ONS data\2019-2020\8921stata_43C8BD258FF85C2B2AF2B155750BB1208FBF1C9A449B266ED92E4C302C7AFCC1_V1\UKDA-8921-stata\stata\stata13\disability_sl_protect.dta"
drop _merge
merge m:m serialanon using "E:\WWCLAV\Housing\ONS data\2019-2020\8921stata_43C8BD258FF85C2B2AF2B155750BB1208FBF1C9A449B266ED92E4C302C7AFCC1_V1\UKDA-8921-stata\stata\stata13\damp_sl_protect.dta"
drop _merge
merge m:m serialanon using "E:\WWCLAV\Housing\ONS data\2019-2020\8921stata_43C8BD258FF85C2B2AF2B155750BB1208FBF1C9A449B266ED92E4C302C7AFCC1_V1\UKDA-8921-stata\stata\stata13\people_sl_protect.dta"
drop _merge
merge m:m serialanon using "E:\WWCLAV\Housing\ONS data\2019-2020\8921stata_43C8BD258FF85C2B2AF2B155750BB1208FBF1C9A449B266ED92E4C302C7AFCC1_V1\UKDA-8921-stata\stata\stata13\attitudes_sl_protect.dta"
gen year = 2019
save "E:\WWCLAV\Housing\ONS data\merged data 2014-2020\2019-2020.dta", replace

**2. Append datasets created for years 2013-2020  
clear
use "E:\WWCLAV\Housing\ONS data\merged data 2014-2020\2013-2014.dta"  , clear
append using "E:\WWCLAV\Housing\ONS data\merged data 2014-2020\2014-2015.dta"
append using "E:\WWCLAV\Housing\ONS data\merged data 2014-2020\2015-2016.dta"
append using "E:\WWCLAV\Housing\ONS data\merged data 2014-2020\2016-2017.dta"
append using  "E:\WWCLAV\Housing\ONS data\merged data 2014-2020\2017-2018.dta"
append using "E:\WWCLAV\Housing\ONS data\merged data 2014-2020\2018-2019.dta"
append using  "E:\WWCLAV\Housing\ONS data\merged data 2014-2020\2019-2020.dta"
** 3. create unique variable for household weights  
replace aagfh13=aagfh14 if year==2014
replace aagfh13=aagfh15 if year==2015
replace aagfh13=aagfh16 if year==2016
replace aagfh13=aagfh17 if year==2017
replace aagfh13=aagfh18 if year==2018
replace aagfh13=aagfh19 if year==2019

** rename wellbeing and sociodemographic variables.
rename QSatis Satis						/// this variable is referred to the household referent person

rename QHappy Happy                     /// this variable is referred to the household referent person

rename QWorth Worth                     /// this variable is referred to the household referent person

rename QAnxious Anxious                  /// this variable is referred to the household referent person

rename agehrp6x age6                     /// this variable is referred to the household referent person

rename emphrpx emp                      /// this variable is referred to the household referent person

rename ethhrp8x ethnicity               /// this variable is referred to the household referent person

rename hhcomp1 hhcomp

**create unique variable for age of all household members
replace age=AGE if year==2013

**recoding categories 5-8 in one category called "other" in  variable ethnicity 
replace ethnicity=8 if ethnicity>=5

**recoding categories 6-10 in category 5 in variable number of dependent child 
replace ndepchild=5 if ndepchild>=5

**replace variables negative values with missing values
local demo Satis Happy Worth Anxious DsMental2 QHealth1 mortwkx rentwkx HiQual HSatis xMarSta2 hhltsick hhwhch Satten2 LldSat HAS44
foreach x of local demo {
 replace `x'=. if `x'<0 
 }
 **create unique variable for Government Office regions
replace gorehs=GorEHS  if year==2013
forvalues t = 2014/2016 {
	  replace gorehs=gorEHS if year==`t'
}
**replace  categories 6 and 7 with category "others" in variable accomodations type 
replace accomhh=6 if accomhh>=6

** define label for variable accommodation type
label define accomhh -9 "does not apply" -8 "no answer" 1 "detached house or bungalow" 2 "semi-detached " 3 "terrace/end of terrace" 4 "purpose built flat/maisonette" 5 "flat conversion/rooms" 6 "caravan, boat and others", replace

** variable "satisfaction with current tenure" has different name in 2013-2014 and 1 more category (no opinion)  than "satisfaction with current tenure" variable for subsequent years. we then create a unique variable for all years and we consider missing the category present only in 2013.
replace Satten=. if Satten==6
replace Satten2=Satten if year==2013

**create only four categories for variable household composition putting together lone parent male and lone parent female, multi family household with male or female HRP, 1 male or 1 female
replace hhcomp=2 if hhcomp==3
replace hhcomp=4 if hhcomp==5
replace hhcomp=6 if hhcomp==7
**create a lable for variable household composition with grouped categories created above
label define hhcompl 1 "married/cohabiting couple" 2 "lone parent male/female HRP" 4 " multi family household male/female HRP" 6 "1 male or female"
label values hhcomp hhcompl

** generate age squared
gen agesq= age^2

**recode variable sex of HRP to have a dummy where female=1 and male =0
gen female=.                     
replace female=1 if sexhrp==2     
replace female=0 if sexhrp==1 
**create label for female
label define female_l 1 "female" 0 "male" 
label values female female_l    

**create one category for number od independent child higher than 3
replace nxdepch=2 if (nxdepch>=3)  & (nxdepch<.)

**create variable for dependent child=0 or dependent child>0
gen depchild=.
replace depchild=0 if ndepchild==0
replace depchild=1 if ndepchild>0
**create label for presence dependent child
label define depchild_l 1 "1 or more dependent child" 0 "0 dependent child" 
label values depchild depchild_l  

**create dummy for use of wheelchair and label 
replace hhwhch=1 if hhwhch>1
label define wheelchairl 0 "nobody use wheelchair" 1 "only indoors, only outdoors, all the time" 
label values hhwhch wheelchairl

**create unique category for number of rooms available to households equals to 1  or  2 rooms, given the low frequencies of these categories
replace  nrooms1a= 2 if nrooms1a==1

**generate satisfaction with accomodation, current tenure, with area and with repairs with reverse coding
gen  HSatisrev=6-HSatis
gen  Satten2rev=6-Satten2
gen HAS44rev=6-HAS44 
gen LldSatrev=7-LldSat

**generate weekly mortgage payment in band
gen mortgage_band=.
replace  mortgage_band=1 if (mortwkx>=0) & (mortwkx<100)
replace  mortgage_band=2 if (mortwkx>=100) & (mortwkx<500)
replace  mortgage_band=3 if (mortwkx>=500) & (mortwkx<1000)
replace  mortgage_band=4 if (mortwkx>=1000) & (mortwkx<.)

**Create variables for High and Low wellbeing, considering that wellbeing variables are asked only to Household Reference Person (HRP)
foreach x of varlist Satis Happy Worth  {
gen High_`x' = `x'>=9
gen Low_`x' = `x'<=4
replace High_`x' = . if `x'==.
replace High_`x'=. if HRP!=persno
replace Low_`x' = . if `x'==.
replace Low_`x'=. if HRP!=persno
}

foreach x of varlist Anxious{
gen High_`x' = `x'>=6
gen Low_`x' = `x'<=1
replace High_`x' = . if `x'==.
replace High_`x'=. if HRP!=persno
replace Low_`x' = . if `x'==.
replace Low_`x'=. if HRP!=persno
}

********************************************Distributions of wellbeing variables***********************************************
** graph densities wellbeing variables by year
tw (histogram Satis if HRP==persno), graphregion(fcolor(white) lcolor(none)) subtitle(, fcolor(white) lcolor(white) bmargin(zero)) by(year)
tw (histogram Happy if HRP==persno), graphregion(fcolor(white) lcolor(none)) subtitle(, fcolor(white) lcolor(white) bmargin(zero)) by(year)
tw (histogram Worth if HRP==persno), graphregion(fcolor(white) lcolor(none)) subtitle(, fcolor(white) lcolor(white) bmargin(zero)) by(year)
tw (histogram Anxious if HRP==persno), graphregion(fcolor(white) lcolor(none))plotregion(fcolor(white)) subtitle(, fcolor(white) lcolor(white) bmargin(zero)) by(year)


**Kolmogorov Smirnoff test for different years
tabulate (year), gen (year_dummy) 
local wellbeing Satis Happy Worth Anxious
local dum_vars year_dummy1 year_dummy2 year_dummy3 year_dummy4 year_dummy5 year_dummy6 year_dummy7

foreach x of local wellbeing {
    foreach v of local dum_vars {
        ksmirnov `x', by(`v')
	}
}
** we want to look at wellbeing densities in the first and last year available (2013 and 2019). We want also to introduce a line for the mean and the median values. For this reason we calculate 
** the mean and median values for the two years 2013 and  2019 
tabstat Satis Happy Worth Anxious [aweight=aagfh13] if year==2013 & HRP==persno , stats(n mean median min max)
tabstat Satis Happy Worth Anxious [aweight=aagfh13] if year==2019 & HRP==persno, stats(n mean median min max)

**density for year 2013-2014 and 2019-2020 for wellbeing variables
kdensity Satis if year==2013 & HRP==persno  [aweight = aagfh13] ,recast(bar) barwidth(0.5) fcolor("58 183 185") ///
graphregion(fcolor(white) lcolor(white)) plotregion(fcolor(white) lcolor(white)) xline(7.4,  lcolor(black) lwidth(thick) ///
lpattern(solid) ) xline(8, lcolor(olive) lwidth(thick) lpattern(dash)) xlabel ///
(1 "1" 2 "2" 3 "3" 4 "4" 5 "5" 6 "6" 7 "7" 7.4 "7.4 mean" 8 " 8 median" 9 "9" 10 "10" , ///
labsize(vsmall) angle(forty_five)) title( Life Satisfaction density 2013-2014, size(medsmall))
graph export "ls2013_14density.png", replace

kdensity Satis if year==2019 [aweight = aagfh13],recast(bar) barwidth(0.3) fcolor("58 183 185") ///
graphregion(fcolor(white)lcolor(white)) plotregion(fcolor(white) lcolor(white)) xline(7.7, lcolor(black) lwidth(thick) lpattern(solid)) ///
xline(8, lcolor(olive)lwidth(thick) lpattern(dash) ) xlabel (1 "1" 2 "2" 3 "3" 4 "4" 5 "5" 6 "6" 7 "7" 7.7 "7.7 mean" 8 " 8 median" 9 "9" 10 "10" , labsize(vsmall) angle(forty_five)) ///
title( Life Satisfaction density 2019-2020, size(medsmall)) 
graph export "ls2019_20density.png", replace

kdensity Happy if year==2013 [aweight = aagfh13],recast(bar) barwidth(0.5) fcolor("58 183 185") graphregion(fcolor(white) lcolor(white))plotregion(fcolor(white) lcolor(white)) /// 
xline(7.3,  lcolor(black) lwidth(thick) lpattern(solid) ) xline(8, lcolor(olive) lwidth(thick) lpattern(dash))  ///
xlabel (1 "1" 2 "2" 3 "3" 4 "4" 5 "5" 6 "6" 7 "7" 7.3 "7.3 mean" 8 " 8 median" 9 "9" 10 "10" , labsize(vsmall) angle(forty_five))title( Happiness density 2013-2014, size(medsmall)) 
graph export "happy2013_14density.png", replace

kdensity Happy if year==2019 [aweight = aagfh13],recast(bar) barwidth(0.5) fcolor("58 183 185") graphregion(fcolor(white) lcolor(white)) ///
plotregion(fcolor(white) lcolor(white)) xline(7.5,  lcolor(black) lwidth(thick) lpattern(solid)) xline(8, lcolor(olive)lwidth(thick) lpattern(dash) )  ///
 xlabel (1 "1" 2 "2" 3 "3" 4 "4" 5 "5" 6 "6" 7 "7" 7.5 "7.5 mean" 8 " 8 median" 9 "9" 10 "10" , labsize(vsmall) angle(forty_five)) title( Happiness density 2019-2020, size(medsmall)) 
graph export "happy2019_20density.png", replace
cd "E:\WWCLAV\Housing\graphs"

kdensity Worth if year==2013 [aweight = aagfh13],recast(bar) barwidth(0.5) fcolor("58 183 185") graphregion(fcolor(white) lcolor(white)) ///
plotregion(fcolor(white) lcolor(white)) xline(7.7,  lcolor(black) lwidth(thick) lpattern(solid) ) xline(8, lcolor(olive) lwidth(thick) lpattern(dash)) ///
xlabel (1 "1" 2 "2" 3 "3" 4 "4" 5 "5" 6 "6" 7 "7" 7.7 "7.7 mean" 8 " 8 median" 9 "9" 10 "10" , labsize(vsmall) angle(forty_five)) title( Worthwhile density 2013-2014, size(medsmall)) 
graph export "worth2013_14density.png", replace

kdensity Worth if year==2019 [aweight = aagfh13],recast(bar) barwidth(0.5) fcolor("58 183 185") graphregion(fcolor(white) lcolor(white)) ///
plotregion(fcolor(white) lcolor(white)) xline(7.9,  lcolor(black) lwidth(thick) lpattern(solid)) xline(8, lcolor(olive)lwidth(thick) lpattern(dash) ) ///
xlabel (1 "1" 2 "2" 3 "3" 4 "4" 5 "5" 6 "6" 7 "7" 7.9 "7.9 mean" 8 " 8 median" 9 "9" 10 "10" , labsize(tiny) angle(forty_five)) title( Worthwhile density 2019-2020, size(medsmall)) 
graph export "worth2019_20density.png", replace

kdensity Anxious if year==2013 [aweight = aagfh13],recast(bar) barwidth(0.5) fcolor("58 183 185") graphregion(fcolor(white) lcolor(white)) plotregion(fcolor(white) lcolor(white)) ///
xline(3,  lcolor(black) lwidth(thick) lpattern(solid) ) xline(2, lcolor(olive) lwidth(thick) lpattern(dash)) xlabel (1 "1" 2 "2 median" 3 "3 mean" 4 "4" 5 "5" 6 "6" 7 "7"  8 "8" 9 "9" 10 "10" , labsize(vsmall) angle(forty_five)) ///
title( Anxiety density 2013-2014, size(medsmall)) 
graph export "anxious2013_14density.png", replace

kdensity Anxious if year==2019 [aweight = aagfh13],recast(bar) barwidth(0.5) fcolor("58 183 185") graphregion(fcolor(white) lcolor(white)) plotregion(fcolor(white) lcolor(white)) ///
xline(2.7,  lcolor(black) lwidth(thick) lpattern(solid)) xline(2, lcolor(olive)lwidth(thick) lpattern(dash) )xlabel (1 "1" 2 "2 median" 2.7 "2.7 mean" 3 "3" 4 "4" 5 "5" 6 "6" 7 "7" 8 "8" 9 "9" 10 "10" , labsize(vsmall) angle(forty_five)) ///
title( Anxiety density 2019-2020, size(medsmall)) 
graph export "anxious2019_20density.png", replace

*****************************Analysis over time************************************
**Mean values for wellbeing  over 2013-2019
foreach x of varlist Satis Happy Worth Anxious{
			mean `x' [aweight =aagfh13 ] if HRP==persno, over( year)
 }
**Graphs of median and mean wellbeing variables over 2013-2019
preserve
collapse (p50)  Satis Happy Worth Anxious [aweight = aagfh13] if HRP==persno, by(year )
twoway (line Satis year) (line Happy year) (line Worth year) (line Anxious year), ytitle(Median wellbeing) ytitle(, size(small)) title(Median wellbeing over 2013-2019, size(medsmall)) graphregion(color(white)) ///
legend(order(1 "Life Satisfaction" 2 " Happiness " 3 " Worthwhile" 4 "Anxiety"  ) size(small) span) xlabel(#10, labsize(vsmall) valuelabel)
restore

preserve
collapse Satis Happy Worth Anxious [aweight = aagfh13] if HRP==persno, by(year) 
twoway (line Satis year) (line Happy year) (line Worth year) (line Anxious year), ytitle(Mean wellbeing) ytitle(, size(small)) title(Mean wellbeing over 2013-2019, size(medsmall)) graphregion(color(white)) ///
legend(order(1 "Life Satisfaction" 2 " Happiness " 3 " Worthwhile" 4 "Anxiety"  ) size(small) span) xlabel(#10, labsize(vsmall) valuelabel)
restore

**correlation matrix for wellbeing variables over years
forvalues t = 2013/2019 {
	     pwcorr  Satis Happy Worth Anxious if year == `t'& HRP==persno, star(.05)
 }
**calculate Kurtosis and simmetry for wellbeing variables
local wellbeing Satis Happy Worth Anxious
forvalues t = 2013/2019 {
		foreach v of local wellbeing {
        summarize `v'  [aweight =aagfh13 ] if year == `t'& HRP==persno, detail
	}
}
**create a trend 
gen trend = year-2013 

**regression analysis of wellbeing variables with trend   
foreach x of varlist Satis Happy Worth Anxious{
reg `x' trend if HRP==persno, vce(robust)
 } 

 **graphs of high/low wellbeing variables scores for the whole period 2013-2020
preserve
collapse High_Satis High_Happy High_Worth Low_Anxious Low_Satis Low_Happy Low_Worth High_Anxious, by(year)

graph hbar (mean) Low_Satis (mean) High_Satis, over(year) graphregion(color(white)) asyvars bar (1,color("58 183 185")) bar(2, color("0 52 66")) ///
legend(order(1 "proportion with""Low Life Satisfaction" 2 "proportion with""High Life Satisfaction") size(small))
graph export "high_lowls.png", replace

graph hbar (mean) Low_Happy (mean) High_Happy, over(year) graphregion(color(white)) asyvars bar (1,color("58 183 185")) bar(2, color("0 52 66")) ///
legend(order(1 "proportion with""Low Happiness" 2 "proportion with""High Happiness") size(small))
graph export "high_lowhappy.png", replace

graph hbar (mean) Low_Worth (mean) High_Worth, over(year) graphregion(color(white)) asyvars bar (1,color("58 183 185")) bar(2, color("0 52 66")) ///
legend(order(1 "proportion with""Low Worthwhile" 2 "proportion with""High Worthwhile") size(small))
graph export "high_lowworth.png", replace

graph hbar (mean) High_Anxious  (mean) Low_Anxious, over(year) graphregion(color(white)) asyvars bar (1,color("58 183 185")) bar(2, color("0 52 66")) ///
legend(order(1 "proportion with High Anxiety" 2 "proportion with Low Anxiety") size(small))
graph export "high_lowanxious.png", replace

restore

****************************Analysis of wellbeing by sociodemographic and household variables ***********************
** calculate sociodemographic characteristics over year for later analysis
tab  age6  year [iweight= aagfh13] if  HRP==persno
tab nxdepch year[iweight= aagfh13] if   HRP==persno 
tab hhwhch year [iweight= aagfh13] if HRP==persno

**calculate mean wellbeing variables by sociodemographics variables and health variables
local wellbeing Satis Happy Worth Anxious
local sociodem agehrp4x ethnicity ndepchild nxdepch emp hhtype7 famnum hhinc5x QHealth1 hhltsick 

foreach x of local wellbeing{
	foreach v of local sociodem {
		 mean `x' [aweight = aagfh13] if HRP==persno, over (`v')
	} 
}
** calculate correlation matrix for wellbeing variables with  personal and households characteristics
local wellbeing Satis Happy Worth Anxiou
local sociodem agehrp4x ethnicity ndepchild emp hhtype6  hhinc5x QHealth1 hhltsick 

foreach x of local wellbeing {
	foreach v of local sociodem {
		pwcorr `x' `v' if HRP==persno, star(.05)
	} 
}
** graph mean wellbeing by age, ethnicity, emp, disability or illness in household, income quintile household type
local wellbeing Satis Happy Worth Anxious
local demo age6 sexhrp ethnicity  ndepchild  hhtype6 emp hhltsick QHealth1 hhwhch hhinc5x depchild mortwkx
foreach x of local wellbeing{
	foreach v of local demo {
		graph bar (mean)`x' [aweight =aagfh13 ]if HRP==persno , over (`v') over(year)   ytitle(Mean `x', size(msmall))graphregion(color(white)) ///
		asyvars bar (1, color("58 183 185")) bar(2, color("0 52 66")) bar (3, color("247 147 33"))
		graph export "mean`x' `v'.png", replace
	}
}

**calculate high and low wellbeing for presence of disable person and dependent child
preserve
collapse High_Satis High_Happy High_Worth Low_Anxious Low_Satis Low_Happy Low_Worth High_Anxious, by(year hhltsick ethnicity emp  ndepchild )
**graph of high and low wellbeing in presence of long term disability or dependent child
graph hbar (mean) High_Anxious  (mean) Low_Anxious if hhltsick==1, over(year)  graphregion(color(white)) ///
title(High and low Anxiety if disability or long term illness present in the household, size(small)) ///
asyvars bar (1,color("58 183 185")) bar(2, color("0 52 66"))bar (3, color("247 147 33"))
graph export "high_lowAnxietydis.png", replace

graph hbar (mean) High_Anxious  (mean) Low_Anxious if hhltsick==2, over(year)  graphregion(color(white)) ///
title(High and low Anxiety if disability or long term illness NOT present in the household, size(small)) ///
asyvars bar (1,color("58 183 185")) bar(2, color("0 52 66"))bar (3, color("247 147 33"))
graph export "high_lowAnxietydis1.png", replace

graph hbar (mean) High_Satis  (mean) Low_Satis if hhltsick==1, over(year)  graphregion(color(white)) ///
title(High and low Life Satisfaction if disability or long term illness present in the household, size(small)) ///
asyvars bar (1,color("58 183 185")) bar(2, color("0 52 66"))bar (3, color("247 147 33"))
graph export "high_lowlsdis.png", replace

graph hbar (mean) High_Satis  (mean) Low_Satis if hhltsick==2, over(year)  graphregion(color(white)) ///
title(High and low Life Satisfaction if disability or long term illness NOT present in the household, size(small)) ///
asyvars bar (1,color("58 183 185")) bar(2, color("0 52 66"))bar (3, color("247 147 33"))
graph export "high_lowlsdis1.png", replace


graph hbar (mean) Low_Satis (mean) High_Satis , over(ndepchild)  graphregion(color(white)) ///
 asyvars bar (1,color("58 183 185")) bar(2, color("0 52 66"))bar (3, color("247 147 33"))  ///
legend(order(1 "proportion with""High Life Satisfaction" 2 "proportion with""Low Life Satisfaction") size(small))
graph export "high_lowlsdepch.png", replace

graph hbar (mean) High_Anxious  (mean) Low_Anxious , over(ndepchild)  graphregion(color(white)) ///
 asyvars bar (1,color("58 183 185")) bar(2, color("0 52 66"))bar (3, color("247 147 33")) ///
legend(order(1 "proportion with High Anxiety" 2 "proportion with Low Anxiety") size(small))
graph export "high_lowAnxietydepch.png", replace
restore

*****************************Descriptives of housing characteristics************************************************
**tables of types of tenures over years, by age groups and ethnicity
tab tenex year[iweight= aagfh13] if HRP==persno
tab tenex age6 [iweight= aagfh13] if HRP==persno & year==2019
tab tenex age6 [iweight= aagfh13] if HRP==persno & year==2013
tab  tenex ethnicity [iweight= aagfh13] if HRP==persno & year==2019
tab  tenex ethnicity [iweight= aagfh13] if HRP==persno & year==2013

*****************************Analysis of wellbeing by housing variables**********************************************
**plot wellbeing variables densities  by tenure and accomodation type
local wellbeing Satis Happy Worth Anxious
foreach x of local wellbeing  {
		twoway (histogram `x' if HRP==persno), xlabel(, labsize(small)) by(, subtitle(, fcolor(white) lcolor(white))) ///
		by(, graphregion(fcolor(white) ifcolor(white)))by(tenex) subtitle(, fcolor(white) lcolor(white) bmargin(zero))
		graph export "density`x'tenure.png", replace
}  
 local wellbeing Satis Happy Worth Anxious
foreach x of local wellbeing  {
		twoway (histogram `x' if HRP==persno)if accomhh!=6, xlabel(, labsize(small)) by(, subtitle(, fcolor(white) lcolor(white))) ///
		by(, graphregion(fcolor(white) ifcolor(white)))by(accomhh)subtitle(, fcolor(white) lcolor(white) bmargin(zero))
		graph export "density`x'accom.png", replace
 } 
** Create set of binary variables for tenure and accomodation types to run Kolmogorov-Smirnov test  and  check equalities of distribution 
tabulate (tenex), gen (tenure_dummy) 
local varlist Satis Happy Worth Anxious
local dum_vars tenure_dummy1 tenure_dummy2 tenure_dummy3 tenure_dummy4 tenure_dummy5 
foreach x of local varlist {
    foreach v of local dum_vars {
        ksmirnov `x', by(`v')
	}
}
tabulate (accomhh), gen (accomhh_dummy) 
local varlist Satis Happy Worth Anxious
local dum_vars accomhh_dummy1 accomhh_dummy2 accomhh_dummy3 accomhh_dummy4 accomhh_dummy5 accomhh_dummy6 
foreach x of local varlist {
    foreach v of local dum_vars {
        ksmirnov `x', by(`v')
	}
}
**calculate mean wellbeing  by type of tenure 
local wellbeing Satis Happy Worth Anxious
foreach x of local wellbeing{
			 mean `x' [aweight = aagfh13] if HRP==persno, over (tenex)
} 
 
**create graphs mean wellbeign variables by type of tenures
local wellbeing Satis Happy Worth Anxious
foreach x of local wellbeing  {
		graph bar (mean) `x' [aweight =aagfh13 ]if HRP==persno, over(tenex) over(year)  graphregion(color(white))ytitle(Mean `x', size(msmall)) ///
		asyvars bar (1, color("58 183 185")) bar(2, color("0 52 66")) bar (3, color("247 147 33"))legend(size(small))
		graph export "mean`x'tenure.png", replace
 } 
**calculate mean wellbeing  by type of accomodations
local wellbeing Satis Happy Worth Anxious
foreach x of local wellbeing{
			 mean `x' [aweight = aagfh13] if HRP==persno, over (accomhh)
}

**create graphs mean wellbeing variables by accomodation types
local wellbeing Satis Happy Worth Anxious
foreach x of local wellbeing  {
		graph bar (mean) `x' [aweight =aagfh13 ]if accomhh!=6 & HRP==persno, over(accomhh) over(year)  graphregion(color(white))ytitle(Mean `x' , size(msmall)) ///
		asyvars bar (1, color("58 183 185")) bar(2, color("0 52 66")) bar (3, color("247 147 33"))legend(size(small))
		graph export "mean`x'accomodation.png", replace
}  
**create graphs mean wellbeing variables by number of rooms available to households
local wellbeing Satis Happy Worth Anxious
foreach x of local wellbeing  {
		graph bar (mean) `x' [aweight =aagfh13 ]if  HRP==persno, over(nrooms1a) over(year)  graphregion(color(white))ytitle(Mean `x' , size(msmall)) ///
		asyvars bar (1, color("58 183 185")) bar(2, color("0 52 66")) bar (3, color("247 147 33"))legend(size(small))
		graph export "mean`x'nrooms1.png", replace
}  

**graph satisfaction with accomodation and current tenure by tenure and accomodations type
graph bar (mean) HSatisrev [aweight =aagfh13 ]if  HRP==persno & accomhh!=6,  over(accomhh)  graphregion(color(white))ytitle(Mean Satisfaction with accomodation, size(msmall)) ///
asyvars bar (1, color("58 183 185")) bar(2, color("0 52 66"))bar (3, color("247 147 33"))legend(size(small))
graph bar (mean) Satten2rev [aweight =aagfh13 ]if  HRP==persno,  over(tenex)  graphregion(color(white))ytitle(Mean Satisfaction with tenure, size(msmall)) ///
asyvars bar (1, color("58 183 185")) bar(2, color("0 52 66"))bar (3, color("247 147 33"))legend(size(small))

*****************************************analysis over regions**********************************
**calculate mean wellbeing by regions and type of tenure
mean Satis [aweight = aagfh13] if HRP==persno, over(gorehs tenex)

** graphs overall wellbeing by regions 
preserve
collapse Satis Anxious Happy Worth [aweight= aagfh13] if HRP==persno , by  (tenex accomhh hhinc5x gorehs  year)
local varlist Satis Happy Worth Anxious
foreach x of local varlist {
    graph bar (mean) `x' , over(gorehs) graphregion(color(white)) asyvars ytitle(Mean `x' , size(msmall))bar (1, color("58 183 185")) bar(2, color("0 52 66"))bar (3, color("247 147 33"))
	graph export "mean`x'region.png", replace
}
** graphs wellbeing by regions and year 
local varlist Satis Happy Worth Anxious
foreach x of local varlist {
    graph bar (mean) `x' if  HRP==persno  ,  over(year) over(gorehs) graphregion(color(white)) asyvars bar (1, color("58 183 185")) bar(2, color("0 52 66"))bar (3, color("247 147 33"))
	graph export "mean`x'regionyear.png", replace
	}

** wellbeing by regions and tenure 
local varlist Satis Happy Worth Anxious
foreach x of local varlist {
    graph bar (mean) `x' if tenex>0 , over(tenex) over(gorehs) graphregion(color(white)) asyvars bar (1, color("58 183 185")) bar(2, color("0 52 66"))bar (3, color("247 147 33")) 
	graph export "mean`x'regiontenure.png", replace
	}

** graphs wellbeing by regions and accomodations type 
local varlist Satis Happy Worth Anxious
foreach x of local varlist {
    graph bar (mean) `x' if accomhh!=6  , over(accomhh) over(gorehs) graphregion(color(white)) asyvars bar (1, color("58 183 185")) bar(2, color("0 52 66"))bar (3, color("247 147 33")) 
    graph export "`x'regionaccom.png", replace 
   	}
** graphs wellbeing by regions and income quintiles 
local varlist Satis Happy Worth Anxious
foreach x of local varlist {
    graph bar (mean) `x' , over(hhinc5x) over(gorehs) graphregion(color(white)) asyvars bar (1, color("58 183 185")) bar(2, color("0 52 66"))bar (3, color("247 147 33"))
	graph export "`x'regionincome.png", replace 
}

**graphs satisfaction with accomodation and tenure by regions
graph bar (mean) HSatisrev [aweight =aagfh13 ]if  HRP==persno ,  over(gorehs)  graphregion(color(white))ytitle(Mean Satisfaction with accomodation, size(msmall)) ///
asyvars bar (1, color("58 183 185")) bar(2, color("0 52 66"))bar (3, color("247 147 33"))legend(size(small))
graph bar (mean) Satten2rev [aweight =aagfh13 ]if  HRP==persno,  over(gorehs)  graphregion(color(white))ytitle(Mean Satisfaction with tenure, size(msmall)) ///
asyvars bar (1, color("58 183 185")) bar(2, color("0 52 66"))bar (3, color("247 147 33"))legend(size(small))

graph bar (mean) HAS44rev [aweight =aagfh13 ]if  HRP==persno ,  over(gorehs)  graphregion(color(white))ytitle(Mean Satisfaction with area, size(msmall)) ///
asyvars bar (1, color("58 183 185")) bar(2, color("0 52 66"))bar (3, color("247 147 33"))legend(size(small))

graph bar (mean) LldSatrev [aweight =aagfh13 ]if  HRP==persno ,  over(gorehs)  graphregion(color(white))ytitle(Mean Satisfaction with repair/mainteinance, size(msmall)) ///
asyvars bar (1, color("58 183 185")) bar(2, color("0 52 66"))bar (3, color("247 147 33"))legend(size(small))



