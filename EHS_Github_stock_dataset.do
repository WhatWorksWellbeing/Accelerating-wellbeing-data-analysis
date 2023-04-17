***********************************************************************************************************************************
*************************************************What Works Centre for Wellbeing ***************************************************
*********************************Analysi on Housing and wellbeing using   English Housing survey 2013-2020 *************************
***************************************************Simona Tenaglia - March 2023*************************************************
************************************************************************************************************************************
**Version 1.0  - Stata 14

**This do file run an analysis using  English Housing Survey (EHS) data. We use the Special License data, released through the UK Data Service.
**The analysis look at wellbeing variables by sociodemographic, households and housing characteristics.
**In this file we use EHS Housing Stock Dataset for years 2014-2016-2018, as the EHS Housing Stock data are released every two years. 
**We do the following steps:
**1. for each year (2014-2016-2018) we merge the relevant datasets and save them 
**2. we append the three datasets
**3. we prepare the variables relevant for the analysis
**4. we describe some housing characteristics over time and by some sociodemographic characteristics
**5. we run a regression analysis with the Housing Stock dataset using the wellbeing variables as dependent variables and some personal, households
** and housing characteristics as explanatory variables.

**********************************************************The English Housing Survey (EHS) data******************************************************
**The EHS consist of 2 main elements: an initial interview survey of around 13,300 households with a follow up physical inspection of a sub-sample 
**of approximately 6,200 of these dwellings, including vacant dwellings.
**These data are available through the UK Data Service and are released under a Special Licence or an End User Licence. The datasets released under the 
**different licences vary by the level of detail of the data.
            
                                                                         **
**In the EHS there are two separate datasets as follows:                                                                                           
** a) EHS Household Dataset – The dataset comprises the full sample primary ‘raw’ interview survey data (available via the Special Licence only),  
** plus associated derived variables (available via both the End User Licence and the Special Licence) for all  cases where an interview has been  
**completed - approximately 13,300 households per annum. Datasets are provided for single financial years together with annual weights. This dataset
**should be used for any analysis where only information from the household interview is required.                                                 
                                                                                                                                                
**b) EHS Housing Stock Dataset – The Housing Stock Dataset should be used for any analysis requiring information relating to the 
**physical characteristics and energy efficiency of the housing stock. The dataset comprises the paired sample primary ‘raw’ interview 
**survey and physical survey data (available via the Special Licence only), plus associated derived variables (available via both the End 
**User Licence and the Special Licence) for all cases where a physical survey has been completed. Housing stock data on 
**occupied dwellings comprise of data from the household interview as well as data from the physical survey. For vacant properties,
**only data for the physical survey are provided. The housing stock data are made available for a two-year rolling sample with the appropriate 
**two-year weights.

***********************************************************Datasets used in this analysis**********************************************
** This file describe the analysis run on EHS Housing Stock Dataset. In  the file EHS_Github_Household we describe the analysis run on the EHS Household Dataset
**We use the following raw and derived dataset for all the years 2014, 2016, 2018:
**RAW DATA
** identity_sl_protect.dta 
**disability_sl_protect.dta
**people_sl_protect.dta
****owner_sl_protect.dta
**renter_sl_protect.dta
**attitudes_sl_protect.dta

**DERIVED DATA
**general_XXplusXX_sl_protect.dta (XX stands for the year)
**interview_XXplusXX_sl_protect.dta
**physical_XXplusXX_sl_protect.dta

****************************************************************Merging datsets*********************************************************
**In the following lines we are going to:
**1. create a unique dataset for each year 2014-2016-2018 merging raw and derived datasets containing variables relevant for the analysis and save them separately. 
**2. append the datasets created
**3. create unique variable for weights

**1.create one dataset for 2014, 2016 and 2018 merging relevant raw and derived datasets 

**2013-2014 (mid point 2014- comprising 2013-2014 and 2014-2015 unweighted cases dwellings 12,297  unweighted cases households 11,851)
use "E:\WWCLAV\Housing\ONS data\2014-2015\to use stock\8068stata_60D939B4547ADB6CD561C1A9D06A2DB0_V1(1)\UKDA-8068-stata\stata\stata11\derived\general_13plus14_sl_protect.dta", clear
merge 1:1 aacode using "E:\WWCLAV\Housing\ONS data\2014-2015\to use stock\8068stata_60D939B4547ADB6CD561C1A9D06A2DB0_V1(1)\UKDA-8068-stata\stata\stata11\derived\interview_13plus14_sl_protect.dta"
drop _merge
merge 1:1 aacode using "E:\WWCLAV\Housing\ONS data\2014-2015\to use stock\8068stata_60D939B4547ADB6CD561C1A9D06A2DB0_V1(1)\UKDA-8068-stata\stata\stata11\derived\physical_13plus14_sl_protect.dta"
drop _merge
merge m:m aacode using "E:\WWCLAV\Housing\ONS data\2014-2015\to use stock\8068stata_60D939B4547ADB6CD561C1A9D06A2DB0_V1(1)\UKDA-8068-stata\stata\stata11\interview\identity_sl_protect.dta"
drop _merge
merge m:m aacode using "E:\WWCLAV\Housing\ONS data\2014-2015\to use stock\8068stata_60D939B4547ADB6CD561C1A9D06A2DB0_V1(1)\UKDA-8068-stata\stata\stata11\interview\people_sl_protect.dta"
drop _merge
merge m:m aacode using "E:\WWCLAV\Housing\ONS data\2014-2015\to use stock\8068stata_60D939B4547ADB6CD561C1A9D06A2DB0_V1(1)\UKDA-8068-stata\stata\stata11\interview\disability_sl_protect.dta"
drop _merge
merge m:m aacode using "E:\WWCLAV\Housing\ONS data\2014-2015\to use stock\8068stata_60D939B4547ADB6CD561C1A9D06A2DB0_V1(1)\UKDA-8068-stata\stata\stata11\interview\owner_sl_protect.dta"
drop _merge
merge m:m aacode using "E:\WWCLAV\Housing\ONS data\2014-2015\to use stock\8068stata_60D939B4547ADB6CD561C1A9D06A2DB0_V1(1)\UKDA-8068-stata\stata\stata11\interview\renter_sl_protect.dta"
drop _merge
merge m:m aacode using "E:\WWCLAV\Housing\ONS data\2014-2015\to use stock\8068stata_60D939B4547ADB6CD561C1A9D06A2DB0_V1(1)\UKDA-8068-stata\stata\stata11\interview\attitudes_sl_protect.dta"
gen year = 2014
save "E:\WWCLAV\Housing\ONS data\merged data stock\2014.dta", replace

**2016-2017 (EHS2016 annual data contents2015-16 and 2016-17  Mid point April 2016 unweighted cases dwellings 12,292 unweighted cases households 11,924 )
use "E:\WWCLAV\Housing\ONS data\2016-2017\stock\8387stata_5EBB54373BFB27196FA0C7343294BDDC_V1\UKDA-8387-stata\stata\stata11\derived\general15plus16_sl_protect.dta", clear
merge 1:1 serialanon using "E:\WWCLAV\Housing\ONS data\2016-2017\stock\8387stata_5EBB54373BFB27196FA0C7343294BDDC_V1\UKDA-8387-stata\stata\stata11\derived\interview_15plus16_sl_protect.dta"
drop _merge
merge 1:1 serialanon using "E:\WWCLAV\Housing\ONS data\2016-2017\stock\8387stata_5EBB54373BFB27196FA0C7343294BDDC_V1\UKDA-8387-stata\stata\stata11\derived\physical_15plus16_sl_protect.dta"
drop _merge
merge m:m serialanon using "E:\WWCLAV\Housing\ONS data\2016-2017\stock\8387stata_5EBB54373BFB27196FA0C7343294BDDC_V1\UKDA-8387-stata\stata\stata11\interview\identity_1516_sl_protect.dta"
drop _merge
merge m:m serialanon using "E:\WWCLAV\Housing\ONS data\2016-2017\stock\8387stata_5EBB54373BFB27196FA0C7343294BDDC_V1\UKDA-8387-stata\stata\stata11\interview\people_1516_sl_protect.dta"
drop _merge
merge m:m serialanon using "E:\WWCLAV\Housing\ONS data\2016-2017\stock\8387stata_5EBB54373BFB27196FA0C7343294BDDC_V1\UKDA-8387-stata\stata\stata11\interview\disability_1516_sl_protect.dta"
drop _merge
merge m:m serialanon using "E:\WWCLAV\Housing\ONS data\2016-2017\stock\8387stata_5EBB54373BFB27196FA0C7343294BDDC_V1\UKDA-8387-stata\stata\stata11\interview\owner_1516_sl_protect.dta"
drop _merge
merge m:m serialanon using "E:\WWCLAV\Housing\ONS data\2016-2017\stock\8387stata_5EBB54373BFB27196FA0C7343294BDDC_V1\UKDA-8387-stata\stata\stata11\interview\renter_1516_sl_protect.dta"
drop _merge
merge m:m serialanon using "E:\WWCLAV\Housing\ONS data\2016-2017\stock\8387stata_5EBB54373BFB27196FA0C7343294BDDC_V1\UKDA-8387-stata\stata\stata11\interview\attitudes_1516_sl_protect.dta"
gen year = 2016
save "E:\WWCLAV\Housing\ONS data\merged data stock\2016.dta", replace

**2018-2019  (EHS 2018 annual data contents  2017-18 and 2018-19 Mid point April 2018 unweighted cases dwellings 12,562 unweighted cases households 12,203)
use "E:\WWCLAV\Housing\ONS data\2018-2019\stock\8851stata_7BE20AE838D2924FC2D770C021FB9B0B1268D2F54B6D00ACF262043325AE782F_V1\UKDA-8851-stata\stata\stata13\derived\general17plus18_sl_protect.dta", clear
merge 1:1 serialanon using "E:\WWCLAV\Housing\ONS data\2018-2019\stock\8851stata_7BE20AE838D2924FC2D770C021FB9B0B1268D2F54B6D00ACF262043325AE782F_V1\UKDA-8851-stata\stata\stata13\derived\interview_17plus18_sl_protect.dta"
drop _merge
merge m:m serialanon using "E:\WWCLAV\Housing\ONS data\2018-2019\stock\8851stata_7BE20AE838D2924FC2D770C021FB9B0B1268D2F54B6D00ACF262043325AE782F_V1\UKDA-8851-stata\stata\stata13\derived\physical_17plus18_sl_protect.dta"
drop _merge
merge m:m serialanon using "E:\WWCLAV\Housing\ONS data\2018-2019\stock\8851stata_7BE20AE838D2924FC2D770C021FB9B0B1268D2F54B6D00ACF262043325AE782F_V1\UKDA-8851-stata\stata\stata13\interview\identity_1718_sl_protect.dta"
drop _merge
merge m:m serialanon using "E:\WWCLAV\Housing\ONS data\2018-2019\stock\8851stata_7BE20AE838D2924FC2D770C021FB9B0B1268D2F54B6D00ACF262043325AE782F_V1\UKDA-8851-stata\stata\stata13\interview\people_1718_sl_protect.dta"
drop _merge
merge m:m serialanon using "E:\WWCLAV\Housing\ONS data\2018-2019\stock\8851stata_7BE20AE838D2924FC2D770C021FB9B0B1268D2F54B6D00ACF262043325AE782F_V1\UKDA-8851-stata\stata\stata13\interview\disability_1718_sl_protect.dta"
drop _merge
merge m:m serialanon using "E:\WWCLAV\Housing\ONS data\2018-2019\stock\8851stata_7BE20AE838D2924FC2D770C021FB9B0B1268D2F54B6D00ACF262043325AE782F_V1\UKDA-8851-stata\stata\stata13\interview\owner_1718_sl_protect.dta"
drop _merge
merge m:m serialanon using "E:\WWCLAV\Housing\ONS data\2018-2019\stock\8851stata_7BE20AE838D2924FC2D770C021FB9B0B1268D2F54B6D00ACF262043325AE782F_V1\UKDA-8851-stata\stata\stata13\interview\renter_1718_sl_protect.dta"
drop _merge
merge m:m serialanon using "E:\WWCLAV\Housing\ONS data\2018-2019\stock\8851stata_7BE20AE838D2924FC2D770C021FB9B0B1268D2F54B6D00ACF262043325AE782F_V1\UKDA-8851-stata\stata\stata13\interview\attitudes_1718_sl_protect.dta"
gen year = 2018
save "E:\WWCLAV\Housing\ONS data\merged data stock\2018.dta", replace

**2. Append the datasets created  
clear
use "E:\WWCLAV\Housing\ONS data\merged data stock\2014.dta", clear
append using "E:\WWCLAV\Housing\ONS data\merged data stock\2016.dta"
append using "E:\WWCLAV\Housing\ONS data\merged data stock\2018.dta"

**3.create unique variable for household and dwellings weights  
replace aagph1314=aagph1516 if year==2016
replace aagph1314=aagph1718 if year==2018
  
replace aagpd1314=aagpd1516 if year==2016
replace aagpd1314=aagpd1718 if year==2018

**create unique variable for regions gorehs for year 2014. 2016, 2018
replace gorehs= gorEHS if year==2014
replace gorehs= gorEHS if year==2016

** rename wellbeing and sociodemographic variables.

rename QSatis Satis						/// this variable is referred to the household referent person

rename QHappy Happy                     /// this variable is referred to the household referent person

rename QWorth Worth                     /// this variable is referred to the household referent person

rename QAnxious Anxious                  /// this variable is referred to the household referent person


rename emphrpx emp                      /// this variable is referred to the household referent person


**replace negative values with missing 
local demo Satis Happy Worth Anxious xMarSta2 HiQual QHealth1 hhltsick ArrPR1 MrgArr
foreach x of local demo {
 replace `x'=. if `x'<0 
 }
**create new variable age of HRP squared
 gen  agehrpx2= agehrpx^2
**create variable female from variable sex of HRP (female=1 and male =0)
gen female = sexhrp==2
replace female=. if sexhrp==.
**create variable for dependent child=0 or dependent child>0
gen depchild=.
replace depchild=0 if ndepchild==0
replace depchild=1 if ndepchild>0
**create label for presence dependent child
label define depchild_l 1 "1 or more dependent child" 0 "0 dependent child" 
label values depchild depchild_l  
** create unique variable for bedroom standard
replace lhastdx= LHAstdx if year==2014
**create  variable for overcrowded houses (1 = overcrowded (1 or 2 or more rommes below bedroom standards), 0=not overcrowded (at standard or 1 or 2 or more rooms above standard))
gen overcrowded=lhastdx<=2
replace overcrowded=. if lhastdx==.
** generate dummy for decent home (1 = 0 criteria failed (decent), 0 = 1 to four criteria failed (not decent)- (26 hazards model) 
gen decent_d = dhnumz==0
replace decent_d = . if dhnumz==.
#delimit ;
label define decent_d_label
		0		"not decent"
		1		"decent";
#delimit cr
label values decent_d decent_d_label

**generate age squared for regression 
gen  agehrpxsq= agehrpx^2

** generate variable for repair costs in band and its labels
gen cststdcx_band=.
replace cststdcx_band=1 if  cststdcx<50
replace cststdcx_band=2 if  (cststdcx>=50) & (cststdcx<100)
replace cststdcx_band=3 if  (cststdcx>=100) & (cststdcx<150)
replace cststdcx_band=4 if  (cststdcx>=150) & (cststdcx<200)
replace cststdcx_band=5 if  (cststdcx>=200) & (cststdcx<250)
replace cststdcx_band=6 if  (cststdcx>=250) & (cststdcx<300)
replace cststdcx_band=7 if  (cststdcx>=300) & (cststdcx<.)

#delimit ;
label define cststdcx_band_label
		1		"from 0 to 50£"
		2		"from 50 to 100£"
		3       "from 100 to 150£"
		4 		"from 150 to 200£"
		5		"from 200 to 250£"
		6       "from 250 to 300£"
		7		"more than 300£";
		
#delimit cr
label values cststdcx_band cststdcx_band_label

*mean Anxious if Anxious>=0 [aweight = aagph1718], over(dhomesy)
**check for Satisfaction frequencies
by year, sort : tabulate Satis [iweight = aagph1314] if Satis>=0 & HRP==persno
tab  Satis  [iweight= aagph1314] if Satis>=0 & HRP==persno
tab  Anxious  [iweight= aagph1314] if Anxious>=0 & HRP==persno
tab  Satis  [iweight= aagph1516] if Satis>=0 & HRP==persno
tab  Satis  [iweight= aagph1718] if Satis>=0 & HRP==persno
**********************Descriptives on housing characteristics***********************
**tables for damp problems for three years
local damp dampcdf damppnf damprsf
forvalues t = 2014/2018 {
		foreach v of local damp {
        tab `v' [iweight= aagpd1314] if year == `t'& HRP==persno
	}
}
local damp dampcdf damppnf damprsf
local wellbeing Satis Happy Worth Anxious
foreach x of local damp {
		foreach v of local wellbeing {
	     pwcorr `v' `x' , star(.05)
	}
 }
**graphs for damp problems over time using "2019-20_EHS_Headline_Report_Section_2_Stock_Annex_Tables(16).xlsx" downloadable from 
**https://www.gov.uk/government/statistics/english-housing-survey-2019-to-2020-headline-report
import excel "2019-20_EHS_Headline_Report_Section_2_Stock_Annex_Tables(18).xlsx", sheet("AT2.5") cellrange(B5:F24) firstrow clear
rename B Year
rename C risingDamp
rename D penetratingDamp
rename E condensationmould
rename F anyDamp
destring Year, replace 
twoway (line risingDamp Year) (line penetratingDamp Year) (line anyDamp Year) (line condensationmould Year), graphregion(color(white)) ytitle( % damp problems )

************************************************Regression analysis************************************************
**Regression analysis for each ONS4 wellbeing question using sociodemographic, households and housing variables. we gradually introduce all the explanatory variables

**regressions with life satisfaction as dependent variable
local predictors  agehrpx agehrpxsq female  i.ethhrp4x ib2.xMarSta2 i.emp  i.hhinc5x i.hhtype7 i.QHealth1 ib2.hhltsick   ib8.gorehs ib2.tenex ib4.dwtypenx overcrowded i.floor5x   decent_d  damprsf i.cststdcx_band
local regressors

foreach p of local predictors {
    local regressors `regressors' `p'
    regress Satis `regressors' if HRP==persno
  outreg2 using tables_mregrevSatis1.docx, excel  label  
}
**regressions with  happiness as dependent variable
local predictors  agehrpx agehrpxsq female  i.ethhrp4x ib2.xMarSta2 i.emp  i.hhinc5x i.hhtype7 i.QHealth1 ib2.hhltsick   ib8.gorehs ib2.tenex ib4.dwtypenx overcrowded i.floor5x   decent_d  damprsf i.cststdcx_band
local regressors

foreach p of local predictors {
    local regressors `regressors' `p'
    regress Happy `regressors' if HRP==persno
  outreg2 using tables_mregrevHappy1.docx, excel  label  
}
**regressions with worthwhile as dependent variable
local predictors  agehrpx agehrpxsq female  i.ethhrp4x ib2.xMarSta2 i.emp  i.hhinc5x i.hhtype7 i.QHealth1 ib2.hhltsick   ib8.gorehs ib2.tenex ib4.dwtypenx overcrowded i.floor5x   decent_d  damprsf i.cststdcx_band
local regressors

foreach p of local predictors {
    local regressors `regressors' `p'
    regress Worth `regressors' if HRP==persno
  outreg2 using tables_mregrevWorth1.docx, excel  label  
}

**regressions with anxiety as dependent variable
local predictors  agehrpx agehrpxsq female  i.ethhrp4x ib2.xMarSta2 i.emp  i.hhinc5x i.hhtype7 i.QHealth1 ib2.hhltsick   ib8.gorehs ib2.tenex ib4.dwtypenx overcrowded i.floor5x   decent_d  damprsf i.cststdcx_band
local regressors

foreach p of local predictors {
    local regressors `regressors' `p'
    regress Anxious `regressors' if HRP==persno
  outreg2 using tables_mregrevAnxious1.docx, excel  label  
}

** Regressions with life satisfaction, happiness, worthwhile and anxiety as dependent variables and with all regressors for saving results in one table
foreach x of varlist Satis Happy Worth Anxious{
reg `x' agehrpx agehrpxsq female  i.ethhrp4x ib2.xMarSta2 i.emp  i.hhinc5x i.hhtype7 i.QHealth1 ib2.hhltsick   ib8.gorehs ib2.tenex ib4.dwtypenx overcrowded i.floor5x   decent_d  damprsf i.cststdcx_band if HRP==persno, vce(robust)
outreg2 using tables_mreg4wbvariable1.docx, excel  label
}
** calculte summary statistics for table A1 of  technical report's Appendix 
sum Satis Happy Worth Anxious agehrpx agehrpx2 female ethhrp4x xMarSta2 emp hhinc5x hhtype7 QHealth1 hhltsick tenex gorehs overcrowded floor5x dwtypenx decent_d damprsf  if HRP==persno

