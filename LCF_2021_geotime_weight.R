rm(list = ls()) #Clear memory



#Loading packages
packages <- c("readr", "dplyr", "tidyr", "stringr", "haven", "ggplot2", "broom", "plyr", "spatstat", 
              "openxlsx", "epade", "Weighted.Desc.Stat")
for(i in 1:length(packages)){
  eval(bquote(if(!require(.(packages[i]))){install.packages(.(packages[i]), dependencies = T)}))
  library(packages[i], character.only = T)
}

#package "dplyr" used for data manipulation
#package "tidyr" used for data transformation
#package "stringr" used for string manipulation
#package "haven" is used for read_dta() function
#package "ggplot2" for graphing
#package "broom" for statistical outputs
#package "plyr" for data splitting
#package "spatstat" need for calculating the weighted median and quartile
#package "epade" used to calculate weighted skewness
#package "Weighted.Desc.Stat" used to calculate weighted excess kurtosis

#Need to include custom weighted functions. The stats weighted functions can't handle NA weights. Set the working directory
#to where you have placed the weighted_mean.R file below.
setwd("/Users/benjaminosullivan/Library/CloudStorage/GoogleDrive-osullivan.benjamin1@gmail.com/My Drive/What Works/LCF")
source("weighted_mean.R")


################################################################################
############### IMPORTANT MESSAGE ##############################################
################################################################################

# The location of the dta files is important to successfully run the script.
# IN YOUR DESIGNATED WORKING DIRECTORY THERE MUST BE FOLDERS WITH NAMES:
# "2014", "2015/16", "2016/17", "2017/18", "2018/19", "2019/20"
# AND THE .dta FILES MUST BE COPIED INTO THE RESPECTIVE FOLDER CORRESPONDING TO THEIR YEAR

# NOTE: the .dta files must be the raw personal dataset as well as the derived variable
# household dataset for each year

#EXTRA NOTE: Probably convenient to place the weighted_mean.R file in the same location
#as the folders where all the dta files are stored in

################################################################################
################################################################################
################################################################################

# These vectors are used to read in the .dta files for each year
#NOTE:Well-being variables only exist for years 2014 to 2019/20
wdvec <- c()
years <- c("2014", "2015:16", "2016:17", "2017:18", "2018:19", "2019:20")
lcfdta <- c("2014_rawper_ukanon.dta", "2015-16_rawper_ukanon.dta", "2016_17_rawper_ukanon.dta",
            "rawper_ukanon_2017-18.dta", "2018_rawper_ukanon_final.dta", "lcfs_2019_rawper_ukanon_final.dta")
lcfhousedta <- c("2014_rawhh_ukanon.dta", "2015-16_rawhh_ukanon.dta", "2016_17_rawhh_ukanon.dta", 
                 "rawhh_ukanon_2017-18.dta", "2018_rawhh_ukanon.dta", "lcfs_2019_rawhh_ukanon.dta")
lcfdvhousedta <- c("2014_dvhh_ukanon.dta", "2015-16_dvhh_ukanon.dta", "2016_17_dvhh_ukanon.dta",
                   "dvhh_ukanon_2017-18.dta", "2018_dvhh_ukanon.dta", "lcfs_2019_dvhh_ukanon.dta")
#Note: using the raw personal and household data, not the derived data

# PLEASE INPUT YOUR WORKING DIRECTORY CONTAINING THE FOLDERS "2014", "2015/16",
# "2016/17", "2017/18", "2018/19", "2019/20" BELOW, ASSIGNING IT TO THE "wd2021" OBJECT. 
wd2021 <- "/Users/benjaminosullivan/Library/CloudStorage/GoogleDrive-osullivan.benjamin1@gmail.com/My Drive/What Works/LCF"
wd2021 <- paste0(wd2021, "/2020:21")

# This is the function that will iteratively read each dta file by year and compute the
# statistics, storing them into one list of lists
#WARNING: this function is long!
lcftime <- function(yearvec, dtavec, dvhhdtavec, wd){
  
  #These are all the lists which will contain the relevant data, e.g. income list
  #will contain all the sample moments for the income variable etc.
  summarylist <- sapply(years, function(x) NULL)
  depchlist <- sapply(years, function(x) NULL)
  nodepchlist <- sapply(years, function(x) NULL)
  mardepchlist <- sapply(years, function(x) NULL)
  sexmardepchlist <- sapply(years, function(x) NULL)
  agelist <- sapply(years, function(x) NULL)
  depchemploylist <- sapply(years, function(x) NULL)
  employlist <- sapply(years, function(x) NULL)
  chbenlist <- sapply(years, function(x) NULL)
  incomelist <- sapply(years, function(x) NULL)
  ethlist <- sapply(years, function(x) NULL)
  sexlist <- sapply(years, function(x) NULL)
  corrlist <- sapply(years, function(x) NULL)
  marrlist <- sapply(years, function(x) NULL)
  inccorrlist <- sapply(years, function(x) NULL)
  regionlist <- sapply(years, function(x) NULL)
  
  #Now looping through each year
  for (i in 1:length(yearvec)){
    
    #Setting working directory
    wdsub <- gsub("2020:21", yearvec[i], wd)
    setwd(wdsub)
    
    #Reading the raw personal dta file
    lcfdata <- read_dta(dtavec[i], encoding = "Latin1")
    
    colnames(lcfdata) <- toupper(colnames(lcfdata))
    
    #Reading the derived variable household file
    lcfdvhhdata <- read_dta(dvhhdtavec[i], encoding = "Latin1")
    
    colnames(lcfdvhhdata) <- toupper(colnames(lcfdvhhdata))
    
    #Certain dta files have a dedicated well-being weight variable for that year
    #but some dta files need to use the household weights instead
    #NOTE: the LCF household weights are integrated to be used as personal weights as well.
    if(is.null(lcfdata$WEIGHTWB)){
      
      #lcfmerge <- merge(lcfdata%>%filter(HRPID == 1), lcfdvhhdata%>%dplyr::select(CASE, WEIGHTA), by = "CASE")
      lcfmerge <- merge(lcfdata, lcfdvhhdata%>%dplyr::select(CASE, WEIGHTA, GORX), by = "CASE")
      
      lcfmerge$addedweight <- lcfmerge$WEIGHTA
  
    }else{
      
      lcfmerge <- merge(lcfdata, lcfdvhhdata%>%dplyr::select(CASE, WEIGHTA, GORX), by = "CASE", all.x = TRUE)
      
      lcfmerge$addedweight <- lcfmerge$WEIGHTWB
      
      print(paste(yearvec[i], "has weightwb"))
      
    }
    

    #This is the main data frame that will be manipulated to compute the sample statistics
    lcfdf <- lcfmerge%>%
      filter(SATIS>=0 | WORTH>=0 | HAPPY>=0 | ANXIOUS>=0)%>%
      dplyr::mutate(SATISnew = case_when(SATIS == -8 ~ NA_integer_, SATIS == -9 ~ NA_integer_, SATIS == 98 ~ NA_integer_, SATIS == 99 ~ NA_integer_, TRUE ~ as.integer(SATIS)),
                    WORTHnew = case_when(WORTH == -8 ~ NA_integer_, WORTH == -9 ~ NA_integer_, WORTH == 98 ~ NA_integer_, WORTH == 99 ~ NA_integer_, TRUE ~ as.integer(WORTH)),
                    HAPPYnew = case_when(HAPPY == -8 ~ NA_integer_, HAPPY == -9 ~ NA_integer_, HAPPY == 98 ~ NA_integer_, HAPPY == 99 ~ NA_integer_, TRUE ~ as.integer(HAPPY)),
                    ANXIOUSnew = case_when(ANXIOUS == -8 ~ NA_integer_, ANXIOUS == -9 ~ NA_integer_, ANXIOUS == 98 ~ NA_integer_, ANXIOUS == 99 ~ NA_integer_, TRUE ~ as.integer(ANXIOUS)))%>%
      dplyr::mutate(SEXnew = case_when(SEX == -9 ~ NA_character_, SEX == -8 ~ NA_character_, SEX == 1 ~ "Male", SEX == 2 ~ "Female", TRUE ~ NA_character_)) 
    
    ############################################################################
    ###### SAMPLE STATISTICS OF THE ENTIRE HOUSEHOLD SAMPLE FOR THE YEAR #######
    ############################################################################
    
    summarylist[[i]] <- lcfdf%>%
      dplyr::select(SATISnew, WORTHnew, HAPPYnew, ANXIOUSnew, addedweight)%>%
      dplyr::rename(Satis = SATISnew, Happy = HAPPYnew, Worth = WORTHnew, Anxious = ANXIOUSnew)%>%
      tidyr::pivot_longer(c(Satis, Worth, Happy, Anxious), names_to = "Wellbeing", values_to = "Score")%>%
      dplyr::group_by(Wellbeing)%>%
      dplyr::summarise(weighted_mean = weighted_mean(Score, addedweight, na.rm = T),
                       min = base::min(Score, na.rm = T),
                       weighted_q1 = weighted.quantile(Score, addedweight, probs = 0.25, na.rm = T, type = 2),
                       weighted_median = weighted.median(Score, addedweight, na.rm = T, type = 2),
                       weighted_q3 = weighted.quantile(Score, addedweight, probs = 0.75, na.rm = T, type = 2),
                       max = base::max(Score, na.rm = T),
                       weighted_variance = weighted.var(Score, addedweight, na.rm = T),
                       weighted_skewness = weighted_skewness(Score, addedweight, na.rm = T),
                       kurtosis = weighted_kurtosis(Score, addedweight, na.rm = T),
                       wellbeingcount = sum(!(is.na(Score))))
    
    ############################################################################
    #################### CORRELATION MATRIX OF ENTIRE SAMPLE ###################
    ############################################################################
    
    #Creating correlation matrices for the well-being variables of the sample
    lcfcorr <- lcfdf%>%
      dplyr::select(SATISnew, WORTHnew, HAPPYnew, ANXIOUSnew, addedweight)%>%
      na.omit%>%
      mutate(corrweight = addedweight)%>%
      dplyr::rename(Satisfaction = SATISnew, Worthwhile = WORTHnew, Happiness = HAPPYnew, Anxiousness = ANXIOUSnew)
    
    corrlist[[i]] <- cov.wt(lcfcorr[,!(colnames(lcfcorr) %in% c("addedweight", "corrweight"))],
                            wt = lcfcorr$corrweight,
                            cor = TRUE)
    
    ############################################################################
    #################### Age ###################################################
    ############################################################################
    
    # Creating a data frame of the sample statistics of the well-being variables by age bands
    agelist[[i]] <- lcfdf%>%
      dplyr::select(SATISnew, WORTHnew, HAPPYnew, ANXIOUSnew, DVAGE18, addedweight)%>%
      dplyr::rename(Satis = SATISnew, Happy = HAPPYnew, Worth = WORTHnew, Anxious = ANXIOUSnew)%>%
      dplyr::filter(DVAGE18 >= 4)%>%
      dplyr::mutate(`Age Bands` = case_when(DVAGE18 == 4 ~ "16 to 19",
                                            DVAGE18 == 5 ~ "20 to 24",
                                            DVAGE18 == 6 ~ "25 to 29",
                                            DVAGE18 == 7 ~ "30 to 34",
                                            DVAGE18 == 8 ~ "35 to 39",
                                            DVAGE18 == 9 ~ "40 to 44",
                                            DVAGE18 == 10 ~ "45 to 49",
                                            DVAGE18 == 11 ~ "50 to 54",
                                            DVAGE18 == 12 ~ "55 to 59",
                                            DVAGE18 == 13 ~ "60 to 64",
                                            DVAGE18 == 14 ~ "65 to 69",
                                            DVAGE18 == 15 ~ "70 to 74",
                                            DVAGE18 == 16 ~ "75 to 79",
                                            DVAGE18 == 17 ~ "80 to 84",
                                            DVAGE18 == 18 ~ "85 and over",
                                            TRUE ~ NA_character_))%>%
      dplyr::group_by(`Age Bands`)%>%
      dplyr::summarise(dplyr::across(c('Satis', 'Worth', 'Happy', 'Anxious'), list(weighted_mean = ~ weighted_mean(.x, addedweight, na.rm = T),
                                                                                   min = ~ base::min(.x, na.rm = T),
                                                                                   weighted_q1 = ~ weighted.quantile(.x, addedweight, probs = 0.25, na.rm = T, type = 2),
                                                                                   weighted_median = ~ weighted.median(.x, addedweight, na.rm = T, type = 2),
                                                                                   weighted_q3 = ~ weighted.quantile(.x, addedweight, probs = 0.75, na.rm = T, type = 2),
                                                                                   max = ~ base::max(.x, na.rm = T),
                                                                                   weighted_variance = ~ weighted.var(.x, addedweight, na.rm = T),
                                                                                   weighted_skewness = ~ weighted_skewness(.x, addedweight, na.rm = T),
                                                                                   kurtosis = ~ weighted_kurtosis(.x, addedweight, na.rm = T),
                                                                                   wellbeingcount = ~ sum(!(is.na(.x)))), .names = "{.col}.{.fn}"),
                       unweighted_count = n(),
                       weighted_count = sum(addedweight, na.rm = T))
    
    ############################################################################
    #################### Sex ###################################################
    ############################################################################
    
    # Creating a data frame of the sample statistics of the well-being variables by sex
    sexlist[[i]] <- lcfdf%>%
      dplyr::select(SATISnew, WORTHnew, HAPPYnew, ANXIOUSnew, SEXnew, addedweight)%>%
      dplyr::rename(Satis = SATISnew, Happy = HAPPYnew, Worth = WORTHnew, Anxious = ANXIOUSnew, Sex = SEXnew)%>%
      dplyr::group_by(Sex)%>%
      dplyr::summarise(dplyr::across(c('Satis', 'Worth', 'Happy', 'Anxious'), list(weighted_mean = ~ weighted_mean(.x, addedweight, na.rm = T),
                                                                                   min = ~ base::min(.x, na.rm = T),
                                                                                   weighted_q1 = ~ weighted.quantile(.x, addedweight, probs = 0.25, na.rm = T, type = 2),
                                                                                   weighted_median = ~ weighted.median(.x, addedweight, na.rm = T, type = 2),
                                                                                   weighted_q3 = ~ weighted.quantile(.x, addedweight, probs = 0.75, na.rm = T, type = 2),
                                                                                   max = ~ base::max(.x, na.rm = T),
                                                                                   weighted_variance = ~ weighted.var(.x, addedweight, na.rm = T),
                                                                                   weighted_skewness = ~ weighted_skewness(.x, addedweight, na.rm = T),
                                                                                   kurtosis = ~ weighted_kurtosis(.x, addedweight, na.rm = T),
                                                                                   wellbeingcount = ~ sum(!(is.na(.x)))), .names = "{.col}.{.fn}"),
                       unweighted_count = n(),
                       weighted_count = sum(addedweight, na.rm = T))
    
    ############################################################################
    ##################### REGIONS ##############################################
    ############################################################################
    
    # Creating a data frame of the sample statistics of the well-being variables by NUTS1 regions
    regionlist[[i]] <- lcfdf%>%
      dplyr::select(SATISnew, WORTHnew, HAPPYnew, ANXIOUSnew, GORX, addedweight)%>%
      dplyr::rename(Satis = SATISnew, Happy = HAPPYnew, Worth = WORTHnew, Anxious = ANXIOUSnew)%>%
      dplyr::mutate(Region = case_when(GORX == 1 ~ "North East",
                                       GORX == 2 ~ "North West & Merseyside",
                                       GORX == 3 ~ "Yorkshire and the Humber",
                                       GORX == 4 ~ "East Midlands",
                                       GORX == 5 ~ "West Midlands",
                                       GORX == 6 ~ "Eastern",
                                       GORX == 7 ~ "London",
                                       GORX == 8 ~ "South East",
                                       GORX == 9 ~ "South West",
                                       GORX == 10 ~ "Wales",
                                       GORX == 11 ~ "Scotland",
                                       GORX == 12 ~ "Northern Ireland",
                                       TRUE ~ NA_character_))%>%
      dplyr::group_by(Region)%>%
      dplyr::summarise(dplyr::across(c('Satis', 'Worth', 'Happy', 'Anxious'), list(weighted_mean = ~ weighted_mean(.x, addedweight, na.rm = T),
                                                                                   min = ~ base::min(.x, na.rm = T),
                                                                                   weighted_q1 = ~ weighted.quantile(.x, addedweight, probs = 0.25, na.rm = T, type = 2),
                                                                                   weighted_median = ~ weighted.median(.x, addedweight, na.rm = T, type = 2),
                                                                                   weighted_q3 = ~ weighted.quantile(.x, addedweight, probs = 0.75, na.rm = T, type = 2),
                                                                                   max = ~ base::max(.x, na.rm = T),
                                                                                   weighted_variance = ~ weighted.var(.x, addedweight, na.rm = T),
                                                                                   weighted_skewness = ~ weighted_skewness(.x, addedweight, na.rm = T),
                                                                                   kurtosis = ~ weighted_kurtosis(.x, addedweight, na.rm = T),
                                                                                   wellbeingcount = ~ sum(!(is.na(.x)))), .names = "{.col}.{.fn}"),
                       unweighted_count = n(),
                       weighted_count = sum(addedweight, na.rm = T))
    
    ############################################################################
    #################### Employment Status #####################################
    ############################################################################
    
    # Creating a data frame of the sample statistics of the well-being variables by employment status
    employlist[[i]] <- lcfdf%>%
      dplyr::select(SATISnew, WORTHnew, HAPPYnew, ANXIOUSnew, DVILO3A, addedweight)%>%
      dplyr::rename(Satis = SATISnew, Happy = HAPPYnew, Worth = WORTHnew, Anxious = ANXIOUSnew, `Employment Status` = DVILO3A)%>%
      dplyr::mutate(`Employment Status` = case_when(`Employment Status` == 1 ~ "In Employment", `Employment Status` == 2 ~ "ILO Unemployed",
                                                    `Employment Status` == 3 ~ "Economically Inactive", TRUE ~ NA_character_))%>%
      dplyr::group_by(`Employment Status`)%>%
      dplyr::summarise(dplyr::across(c('Satis', 'Worth', 'Happy', 'Anxious'), list(weighted_mean = ~ weighted_mean(.x, addedweight, na.rm = T),
                                                                                   min = ~ base::min(.x, na.rm = T),
                                                                                   weighted_q1 = ~ weighted.quantile(.x, addedweight, probs = 0.25, na.rm = T, type = 2),
                                                                                   weighted_median = ~ weighted.median(.x, addedweight, na.rm = T, type = 2),
                                                                                   weighted_q3 = ~ weighted.quantile(.x, addedweight, probs = 0.75, na.rm = T, type = 2),
                                                                                   max = ~ base::max(.x, na.rm = T),
                                                                                   weighted_variance = ~ weighted.var(.x, addedweight, na.rm = T),
                                                                                   weighted_skewness = ~ weighted_skewness(.x, addedweight, na.rm = T),
                                                                                   kurtosis = ~ weighted_kurtosis(.x, addedweight, na.rm = T),
                                                                                   wellbeingcount = ~ sum(!(is.na(.x)))), .names = "{.col}.{.fn}"),
                       unweighted_count = n(),
                       weighted_count = sum(addedweight, na.rm = T))
    
    ############################################################################
    #################### Child Benefit #########################################
    ############################################################################
    
    # Creating a data frame of the sample statistics of the well-being variables by child benefits received
    #Child Benefit is recorded as the last amount received for weekly child benefits.
    chbenlist[[i]] <- lcfdf%>%
      dplyr::select(SATISnew, WORTHnew, HAPPYnew, ANXIOUSnew, DVCB, addedweight)%>%
      dplyr::rename(Satis = SATISnew, Happy = HAPPYnew, Worth = WORTHnew, Anxious = ANXIOUSnew, `Child Benefit` = DVCB)%>%
      dplyr::mutate(`Child Benefit` = case_when(`Child Benefit` < 30 ~ "Less than £30",
                                                `Child Benefit`>=30 & `Child Benefit`<60 ~ "£30-£59.99",
                                                `Child Benefit`>=60 & `Child Benefit`<90 ~ "£60-£89.99",
                                                `Child Benefit`>=90 & `Child Benefit`<120 ~ "£90-£119.99",
                                                TRUE ~ NA_character_))%>%
      dplyr::group_by(`Child Benefit`)%>%
      dplyr::summarise(dplyr::across(c('Satis', 'Worth', 'Happy', 'Anxious'), list(weighted_mean = ~ weighted_mean(.x, addedweight, na.rm = T),
                                                                                   min = ~ base::min(.x, na.rm = T),
                                                                                   weighted_q1 = ~ weighted.quantile(.x, addedweight, probs = 0.25, na.rm = T, type = 2),
                                                                                   weighted_median = ~ weighted.median(.x, addedweight, na.rm = T, type = 2),
                                                                                   weighted_q3 = ~ weighted.quantile(.x, addedweight, probs = 0.75, na.rm = T, type = 2),
                                                                                   max = ~ base::max(.x, na.rm = T),
                                                                                   weighted_variance = ~ weighted.var(.x, addedweight, na.rm = T),
                                                                                   weighted_skewness = ~ weighted_skewness(.x, addedweight, na.rm = T),
                                                                                   kurtosis = ~ weighted_kurtosis(.x, addedweight, na.rm = T),
                                                                                   wellbeingcount = ~ sum(!(is.na(.x)))), .names = "{.col}.{.fn}"),
                       unweighted_count = n(),
                       weighted_count = sum(addedweight, na.rm = T))
    
    ############################################################################
    #################### Gross Weekly Income Bands #############################
    ############################################################################
    
    #Creating a data frame of the sample statistics of the well-being variables by income bands
    #Also creating a correlation matrix of well-being and income
    #Years 2014 to 2016/17 contain income variable "GWKINC" and later years contain income variable "GWKINCP"
    #hence, must create a loop to discriminate this
    
    if(yearvec[i] %in% c("2014", "2015:16", "2016:17")){
      
    incomelist[[i]] <- lcfdf%>%
      dplyr::select(SATISnew, WORTHnew, HAPPYnew, ANXIOUSnew, GWKINC, addedweight)%>%
      dplyr::rename(Satis = SATISnew, Happy = HAPPYnew, Worth = WORTHnew, Anxious = ANXIOUSnew)%>%
      dplyr::filter(!is.na(GWKINC) & GWKINC > 0 & GWKINC < 2500)%>%
      dplyr::mutate(`Gross Weekly Income` = case_when(
        GWKINC == -9 ~ NA_character_,
        GWKINC == -8 ~ NA_character_,
        GWKINC >=0 & GWKINC <200 ~ "0-199",
        GWKINC >=200 & GWKINC <400 ~ "200-399",
        GWKINC >=400 & GWKINC <600 ~ "400-599",
        GWKINC >=600 & GWKINC <800 ~ "600-799",
        GWKINC >=800 & GWKINC <1000 ~ "800-999",
        GWKINC >=1000 & GWKINC <1200 ~ "1000-1199",
        TRUE ~ "1200 or more"))%>%
      dplyr::group_by(`Gross Weekly Income`)%>%
      dplyr::summarise(dplyr::across(c('Satis', 'Worth', 'Happy', 'Anxious'), list(weighted_mean = ~ weighted_mean(.x, addedweight, na.rm = T),
                                                                                   min = ~ base::min(.x, na.rm = T),
                                                                                   weighted_q1 = ~ weighted.quantile(.x, addedweight, probs = 0.25, na.rm = T, type = 2),
                                                                                   weighted_median = ~ weighted.median(.x, addedweight, na.rm = T, type = 2),
                                                                                   weighted_q3 = ~ weighted.quantile(.x, addedweight, probs = 0.75, na.rm = T, type = 2),
                                                                                   max = ~ base::max(.x, na.rm = T),
                                                                                   weighted_variance = ~ weighted.var(.x, addedweight, na.rm = T),
                                                                                   weighted_skewness = ~ weighted_skewness(.x, addedweight, na.rm = T),
                                                                                   kurtosis = ~ weighted_kurtosis(.x, addedweight, na.rm = T),
                                                                                   wellbeingcount = ~ sum(!(is.na(.x)))), .names = "{.col}.{.fn}"),
                       unweighted_count = n(),
                       weighted_count = sum(addedweight, na.rm = T))
    
    ############################################################################
    #################### INCOME CORRELATION MATRIX #############################
    ############################################################################
    
    lcfinccorr <- lcfdf%>%
      dplyr::select(SATISnew, WORTHnew, HAPPYnew, ANXIOUSnew, addedweight, GWKINC)%>%
      na.omit%>%
      mutate(corrweight = addedweight)%>%
      dplyr::rename(Satisfaction = SATISnew, Worthwhile = WORTHnew, Happiness = HAPPYnew, Anxiousness = ANXIOUSnew, Income = GWKINC)
    
    inccorrlist[[i]] <- cov.wt(lcfinccorr[,!(colnames(lcfinccorr) %in% c("addedweight", "corrweight"))],
                            wt = lcfinccorr$corrweight,
                            cor = TRUE)
    }else{
      
      incomelist[[i]] <- lcfdf%>%
        dplyr::select(SATISnew, WORTHnew, HAPPYnew, ANXIOUSnew, GWKINCP, addedweight)%>%
        dplyr::rename(Satis = SATISnew, Happy = HAPPYnew, Worth = WORTHnew, Anxious = ANXIOUSnew)%>%
        dplyr::filter(!is.na(GWKINCP) & GWKINCP > 0 & GWKINCP < 2500)%>%
        dplyr::mutate(`Gross Weekly Income` = case_when(
          GWKINCP == -9 ~ NA_character_,
          GWKINCP == -8 ~ NA_character_,
          GWKINCP >=0 & GWKINCP <200 ~ "0-199",
          GWKINCP >=200 & GWKINCP <400 ~ "200-399",
          GWKINCP >=400 & GWKINCP <600 ~ "400-599",
          GWKINCP >=600 & GWKINCP <800 ~ "600-799",
          GWKINCP >=800 & GWKINCP <1000 ~ "800-999",
          GWKINCP >=1000 & GWKINCP <1200 ~ "1000-1199",
          TRUE ~ "1200 or more"))%>%
        dplyr::group_by(`Gross Weekly Income`)%>%
        dplyr::summarise(dplyr::across(c('Satis', 'Worth', 'Happy', 'Anxious'), list(weighted_mean = ~ weighted_mean(.x, addedweight, na.rm = T),
                                                                                     min = ~ base::min(.x, na.rm = T),
                                                                                     weighted_q1 = ~ weighted.quantile(.x, addedweight, probs = 0.25, na.rm = T, type = 2),
                                                                                     weighted_median = ~ weighted.median(.x, addedweight, na.rm = T, type = 2),
                                                                                     weighted_q3 = ~ weighted.quantile(.x, addedweight, probs = 0.75, na.rm = T, type = 2),
                                                                                     max = ~ base::max(.x, na.rm = T),
                                                                                     weighted_variance = ~ weighted.var(.x, addedweight, na.rm = T),
                                                                                     weighted_skewness = ~ weighted_skewness(.x, addedweight, na.rm = T),
                                                                                     kurtosis = ~ weighted_kurtosis(.x, addedweight, na.rm = T),
                                                                                     wellbeingcount = ~ sum(!(is.na(.x)))), .names = "{.col}.{.fn}"),
                         unweighted_count = n(),
                         weighted_count = sum(addedweight, na.rm = T))
    
    ############################################################################  
    #################### INCOME CORRELATION MATRIX #############################
    ############################################################################
      
    lcfinccorr <- lcfdf%>%
      dplyr::select(SATISnew, WORTHnew, HAPPYnew, ANXIOUSnew, addedweight, GWKINCP)%>%
      na.omit%>%
      mutate(corrweight = addedweight)%>%
      dplyr::rename(Satisfaction = SATISnew, Worthwhile = WORTHnew, Happiness = HAPPYnew, Anxiousness = ANXIOUSnew, Income = GWKINCP)
      
    inccorrlist[[i]] <- cov.wt(lcfinccorr[,!(colnames(lcfinccorr) %in% c("addedweight", "corrweight"))],
                                wt = lcfinccorr$corrweight,
                                cor = TRUE)
      
    }#END OF INCOME LOOP
    
    ############################################################################
    #################### Ethnicity #############################################
    ############################################################################
    
    #Creating a data frame of the sample statistics for well-being by ethnicity
    ethlist[[i]] <- lcfdf%>%
      dplyr::select(SATISnew, WORTHnew, HAPPYnew, ANXIOUSnew, ETHEP, ETHNIP, ETHSP, ETHWP, addedweight)%>%
      dplyr::rename(Satis = SATISnew, Happy = HAPPYnew, Worth = WORTHnew, Anxious = ANXIOUSnew)%>%
      dplyr::mutate(Ethnicity = case_when(ETHEP == 1 | ETHNIP == 1 | ETHSP == 1 | ETHWP == 1 ~ "White",
                                          ETHEP == 2 | ETHNIP == 2 | ETHSP == 2 | ETHWP == 2 ~ "Mixed Race",
                                          ETHEP == 3 | ETHNIP == 3 | ETHSP == 3 | ETHWP == 3 ~ "Asian",
                                          ETHEP == 4 | ETHNIP == 4 | ETHSP == 4 | ETHWP == 4 ~ "Black",
                                          ETHEP == 5 | ETHNIP == 5 | ETHSP == 5 | ETHWP == 5 ~ "Other",
                                          TRUE ~ NA_character_))%>%
      dplyr::group_by(Ethnicity)%>%
      dplyr::summarise(dplyr::across(c('Satis', 'Worth', 'Happy', 'Anxious'), list(weighted_mean = ~ weighted_mean(.x, addedweight, na.rm = T),
                                                                                   min = ~ base::min(.x, na.rm = T),
                                                                                   weighted_q1 = ~ weighted.quantile(.x, addedweight, probs = 0.25, na.rm = T, type = 2),
                                                                                   weighted_median = ~ weighted.median(.x, addedweight, na.rm = T, type = 2),
                                                                                   weighted_q3 = ~ weighted.quantile(.x, addedweight, probs = 0.75, na.rm = T, type = 2),
                                                                                   max = ~ base::max(.x, na.rm = T),
                                                                                   weighted_variance = ~ weighted.var(.x, addedweight, na.rm = T),
                                                                                   weighted_skewness = ~ weighted_skewness(.x, addedweight, na.rm = T),
                                                                                   kurtosis = ~ weighted_kurtosis(.x, addedweight, na.rm = T),
                                                                                   wellbeingcount = ~ sum(!(is.na(.x)))), .names = "{.col}.{.fn}"),
                       unweighted_count = n(),
                       weighted_count = sum(addedweight, na.rm = T))
    
    ############################################################################
    #################### MARITAL STATUS ########################################
    ############################################################################
    
    # Creating a data frame of the sample statistics of the well-being variables by marital status
    marrlist[[i]] <- lcfdf%>%
      dplyr::select(SATISnew, WORTHnew, HAPPYnew, ANXIOUSnew, HASDEP, addedweight, MARSTA_P)%>%
      dplyr::rename(Satis = SATISnew, Happy = HAPPYnew, Worth = WORTHnew, Anxious = ANXIOUSnew, `Marital Status` = MARSTA_P)%>%
      dplyr::filter(`Marital Status` != 6)%>%
      dplyr::mutate(`Marital Status` = case_when(`Marital Status` == 1 ~ "Single, never married",
                                                 `Marital Status` == 2 ~ "Married and living with husband/wife",
                                                 `Marital Status` == 3 ~ "Married and separated from husband/wife",
                                                 `Marital Status` == 4 ~ "Divorced",
                                                 `Marital Status` == 5 ~ "Widowed",
                                                 TRUE ~ NA_character_))%>%
      dplyr::group_by(`Marital Status`)%>%
      dplyr::summarise(dplyr::across(c('Satis', 'Worth', 'Happy', 'Anxious'), list(weighted_mean = ~ weighted_mean(.x, addedweight, na.rm = T),
                                                                                   min = ~ base::min(.x, na.rm = T),
                                                                                   weighted_q1 = ~ weighted.quantile(.x, addedweight, probs = 0.25, na.rm = T, type = 2),
                                                                                   weighted_median = ~ weighted.median(.x, addedweight, na.rm = T, type = 2),
                                                                                   weighted_q3 = ~ weighted.quantile(.x, addedweight, probs = 0.75, na.rm = T, type = 2),
                                                                                   max = ~ base::max(.x, na.rm = T),
                                                                                   weighted_variance = ~ weighted.var(.x, addedweight, na.rm = T),
                                                                                   weighted_skewness = ~ weighted_skewness(.x, addedweight, na.rm = T),
                                                                                   kurtosis = ~ weighted_kurtosis(.x, addedweight, na.rm = T),
                                                                                   wellbeingcount = ~ sum(!(is.na(.x)))), .names = "{.col}.{.fn}"),
                       unweighted_count = n(),
                       weighted_count = sum(addedweight, na.rm = T))
      
    ############################################################################
    #################### Dependent Children ####################################
    ############################################################################
    
    #Creating data frames of the sample statistics of the well-being variables by varying demographic groups
    #Dissecting by whether the respondent has dependent children or not
    depchlist[[i]] <- lcfdf%>%
      dplyr::select(SATISnew, WORTHnew, HAPPYnew, ANXIOUSnew, HASDEP, addedweight)%>%
      dplyr::rename(Satis = SATISnew, Happy = HAPPYnew, Worth = WORTHnew, Anxious = ANXIOUSnew)%>%
      dplyr::mutate(HASDEP = case_when(HASDEP == 1 ~ "Has Dependent Children", HASDEP == 2 ~ "Does not have dependent children", TRUE ~ NA_character_))%>%
      dplyr::group_by(HASDEP)%>%
      dplyr::summarise(dplyr::across(c('Satis', 'Worth', 'Happy', 'Anxious'), list(weighted_mean = ~ weighted_mean(.x, addedweight, na.rm = T),
                                                                                   min = ~ base::min(.x, na.rm = T),
                                                                                   weighted_q1 = ~ weighted.quantile(.x, addedweight, probs = 0.25, na.rm = T, type = 2),
                                                                                   weighted_median = ~ weighted.median(.x, addedweight, na.rm = T, type = 2),
                                                                                   weighted_q3 = ~ weighted.quantile(.x, addedweight, probs = 0.75, na.rm = T, type = 2),
                                                                                   max = ~ base::max(.x, na.rm = T),
                                                                                   weighted_variance = ~ weighted.var(.x, addedweight, na.rm = T),
                                                                                   weighted_skewness = ~ weighted_skewness(.x, addedweight, na.rm = T),
                                                                                   kurtosis = ~ weighted_kurtosis(.x, addedweight, na.rm = T),
                                                                                   wellbeingcount = ~ sum(!(is.na(.x)))), .names = "{.col}.{.fn}"),
                       unweighted_count = n(),
                       weighted_count = sum(addedweight, na.rm = T))
    
    
    #Looking purely at the number of dependent children
    nodepchlist[[i]] <- lcfdf%>%
      dplyr::select(SATISnew, WORTHnew, HAPPYnew, ANXIOUSnew, NDEPC, addedweight)%>%
      dplyr::filter(NDEPC < 7)%>%
      dplyr::rename(Satis = SATISnew, Happy = HAPPYnew, Worth = WORTHnew, Anxious = ANXIOUSnew, `Number of Dependent Children` = NDEPC)%>%
      dplyr::group_by(`Number of Dependent Children`)%>%
      dplyr::summarise(dplyr::across(c('Satis', 'Worth', 'Happy', 'Anxious'), list(weighted_mean = ~ weighted_mean(.x, addedweight, na.rm = T),
                                                                                   min = ~ base::min(.x, na.rm = T),
                                                                                   weighted_q1 = ~ weighted.quantile(.x, addedweight, probs = 0.25, na.rm = T, type = 2),
                                                                                   weighted_median = ~ weighted.median(.x, addedweight, na.rm = T, type = 2),
                                                                                   weighted_q3 = ~ weighted.quantile(.x, addedweight, probs = 0.75, na.rm = T, type = 2),
                                                                                   max = ~ base::max(.x, na.rm = T),
                                                                                   weighted_variance = ~ weighted.var(.x, addedweight, na.rm = T),
                                                                                   weighted_skewness = ~ weighted_skewness(.x, addedweight, na.rm = T),
                                                                                   kurtosis = ~ weighted_kurtosis(.x, addedweight, na.rm = T),
                                                                                   wellbeingcount = ~ sum(!(is.na(.x)))), .names = "{.col}.{.fn}"),
                       unweighted_count = n(),
                       weighted_count = sum(addedweight, na.rm = T))
    
    
    #Breaking down those with dependent children by marital status
    mardepchlist[[i]] <- lcfdf%>%
      dplyr::select(SATISnew, WORTHnew, HAPPYnew, ANXIOUSnew, HASDEP, MARSTA_P, addedweight)%>%
      dplyr::rename(Satis = SATISnew, Happy = HAPPYnew, Worth = WORTHnew, Anxious = ANXIOUSnew, `Marital Status` = MARSTA_P)%>%
      dplyr::filter(HASDEP == 1)%>%
      dplyr::filter(`Marital Status` != 6)%>%
      dplyr::mutate(`Marital Status` = case_when(`Marital Status` == 1 ~ "Single, never married",
                                                 `Marital Status` == 2 ~ "Married and living with husband/wife",
                                                 `Marital Status` == 3 ~ "Married and separated from husband/wife",
                                                 `Marital Status` == 4 ~ "Divorced",
                                                 `Marital Status` == 5 ~ "Widowed",
                                                 TRUE ~ NA_character_))%>%
      dplyr::group_by(`Marital Status`)%>%
      dplyr::summarise(dplyr::across(c('Satis', 'Worth', 'Happy', 'Anxious'), list(weighted_mean = ~ weighted_mean(.x, addedweight, na.rm = T),
                                                                                   min = ~ base::min(.x, na.rm = T),
                                                                                   weighted_q1 = ~ weighted.quantile(.x, addedweight, probs = 0.25, na.rm = T, type = 2),
                                                                                   weighted_median = ~ weighted.median(.x, addedweight, na.rm = T, type = 2),
                                                                                   weighted_q3 = ~ weighted.quantile(.x, addedweight, probs = 0.75, na.rm = T, type = 2),
                                                                                   max = ~ base::max(.x, na.rm = T),
                                                                                   weighted_variance = ~ weighted.var(.x, addedweight, na.rm = T),
                                                                                   weighted_skewness = ~ weighted_skewness(.x, addedweight, na.rm = T),
                                                                                   kurtosis = ~ weighted_kurtosis(.x, addedweight, na.rm = T),
                                                                                   wellbeingcount = ~ sum(!(is.na(.x)))), .names = "{.col}.{.fn}"),
                       unweighted_count = n(),
                       weighted_count = sum(addedweight, na.rm = T))
    
    #Looking at those with dependent children and breaking down by marital status and sex
    sexmardepchlist[[i]] <- lcfdf%>%
      dplyr::select(SATISnew, WORTHnew, HAPPYnew, ANXIOUSnew, HASDEP, MARSTA_P, SEX, SINGPAR, addedweight)%>%
      dplyr::rename(Satis = SATISnew, Happy = HAPPYnew, Worth = WORTHnew, Anxious = ANXIOUSnew)%>%
      dplyr::mutate(`Sex and Marrital Status with Dependent Children` = case_when(HASDEP == 1 & MARSTA_P == 2 & SEX == 1 ~ "Married Male with Dependent Children",
                                                                                  HASDEP == 1 & MARSTA_P == 2 & SEX == 2 ~ "Married Female with Dependent Children",
                                                                                  HASDEP == 1 & MARSTA_P == 4 & SEX == 1 ~ "Divorced Male with Dependent Children",
                                                                                  HASDEP == 1 & MARSTA_P == 4 & SEX == 2 ~ "Divorced Female with Dependent Children",
                                                                                  HASDEP == 1 & SINGPAR == 1 & SEX == 1 ~ "Male, Single Parent",
                                                                                  HASDEP == 1 & SINGPAR == 1 & SEX == 2 ~ "Female, Single Parent",
                                                                                  TRUE ~ NA_character_))%>%
      dplyr::group_by(`Sex and Marrital Status with Dependent Children`)%>%
      dplyr::summarise(dplyr::across(c('Satis', 'Worth', 'Happy', 'Anxious'), list(weighted_mean = ~ weighted_mean(.x, addedweight, na.rm = T),
                                                                                   min = ~ base::min(.x, na.rm = T),
                                                                                   weighted_q1 = ~ weighted.quantile(.x, addedweight, probs = 0.25, na.rm = T, type = 2),
                                                                                   weighted_median = ~ weighted.median(.x, addedweight, na.rm = T, type = 2),
                                                                                   weighted_q3 = ~ weighted.quantile(.x, addedweight, probs = 0.75, na.rm = T, type = 2),
                                                                                   max = ~ base::max(.x, na.rm = T),
                                                                                   weighted_variance = ~ weighted.var(.x, addedweight, na.rm = T),
                                                                                   weighted_skewness = ~ weighted_skewness(.x, addedweight, na.rm = T),
                                                                                   kurtosis = ~ weighted_kurtosis(.x, addedweight, na.rm = T),
                                                                                   wellbeingcount = ~ sum(!(is.na(.x)))), .names = "{.col}.{.fn}"),
                       unweighted_count = n(),
                       weighted_count = sum(addedweight, na.rm = T))
    
    
    #Looking at the employment status of those with dependent children
    depchemploylist[[i]] <- lcfdf%>%
      dplyr::select(SATISnew, WORTHnew, HAPPYnew, ANXIOUSnew, HASDEP, MARSTA_P, SEX, DVILO3A, SINGPAR, WEIGHTA, addedweight, FTPTWK)%>%
      dplyr::rename(Satis = SATISnew, Happy = HAPPYnew, Worth = WORTHnew, Anxious = ANXIOUSnew)%>%
      dplyr::mutate(`Sex and Marrital Status with Dependent Children` = case_when(HASDEP == 1 & MARSTA_P == 2 & SEX == 1 ~ "Married Male with Dependent Children",
                                                                                  HASDEP == 1 & MARSTA_P == 2 & SEX == 2 ~ "Married Female with Dependent Children",
                                                                                  HASDEP == 1 & MARSTA_P == 4 & SEX == 1 ~ "Divorced Male with Dependent Children",
                                                                                  HASDEP == 1 & MARSTA_P == 4 & SEX == 2 ~ "Divorced Female with Dependent Children",
                                                                                  HASDEP == 1 & SINGPAR == 1 & SEX == 1 ~ "Male, Single Parent",
                                                                                  HASDEP == 1 & SINGPAR == 1 & SEX == 2 ~ "Female, Single Parent",
                                                                                  TRUE ~ NA_character_),
                    `Employment Status` = case_when(DVILO3A == 1 ~ "In Employment",
                                                    DVILO3A == 2 ~ "ILO Unemployed",
                                                    DVILO3A == 3 ~ "Economically Inactive",
                                                    TRUE ~ NA_character_),
                    `Full or Part-time` = case_when(FTPTWK == 1 ~ "Full time",
                                                    FTPTWK == 2 ~ "Part-time",
                                                    TRUE ~ NA_character_))%>%
      dplyr::group_by(`Sex and Marrital Status with Dependent Children`, `Employment Status`, `Full or Part-time`)%>%
      dplyr::summarise(count = sum(WEIGHTA, na.rm = T))
    
    
    
  }#END OF LOOP
  
  # This is the list of lists which contain all the data that is to be outputted by the function
  finalList <- list("Summary Statistics of Wellbeing Variables" = summarylist,
                    "Summary Statistics of those With/Without Dependent Children" = depchlist,
                    "Summary Statistics by Number of Dependent Children" = nodepchlist,
                    "Summary Statistics by Marital Status of those with Dependent Children" = mardepchlist,
                    "Summary Statistics by Marital Status and Sex of those with Dependent Children" = sexmardepchlist,
                    "Summary Statistics by Age Bands" = agelist,
                    "Count by Employment Status and Marital Status of those with Dependent Children" = depchemploylist,
                    "Summary Statistics by Employment Status" = employlist,
                    "Summary Statistics by Amount of Child Benefits Collected" = chbenlist,
                    "Summary Statistics by Gross Weekly Income Bands" = incomelist,
                    "Summary Statistics by Ethnicity Group" = ethlist,
                    "Correlation Matrices" = corrlist,
                    "Summary Statistics by Sex" = sexlist,
                    "Summary Statistics by Marital Status" = marrlist,
                    "Income Correlation Matrices" = inccorrlist,
                    "Summary Statistics by Region" = regionlist)
  
  
  return(finalList)
  
}#END OF FUNCTION

################# IMPORTANT ####################################################
#This runs the function with the necessary inputs which were created at the beginning of the script
lcflist <- lcftime(yearvec = years, dtavec = lcfdta, wd = wd2021, dvhhdtavec = lcfdvhousedta)
################################################################################

#Now sorting out the correlation data, as we only want the correlation matrix from every list element
corrmatrices <- list()
incomecorrmatrices <- list()
for (i in 1:length(years)){
  
  corrmatrices[[i]] <- apsts$correlations[[i]]$cor
  
  incomecorrmatrices[[i]] <- apsts$incomecorrelations[[i]]$cor
  
}


#Now, must write each list as a excel file containing all the years that have been read in
#By default, this function organises the list elements into separate worksheets

openxlsx::write.xlsx(lcflist$`Summary Statistics of Wellbeing Variables`, file = "summarydata.xlsx") 

openxlsx::write.xlsx(corrmatrices, file = "correlationMatrices.xlsx")

openxlsx::write.xlsx(incomecorrmatrices, file = "incomeCorrelationMatrices.xlsx")

openxlsx::write.xlsx(lcflist$`Summary Statistics by Sex`, file = "summarySexdata.xlsx")

openxlsx::write.xlsx(lcflist$`Summary Statistics by Ethnicity Group`, file = "summaryEthnicitydata.xlsx")

openxlsx::write.xlsx(lcflist$`Summary Statistics by Region`, file = "summaryRegiondata.xlsx")

openxlsx::write.xlsx(lcflist$`Summary Statistics by Gross Weekly Income Bands`, file = "summaryIncomedata.xlsx")

openxlsx::write.xlsx(lcflist$`Summary Statistics by Age Bands`, file = "summaryAgedata.xlsx")

openxlsx::write.xlsx(lcflist$`Summary Statistics by Employment Status`, file = "summaryEmploymentdata.xlsx")

openxlsx::write.xlsx(lcflist$`Summary Statistics by Amount of Child Benefits Collected`, file = "summaryChildBenefitdata.xlsx")

openxlsx::write.xlsx(lcflist$`Summary Statistics by Number of Dependent Children`, file = "summaryNumberDependentChildrendata.xlsx")

openxlsx::write.xlsx(lcflist$`Summary Statistics of those With/Without Dependent Children`, file = "summaryDependentChildrendata.xlsx")

openxlsx::write.xlsx(lcflist$`Summary Statistics by Marital Status of those with Dependent Children`, file = "summaryMaritalDependentChildren.xlsx")

openxlsx::write.xlsx(lcflist$`Summary Statistics by Marital Status and Sex of those with Dependent Children`, file = "summarySexMaritalDependentChildren.xlsx")

openxlsx::write.xlsx(lcflist$`Count by Employment Status and Marital Status of those with Dependent Children`, file = "numberEmploymentMaritalDepChild.xlsx")

