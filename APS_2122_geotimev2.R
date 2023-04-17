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
setwd("/Users/benjaminosullivan/Library/CloudStorage/GoogleDrive-osullivan.benjamin1@gmail.com/My Drive/What Works/APS")
source("weighted_mean.R")


################################################################################
############### IMPORTANT MESSAGE ##############################################
################################################################################

# The location of the dta files is important to successfully run the script.
# IN YOUR DESIGNATED WORKING DIRECTORY THERE MUST BE FOLDERS WITH NAMES:
# "2012/13", "2013/14", "2014/15", "2015/16", "2016/17", "2017/18", "2018/19", "2019/20",
# "2020/21", "2021/22"
# AND THE .dta FILES MUST BE COPIED INTO THE RESPECTIVE FOLDER CORRESPONDING TO THEIR YEAR

# NOTE: the .dta files must be the raw personal dataset as well as the derived variable
# household dataset for each year

#EXTRA NOTE: Probably convenient to place the weighted_mean.R file in the same location
#as the folders where all the dta files are stored in

################################################################################
################################################################################
################################################################################

# These vectors are used to read in the .dta files for each year
#NOTE:Well-being variables only exist for years 2012/13 to 2021/22
wdvec <- c()
years <- c("2012:13", "2013:14", "2014:15", "2015:16", "2016:17", "2017:18", "2018:19", "2019:20", "2020:21", "2021:22")
apsdta <- c("a12m13_wellbeing_eul.dta", "a13m14_wellbeing_eul.dta", "a14m15_wellbeing_eul.dta", "apsp_a15m_eul.dta",
            "apsp_a16m17_eul.dta", "apsp_apr17mar18_eul.dta", "apsp_a18m19_eul_pwta18.dta", "apsp_a19m20_eul_pwta20.dta",
            "apsp_a20m21_eul_pwta20.dta", "apsp_a21m22_eul_pwta22.dta")

# PLEASE INPUT YOUR WORKING DIRECTORY CONTAINING THE FOLDERS "2012/13", "2013/14", "2014/15", "2015/16",
# "2016/17", "2017/18", "2018/19", "2019/20", "2020/21", "2021/22" BELOW, ASSIGNING IT TO THE "wd2021" OBJECT. 
wd2122 <- "/Users/benjaminosullivan/Library/CloudStorage/GoogleDrive-osullivan.benjamin1@gmail.com/My Drive/What Works/APS"
wd2122 <- paste0(wd2122, "/2021:22")

#Well-being variables are SATIS, WORTH, HAPPY, ANXIOUS
#For well-being variables, those that did not answer either have values -8 or -9 assigned
#Well-being weights are NPWT (which is named as "NPWT**", WHERE the ** is the two digits of the revision year)

#Function which reads all APS data sets and computes the well-being statistics
#WARNING: this function is long!
apstime <- function(yearvec, dtavec, wd){
  
  #These are all the lists which will contain the relevant data, e.g. income list
  #will contain all the sample moments for the income variable etc.
  summarylist <- sapply(years, function(x) NULL)
  weightedlist <- sapply(years, function(x) NULL)
  weightedquant <- sapply(years, function(x) NULL)
  regionlist <- sapply(years, function(x) NULL)
  regionquant <- sapply(years, function(x) NULL)
  corrlist <- sapply(years, function(x) NULL)
  sexgenderlist <- sapply(years, function(x) NULL)
  sexquant <- sapply(years, function(x) NULL)
  agelist <- sapply(years, function(x) NULL)
  agequant <- sapply(years, function(x) NULL)
  incomelist <- sapply(years, function(x) NULL)
  incomequant <- sapply(years, function(x) NULL)
  incomecorr <- sapply(years, function(x) NULL)
  industrylist <- sapply(years, function(x) NULL)
  industryquant <- sapply(years, function(x) NULL)
  industryhourslist <- sapply(years, function(x) NULL)
  educationlist <- sapply(years, function(x) NULL)
  educationquant <- sapply(years, function(x) NULL)
  ethnicitylist <- sapply(years, function(x) NULL)
  ethnicityquant <- sapply(years, function(x) NULL)
  religionlist <- sapply(years, function(x) NULL)
  religionquant <- sapply(years, function(x) NULL)
  accommodationlist <- sapply(years, function(x) NULL)
  accommodationquant <- sapply(years, function(x) NULL)
  employlist <- sapply(years, function(x) NULL)
  employquant <- sapply(years, function(x) NULL)
  benchmarkcount <- sapply(years, function(x) NULL)

  
  #Now looping through each year
  for (i in 1:length(yearvec)){
    
    
    #Setting working directory
    wdsub <- gsub("2021:22", yearvec[i], wd)
    setwd(wdsub)
    
    #Reading dta file
    apsdata <- read_dta(dtavec[i], encoding = "Latin1")
    
    colnames(apsdata) <- toupper(colnames(apsdata))
    
    #Setting well-being weight 
    wbweight <- apsdata[,which(substr(names(apsdata), start = 1, stop = 2) == "NP")]
    wbweight <- unlist(wbweight)
    
    apsdata$addedweight <- wbweight
    
    totalweight <- sum(wbweight, na.rm = TRUE)
    
    
    #This is the main data frame that will be used to construct the data frames of statistics
    apsdf <- apsdata%>%
      filter(SATIS>=0 | WORTH>=0 | HAPPY>=0 | ANXIOUS>=0)%>%
      dplyr::mutate(SATISnew = case_when(SATIS == -8 ~ NA_integer_, SATIS == -9 ~ NA_integer_, TRUE ~ as.integer(SATIS)),
                    WORTHnew = case_when(WORTH == -8 ~ NA_integer_, WORTH == -9 ~ NA_integer_, TRUE ~ as.integer(WORTH)),
                    HAPPYnew = case_when(HAPPY == -8 ~ NA_integer_, HAPPY == -9 ~ NA_integer_, TRUE ~ as.integer(HAPPY)),
                    ANXIOUSnew = case_when(ANXIOUS == -8 ~ NA_integer_, ANXIOUS == -9 ~ NA_integer_, TRUE ~ as.integer(ANXIOUS)))%>%
      dplyr::mutate(propNPWT = addedweight/totalweight)%>%
      dplyr::mutate(SEXnew = case_when(SEX == -9 ~ NA_character_, SEX == -8 ~ NA_character_, SEX == 1 ~ "Male", SEX == 2 ~ "Female", TRUE ~ NA_character_)) 
    
    ############################################################################
    ################## SUMMARY STATISTICS FOR THE SAMPLE YEAR ##################
    ############################################################################
    
    summarylist[[i]] <- data.frame("Satisfaction Summary" = unclass(summary(na.omit(apsdf$SATIS))),
                                   "Worthwhile Summary" = unclass(summary(na.omit(apsdf$WORTH))),
                                   "Happiness Summary" = unclass(summary(na.omit(apsdf$HAPPY))),
                                   "Anxiousness Summary" = unclass(summary(na.omit(apsdf$ANXIOUS))),
                                   check.names = FALSE,
                                   stringsAsFactors = FALSE)
    
    ############################################################################
    ################## WEIGHTED STATISTICS FOR THE SAMPLE YEAR #################
    ############################################################################
    
    weightedlist[[i]] <- apsdf%>%
      dplyr::select(SATISnew, WORTHnew, HAPPYnew, ANXIOUSnew, addedweight)%>%
      tidyr::pivot_longer(c('SATISnew', 'WORTHnew', 'HAPPYnew', 'ANXIOUSnew'), names_to = 'wellbeing', values_to = 'ratings')%>%
      dplyr::group_by(wellbeing)%>%
      dplyr::summarise(weighted_mean = stats::weighted.mean(ratings, addedweight, na.rm = T),
                       weighted_median = weighted.median(ratings, addedweight, na.rm = T),
                       weighted_variance = weighted.var(ratings, addedweight, na.rm = T),
                       weighted_skewness = skewness.ade(ratings, na.rm = T, w = addedweight),
                       weighted_kurtosis = weighted_kurtosis(ratings, addedweight, na.rm = T),
                       unweighted_count = n(),
                       weighted_count = sum(addedweight, na.rm = T),
                       unweighted_sample_proportion = n()/nrow(apsdata))
    
    ############################################################################
    ############# WEIGHTED PERCENTILES FOR THE SAMPLE YEAR #####################
    ############################################################################
    
    weightedquant[[i]] <- data.frame("Satisfaction Quartile" = unclass(weighted.quantile(apsdf$SATISnew, apsdf$propNPWT, probs = seq(0, 1, 0.25),na.rm = TRUE, type = 2)),
                                     "Worthwhile Quartile" = unclass(weighted.quantile(apsdf$WORTHnew, apsdf$propNPWT, probs = seq(0, 1, 0.25),na.rm = TRUE, type = 2)),
                                     "Happiness Quartile" = unclass(weighted.quantile(apsdf$HAPPYnew, apsdf$propNPWT, probs = seq(0, 1, 0.25),na.rm = TRUE, type = 2)),
                                     "Anxiousness Quartile" = unclass(weighted.quantile(apsdf$ANXIOUSnew, apsdf$propNPWT, probs = seq(0, 1, 0.25),na.rm = TRUE, type = 2)),
                                     check.names = FALSE,
                                     stringsAsFactors = FALSE)
    
    ############################################################################
    ############# BENCHMARK COUNTING ###########################################
    ############################################################################
    
    sumweight <- sum(apsdf$addedweight, na.rm = T)
    
    benchmarkcount[[i]] <- apsdf%>%
      dplyr::select(SATISnew, WORTHnew, HAPPYnew, ANXIOUSnew, addedweight)%>%
      tidyr::pivot_longer(c('SATISnew', 'WORTHnew', 'HAPPYnew', 'ANXIOUSnew'), names_to = 'wellbeing', values_to = 'ratings')%>%
      dplyr::mutate(`Range 4 to 6` = case_when(ratings > 3 & ratings < 7 ~ as.integer(addedweight), TRUE ~ as.integer(0)),
                    `Range 0 to 3` = case_when(ratings >= 0 & ratings < 4 ~ as.integer(addedweight), TRUE ~ as.integer(0)),
                    `Range 7 to 10` = case_when(ratings > 6 & ratings <= 10 ~ as.integer(addedweight), TRUE ~ as.integer(0)))%>%
      dplyr::group_by(wellbeing)%>%
      dplyr::summarise(dplyr::across(c('Range 4 to 6', 'Range 0 to 3', 'Range 7 to 10'), list(proportion = ~ sum(.x, na.rm = T)/sumweight,
                                                                                              count = ~ sum(.x, na.rm = T)), .names = "{.col}.{.fn}"),
                       total_count = sum(addedweight, na.rm = T))
    
    ############################################################################
    ############### CORRELATION MATRIX OF WELLBEING FOR ENTIRE SAMPLE ##########
    ############################################################################
    
    apscorr <- apsdf%>%
      dplyr::select(SATISnew, WORTHnew, HAPPYnew, ANXIOUSnew, addedweight)%>%
      na.omit%>%
      mutate(corrweight = addedweight/totalweight)%>%
      dplyr::rename(Satisfaction = SATISnew, Worthwhile = WORTHnew, Happiness = HAPPYnew, Anxiousness = ANXIOUSnew)
    
    corrlist[[i]] <- cov.wt(apscorr[,!(colnames(apscorr) %in% c("addedweight", "corrweight"))],
                            wt = apscorr$corrweight,
                            cor = TRUE)
    
    ############################################################################
    ########### WEIGHTED STATISTICS BY REGION ##################################
    ############################################################################
    
    #Need to use a different regional variable depending on the year
    if (yearvec[i] %in% c("2012:13", "2013:14", "2014:15")){
      
      apsdf$regions <- apsdf$GOR
      
      apsdf <- apsdf%>%
        dplyr::mutate(regionsvar = case_when(regions == -9 ~ NA_integer_,
                                             regions == -8 ~ NA_integer_,
                                             regions == 3 ~ as.integer(2), #Generally, Merseyside is included in the North West
                                             TRUE ~ as.integer(regions)))
      
    }else{
      
      apsdf$regionsvar <- apsdf$GOR9D
      
    }
    
    #This is the data frame outputting the statistics for the well-being variables by region
    regionlist[[i]] <- apsdf%>%
      dplyr::group_by(regionsvar)%>%
      dplyr::summarise(dplyr::across(c('SATISnew', 'WORTHnew', 'HAPPYnew', 'ANXIOUSnew'), list(weightedmean = ~ stats::weighted.mean(.x, addedweight, na.rm = T),
                                                                                               weightedmedian = ~ weighted.median(.x, addedweight, na.rm = T),
                                                                                               weightedvariance = ~ weighted.var(.x, addedweight, na.rm = T),
                                                                                               weightedskewness = ~ skewness.ade(.x, na.rm = T, w = addedweight),
                                                                                               weightedkurtosis = ~ weighted_kurtosis(.x, w = addedweight, na.rm = T)), .names = "{.col}.{.fn}"),
                       unweighted_count = n(),
                       weighted_count = sum(addedweight, na.rm = T))
    
    ############################################################################
    ################# WEIGHTED PERCENTILES BY REGION ###########################
    ############################################################################
    
    regionquant[[i]] <- apsdf%>%
      dplyr::group_by(regionsvar)%>%
      dplyr::summarise(SATISqs = weighted.quantile(SATISnew, propNPWT, probs = seq(0,1,0.25), na.rm = T, type = 2),
                       WORTHqs = weighted.quantile(WORTHnew, propNPWT, probs = seq(0,1,0.25), na.rm = T, type = 2),
                       HAPPYqs = weighted.quantile(HAPPYnew, propNPWT, probs = seq(0,1,0.25), na.rm = T, type = 2),
                       ANXIOUSqs = weighted.quantile(ANXIOUSnew, propNPWT, probs = seq(0,1,0.25), na.rm = T, type = 2))%>%
      dplyr::rename(SATIS_quantile = SATISqs,
                    WORTH_quantile = WORTHqs,
                    HAPPY_quantile = HAPPYqs,
                    ANXIOUS_quantile = ANXIOUSqs)
    
    ############################################################################
    ####################### WEIGHTED STATISTICS BY GENDER/SEX ##################
    ############################################################################
    
    sexgenderlist[[i]] <- apsdf%>%
      dplyr::group_by(SEXnew)%>%
      dplyr::summarise(dplyr::across(c('SATISnew', 'WORTHnew', 'HAPPYnew', 'ANXIOUSnew'), list(weightedmean = ~ stats::weighted.mean(.x, addedweight, na.rm = T),
                                                                                               weightedmedian = ~ weighted.median(.x, addedweight, na.rm = T),
                                                                                               weightedvariance = ~ weighted.var(.x, addedweight, na.rm = T),
                                                                                               weightedskewness = ~ skewness.ade(.x, na.rm = T, w = addedweight),
                                                                                               weightedkurtosis = ~ weighted_kurtosis(.x, w = addedweight, na.rm = T)), .names = "{.col}.{.fn}"),
                       unweighted_count = n(),
                       weighted_count = sum(addedweight, na.rm = T))
    
    ############################################################################
    ###################### WEIGHTED PERCENTILES BY SEX #########################
    ############################################################################
    
    sexquant[[i]] <- apsdf%>%
      dplyr::group_by(SEXnew)%>%
      dplyr::summarise(SATIS_quantile = weighted.quantile(SATISnew, addedweight, probs = seq(0,1,0.25), na.rm = T, type = 2),
                       WORTH_quantile = weighted.quantile(WORTHnew, addedweight, probs = seq(0,1,0.25), na.rm = T, type = 2),
                       HAPPY_quantile = weighted.quantile(HAPPYnew, addedweight, probs = seq(0,1,0.25), na.rm = T, type = 2),
                       ANXIOUS_quantile = weighted.quantile(ANXIOUSnew, addedweight, probs = seq(0,1,0.25), na.rm = T, type = 2))
    
    ############################################################################
    ################ WEIGHTED STATISTCS AND PERCENTILES BY AGE BANDS ###########
    ############################################################################
    
    # Certain age variables are used depending on the year
    if (!(yearvec[i] %in% c("2012:13", "2013:14", "2014:15"))){
      
      agelist[[i]] <- apsdf%>%
        mutate(AGEnew = case_when(AAGE == -9 ~ NA_character_,
                                  AAGE == -8 ~ NA_character_,
                                  AAGE == 1 ~ "Under 16",
                                  AAGE == 2 ~ "16-17",
                                  AAGE == 3 ~ "18-19",
                                  AAGE == 4 ~ "20-24",
                                  AAGE == 5 ~ "25-29",
                                  AAGE == 6 ~ "30-34",
                                  AAGE == 7 ~ "35-39",
                                  AAGE == 8 ~ "40-44",
                                  AAGE == 9 ~ "45-49",
                                  AAGE == 10 ~ "50-54",
                                  AAGE == 11 ~ "55-59",
                                  AAGE == 12 ~ "60-64",
                                  AAGE == 13 ~ "65-99",
                                  TRUE ~ NA_character_))%>%
        dplyr::group_by(AGEnew)%>%
        dplyr::summarise(dplyr::across(c('SATISnew', 'WORTHnew', 'HAPPYnew', 'ANXIOUSnew'), list(weightedmean =  ~ stats::weighted.mean(.x, addedweight, na.rm = T),
                                                                                                 weightedmedian = ~ weighted.median(.x, addedweight, na.rm = T),
                                                                                                 weightedvariance = ~ weighted.var(.x, addedweight, na.rm = T),
                                                                                                 weightedskewness = ~ skewness.ade(.x, na.rm = T, w = addedweight),
                                                                                                 weightedkurtosis = ~ weighted_kurtosis(.x, w = addedweight, na.rm = T)), .names = "{.col}.{.fn}"),
                         unweighted_count = n(),
                         weighted_count = sum(addedweight, na.rm = T))
      
      agequant[[i]] <- apsdf%>%
        mutate(AGEnew = case_when(AAGE == -9 ~ NA_character_,
                                  AAGE == -8 ~ NA_character_,
                                  AAGE == 1 ~ "Under 16",
                                  AAGE == 2 ~ "16-17",
                                  AAGE == 3 ~ "18-19",
                                  AAGE == 4 ~ "20-24",
                                  AAGE == 5 ~ "25-29",
                                  AAGE == 6 ~ "30-34",
                                  AAGE == 7 ~ "35-39",
                                  AAGE == 8 ~ "40-44",
                                  AAGE == 9 ~ "45-49",
                                  AAGE == 10 ~ "50-54",
                                  AAGE == 11 ~ "55-59",
                                  AAGE == 12 ~ "60-64",
                                  AAGE == 13 ~ "65-99",
                                  TRUE ~ NA_character_))%>%
        dplyr::group_by(AGEnew)%>%
        dplyr::summarise(SATIS_quantile = weighted.quantile(SATISnew, addedweight, probs = seq(0,1,0.25), na.rm = T, type = 2),
                         WORTH_quantile = weighted.quantile(WORTHnew, addedweight, probs = seq(0,1,0.25), na.rm = T, type = 2),
                         HAPPY_quantile = weighted.quantile(HAPPYnew, addedweight, probs = seq(0,1,0.25), na.rm = T, type = 2),
                         ANXIOUS_quantile = weighted.quantile(ANXIOUSnew, addedweight, probs = seq(0,1,0.25), na.rm = T, type = 2))
      
    }else{
      
      agelist[[i]] <- apsdf%>%
        mutate(AGEnew = case_when(AGEGRP == -9 ~ NA_character_,
                                  AGEGRP == -8 ~ NA_character_,
                                  AGEGRP == 4 ~ "16-19",
                                  AGEGRP == 5 ~ "20-24",
                                  AGEGRP == 6 ~ "25-29",
                                  AGEGRP == 7 ~ "30-34",
                                  AGEGRP == 8 ~ "35-39",
                                  AGEGRP == 9 ~ "40-44",
                                  AGEGRP == 10 ~ "45-49",
                                  AGEGRP == 11 ~ "50-54",
                                  AGEGRP == 12 ~ "55-59",
                                  AGEGRP == 13 ~ "60-64",
                                  AGEGRP == 14 ~ "65-69",
                                  AGEGRP == 15 ~ "70-74",
                                  AGEGRP == 16 ~ "75-79",
                                  AGEGRP == 17 ~ "80 & over",
                                  TRUE ~ NA_character_))%>%
        dplyr::group_by(AGEnew)%>%
        dplyr::summarise(dplyr::across(c('SATISnew', 'WORTHnew', 'HAPPYnew', 'ANXIOUSnew'), list(weightedmean = ~ stats::weighted.mean(.x, addedweight, na.rm = T),
                                                                                                 weightedmedian = ~ weighted.median(.x, addedweight, na.rm = T),
                                                                                                 weightedvariance = ~ weighted.var(.x, addedweight, na.rm = T),
                                                                                                 weightedskewness = ~ skewness.ade(.x, na.rm = T, w = addedweight),
                                                                                                 weightedkurtosis = ~ weighted_kurtosis(.x, w = addedweight, na.rm = T)), .names = "{.col}.{.fn}"),
                         unweighted_count = n(),
                         weighted_count = sum(addedweight, na.rm = T))
      
      agequant[[i]] <- apsdf%>%
        mutate(AGEnew = case_when(AGEGRP == -9 ~ NA_character_,
                                  AGEGRP == -8 ~ NA_character_,
                                  AGEGRP == 4 ~ "16-19",
                                  AGEGRP == 5 ~ "20-24",
                                  AGEGRP == 6 ~ "25-29",
                                  AGEGRP == 7 ~ "30-34",
                                  AGEGRP == 8 ~ "35-39",
                                  AGEGRP == 9 ~ "40-44",
                                  AGEGRP == 10 ~ "45-49",
                                  AGEGRP == 11 ~ "50-54",
                                  AGEGRP == 12 ~ "55-59",
                                  AGEGRP == 13 ~ "60-64",
                                  AGEGRP == 14 ~ "65-69",
                                  AGEGRP == 15 ~ "70-74",
                                  AGEGRP == 16 ~ "75-79",
                                  AGEGRP == 17 ~ "80 & over",
                                  TRUE ~ NA_character_))%>%
        dplyr::group_by(AGEnew)%>%
        dplyr::summarise(SATIS_quantile = weighted.quantile(SATISnew, addedweight, probs = seq(0,1,0.25), na.rm = T, type = 2),
                         WORTH_quantile = weighted.quantile(WORTHnew, addedweight, probs = seq(0,1,0.25), na.rm = T, type = 2),
                         HAPPY_quantile = weighted.quantile(HAPPYnew, addedweight, probs = seq(0,1,0.25), na.rm = T, type = 2),
                         ANXIOUS_quantile = weighted.quantile(ANXIOUSnew, addedweight, probs = seq(0,1,0.25), na.rm = T, type = 2))
      
      
    }
    
    ############################################################################
    ############# WEIGHTED STATISTICS/PERCENTILE BY INCOME BANDS ###############
    ############# AND CORRELATION MATRICES BETWEEN INCOME AND WELLBEING ########
    ############################################################################
    
    #Need to filter income and some years use different variables to filter
    if ('HOURPAY' %in% names(apsdf)){
      incomelist[[i]] <- apsdf%>%
        dplyr::filter(GRSSWK>0 & HOURPAY>0 & HOURPAY<100)%>%
        dplyr::mutate(GRSSWKbands = case_when(GRSSWK == -9 ~ NA_character_,
                                              GRSSWK == -8 ~ NA_character_,
                                              GRSSWK >=0 & GRSSWK <100 ~ "0-99",
                                              GRSSWK >=100 & GRSSWK <200 ~ "100-199",
                                              GRSSWK >=200 & GRSSWK <300 ~ "200-299",
                                              GRSSWK >=300 & GRSSWK <400 ~ "300-399",
                                              GRSSWK >=400 & GRSSWK <500 ~ "400-499",
                                              GRSSWK >=500 & GRSSWK <600 ~ "500-599",
                                              GRSSWK >=600 & GRSSWK <700 ~ "600-699",
                                              GRSSWK >=700 & GRSSWK <800 ~ "700-799",
                                              GRSSWK >=800 & GRSSWK <900 ~ "800-899",
                                              GRSSWK >=900 & GRSSWK <1000 ~ "900-999",
                                              TRUE ~ "1000 or more"
        ))%>%
        dplyr::group_by(GRSSWKbands)%>%
        dplyr::summarise(dplyr::across(c('SATISnew', 'WORTHnew', 'HAPPYnew', 'ANXIOUSnew'), list(weightedmean = ~ stats::weighted.mean(.x, addedweight, na.rm = T),
                                                                                                 weightedmedian = ~ weighted.median(.x, addedweight, na.rm = T),
                                                                                                 weightedvariance = ~ weighted.var(.x, addedweight, na.rm = T),
                                                                                                 weightedskewness = ~ skewness.ade(.x, na.rm = T, w = addedweight),
                                                                                                 weightedkurtosis = ~ weighted_kurtosis(.x, w = addedweight, na.rm = T)), .names = "{.col}.{.fn}"),
                         unweighted_count = n(),
                         weighted_count = sum(addedweight, na.rm = T))
      
      incomequant[[i]] <- apsdf%>%
        dplyr::filter(GRSSWK>0 & HOURPAY>0 & HOURPAY<100)%>%
        dplyr::mutate(GRSSWKbands = case_when(GRSSWK == -9 ~ NA_character_,
                                              GRSSWK == -8 ~ NA_character_,
                                              GRSSWK >=0 & GRSSWK <100 ~ "0-99",
                                              GRSSWK >=100 & GRSSWK <200 ~ "100-199",
                                              GRSSWK >=200 & GRSSWK <300 ~ "200-299",
                                              GRSSWK >=300 & GRSSWK <400 ~ "300-399",
                                              GRSSWK >=400 & GRSSWK <500 ~ "400-499",
                                              GRSSWK >=500 & GRSSWK <600 ~ "500-599",
                                              GRSSWK >=600 & GRSSWK <700 ~ "600-699",
                                              GRSSWK >=700 & GRSSWK <800 ~ "700-799",
                                              GRSSWK >=800 & GRSSWK <900 ~ "800-899",
                                              GRSSWK >=900 & GRSSWK <1000 ~ "900-999",
                                              TRUE ~ "1000 or more"
        ))%>%
        dplyr::group_by(GRSSWKbands)%>%
        dplyr::summarise(SATIS_quantile = weighted.quantile(SATISnew, addedweight, probs = seq(0,1,0.25), na.rm = T, type = 2),
                         WORTH_quantile = weighted.quantile(WORTHnew, addedweight, probs = seq(0,1,0.25), na.rm = T, type = 2),
                         HAPPY_quantile = weighted.quantile(HAPPYnew, addedweight, probs = seq(0,1,0.25), na.rm = T, type = 2),
                         ANXIOUS_quantile = weighted.quantile(ANXIOUSnew, addedweight, probs = seq(0,1,0.25), na.rm = T, type = 2))
      
      incomecorrdf <- apsdf%>%
        dplyr::filter(GRSSWK>0 & HOURPAY>0 & HOURPAY<100)%>%
        dplyr::select(SATISnew, WORTHnew, HAPPYnew, ANXIOUSnew, addedweight, GRSSWK)%>%
        na.omit%>%
        dplyr::rename(Satisfaction = SATISnew, Worthwhile = WORTHnew, Happiness = HAPPYnew, Anxiousness = ANXIOUSnew, `Gross Weekly Income` = GRSSWK)
      
      incomecorr[[i]] <- cov.wt(incomecorrdf[,!(colnames(incomecorrdf) %in% c("addedweight"))],
                                wt = incomecorrdf$addedweight,
                                cor = TRUE)
      
      
      
    }else{
      
      incomelist[[i]] <- apsdf%>%
        dplyr::filter(GRSSWK>0)%>%
        dplyr::mutate(GRSSWKbands = case_when(GRSSWK == -9 ~ NA_character_,
                                              GRSSWK == -8 ~ NA_character_,
                                              GRSSWK >=0 & GRSSWK <100 ~ "0-99",
                                              GRSSWK >=100 & GRSSWK <200 ~ "100-199",
                                              GRSSWK >=200 & GRSSWK <300 ~ "200-299",
                                              GRSSWK >=300 & GRSSWK <400 ~ "300-399",
                                              GRSSWK >=400 & GRSSWK <500 ~ "400-499",
                                              GRSSWK >=500 & GRSSWK <600 ~ "500-599",
                                              GRSSWK >=600 & GRSSWK <700 ~ "600-699",
                                              GRSSWK >=700 & GRSSWK <800 ~ "700-799",
                                              GRSSWK >=800 & GRSSWK <900 ~ "800-899",
                                              GRSSWK >=900 & GRSSWK <1000 ~ "900-999",
                                              TRUE ~ "1000 or more"
        ))%>%
        dplyr::group_by(GRSSWKbands)%>%
        dplyr::summarise(dplyr::across(c('SATISnew', 'WORTHnew', 'HAPPYnew', 'ANXIOUSnew'), list(weightedmean = ~ stats::weighted.mean(.x, addedweight, na.rm = T),
                                                                                                 weightedmedian = ~ weighted.median(.x, addedweight, na.rm = T),
                                                                                                 weightedvariance = ~ weighted.var(.x, addedweight, na.rm = T),
                                                                                                 weightedskewness = ~ skewness.ade(.x, na.rm = T, w = addedweight),
                                                                                                 weightedkurtosis = ~ weighted_kurtosis(.x, w = addedweight, na.rm = T)), .names = "{.col}.{.fn}"),
                         unweighted_count = n(),
                         weighted_count = sum(addedweight, na.rm = T))
      
      incomequant[[i]] <- apsdf%>%
        dplyr::filter(GRSSWK>0)%>%
        dplyr::mutate(GRSSWKbands = case_when(GRSSWK == -9 ~ NA_character_,
                                              GRSSWK == -8 ~ NA_character_,
                                              GRSSWK >=0 & GRSSWK <100 ~ "0-99",
                                              GRSSWK >=100 & GRSSWK <200 ~ "100-199",
                                              GRSSWK >=200 & GRSSWK <300 ~ "200-299",
                                              GRSSWK >=300 & GRSSWK <400 ~ "300-399",
                                              GRSSWK >=400 & GRSSWK <500 ~ "400-499",
                                              GRSSWK >=500 & GRSSWK <600 ~ "500-599",
                                              GRSSWK >=600 & GRSSWK <700 ~ "600-699",
                                              GRSSWK >=700 & GRSSWK <800 ~ "700-799",
                                              GRSSWK >=800 & GRSSWK <900 ~ "800-899",
                                              GRSSWK >=900 & GRSSWK <1000 ~ "900-999",
                                              TRUE ~ "1000 or more"
        ))%>%
        dplyr::group_by(GRSSWKbands)%>%
        dplyr::summarise(SATIS_quantile = weighted.quantile(SATISnew, addedweight, probs = seq(0,1,0.25), na.rm = T, type = 2),
                         WORTH_quantile = weighted.quantile(WORTHnew, addedweight, probs = seq(0,1,0.25), na.rm = T, type = 2),
                         HAPPY_quantile = weighted.quantile(HAPPYnew, addedweight, probs = seq(0,1,0.25), na.rm = T, type = 2),
                         ANXIOUS_quantile = weighted.quantile(ANXIOUSnew, addedweight, probs = seq(0,1,0.25), na.rm = T, type = 2))
      
      incomecorrdf <- apsdf%>%
        dplyr::filter(GRSSWK>0)%>%
        dplyr::select(SATISnew, WORTHnew, HAPPYnew, ANXIOUSnew, addedweight, GRSSWK)%>%
        na.omit%>%
        dplyr::rename(Satisfaction = SATISnew, Worthwhile = WORTHnew, Happiness = HAPPYnew, Anxiousness = ANXIOUSnew, `Gross Weekly Income` = GRSSWK)
      
      incomecorr[[i]] <- cov.wt(incomecorrdf[,!(colnames(incomecorrdf) %in% c("addedweight"))],
                                wt = incomecorrdf$addedweight,
                                cor = TRUE)
      
      
    }
    
    ############################################################################
    ############## WEIGHTED STATISTICS/PERCENTILE BY INDUSTRY ##################
    ############################################################################
    
    #INDUSTRY SECTORS -> Filtering for respondents older than 16, employed for more than 6 months
    
    if (yearvec[i] %in% c("2012:13", "2013:14", "2014:15")){
      
      industrylist[[i]] <- apsdf%>%
        dplyr::filter(AGEGRP >= 4, EMPLEN >= 3)%>% 
        dplyr::group_by(INDE07M)%>%
        dplyr::summarise(dplyr::across(c('SATISnew', 'WORTHnew', 'HAPPYnew', 'ANXIOUSnew'), list(weightedmean = ~ stats::weighted.mean(.x, addedweight, na.rm = T),
                                                                                                 weightedmedian = ~ weighted.median(.x, addedweight, na.rm = T),
                                                                                                 weightedvariance = ~ weighted.var(.x, addedweight, na.rm = T),
                                                                                                 weightedskewness = ~ skewness.ade(.x, na.rm = T, w = addedweight),
                                                                                                 weightedkurtosis = ~ weighted_kurtosis(.x, w = addedweight, na.rm = T)), .names = "{.col}.{.fn}"),
                         unweighted_count = n(),
                         weighted_count = sum(addedweight, na.rm = T))
      
      industryquant[[i]] <- apsdf%>%
        dplyr::filter(AGEGRP >= 4, EMPLEN >= 3)%>% 
        dplyr::group_by(INDE07M)%>%
        dplyr::summarise(SATIS_quantile = weighted.quantile(SATISnew, addedweight, probs = seq(0,1,0.25), na.rm = T, type = 2),
                         WORTH_quantile = weighted.quantile(WORTHnew, addedweight, probs = seq(0,1,0.25), na.rm = T, type = 2),
                         HAPPY_quantile = weighted.quantile(HAPPYnew, addedweight, probs = seq(0,1,0.25), na.rm = T, type = 2),
                         ANXIOUS_quantile = weighted.quantile(ANXIOUSnew, addedweight, probs = seq(0,1,0.25), na.rm = T, type = 2))
      
      
    }else{
      
      industrylist[[i]] <- apsdf%>%
        dplyr::filter(AAGE >= 2, EMPLEN >= 3)%>% #TTACHR >= 20, EMPLEN >= 3
        dplyr::group_by(INDE07M)%>%
        dplyr::summarise(dplyr::across(c('SATISnew', 'WORTHnew', 'HAPPYnew', 'ANXIOUSnew'), list(weightedmean = ~ stats::weighted.mean(.x, addedweight, na.rm = T),
                                                                                                 weightedmedian = ~ weighted.median(.x, addedweight, na.rm = T),
                                                                                                 weightedvariance = ~ weighted.var(.x, addedweight, na.rm = T),
                                                                                                 weightedskewness = ~ skewness.ade(.x, na.rm = T, w = addedweight),
                                                                                                 weightedkurtosis = ~ weighted_kurtosis(.x, w = addedweight, na.rm = T)), .names = "{.col}.{.fn}"),
                         unweighted_count = n(),
                         weighted_count = sum(addedweight, na.rm = T))
      
      industryquant[[i]] <- apsdf%>%
        dplyr::filter(AAGE >= 2, EMPLEN >= 3)%>% #TTACHR >= 20, EMPLEN >= 3
        dplyr::group_by(INDE07M)%>%
        dplyr::summarise(SATIS_quantile = weighted.quantile(SATISnew, addedweight, probs = seq(0,1,0.25), na.rm = T, type = 2),
                         WORTH_quantile = weighted.quantile(WORTHnew, addedweight, probs = seq(0,1,0.25), na.rm = T, type = 2),
                         HAPPY_quantile = weighted.quantile(HAPPYnew, addedweight, probs = seq(0,1,0.25), na.rm = T, type = 2),
                         ANXIOUS_quantile = weighted.quantile(ANXIOUSnew, addedweight, probs = seq(0,1,0.25), na.rm = T, type = 2))
      
    }
    
    ############################################################################
    ############# WEEKLY HOURS WORKED BY INDUSTRY ##############################
    ############################################################################
    
    #HOURS BY INDUSTRY -> Including those that are older than 16 years, are not part-time students, and have been employed for more than 6 months
    
    if (yearvec[i] %in% c("2012:13", "2013:14", "2014:15")){
      
      industryhourslist[[i]] <- apsdf%>%
        dplyr::filter(AGEGRP >= 4, EMPLEN >= 3, FTPTW != 1)%>%
        dplyr::group_by(INDE07M)%>%
        dplyr::summarise(`Average Total Actual Hours` = stats::weighted.mean(TTACHR, addedweight, na.rm = T),
                         `Median Total Actual Hours` = weighted.median(TTACHR, addedweight, na.rm = T))
    }else{
      
      industryhourslist[[i]] <- apsdf%>%
        dplyr::filter(AAGE >= 2, EMPLEN >= 3, FTPTW != 1)%>%
        dplyr::group_by(INDE07M)%>%
        dplyr::summarise(`Average Total Actual Hours` = stats::weighted.mean(TTACHR, addedweight, na.rm = T),
                         `Median Total Actual Hours` = weighted.median(TTACHR, addedweight, na.rm = T))
      
    }
    
    ############################################################################
    ###### WEIGHTED STATISTICS/PERCENTILES BY HIGHEST QUALIFICATION ############
    ############################################################################
    
    #Different years require different variables for highest qualification achieved
    if (yearvec[i] %in% c("2012:13", "2013:14", "2014:15")){
      
      educationlist[[i]] <- apsdf%>%
        dplyr::group_by(HIQUL11D)%>%
        dplyr::summarise(dplyr::across(c('SATISnew', 'WORTHnew', 'HAPPYnew', 'ANXIOUSnew'), list(weightedmean = ~ stats::weighted.mean(.x, addedweight, na.rm = T),
                                                                                                 weightedmedian = ~ weighted.median(.x, addedweight, na.rm = T),
                                                                                                 weightedvariance = ~ weighted.var(.x, addedweight, na.rm = T),
                                                                                                 weightedskewness = ~ skewness.ade(.x, na.rm = T, w = addedweight),
                                                                                                 weightedkurtosis = ~ weighted_kurtosis(.x, w = addedweight, na.rm = T)), .names = "{.col}.{.fn}"),
                         unweighted_count = n(),
                         weighted_count = sum(addedweight, na.rm = T))
      
      educationquant[[i]] <- apsdf%>%
        dplyr::group_by(HIQUL11D)%>%
        dplyr::summarise(SATIS_quantile = weighted.quantile(SATISnew, addedweight, probs = seq(0,1,0.25), na.rm = T, type = 2),
                         WORTH_quantile = weighted.quantile(WORTHnew, addedweight, probs = seq(0,1,0.25), na.rm = T, type = 2),
                         HAPPY_quantile = weighted.quantile(HAPPYnew, addedweight, probs = seq(0,1,0.25), na.rm = T, type = 2),
                         ANXIOUS_quantile = weighted.quantile(ANXIOUSnew, addedweight, probs = seq(0,1,0.25), na.rm = T, type = 2))
      
      
      
    }else{
      
      educationlist[[i]] <- apsdf%>%
        dplyr::group_by(HIQUL15D)%>%
        dplyr::summarise(dplyr::across(c('SATISnew', 'WORTHnew', 'HAPPYnew', 'ANXIOUSnew'), list(weightedmean = ~ stats::weighted.mean(.x, addedweight, na.rm = T),
                                                                                                 weightedmedian = ~ weighted.median(.x, addedweight, na.rm = T),
                                                                                                 weightedvariance = ~ weighted.var(.x, addedweight, na.rm = T),
                                                                                                 weightedskewness = ~ skewness.ade(.x, na.rm = T, w = addedweight),
                                                                                                 weightedkurtosis = ~ weighted_kurtosis(.x, w = addedweight, na.rm = T)), .names = "{.col}.{.fn}"),
                         unweighted_count = n(),
                         weighted_count = sum(addedweight, na.rm = T))
      
      educationquant[[i]] <- apsdf%>%
        dplyr::group_by(HIQUL15D)%>%
        dplyr::summarise(SATIS_quantile = weighted.quantile(SATISnew, addedweight, probs = seq(0,1,0.25), na.rm = T, type = 2),
                         WORTH_quantile = weighted.quantile(WORTHnew, addedweight, probs = seq(0,1,0.25), na.rm = T, type = 2),
                         HAPPY_quantile = weighted.quantile(HAPPYnew, addedweight, probs = seq(0,1,0.25), na.rm = T, type = 2),
                         ANXIOUS_quantile = weighted.quantile(ANXIOUSnew, addedweight, probs = seq(0,1,0.25), na.rm = T, type = 2))
      
      
    }
    
    ############################################################################
    ###### WEIGHTED STATISTICS/PERCENTILES BY ETHNICITY ########################
    ############################################################################
    
    ethnicitylist[[i]] <- apsdf%>%
      dplyr::group_by(ETHUKEUL)%>%
      dplyr::summarise(dplyr::across(c('SATISnew', 'WORTHnew', 'HAPPYnew', 'ANXIOUSnew'), list(weightedmean = ~ stats::weighted.mean(.x, addedweight, na.rm = T),
                                                                                               weightedmedian = ~ weighted.median(.x, addedweight, na.rm = T),
                                                                                               weightedvariance = ~ weighted.var(.x, addedweight, na.rm = T),
                                                                                               weightedskewness = ~ skewness.ade(.x, na.rm = T, w = addedweight),
                                                                                               weightedkurtosis = ~ weighted_kurtosis(.x, w = addedweight, na.rm = T)), .names = "{.col}.{.fn}"),
                       unweighted_count = n(),
                       weighted_count = sum(addedweight, na.rm = T))
    
    ethnicityquant[[i]] <- apsdf%>%
      dplyr::group_by(ETHUKEUL)%>%
      dplyr::summarise(SATIS_quantile = weighted.quantile(SATISnew, addedweight, probs = seq(0,1,0.25), na.rm = T, type = 2),
                       WORTH_quantile = weighted.quantile(WORTHnew, addedweight, probs = seq(0,1,0.25), na.rm = T, type = 2),
                       HAPPY_quantile = weighted.quantile(HAPPYnew, addedweight, probs = seq(0,1,0.25), na.rm = T, type = 2),
                       ANXIOUS_quantile = weighted.quantile(ANXIOUSnew, addedweight, probs = seq(0,1,0.25), na.rm = T, type = 2))
    
    ############################################################################
    ############### WEIGHTED STATISTICS/PERCENTILES BY RELIGION ################
    ############################################################################
    
    religionlist[[i]] <- apsdf%>%
      dplyr::group_by(RELIG11)%>%
      dplyr::summarise(dplyr::across(c('SATISnew', 'WORTHnew', 'HAPPYnew', 'ANXIOUSnew'), list(weightedmean = ~ stats::weighted.mean(.x, addedweight, na.rm = T),
                                                                                               weightedmedian = ~ weighted.median(.x, addedweight, na.rm = T),
                                                                                               weightedvariance = ~ weighted.var(.x, addedweight, na.rm = T),
                                                                                               weightedskewness = ~ skewness.ade(.x, na.rm = T, w = addedweight),
                                                                                               weightedkurtosis = ~ weighted_kurtosis(.x, w = addedweight, na.rm = T)), .names = "{.col}.{.fn}"),
                       unweighted_count = n(),
                       weighted_count = sum(addedweight, na.rm = T))
    
    religionquant[[i]] <- apsdf%>%
      dplyr::group_by(RELIG11)%>%
      dplyr::summarise(SATIS_quantile = weighted.quantile(SATISnew, addedweight, probs = seq(0,1,0.25), na.rm = T, type = 2),
                       WORTH_quantile = weighted.quantile(WORTHnew, addedweight, probs = seq(0,1,0.25), na.rm = T, type = 2),
                       HAPPY_quantile = weighted.quantile(HAPPYnew, addedweight, probs = seq(0,1,0.25), na.rm = T, type = 2),
                       ANXIOUS_quantile = weighted.quantile(ANXIOUSnew, addedweight, probs = seq(0,1,0.25), na.rm = T, type = 2),
                       unweighted_count = n(),
                       weighted_count = sum(addedweight, na.rm = T))
    
    ############################################################################
    ########### WEIGHTED STATISTICS/PERCENTILES BY ACCOMMODATION STATUS ########
    ############################################################################
    
    accommodationlist[[i]] <- apsdf%>%
      dplyr::group_by(TEN1)%>%
      dplyr::summarise(dplyr::across(c('SATISnew', 'WORTHnew', 'HAPPYnew', 'ANXIOUSnew'), list(weightedmean = ~ stats::weighted.mean(.x, addedweight, na.rm = T),
                                                                                               weightedmedian = ~ weighted.median(.x, addedweight, na.rm = T),
                                                                                               weightedvariance = ~ weighted.var(.x, addedweight, na.rm = T),
                                                                                               weightedskewness = ~ skewness.ade(.x, na.rm = T, w = addedweight),
                                                                                               weightedkurtosis = ~ weighted_kurtosis(.x, w = addedweight, na.rm = T)), .names = "{.col}.{.fn}"),
                       unweighted_count = n(),
                       weighted_count = sum(addedweight, na.rm = T))
    
    accommodationquant[[i]] <- apsdf%>%
      dplyr::group_by(TEN1)%>%
      dplyr::summarise(SATIS_quantile = weighted.quantile(SATISnew, addedweight, probs = seq(0,1,0.25), na.rm = T, type = 2),
                       WORTH_quantile = weighted.quantile(WORTHnew, addedweight, probs = seq(0,1,0.25), na.rm = T, type = 2),
                       HAPPY_quantile = weighted.quantile(HAPPYnew, addedweight, probs = seq(0,1,0.25), na.rm = T, type = 2),
                       ANXIOUS_quantile = weighted.quantile(ANXIOUSnew, addedweight, probs = seq(0,1,0.25), na.rm = T, type = 2),
                       unweighted_count = n(),
                       weighted_count = sum(addedweight, na.rm = T))
    
    ############################################################################
    ############# WEIGHTED STATISTICS/PERCENTILES BY EMPLOYMENT STATUS #########
    ############################################################################
    
    employlist[[i]] <- apsdf%>%
      dplyr::mutate(`Employment Status` = case_when(ILODEFR == 1 ~ "In Employment", ILODEFR == 2 ~ "ILO Unemployed", ILODEFR == 3 ~ "Inactive", TRUE ~ NA_character_))%>%
      dplyr::group_by(`Employment Status`)%>%
      dplyr::summarise(dplyr::across(c('SATISnew', 'WORTHnew', 'HAPPYnew', 'ANXIOUSnew'), list(weightedmean = ~ stats::weighted.mean(.x, addedweight, na.rm = T),
                                                                                               weightedmedian = ~ weighted.median(.x, addedweight, na.rm = T),
                                                                                               weightedvariance = ~ weighted.var(.x, addedweight, na.rm = T),
                                                                                               weightedskewness = ~ skewness.ade(.x, na.rm = T, w = addedweight),
                                                                                               weightedkurtosis = ~ weighted_kurtosis(.x, w = addedweight, na.rm = T)), .names = "{.col}.{.fn}"),
                       unweighted_count = n(),
                       weighted_count = sum(addedweight, na.rm = T))
    
    employquant[[i]] <- apsdf%>%
      dplyr::mutate(`Employment Status` = case_when(ILODEFR == 1 ~ "In Employment", ILODEFR == 2 ~ "ILO Unemployed", ILODEFR == 3 ~ "Inactive", TRUE ~ NA_character_))%>%
      dplyr::group_by(`Employment Status`)%>%
      dplyr::summarise(SATIS_quantile = weighted.quantile(SATISnew, addedweight, probs = seq(0,1,0.25), na.rm = T, type = 2),
                       WORTH_quantile = weighted.quantile(WORTHnew, addedweight, probs = seq(0,1,0.25), na.rm = T, type = 2),
                       HAPPY_quantile = weighted.quantile(HAPPYnew, addedweight, probs = seq(0,1,0.25), na.rm = T, type = 2),
                       ANXIOUS_quantile = weighted.quantile(ANXIOUSnew, addedweight, probs = seq(0,1,0.25), na.rm = T, type = 2),
                       unweighted_count = n(),
                       weighted_count = sum(addedweight, na.rm = T))
    
    
  } #END OF THE LOOP
  
  #This is the list of lists that contain all the outputted data frames of the well-being statistics for all sample years
  finalList <- list("summary" = summarylist, # Un-weighted summary statistics of the well-being variables from the samples
                    "weightedlist" = weightedlist, # WEIGHTED summary statistics of the well-being from the samples
                    "weightedquantiles" = weightedquant, # WEIGHTED percentiles of the well-being variables
                    "regions" = regionlist, # WEIGHTED summary statistics of the well-being variables by region
                    "regionsquantiles" = regionquant, # WEIGHTED percentiles of the well-being variables by region
                    "correlations" = corrlist, # WEIGHTED correlations of the well-being variables 
                    "sexweightedmoments" = sexgenderlist, # WEIGHTED summary statistics of the well-being variables by Sex
                    "ageweightmoments" = agelist, # WEIGHTED summary statistics of the well-being variables by age bands
                    "incomeweightmoments" = incomelist, # WEIGHTED summary statistics of the well-being variables by income bands
                    "industrygroupmoments" = industrylist, # WEIGHTED summary statistics of the wellbeing variables by industry
                    "industryhours" = industryhourslist, # WEIGHTED mean and median of hours worked by industry
                    "educationmoments" = educationlist, # WEIGHTED summary statistics of the wellbeing variables by highest qualification achieved
                    "ethnicitymoments" = ethnicitylist, # WEIGHTED summary statistics of the wellbeing variables by ethnicity
                    "religionmoments" = religionlist, # WEIGHTED summary statistics of the wellbeing variables by religious group
                    "sexquantiles" = sexquant, # WEIGHTED percentiles of the wellbeing variables by sex
                    "agequantiles" = agequant, # WEIGHTED percentiles of the wellbeing variables by age bands
                    "incomecorrelations" = incomecorr, # WEIGHTED correlations between income and the well-being variables
                    "incomequantiles" = incomequant, # WEIGHTED percentiles of the wellbeing variables by income bands
                    "industryquantiles" = industryquant, # WEIGHTED percentiles of the wellbeing variables by industry
                    "educationquantiles" = educationquant, #WEIGHTED percentiles of the wellbeing variables by highest qualifications achieved
                    "ethnicityquantiles" = ethnicityquant, #WEIGHTED percentiles of the wellbeing variables by ethnicity
                    "religionquantiles" = religionquant, #WEIGHTED percentiles of the wellbeing variables by religious group
                    "accommodationstatusmoments" = accommodationlist, #WEIGHTED summary statistics of the wellbeing variables by housing status
                    "accommodationquantiles" = accommodationquant, #WEIGHTED percentiles of the wellbeing variables by housing status
                    "Counting the interval of well-being ratings" = benchmarkcount, #WEIGHTED proportion (and count) of the ranges of scores given for the well-being variables
                    "Weighted Statistics of the Well-Being Variables by Employment Status" = employlist,
                    "Weighted Percentiles of the Well-Being Variables by Employment Status" = employquant
  )
  
  return(finalList)
  
} #END OF FUNCTION

######################## IMPORTANT #############################################
#This runs the function with the necessary inputs which were created at the beginning of the script
apsts <- apstime(yearvec = years, dtavec = apsdta, wd = wd2122)
################################################################################

#Caution has to be taken as some .dta files may not have been read in, so must determine which years have been read
yearsloaded <- c()
yearsignored <- c()
for (i in 1:length(years)){
  
  if (i %in% which(unlist(lapply(apsts$summary, is.null)))){
    yearsignored <- append(yearsignored, years[i])
  }else{
    yearsloaded <- append(yearsloaded, years[i])
  }
}
yearsloaded <- gsub(":", "_", yearsloaded)

#Now sorting out the correlation data, as we only want the correlation matrix from every list element
corrmatrices <- list()
incomecorrmatrices <- list()
for (i in 1:length(years)){
  
  corrmatrices[[i]] <- apsts$correlations[[i]]$cor
  
  incomecorrmatrices[[i]] <- apsts$incomecorrelations[[i]]$cor
  
}

#Removing the years which didn't read from the lists

corrmatrices <- corrmatrices[!sapply(corrmatrices, is.null)]

incomecorrmatrices <- incomecorrmatrices[!sapply(incomecorrmatrices, is.null)]

names(corrmatrices) <- yearsloaded

names(incomecorrmatrices) <- yearsloaded

apsts <- lapply(apsts, function(x){x[!sapply(x, is.null)]})

apsts <- lapply(apsts, function(x){setNames(x, yearsloaded)})


#Now, must write each list as a excel file containing all the years that have been read in
#By default, this function organises the list elements into separate worksheets

openxlsx::write.xlsx(apsts$summary, file = "summarydata.xlsx") #un-weighted summary statistics of the annual population

openxlsx::write.xlsx(apsts$weightedlist, file = "weightedStatistics.xlsx") #weighted summary statistics of the annual population

openxlsx::write.xlsx(apsts$weightedquant, file = "weightedPercentiles.xlsx")

openxlsx::write.xlsx(corrmatrices, file = "correlationmatrices.xlsx")

openxlsx::write.xlsx(apsts$regions, file = "regionalWeightedStatistics.xlsx")

openxlsx::write.xlsx(apsts$regionsquant, file = "regionalWeightedPercentiles.xlsx")

openxlsx::write.xlsx(apsts$sexweightedmoments, file = "weightedStatisticsSex.xlsx")

openxlsx::write.xlsx(apsts$sexquantiles, file = "weightedPercentilesSex.xlsx")

openxlsx::write.xlsx(apsts$ageweightmoments, file = "weightedStatisticsAge.xlsx")

openxlsx::write.xlsx(apsts$agequantiles, file = "weightedPercentilesAge.xlsx")

openxlsx::write.xlsx(apsts$incomeweightmoments, file = "weightedStatisticsIncome.xlsx")

openxlsx::write.xlsx(apsts$incomequantiles, file = "weightedPercentilesIncome.xlsx")

openxlsx::write.xlsx(apsts$incomecorrelations, file = "incomecorrelationmatrices.xlsx")

openxlsx::write.xlsx(apsts$industrygroupmoments, file = "weightedStatisticsIndustry.xlsx")

openxlsx::write.xlsx(apsts$industryquantiles, file = "weightedPercentilesIndustry.xlsx")

openxlsx::write.xlsx(apsts$industryhours, file = "hoursIndustry.xlsx")

openxlsx::write.xlsx(apsts$educationmoments, file = "weightedStatisticsHighestQual.xlsx")

openxlsx::write.xlsx(apsts$educationquantiles, file = "weightedPercentilesHighestQual.xlsx")

openxlsx::write.xlsx(apsts$ethnicitymoments, file = "weightedStatisticsEthnicity.xlsx")

openxlsx::write.xlsx(apsts$ethnicityquantiles, file = "weightedPercentilesEthnicity.xlsx")

openxlsx::write.xlsx(apsts$religionmoments, file = "weightedStatisticsReligion.xlsx")

openxlsx::write.xlsx(apsts$religionquantiles, file = "weightedPercentilesReligion.xlsx")

openxlsx::write.xlsx(apsts$accommodationstatusmoments, file = "weightedStatisticsHousingStatus.xlsx")

openxlsx::write.xlsx(apsts$accommodationquantiles, file = "weightedPercentilesHousingStatus.xlsx")