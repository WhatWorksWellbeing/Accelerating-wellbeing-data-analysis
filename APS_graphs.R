

############# IMPORTANT ########################################################

# Before running this script the aps_2122_geotime.R script must have been run first

################################################################################

if (!require("corrplot")){ install.packages("corrplot")}

library(corrplot)

if (!require("ggplot2")){ install.packages("ggplot2")}

library(ggplot2)

if (!require("ggsurvey")){ install.packages("ggsurvey", dependencies = T)}

library(ggsurvey)

if (!require("epade")){ install.packages("epade", dependencies = T)}

library(epade)

################################################################################
############### WELL-BEING AND INCOME CORRELATION MATRICES #####################
################################################################################

for (i in 1:length(corrmatrices)){
  
  png(filename = paste0("corrmatrix", names(corrmatrices)[i], ".png"))
  corrplot(corrmatrices[[i]], method = 'square', order = 'AOE', addCoef.col = 'black', col = COL2('BrBG'),
           title = paste("Correlation Matrix for", names(corrmatrices)[i]))
  
  dev.off()
  
  png(filename = paste0("INCOMEcorrmatrix", names(incomecorrmatrices)[i], ".png"))
  corrplot(incomecorrmatrices[[i]], method = "square", order = "AOE", addCoef.col = "black", col = COL2('BrBG'),
           title = paste("Correlation Matrix for", names(incomecorrmatrices)[i]))
  
  dev.off()
}

################################################################################
#### PLOTTING QUANTILES FOR EACH WELL-BEING VARIABLE FOR EVERY SAMPLE YEAR #####
################################################################################

for (i in 1:length(apsts$weightedquant)){
  
  print(ggplot(data = as.data.frame(apsts$weightedquant[[i]])%>%
                 tibble::rownames_to_column("Quantile")%>%
                 tidyr::pivot_longer(!Quantile, names_to = "wellbeing variables", values_to = "ratings"))+
          aes(x = `wellbeing variables`, y = ratings)+
          geom_point(aes(fill = Quantile, colour = Quantile), size = 4, shape = 15)+
          labs(y = "Rating (out of 10)", title = paste("Quantiles for", names(apsts$weightedquant)[i])))
  
}


################################################################################
##### HISTOGRAMS, BOXPLOTS, QUANTILE REGRESSIONS ###############################
################################################################################

#Looping through the sample years to plot the graphs
for (i in 1:length(years)){
  
  #Setting the working directory and creating folders for the graphs
  wdsub <- gsub("2021:22", years[i], wd2122)
  setwd(wdsub)
  dir.create("CORRELATION MATRICES")
  dir.create("HISTOGRAM plots")
  dir.create("HISTOGRAM plots/POPULATION")
  dir.create("HISTOGRAM plots/EMPLOYMENT")
  dir.create("HISTOGRAM plots/REGION")
  dir.create("HISTOGRAM plots/SEX")
  dir.create("QUANTILE plots")
  dir.create("BOXPLOT plots")
  
  #Reading the aps dta file
  apsdata <- read_dta(apsdta[i], encoding = "Latin1")
  
  
  colnames(apsdata) <- toupper(colnames(apsdata))
  
  #Setting the well-being weight variable
  wbweight <- apsdata[,which(substr(names(apsdata), start = 1, stop = 2) == "NP")]
  wbweight <- unlist(wbweight)
  
  apsdata$addedweight <- wbweight
  
  totalweight <- sum(wbweight, na.rm = TRUE)
  
  #This is the main data frame that will be used to plot the graphs
  apsdf <- apsdata%>%
    filter(SATIS>=0 | WORTH>=0 | HAPPY>=0 | ANXIOUS>=0)%>%
    dplyr::mutate(SATISnew = case_when(SATIS == -8 ~ NA_integer_, SATIS == -9 ~ NA_integer_, TRUE ~ as.integer(SATIS)),
                  WORTHnew = case_when(WORTH == -8 ~ NA_integer_, WORTH == -9 ~ NA_integer_, TRUE ~ as.integer(WORTH)),
                  HAPPYnew = case_when(HAPPY == -8 ~ NA_integer_, HAPPY == -9 ~ NA_integer_, TRUE ~ as.integer(HAPPY)),
                  ANXIOUSnew = case_when(ANXIOUS == -8 ~ NA_integer_, ANXIOUS == -9 ~ NA_integer_, TRUE ~ as.integer(ANXIOUS)))%>%
    dplyr::mutate(propNPWT = addedweight/totalweight)%>%
    dplyr::mutate(SEXnew = case_when(SEX == -9 ~ NA_character_, SEX == -8 ~ NA_character_, SEX == 1 ~ "Male", SEX == 2 ~ "Female", TRUE ~ NA_character_))%>%
    dplyr::mutate(`Employment Status` = case_when(ILODEFR == 1 ~ "In Employment", ILODEFR == 2 ~ "ILO Unemployed", ILODEFR == 3 ~ "Inactive", TRUE ~ NA_character_))
  
  #Need to use a different regional variable for the apsdf data frame depending on the year
  if (years[i] %in% c("2012:13", "2013:14", "2014:15")){
    
    apsdf$regions <- apsdf$GOR
    
    apsdf <- apsdf%>%
      dplyr::mutate(regionsvar = case_when(regions == -9 ~ NA_integer_,
                                           regions == -8 ~ NA_integer_,
                                           regions == 3 ~ as.integer(2), #Generally, Merseyside is included in the North West
                                           TRUE ~ as.integer(regions)))
    
  }else{
    
    apsdf$regionsvar <- apsdf$GOR9D
    
  }#END OF REGION LOOP
  
  #This is the main data frame pivoted for the boxplot graphs
  apspivot <- apsdf%>%
    tidyr::pivot_longer(cols = c(SATISnew, HAPPYnew, WORTHnew, ANXIOUSnew), names_to = "wellbeing", values_to = "ratings")
  
  ##############################################################################
  ######## BOXPLOTS ############################################################
  ##############################################################################
  
  png(filename = paste0("BOXPLOT plots/Boxplot", years[i], ".png"))
  print(ggplot(apspivot, aes(x = wellbeing, y = ratings, color = wellbeing, fill = wellbeing))+
          geom_boxplot(aes(weight = addedweight))+
          geom_point(data = apspivot%>%group_by(wellbeing)%>%dplyr::summarise(wtdmedian = weighted.median(ratings, addedweight, na.rm = T)),
                     aes(x = wellbeing, y = wtdmedian), shape = 23, size = 3, fill = "white")+
          labs(title = paste("Boxplot for", years[i]))+
          labs(y = "Rating", x = "Wellbeing Variables"))
  
  dev.off()
  
  ##############################################################################
  ####### QUANTILE REGRESSION AGAINST INCOME ###################################
  ##############################################################################
  
  if ('HOURPAY' %in% names(apsdf)){
    
    png(filename = paste0("QUANTILE plots/QuantileReg", years[i], ".png"))
    print(ggplot(apspivot%>%filter(GRSSWK>0 & HOURPAY>0 & HOURPAY<100))+
            geom_point(aes(x = GRSSWK, y = `ratings`), alpha = 0.08, position = "jitter")+
            geom_quantile(aes(x = GRSSWK, y = `ratings`), formula = y ~ x + I(x^2))+
            facet_wrap(~wellbeing)+
            labs(title = paste("Quantile Regression Wellbeing ~ Income + Income^2", years[i]))+
            labs(x = "Income", y = "Wellbeing Score"))
    dev.off()
    
  }else{
    
    png(filename = paste0("QUANTILE plots/QuantileReg", years[i], ".png"))
    print(ggplot(apspivot%>%filter(GRSSWK>0))+
            geom_point(aes(x = GRSSWK, y = `ratings`), alpha = 0.08, position = "jitter")+
            geom_quantile(aes(x = GRSSWK, y = `ratings`), formula = y ~ x + I(x^2))+
            facet_wrap(~wellbeing)+
            labs(title = paste("Quantile Regression Wellbeing ~ Income + Income^2", years[i]))+
            labs(x = "Income", y = "Wellbeing Score"))
    dev.off()
    
  }
    
  ##############################################################################
  ####### HISTOGRAMS ###########################################################
  ##############################################################################
  
  #Now creating histograms for each well-being variable and dissecting the histograms
  #by certain characteristics, such as sex/gender, region etc.
  
  #Setting vectors of the well-being variables that are needed for the loop
  wbvars <- c("SATISnew", "WORTHnew", "HAPPYnew", "ANXIOUSnew")
  wbtitle <- c("Satisfaction", "Worthwhile", "Happiness", "Anxious")
  
  for(j in 1:length(wbvars)){
  
  png(filename = paste0("HISTOGRAM plots/POPULATION/", years[i], wbtitle[j], ".png"))
  print(ggplot(apsdf, aes_string(x = wbvars[j]))+
          geom_histogram(color = "darkblue", fill = "lightblue", aes(weight=addedweight), bins = 11)+
          geom_vline(aes(xintercept = weighted_mean(unlist(apsdf[,which(colnames(apsdf) == wbvars[j])]), apsdf$addedweight, na.rm = T)),
                     color = "red", linetype = "dashed", size = 1)+
          annotate("text", x = (weighted_mean(apsdf$SATISnew, apsdf$addedweight, na.rm = T)-0.1), y = 13000000, label = "Weighted Mean", angle = 90)+
          geom_vline(aes(xintercept = weighted.median(unlist(apsdf[,which(colnames(apsdf) == wbvars[j])]), apsdf$addedweight, na.rm = T)),
                     color = "red", size = 1)+
          annotate("text", x = (weighted.median(apsdf$SATISnew, apsdf$addedweight, na.rm = T)-0.1), y = 5000000, label = "Weighted Median", angle = 90)+
          labs(title = paste(wbtitle[j], years[i]))+
          labs(x = wbtitle[j], y = "Frequency"))
  
  dev.off()
  
  #By Sex
  png(filename = paste0("HISTOGRAM plots/SEX/", years[i], wbtitle[j], ".png"))
  print(ggplot(apsdf, aes_string(x = wbvars[j]))+
          geom_histogram(color = "darkblue", fill = "lightblue", aes(weight=addedweight), bins = 11)+
          geom_vline(aes(xintercept = weighted_mean(unlist(apsdf[,which(colnames(apsdf) == wbvars[j])]), apsdf$addedweight, na.rm = T)),
                     color = "red", linetype = "dashed", size = 1)+
          annotate("text", x = (weighted_mean(apsdf$SATISnew, apsdf$addedweight, na.rm = T)-0.1), y = 13000000, label = "Weighted Mean", angle = 90)+
          geom_vline(aes(xintercept = weighted.median(unlist(apsdf[,which(colnames(apsdf) == wbvars[j])]), apsdf$addedweight, na.rm = T)),
                     color = "red", size = 1)+
          annotate("text", x = (weighted.median(apsdf$SATISnew, apsdf$addedweight, na.rm = T)-0.1), y = 5000000, label = "Weighted Median", angle = 90)+
          facet_wrap(~SEXnew)+
          labs(title = paste(wbtitle[j], years[i]))+
          labs(x = wbtitle[j], y = "Frequency"))
  
  dev.off()
  
  #By Employment Status
  png(filename = paste0("HISTOGRAM plots/EMPLOYMENT/", years[i], wbtitle[j], ".png"))
  print(ggplot(apsdf, aes_string(x = wbvars[j]))+
          geom_histogram(color = "darkblue", fill = "lightblue", aes(weight=addedweight), bins = 11)+
          geom_vline(aes(xintercept = weighted_mean(unlist(apsdf[,which(colnames(apsdf) == wbvars[j])]), apsdf$addedweight, na.rm = T)),
                     color = "red", linetype = "dashed", size = 1)+
          annotate("text", x = (weighted_mean(apsdf$SATISnew, apsdf$addedweight, na.rm = T)-0.1), y = 13000000, label = "Weighted Mean", angle = 90)+
          geom_vline(aes(xintercept = weighted.median(unlist(apsdf[,which(colnames(apsdf) == wbvars[j])]), apsdf$addedweight, na.rm = T)),
                     color = "red", size = 1)+
          annotate("text", x = (weighted.median(apsdf$SATISnew, apsdf$addedweight, na.rm = T)-0.1), y = 5000000, label = "Weighted Median", angle = 90)+
          facet_wrap(~`Employment Status`)+
          labs(title = paste(wbtitle[j], years[i]))+
          labs(x = wbtitle[j], y = "Frequency"))
  
  dev.off()
  
  #By Regions
  png(filename = paste0("HISTOGRAM plots/REGION/", years[i], wbtitle[j], ".png"))
  print(ggplot(apsdf, aes_string(x = wbvars[j]))+
          geom_histogram(color = "darkblue", fill = "lightblue", aes(weight=addedweight), bins = 11)+
          geom_vline(aes(xintercept = weighted_mean(unlist(apsdf[,which(colnames(apsdf) == wbvars[j])]), apsdf$addedweight, na.rm = T)),
                     color = "red", linetype = "dashed", size = 1)+
          annotate("text", x = (weighted_mean(apsdf$SATISnew, apsdf$addedweight, na.rm = T)-0.1), y = 13000000, label = "Weighted Mean", angle = 90)+
          geom_vline(aes(xintercept = weighted.median(unlist(apsdf[,which(colnames(apsdf) == wbvars[j])]), apsdf$addedweight, na.rm = T)),
                     color = "red", size = 1)+
          annotate("text", x = (weighted.median(apsdf$SATISnew, apsdf$addedweight, na.rm = T)-0.1), y = 5000000, label = "Weighted Median", angle = 90)+
          facet_wrap(~regionsvar)+
          labs(title = paste(wbtitle[j], years[i]))+
          labs(x = wbtitle[j], y = "Frequency"))
  
  dev.off()
  
  }#END OF WELL-BEING LOOP
  
}#END OF LOOPING THROUGH THE SAMPLE YEARS

################################################################################
##### WEIGHTED MEAN AND MEDIAN OVER TIME AND GROUPED BY A GIVEN VARIABLE #######
################################################################################

# Now looking at how weighted median and mean values change over time and decomposing 
# the samples by certain characteristics 

################# POPULATION (UN-WEIGHTED) #####################################

apsmediants <- do.call(cbind.data.frame, apsts$summary)%>%
  tibble::rownames_to_column("Quantile")%>%
  filter(Quantile == "Median")%>%
  tidyr::pivot_longer(!Quantile, names_to = "WellbeingYear", values_to = "Median")%>%
  dplyr::mutate(Wellbeing = str_sub(WellbeingYear, start = 9, end = -9),
                Year = str_sub(WellbeingYear, start = 1, end = 7))%>%
  dplyr::mutate(Year = str_replace(Year, "_", "/"))

ggplot(apsmediants, aes(x = Year, y = Median, color = Wellbeing))+
  geom_point(size = 3, shape = 15)

################ REGIONS #######################################################
apsmedianregionts <- do.call(cbind.data.frame, apsts$regions)

apsmedianregionts$region <- c("North East", "North West", "Yorkshire and The Humber", "East Midlands", "West Midlands",
                              "East of England", "London", "South East", "South West", "Wales", "Scotland", "Northern Ireland")

apsmedianmeanregionts <- apsmedianregionts%>%
  dplyr::select(contains("median"), contains("mean"), region)%>%
  tidyr::pivot_longer(!region, names_to = "WellbeingMomentYear", values_to = "MeanMedian")%>%
  dplyr::mutate(Year = str_sub(WellbeingMomentYear, start = 1, end = 7),
                Moment = str_sub(WellbeingMomentYear, start = -4, end = -1),
                Wellbeing = str_sub(WellbeingMomentYear, start = 9, end = 13))

apsmedianregion <- apsmedianmeanregionts%>%
  dplyr::filter(Moment == "dian")%>%
  dplyr::mutate(Wellbeing = case_when(Wellbeing == "SATIS" ~ "Satisfaction",
                                      Wellbeing == "WORTH" ~ "Worthwhile",
                                      Wellbeing == "HAPPY" ~ "Happiness",
                                      Wellbeing == "ANXIO" ~ "Anxiousness",
                                      TRUE ~ as.character(Wellbeing)))

ggplot(apsmedianregion, aes(x = Year, y = MeanMedian, color = Wellbeing))+
  geom_point(size = 3, shape = 15)+
  facet_wrap(~region)+
  ylim(0,10)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  labs(y = "Median")

apsmeanregion <- apsmedianmeanregionts%>%
  dplyr::filter(Moment == "mean")%>%
  dplyr::mutate(Wellbeing = case_when(Wellbeing == "SATIS" ~ "Satisfaction",
                                      Wellbeing == "WORTH" ~ "Worthwhile",
                                      Wellbeing == "HAPPY" ~ "Happiness",
                                      Wellbeing == "ANXIO" ~ "Anxiousness",
                                      TRUE ~ as.character(Wellbeing)))

ggplot(apsmeanregion, aes(x = Year, y = MeanMedian, color = Wellbeing))+
  geom_point(size = 3, shape = 15)+
  facet_wrap(~region)+
  ylim(0, 10)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  labs(y = "Mean")

########## GROSS WEEKLY INCOME BANDS ###########################################

apsincomets <- do.call(cbind.data.frame, apsts$incomeweightmoments)%>%
  dplyr::select(contains("mean"), contains("median"), `2012:13.GRSSWKbands`)%>%
  dplyr::rename(WeeklyIncomeBands = `2012:13.GRSSWKbands`)%>%
  tidyr::pivot_longer(!WeeklyIncomeBands, names_to = "YearMomentsWellbeing", values_to = "MedianMean")%>%
  dplyr::mutate(Year = str_sub(YearMomentsWellbeing, start = 1, end = 7),
                Moment = str_sub(YearMomentsWellbeing, start = -4, end = -1),
                Wellbeing = str_sub(YearMomentsWellbeing, start = 9, end = 13))%>%
  dplyr::mutate(Wellbeing = case_when(Wellbeing == "SATIS" ~ "Satisfaction",
                                      Wellbeing == "WORTH" ~ "Worthwhile",
                                      Wellbeing == "HAPPY" ~ "Happiness",
                                      Wellbeing == "ANXIO" ~ "Anxiousness",
                                      TRUE ~ as.character(Wellbeing)))

apsincomemedian <- apsincomets%>%
  dplyr::filter(Moment == "dian")

ggplot(apsincomemedian, aes(x = Year, y = MedianMean, color = Wellbeing))+
  geom_point(size = 3, shape = 15)+
  facet_wrap(~WeeklyIncomeBands)+
  ylim(0, 10)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  labs(y = "Median")

apsincomemean <- apsincomets%>%
  dplyr::filter(Moment == "mean")

ggplot(apsincomemean, aes(x = Year, y = MedianMean, color = Wellbeing))+
  geom_point(size = 3, shape = 15)+
  facet_wrap(~WeeklyIncomeBands)+
  ylim(0,10)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  labs(y = "Mean")

########## GENDER/SEX ##########################################################

apsgenderts <- do.call(cbind.data.frame, apsts$sexweightedmoments)%>%
  dplyr::select(contains("mean"), contains("median"), `2012:13.SEXnew`)%>%
  dplyr::rename(Sex = `2012:13.SEXnew`)%>%
  tidyr::pivot_longer(!Sex, names_to = "YearMomentsWellbeing", values_to = "MedianMean")%>%
  dplyr::mutate(Year = str_sub(YearMomentsWellbeing, start = 1, end = 7),
                Moment = str_sub(YearMomentsWellbeing, start = -4, end = -1),
                Wellbeing = str_sub(YearMomentsWellbeing, start = 9, end = 13))%>%
  dplyr::mutate(Wellbeing = case_when(Wellbeing == "SATIS" ~ "Satisfaction",
                                      Wellbeing == "WORTH" ~ "Worthwhile",
                                      Wellbeing == "HAPPY" ~ "Happiness",
                                      Wellbeing == "ANXIO" ~ "Anxiousness",
                                      TRUE ~ as.character(Wellbeing)))

apsgendermedian <- apsgenderts%>%
  dplyr::filter(Moment == "dian")

ggplot(apsgendermedian, aes(x = Year, y = MedianMean, color = Wellbeing))+
  geom_point(size = 3, shape = 15)+
  facet_wrap(~Sex)+
  ylim(0, 10)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  labs(y = "Median")

apsgendermean <- apsgenderts%>%
  dplyr::filter(Moment == "mean")

ggplot(apsgendermean, aes(x = Year, y = MedianMean, color = Wellbeing))+
  geom_point(size = 3, shape = 15)+
  facet_wrap(~Sex)+
  ylim(0,10)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  labs(y = "Mean")

########### AGE BANDS ##########################################################

apsagets <- do.call(cbind.data.frame, apsts$ageweightmoments[c(4:length(apsts$ageweightmoments))])%>%
  dplyr::select(contains("mean"), contains("median"), `2015:16.AGEnew`)%>%
  dplyr::rename(Age = `2015:16.AGEnew`)%>%
  tidyr::pivot_longer(!Age, names_to = "YearMomentsWellbeing", values_to = "MedianMean")%>%
  dplyr::mutate(Year = str_sub(YearMomentsWellbeing, start = 1, end = 7),
                Moment = str_sub(YearMomentsWellbeing, start = -4, end = -1),
                Wellbeing = str_sub(YearMomentsWellbeing, start = 9, end = 13))%>%
  dplyr::mutate(Wellbeing = case_when(Wellbeing == "SATIS" ~ "Satisfaction",
                                      Wellbeing == "WORTH" ~ "Worthwhile",
                                      Wellbeing == "HAPPY" ~ "Happiness",
                                      Wellbeing == "ANXIO" ~ "Anxiousness",
                                      TRUE ~ as.character(Wellbeing)))

apsagemedian <- apsagets%>%
  dplyr::filter(Moment == "dian")

ggplot(apsagemedian, aes(x = Year, y = MedianMean, color = Wellbeing))+
  geom_point(size = 3, shape = 15)+
  facet_wrap(~Age)+
  ylim(0,10)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  labs(y = "Median")

apsagemean <- apsagets%>%
  dplyr::filter(Moment == "mean")

ggplot(apsagemean, aes(x = Year, y = MedianMean, color = Wellbeing))+
  geom_point(size = 3, shape = 15)+
  facet_wrap(~Age)+
  ylim(0, 10)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  labs(y = "Mean")

############ POPULATION (WEIGHTED) #############################################

apsgeneralts <- do.call(cbind.data.frame, apsts$weightedlist)%>%
  dplyr::select(contains("mean"), contains("median"), `2012:13.wellbeing`)%>%
  dplyr::rename(wellbeing = `2012:13.wellbeing`)%>%
  tidyr::pivot_longer(!wellbeing, names_to = "YearMoments", values_to = "ratings")%>%
  dplyr::mutate(Year = str_sub(YearMoments, start = 1, end = 7),
                Moments = str_sub(YearMoments, start = -4, end = -1))%>%
  dplyr::mutate(Wellbeing = case_when(wellbeing == "SATISnew" ~ "Satisfaction",
                                      wellbeing == "WORTHnew" ~ "Worthwhile",
                                      wellbeing == "HAPPYnew" ~ "Happiness",
                                      wellbeing == "ANXIOUSnew" ~ "Anxiousness",
                                      TRUE ~ as.character(wellbeing)),
                moments = case_when(Moments == "dian" ~ "Median",
                                    Moments == "mean" ~ "Mean",
                                    TRUE ~ as.character(Moments)))

ggplot(apsgeneralts, aes(x = Year, y = ratings, color = Wellbeing))+
  geom_point(size = 3, shape = 15)+
  facet_wrap(~moments)+
  ylim(0,10)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  labs(y = "Median/Mean")

######### INDUSTRY #############################################################

apsindustryts <- do.call(cbind.data.frame, apsts$industrygroupmoments)%>%
  dplyr::select(contains("mean"), contains("median"), `2012_13.INDE07M`)%>%
  dplyr::rename(industry = `2012_13.INDE07M`)%>%
  tidyr::pivot_longer(!industry, names_to = "YearMoment", values_to = "ratings")%>%
  dplyr::mutate(Year = str_sub(YearMoment, start = 1, end = 7),
                Moments = str_sub(YearMoment, start = -4, end = -1),
                wellbeing = str_sub(YearMoment, start = 9, end = 13))%>%
  dplyr::mutate(Wellbeing = case_when(wellbeing == "SATIS" ~ "Satisfaction",
                                      wellbeing == "WORTH" ~ "Worthwhile",
                                      wellbeing == "HAPPY" ~ "Happiness",
                                      wellbeing == "ANXIO" ~ "Anxiousness",
                                      TRUE ~ as.character(wellbeing)),
                moments = case_when(Moments == "dian" ~ "Median",
                                    Moments == "mean" ~ "Mean",
                                    TRUE ~ as.character(Moments)),
                Year = str_replace(Year, "_", ":"))%>%
  dplyr::filter(industry != -8)%>%
  dplyr::mutate(Industry = case_when(industry == 1 ~ "Agriculture, forestry and fishing",
                                     industry == 2 ~ "Energy and water",
                                     industry == 3 ~ "Manufacturing",
                                     industry == 4 ~ "Construction",
                                     industry == 5 ~ "Distribution, hotels and restaurants",
                                     industry == 6 ~ "Transport and communication",
                                     industry == 7 ~ "Banking and finance",
                                     industry == 8 ~ "Public admin, education and health",
                                     industry == 9 ~ "Other services",
                                     TRUE ~ as.character(industry)))

apsindustrymedian <- apsindustryts%>%
  dplyr::filter(moments == "Median")

ggplot(apsindustrymedian, aes(x = Year, y = ratings, color = Wellbeing))+
  geom_point(size = 3, shape = 15)+
  facet_wrap(~Industry)+
  ylim(0,10)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  labs(y = "Median")

apsindustrymean <- apsindustryts%>%
  dplyr::filter(moments == "Mean")

ggplot(apsindustrymean, aes(x = Year, y = ratings, color = Wellbeing))+
  geom_point(size = 3, shape = 15)+
  facet_wrap(~Industry)+
  ylim(0,10)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  labs(y = "Mean")

########## WEEKLY HOURS WORKED BY INDUSTRY #####################################

apsindustryhours <- do.call(cbind.data.frame, apsts$industryhours)%>%
  dplyr::select(contains("Average"), contains("Median"), `2012_13.INDE07M`)%>%
  dplyr::rename(industry = `2012_13.INDE07M`)%>%
  tidyr::pivot_longer(!industry, names_to = "YearMoment", values_to = "ratings")%>%
  dplyr::mutate(Year = str_sub(YearMoment, start = 1, end = 7),
                Moment = str_sub(YearMoment, start = 9, end = -1))%>%
  dplyr::mutate(Year = str_replace(Year, "_", ":"))%>%
  dplyr::filter(industry != -8)%>%
  dplyr::mutate(Industry = case_when(industry == 1 ~ "Agriculture, forestry and fishing",
                                     industry == 2 ~ "Energy and water",
                                     industry == 3 ~ "Manufacturing",
                                     industry == 4 ~ "Construction",
                                     industry == 5 ~ "Distribution, hotels and restaurants",
                                     industry == 6 ~ "Transport and communication",
                                     industry == 7 ~ "Banking and finance",
                                     industry == 8 ~ "Public admin, education and health",
                                     industry == 9 ~ "Other services",
                                     TRUE ~ as.character(industry)))

apsindustryhoursmean <- apsindustryhours%>%
  dplyr::filter(Moment == "Average Total Actual Hours")

ggplot(apsindustryhoursmean, aes(x = Year, y = ratings, color = Industry))+
  geom_point(size = 3, shape = 15)+
  facet_wrap(~Industry)

########## HIGHEST QUALIFICATION ###############################################

apseducts <- do.call(cbind.data.frame, apsts$educationmoments)%>%
  dplyr::select(contains("mean"), contains("median"), `2012:13.HIQUL11D`)%>%
  dplyr::rename(education = `2012:13.HIQUL11D`)%>%
  dplyr::filter(education != -9, education != -8, education != 2, education != 5, education != 7)%>%
  tidyr::pivot_longer(!education, names_to = "YearMoment", values_to = "ratings")%>%
  dplyr::mutate(Education = case_when(education == 1 ~ "Degree or equivalent",
                                      education == 3 ~ "GCE A level or equivalent",
                                      education == 4 ~ "GCSE grades A* - C or equivalent",
                                      education == 6 ~ "No qualification",
                                      TRUE ~ as.character(education)))%>%
  dplyr::mutate(Year = str_sub(YearMoment, start = 1, end = 7),
                Moment = str_sub(YearMoment, start = -4, end = -1),
                Wellbeing = str_sub(YearMoment, start = 9, end = 13))%>%
  dplyr::mutate(Wellbeing = case_when(Wellbeing == "SATIS" ~ "Satisfaction",
                                      Wellbeing == "WORTH" ~ "Worthwhile",
                                      Wellbeing == "HAPPY" ~ "Happiness",
                                      Wellbeing == "ANXIO" ~ "Anxiousness",
                                      TRUE ~ as.character(Wellbeing)))

apseducmean <- apseducts%>%
  dplyr::filter(Moment == "mean")

ggplot(apseducmean, aes(x = Year, y = ratings, color = Wellbeing))+
  geom_point(size = 3, shape = 15)+
  facet_wrap(~Education)+
  ylim(0,10)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  labs(y = "Mean")

apseducmedian <- apseducts%>%
  dplyr::filter(Moment == "dian")

ggplot(apseducmedian, aes(x = Year, y = ratings, color = Wellbeing))+
  geom_point(size = 3, shape = 15)+
  facet_wrap(~Education)+
  ylim(0,10)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  labs(y = "Median")

############## ETHNICITY #######################################################

apsethnicityts <- do.call(cbind.data.frame, apsts$ethnicitymoments)%>%
  dplyr::select(contains("mean"), contains("median"), `2012:13.ETHUKEUL`)%>%
  dplyr::rename(ethnicity = `2012:13.ETHUKEUL`)%>%
  tidyr::pivot_longer(!ethnicity, names_to = "YearMoment", values_to = "ratings")%>%
  dplyr::filter(ethnicity != -9, ethnicity != -8)%>%
  dplyr::mutate(Year = str_sub(YearMoment, start = 1, end = 7),
                Moment = str_sub(YearMoment, start = -4, end = -1),
                wellbeing = str_sub(YearMoment, start = 9, end = 13))%>%
  dplyr::mutate(Ethnicity = case_when(ethnicity == 1 ~ "White",
                                      ethnicity == 2 ~ "Mixed ethnic groups",
                                      ethnicity == 3 ~ "Indian",
                                      ethnicity == 4 ~ "Pakistani",
                                      ethnicity == 5 ~ "Bangladeshi",
                                      ethnicity == 6 ~ "Chinese",
                                      ethnicity == 7 ~ "Any other Asian background",
                                      ethnicity == 8 ~ "Black/African/Caribbean/Black British",
                                      ethnicity == 9 ~ "Other ethnic group",
                                      TRUE ~ as.character(ethnicity)),
                Wellbeing = case_when(wellbeing == "SATIS" ~ "Satisfaction",
                                      wellbeing == "WORTH" ~ "Worthwhile",
                                      wellbeing == "HAPPY" ~ "Happiness",
                                      wellbeing == "ANXIO" ~ "Anxiousness",
                                      TRUE ~ as.character(wellbeing)))

apsethnicitymean <- apsethnicityts%>%
  dplyr::filter(Moment == "mean")


ggplot(apsethnicitymean, aes(x = Year, y = ratings, color = Wellbeing))+
  geom_point(size = 3, shape = 15)+
  facet_wrap(~Ethnicity)+
  ylim(0, 10)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  labs(y = "Mean")

apsethnicitymedian <- apsethnicityts%>%
  dplyr::filter(Moment == "dian")

ggplot(apsethnicitymedian, aes(x = Year, y = ratings, color = Wellbeing))+
  geom_point(size = 3, shape = 15)+
  facet_wrap(~Ethnicity)+
  ylim(0, 10)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  labs(y = "Median")

############ RELIGION ##########################################################

apsreligionts <- do.call(cbind.data.frame, apsts$religionmoments)%>%
  dplyr::select(contains("mean"), contains("median"), `2012:13.RELIG11`)%>%
  dplyr::rename(religion = `2012:13.RELIG11`)%>%
  tidyr::pivot_longer(!religion, names_to = "YearMoment", values_to = "ratings")%>%
  dplyr::filter(religion != -9, religion != -8)%>%
  dplyr::mutate(Year = str_sub(YearMoment, start = 1, end = 7),
                Moment = str_sub(YearMoment, start = -4, end = -1),
                wellbeing = str_sub(YearMoment, start = 9, end = 13))%>%
  dplyr::mutate(Religion = case_when(religion == 1 ~ "No Religion",
                                     religion == 2 ~ "Christian (all denominations)",
                                     religion == 3 ~ "Buddhist",
                                     religion == 4 ~ "Hindu",
                                     religion == 5 ~ "Jewish",
                                     religion == 6 ~ "Muslim",
                                     religion == 7 ~ "Sikh",
                                     religion == 8 ~ "Any other Religion",
                                     TRUE ~ as.character(religion)),
                Wellbeing = case_when(wellbeing == "SATIS" ~ "Satisfaction",
                                      wellbeing == "WORTH" ~ "Worthwhile",
                                      wellbeing == "HAPPY" ~ "Happiness",
                                      wellbeing == "ANXIO" ~ "Anxiousness",
                                      TRUE ~ as.character(wellbeing)))

apsreligionmean <- apsreligionts%>%
  dplyr::filter(Moment == "mean")

ggplot(apsreligionmean, aes(x = Year, y = ratings, color = Wellbeing))+
  geom_point(size = 3, shape = 15)+
  facet_wrap(~Religion)+
  ylim(0, 10)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  labs(y = "Mean")

apsreligionmedian <- apsreligionts%>%
  dplyr::filter(Moment == "dian")

ggplot(apsreligionmedian, aes(x = Year, y = ratings, color = Wellbeing))+
  geom_point(size = 3, shape = 15)+
  facet_wrap(~Religion)+
  ylim(0, 10)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  labs(y = "Median")

############# HOUSING STATUS ###################################################

apshousingstatusts <- purrr::map(apsts$accommodationstatusmoments, ~dplyr::filter(.x, TEN1 > 0))

apshousingstatusts <- do.call(cbind.data.frame, apshousingstatusts)%>%
  dplyr::select(contains("mean"), contains("median"), `2012:13.TEN1`)%>%
  dplyr::rename(housingstatus = `2012:13.TEN1`)%>%
  tidyr::pivot_longer(!housingstatus, names_to = "YearMoment", values_to = "ratings")%>%
  dplyr::mutate(Year = str_sub(YearMoment, start = 1, end = 7),
                Moment = str_sub(YearMoment, start = -4, end = -1),
                wellbeing = str_sub(YearMoment, start = 9, end = 13))%>%
  dplyr::mutate(housingstatus = case_when(housingstatus == 1 ~ "Owned outright",
                                          housingstatus == 2 ~ "Being bought with mortgage or loan",
                                          housingstatus == 3 ~ "Part rent, part mortgage",
                                          housingstatus == 4 ~ "Rented",
                                          housingstatus == 5 ~ "Rent free or squatted",
                                          housingstatus == 6 ~ "Squatting",
                                          TRUE ~ as.character(housingstatus)),
                Wellbeing = case_when(wellbeing == "SATIS" ~ "Satisfaction",
                                      wellbeing == "WORTH" ~ "Worthwhile",
                                      wellbeing == "HAPPY" ~ "Happiness",
                                      wellbeing == "ANXIO" ~ "Anxiousness",
                                      TRUE ~ as.character(wellbeing)))

apshousingstatusmean <- apshousingstatusts%>%
  dplyr::filter(Moment == "mean")

png(filename = "Housing Status Mean.png", width = 700, height = 700)
ggplot(apshousingstatusmean, aes(x = Year, y = ratings, color = Wellbeing))+
  geom_point(size = 3, shape = 15)+
  facet_wrap(~housingstatus)+
  ylim(0, 10)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  labs(y = "Mean", x = "Housing Status")+
  guides(fill=guide_legend(title="Housing Status"))
dev.off()

apshousingstatusmedian <- apshousingstatusts%>%
  dplyr::filter(Moment == "dian")

png(filename = "Housing Status Median.png", width = 700, height = 700)
ggplot(apshousingstatusmedian, aes(x = Year, y = ratings, color = Wellbeing))+
  geom_point(size = 3, shape = 15)+
  facet_wrap(~housingstatus)+
  ylim(0, 10)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  labs(y = "Median", x = "Housing Status")+
  guides(fill=guide_legend(title="Housing Status"))
dev.off()
