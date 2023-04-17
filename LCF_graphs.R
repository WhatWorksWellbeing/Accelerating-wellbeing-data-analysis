

############# IMPORTANT ########################################################

# Before running this script the lcf_2021_geotime_weight.R script must have been run first

################################################################################

# Loading packages
if (!require("corrplot")){ install.packages("corrplot")}

library(corrplot)

if (!require("ggplot2")){ install.packages("ggplot2")}

library(ggplot2)

if (!require("ggsurvey")){ install.packages("ggsurvey", dependencies = T)}

library(ggsurvey)

if (!require("epade")){ install.packages("epade", dependencies = T)}

library(epade)

if (!require("ggcorrplot")){ install.packages("ggcorrplot", dependencies = T)}

library(ggcorrplot)

################################################################################

#Setting working directory
setwd(wd2021)

#Creating new files in the working directory
dir.create("MEDIAN & MEAN plots")
dir.create("CORRELATION MATRICES")
dir.create("HISTOGRAM plots")
dir.create("HISTOGRAM plots/POPULATION")
dir.create("HISTOGRAM plots/EMPLOYMENT")
dir.create("HISTOGRAM plots/REGION")
dir.create("HISTOGRAM plots/SEX")
dir.create("QUANTILE plots")

################################################################################
#### LOOPING TO CREATE HISTOGRAMS, CORRELATION MATRICES AND REGRESSION PLOTS ###
################################################################################

#Looping through every sample year to create correlation matrices, histograms and regression plots
for(i in 1:length(years)){
  
  #Setting working directory and creating folders for each year
  wdsub <- gsub("2020:21", years[i], wd2021)
  setwd(wdsub)
  dir.create("CORRELATION MATRICES")
  dir.create("HISTOGRAM plots")
  dir.create("HISTOGRAM plots/POPULATION")
  dir.create("HISTOGRAM plots/EMPLOYMENT")
  dir.create("HISTOGRAM plots/REGION")
  dir.create("HISTOGRAM plots/SEX")
  dir.create("QUANTILE plots")
  
  #Reading raw, personal data and derived variable household data
  lcfdata <- read_dta(lcfdta[i], encoding = "Latin1")
  
  colnames(lcfdata) <- toupper(colnames(lcfdata))
  
  lcfdvhhdata <- read_dta(lcfdvhousedta[i], encoding = "Latin1")
  
  colnames(lcfdvhhdata) <- toupper(colnames(lcfdvhhdata))
  
  #Determining which well-being weight to use depending on the sample year
  if(is.null(lcfdata$WEIGHTWB)){
    
    #lcfmerge <- merge(lcfdata%>%filter(HRPID == 1), lcfdvhhdata%>%dplyr::select(CASE, WEIGHTA), by = "CASE")
    lcfmerge <- merge(lcfdata, lcfdvhhdata%>%dplyr::select(CASE, WEIGHTA, GORX), by = "CASE")
    
    lcfmerge$addedweight <- lcfmerge$WEIGHTA
    
  }else{
    
    lcfmerge <- merge(lcfdata, lcfdvhhdata%>%dplyr::select(CASE, WEIGHTA, GORX), by = "CASE", all.x = TRUE)
    
    lcfmerge$addedweight <- lcfmerge$WEIGHTWB
    
    print(paste(years[i], "has weightwb"))
    
  }
  
  #This is the main data frame that will be used to create the plots for each sample year
  lcfdf <- lcfmerge%>%
    filter(SATIS>=0 | WORTH>=0 | HAPPY>=0 | ANXIOUS>=0)%>%
    dplyr::mutate(SATISnew = case_when(SATIS == -8 ~ NA_integer_, SATIS == -9 ~ NA_integer_, SATIS == 98 ~ NA_integer_, SATIS == 99 ~ NA_integer_, TRUE ~ as.integer(SATIS)),
                  WORTHnew = case_when(WORTH == -8 ~ NA_integer_, WORTH == -9 ~ NA_integer_, WORTH == 98 ~ NA_integer_, WORTH == 99 ~ NA_integer_, TRUE ~ as.integer(WORTH)),
                  HAPPYnew = case_when(HAPPY == -8 ~ NA_integer_, HAPPY == -9 ~ NA_integer_, HAPPY == 98 ~ NA_integer_, HAPPY == 99 ~ NA_integer_, TRUE ~ as.integer(HAPPY)),
                  ANXIOUSnew = case_when(ANXIOUS == -8 ~ NA_integer_, ANXIOUS == -9 ~ NA_integer_, ANXIOUS == 98 ~ NA_integer_, ANXIOUS == 99 ~ NA_integer_, TRUE ~ as.integer(ANXIOUS)))%>%
    dplyr::mutate(SEXnew = case_when(SEX == -9 ~ NA_character_, SEX == -8 ~ NA_character_, SEX == 1 ~ "Male", SEX == 2 ~ "Female", TRUE ~ NA_character_))%>%
    dplyr::rename(`Employment Status` = DVILO3A)%>%
    dplyr::mutate(`Employment Status` = case_when(`Employment Status` == 1 ~ "In Employment", `Employment Status` == 2 ~ "ILO Unemployed",
                                                  `Employment Status` == 3 ~ "Economically Inactive", TRUE ~ NA_character_))%>%
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
                                     TRUE ~ NA_character_))
  
  #This data frame will be used to create the regression fit plots
  lcfpivot <- lcfdf%>%
    dplyr::rename(Satis = SATISnew, Worth = WORTHnew, Happy = HAPPYnew, Anxious = ANXIOUSnew)%>%
    tidyr::pivot_longer(c(Satis, Worth, Happy, Anxious), names_to = "Wellbeing", values_to = "Wellbeing Score")
  
  ##############################################################################
  ##################### CORRELATION PLOTS ######################################
  ##############################################################################
  
  #Correlation matrix for the well-being variables
  png(filename = paste0("CORRELATION MATRICES/CorrelationMatrix", years[i], ".png"))
  print(corrplot(lcflist$`Correlation Matrices`[[i]]$cor, method = 'square', order = 'AOE', addCoef.col = 'black', col = COL2('BrBG'),
           title = paste("Correlation Matrix for", names(lcflist$`Correlation Matrices`)[i])))
  dev.off()
  
  #Correlation matrix for the well-being variables and income
  png(filename = paste0("CORRELATION MATRICES/CorrelationINCOMEMatrix", years[i], ".png"))
  print(corrplot(lcflist$`Income Correlation Matrices`[[i]]$cor, method = 'square', order = 'AOE', addCoef.col = 'black', col = COL2('BrBG'),
                 title = paste("Income Correlation Matrix for", names(lcflist$`Income Correlation Matrices`)[i])))
  dev.off()
  
  ##############################################################################
  ##################### QUANTILE REGRESSION AGAINST INCOME #####################
  ##############################################################################
  
  #Plotting the predicted values from a quantile regression of the 4 well-being variables against income
  # For the plots, the well-being variables are regressed on income and income squared
  #NOTE: depending on the sample year a certain income variable is used
  if(years[i] %in% c("2014", "2015:16", "2016:17")){
  
  png(filename = paste0("QUANTILE plots/QuantileReg", years[i], ".png"))
  print(ggplot(lcfpivot%>%filter(GWKINC>0 & GWKINC<2500))+
    geom_point(aes(x = GWKINC, y = `Wellbeing Score`), alpha = 0.08, position = "jitter")+
    geom_quantile(aes(x = GWKINC, y = `Wellbeing Score`), formula = y ~ x + I(x^2))+
    facet_wrap(~Wellbeing)+
    labs(title = paste("Quantile Regression Wellbeing ~ Income + Income^2", years[i]))+
    labs(x = "Income", y = "Wellbeing Score"))
  dev.off()
    
  }else{
    
    png(filename = paste0("QUANTILE plots/QuantileReg", years[i], ".png"))
    print(ggplot(lcfpivot%>%filter(GWKINCP>0 & GWKINCP<2500))+
      geom_point(aes(x = GWKINCP, y = `Wellbeing Score`), alpha = 0.08, position = "jitter")+
      geom_quantile(aes(x = GWKINCP, y = `Wellbeing Score`), formula = y ~ x + I(x^2))+
      facet_wrap(~Wellbeing)+
      labs(title = paste("Quantile Regression Wellbeing ~ Income + Income^2", years[i]))+
      labs(x = "Income", y = "Wellbeing Score"))
    dev.off()
  }
  
  ##############################################################################
  #################### LOOPING #################################################
  ##############################################################################
  
  #Now creating histograms for each well-being variable and dissecting the histograms
  #by certain characteristics, such as sex/gender, region etc.
  
  #Setting vectors of the well-being variables that are needed for the loop
  wbvars <- c("SATISnew", "WORTHnew", "HAPPYnew", "ANXIOUSnew")
  wbtitle <- c("Satisfaction", "Worthwhile", "Happiness", "Anxious")
  
  for(j in 1:length(wbvars)){
    
  #################### HISTOGRAMS ##############################################
  
    png(filename = paste0("HISTOGRAM plots/POPULATION", years[i], wbtitle[j], ".png"))
    print(ggplot(lcfdf, aes_string(x = wbvars[j]))+
            geom_histogram(color = "darkblue", fill = "lightblue", aes(weight=addedweight), bins = 11)+
            geom_vline(aes(xintercept = weighted_mean(lcfdf[,which(colnames(lcfdf) == wbvars[j])], lcfdf$addedweight, na.rm = T)),
                       color = "red", linetype = "dashed", size = 1)+
            annotate("text", x = (weighted_mean(lcfdf[,which(colnames(lcfdf) == wbvars[j])], lcfdf$addedweight, na.rm = T)-0.1), y = 10000, label = "Weighted Mean", angle = 90)+
            geom_vline(aes(xintercept = weighted.median(lcfdf[,which(colnames(lcfdf) == wbvars[j])], lcfdf$addedweight, na.rm = T)),
                       color = "red", size = 1)+
            annotate("text", x = (weighted.median(lcfdf[,which(colnames(lcfdf) == wbvars[j])], lcfdf$addedweight, na.rm = T)-0.1), y = 6000, label = "Weighted Median", angle = 90)+
            labs(title = paste(wbtitle[j], years[i]))+
            labs(x = wbtitle[j], y = "Frequency"))
    dev.off()
    
    #By Employment Status
    png(filename = paste0("HISTOGRAM plots/EMPLOYMENT", years[i], wbtitle[j], ".png"))
    print(ggplot(lcfdf, aes_string(x = wbvars[j]))+
            geom_histogram(color = "darkblue", fill = "lightblue", aes(weight=addedweight), bins = 11)+
            geom_vline(aes(xintercept = weighted_mean(lcfdf[,which(colnames(lcfdf) == wbvars[j])], lcfdf$addedweight, na.rm = T)),
                       color = "red", linetype = "dashed", size = 1)+
            annotate("text", x = (weighted_mean(lcfdf[,which(colnames(lcfdf) == wbvars[j])], lcfdf$addedweight, na.rm = T)-0.1), y = 7000, label = "Weighted Mean", angle = 90)+
            geom_vline(aes(xintercept = weighted.median(lcfdf[,which(colnames(lcfdf) == wbvars[j])], lcfdf$addedweight, na.rm = T)),
                       color = "red", size = 1)+
            annotate("text", x = (weighted.median(lcfdf[,which(colnames(lcfdf) == wbvars[j])], lcfdf$addedweight, na.rm = T)-0.1), y = 6000, label = "Weighted Median", angle = 90)+
            facet_wrap(~`Employment Status`)+
            labs(title = paste(wbtitle[j], years[i]))+
            labs(x = wbtitle[j], y = "Frequency"))
    dev.off()
    
    #By Regions
    png(filename = paste0("HISTOGRAM plots/REGION", years[i], wbtitle[j], ".png"))
    print(ggplot(lcfdf, aes_string(x = wbvars[j]))+
            geom_histogram(color = "darkblue", fill = "lightblue", aes(weight=addedweight), bins = 11)+
            geom_vline(aes(xintercept = weighted_mean(lcfdf[,which(colnames(lcfdf) == wbvars[j])], lcfdf$addedweight, na.rm = T)),
                       color = "red", linetype = "dashed", size = 1)+
            annotate("text", x = (weighted_mean(lcfdf[,which(colnames(lcfdf) == wbvars[j])], lcfdf$addedweight, na.rm = T)-0.1), y = 7000, label = "Weighted Mean", angle = 90)+
            geom_vline(aes(xintercept = weighted.median(lcfdf[,which(colnames(lcfdf) == wbvars[j])], lcfdf$addedweight, na.rm = T)),
                       color = "red", size = 1)+
            annotate("text", x = (weighted.median(lcfdf[,which(colnames(lcfdf) == wbvars[j])], lcfdf$addedweight, na.rm = T)-0.1), y = 6000, label = "Weighted Median", angle = 90)+
            facet_wrap(~Region)+
            labs(title = paste(wbtitle[j], years[i]))+
            labs(x = wbtitle[j], y = "Frequency"))
    dev.off()
    
    #By Sex
    png(filename = paste0("HISTOGRAM plots/SEX", years[i], wbtitle[j], ".png"))
    print(ggplot(lcfdf, aes_string(x = wbvars[j]))+
            geom_histogram(color = "darkblue", fill = "lightblue", aes(weight=addedweight), bins = 11)+
            geom_vline(aes(xintercept = weighted_mean(lcfdf[,which(colnames(lcfdf) == wbvars[j])], lcfdf$addedweight, na.rm = T)),
                       color = "red", linetype = "dashed", size = 1)+
            annotate("text", x = (weighted_mean(lcfdf[,which(colnames(lcfdf) == wbvars[j])], lcfdf$addedweight, na.rm = T)-0.1), y = 7000, label = "Weighted Mean", angle = 90)+
            geom_vline(aes(xintercept = weighted.median(lcfdf[,which(colnames(lcfdf) == wbvars[j])], lcfdf$addedweight, na.rm = T)),
                       color = "red", size = 1)+
            annotate("text", x = (weighted.median(lcfdf[,which(colnames(lcfdf) == wbvars[j])], lcfdf$addedweight, na.rm = T)-0.1), y = 6000, label = "Weighted Median", angle = 90)+
            facet_wrap(~SEXnew)+
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

################## REGIONS #####################################################

lcfregionts <- do.call(cbind.data.frame, lcflist$`Summary Statistics by Region`)%>%
  dplyr::select(contains("median"), contains("mean"), `2014.Region`)%>%
  dplyr::rename(Region = `2014.Region`)%>%
  tidyr::pivot_longer(!Region, names_to = "YearMomentsWellbeing", values_to = "MeanMedian")%>%
  dplyr::mutate(Year = str_sub(YearMomentsWellbeing, start = 1, end = 4),
                Moment = str_sub(YearMomentsWellbeing, start = -4, end = -1),
                Wellbeing = str_sub(YearMomentsWellbeing, start = 9, end = 13))%>%
  dplyr::mutate(Wellbeing = case_when(Wellbeing == "Satis" | Wellbeing == "is.we" ~ "Satisfaction",
                                      Wellbeing == "Worth" | Wellbeing == "th.we" ~ "Worthwhile",
                                      Wellbeing == "Happy" | Wellbeing == "py.we" ~ "Happiness",
                                      Wellbeing == "Anxio" | Wellbeing == "ious." ~ "Anxiousness",
                                      TRUE ~ as.character(Wellbeing)))

lcfregionmedian <- lcfregionts%>%
  dplyr::filter(Moment == "dian")

png(filename = paste0("MEDIAN & MEAN plots/REGIONMEDIAN.png"))
ggplot(lcfregionmedian, aes( x = Year, y = MeanMedian, color = Wellbeing))+
  geom_point(size = 3, shape = 15)+
  facet_wrap(~Region)+
  ylim(0,10)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  labs(y = "Median")
dev.off()

lcfregionmean <- lcfregionts%>%
  dplyr::filter(Moment == "mean")

png(filename = paste0("MEDIAN & MEAN plots/REGIONMEAN.png"))
ggplot(lcfregionmean, aes( x = Year, y = MeanMedian, color = Wellbeing))+
  geom_point(size = 3, shape = 15)+
  facet_wrap(~Region)+
  ylim(0,10)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  labs(y = "Mean")
dev.off()

################## EMPLOYMENT STATUS ###########################################

lcfemployts <- do.call(cbind.data.frame, lcflist$`Summary Statistics by Employment Status`)%>%
  dplyr::select(contains("mean"), contains("median"), `2014.Employment Status`)%>%
  dplyr::rename(`Employment Status` = `2014.Employment Status`)%>%
  tidyr::pivot_longer(!`Employment Status`, names_to = "YearMomentsWellbeing", values_to = "MeanMedian")%>%
  dplyr::mutate(Year = str_sub(YearMomentsWellbeing, start = 1, end = 4),
                Moment = str_sub(YearMomentsWellbeing, start = -4, end = -1),
                Wellbeing = str_sub(YearMomentsWellbeing, start = 9, end = 13))%>%
  dplyr::mutate(Wellbeing = case_when(Wellbeing == "Satis" | Wellbeing == "is.we" ~ "Satisfaction",
                                      Wellbeing == "Worth" | Wellbeing == "th.we" ~ "Worthwhile",
                                      Wellbeing == "Happy" | Wellbeing == "py.we" ~ "Happiness",
                                      Wellbeing == "Anxio" | Wellbeing == "ious." ~ "Anxiousness",
                                      TRUE ~ as.character(Wellbeing)))

lcfemploymedian <- lcfemployts%>%
  dplyr::filter(Moment == "dian")

png(filename = paste0("MEDIAN & MEAN plots/EmploymentMEDIAN.png"))
ggplot(lcfemploymedian, aes( x = Year, y = MeanMedian, color = Wellbeing))+
  geom_point(size = 3, shape = 15)+
  facet_wrap(~`Employment Status`)+
  ylim(0,10)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  labs(y = "Median")
dev.off()

lcfemploymean <- lcfemployts%>%
  dplyr::filter(Moment == "mean")

png(filename = paste0("MEDIAN & MEAN plots/EmploymentMEAN.png"))
ggplot(lcfemploymean, aes( x = Year, y = MeanMedian, color = Wellbeing))+
  geom_point(size = 3, shape = 15)+
  facet_wrap(~`Employment Status`)+
  ylim(0,10)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  labs(y = "Mean")
dev.off()

############### SEX ############################################################

lcfsexts <- do.call(cbind.data.frame, lcflist$`Summary Statistics by Sex`)%>%
  dplyr::select(contains("mean"), contains("median"), `2014.Sex`)%>%
  dplyr::rename(Sex = `2014.Sex`)%>%
  tidyr::pivot_longer(!Sex, names_to = "YearMomentsWellbeing", values_to = "MeanMedian")%>%
  dplyr::mutate(Year = str_sub(YearMomentsWellbeing, start = 1, end = 4),
                Moment = str_sub(YearMomentsWellbeing, start = -4, end = -1),
                Wellbeing = str_sub(YearMomentsWellbeing, start = 9, end = 13))%>%
  dplyr::mutate(Wellbeing = case_when(Wellbeing == "Satis" | Wellbeing == "is.we" ~ "Satisfaction",
                                      Wellbeing == "Worth" | Wellbeing == "th.we" ~ "Worthwhile",
                                      Wellbeing == "Happy" | Wellbeing == "py.we" ~ "Happiness",
                                      Wellbeing == "Anxio" | Wellbeing == "ious." ~ "Anxiousness",
                                      TRUE ~ as.character(Wellbeing)))

lcfsexmedian <- lcfsexts%>%
  dplyr::filter(Moment == "dian")

png(filename = paste0("MEDIAN & MEAN plots/SEXMEDIAN.png"))
ggplot(lcfsexmedian, aes( x = Year, y = MeanMedian, color = Wellbeing))+
  geom_point(size = 3, shape = 15)+
  facet_wrap(~Sex)+
  ylim(0,10)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  labs(y = "Median")
dev.off()

lcfsexmean <- lcfsexts%>%
  dplyr::filter(Moment == "mean")

png(filename = paste0("MEDIAN & MEAN plots/SEXMEAN.png"))
ggplot(lcfsexmean, aes( x = Year, y = MeanMedian, color = Wellbeing))+
  geom_point(size = 3, shape = 15)+
  facet_wrap(~Sex)+
  ylim(0,10)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  labs(y = "Mean")
dev.off()

############ AGE BANDS #########################################################

lcfagets <- do.call(cbind.data.frame, lcflist$`Summary Statistics by Age Bands`)%>%
  dplyr::select(contains("mean"), contains("median"), `2014.Age Bands`)%>%
  dplyr::rename(`Age Bands` = `2014.Age Bands`)%>%
  tidyr::pivot_longer(!`Age Bands`, names_to = "YearMomentsWellbeing", values_to = "MeanMedian")%>%
  dplyr::mutate(Year = str_sub(YearMomentsWellbeing, start = 1, end = 4),
                Moment = str_sub(YearMomentsWellbeing, start = -4, end = -1),
                Wellbeing = str_sub(YearMomentsWellbeing, start = 9, end = 13))%>%
  dplyr::mutate(Wellbeing = case_when(Wellbeing == "Satis" | Wellbeing == "is.we" ~ "Satisfaction",
                                      Wellbeing == "Worth" | Wellbeing == "th.we" ~ "Worthwhile",
                                      Wellbeing == "Happy" | Wellbeing == "py.we" ~ "Happiness",
                                      Wellbeing == "Anxio" | Wellbeing == "ious." ~ "Anxiousness",
                                      TRUE ~ as.character(Wellbeing)))

lcfagemedian <- lcfagets%>%
  dplyr::filter(Moment == "dian")

png(filename = paste0("MEDIAN & MEAN plots/AGEMEDIAN.png"))
ggplot(lcfagemedian, aes(x = Year, y = MeanMedian, color = Wellbeing))+
  geom_point(size = 3, shape = 15)+
  facet_wrap(~`Age Bands`)+
  ylim(0,10)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  labs(y = "Median")
dev.off()

lcfagemean <- lcfagets%>%
  dplyr::filter(Moment == "mean")

png(filename = paste0("MEDIAN & MEAN plots/AGEMEAN.png"))
ggplot(lcfagemean, aes(x = Year, y = MeanMedian, color = Wellbeing))+
  geom_point(size = 3, shape = 15)+
  facet_wrap(~`Age Bands`)+
  ylim(0,10)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  labs(y = "Mean")
dev.off()

############### GROSS WEEKLY INCOME BANDS ######################################

lcfincomets <- do.call(cbind.data.frame, lcflist$`Summary Statistics by Gross Weekly Income Bands`)%>%
  dplyr::select(contains("mean"), contains("median"), `2014.Gross Weekly Income`)%>%
  dplyr::rename(`Income Bands` = `2014.Gross Weekly Income`)%>%
  tidyr::pivot_longer(!`Income Bands`, names_to = "YearMomentsWellbeing", values_to = "MeanMedian")%>%
  dplyr::mutate(Year = str_sub(YearMomentsWellbeing, start = 1, end = 4),
                Moment = str_sub(YearMomentsWellbeing, start = -4, end = -1),
                Wellbeing = str_sub(YearMomentsWellbeing, start = 9, end = 13))%>%
  dplyr::mutate(Wellbeing = case_when(Wellbeing == "Satis" | Wellbeing == "is.we" ~ "Satisfaction",
                                      Wellbeing == "Worth" | Wellbeing == "th.we" ~ "Worthwhile",
                                      Wellbeing == "Happy" | Wellbeing == "py.we" ~ "Happiness",
                                      Wellbeing == "Anxio" | Wellbeing == "ious." ~ "Anxiousness",
                                      TRUE ~ as.character(Wellbeing)))

lcfincomemedian <- lcfincomets%>%
  dplyr::filter(Moment == "dian")

png(filename = paste0("MEDIAN & MEAN plots/INCOMEMEDIAN.png"))
ggplot(lcfincomemedian, aes(x = Year, y = MeanMedian, color = Wellbeing))+
  geom_point(size = 3, shape = 15)+
  facet_wrap(~`Income Bands`)+
  ylim(0,10)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  labs(y = "Median")
dev.off()

lcfincomemean <- lcfincomets%>%
  dplyr::filter(Moment == "mean")

png(filename = paste0("MEDIAN & MEAN plots/INCOMEMEAN.png"))
ggplot(lcfincomemean, aes(x = Year, y = MeanMedian, color = Wellbeing))+
  geom_point(size = 3, shape = 15)+
  facet_wrap(~`Income Bands`)+
  ylim(0,10)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  labs(y = "Mean")
dev.off()

################## ETHNICITY ###################################################

lcfethts <- do.call(cbind.data.frame, lcflist$`Summary Statistics by Ethnicity Group`)%>%
  dplyr::select(contains("median"), contains("mean"), `2014.Ethnicity`)%>%
  dplyr::rename(Ethnicity = `2014.Ethnicity`)%>%
  tidyr::pivot_longer(!Ethnicity, names_to = "YearMomentsWellbeing", values_to = "MeanMedian")%>%
  dplyr::mutate(Year = str_sub(YearMomentsWellbeing, start = 1, end = 4),
                Moment = str_sub(YearMomentsWellbeing, start = -4, end = -1),
                Wellbeing = str_sub(YearMomentsWellbeing, start = 9, end = 13))%>%
  dplyr::mutate(Wellbeing = case_when(Wellbeing == "Satis" | Wellbeing == "is.we" ~ "Satisfaction",
                                      Wellbeing == "Worth" | Wellbeing == "th.we" ~ "Worthwhile",
                                      Wellbeing == "Happy" | Wellbeing == "py.we" ~ "Happiness",
                                      Wellbeing == "Anxio" | Wellbeing == "ious." ~ "Anxiousness",
                                      TRUE ~ as.character(Wellbeing)))

lcfethmedian <- lcfethts%>%
  dplyr::filter(Moment == "dian")

png(filename = paste0("MEDIAN & MEAN plots/ETHNICITYMEDIAN.png"))
ggplot(lcfethmedian, aes(x = Year, y = MeanMedian, color = Wellbeing))+
  geom_point(size = 3, shape = 15)+
  facet_wrap(~Ethnicity)+
  ylim(0,10)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  labs(y = "Median")
dev.off()

lcfethmean <- lcfethts%>%
  dplyr::filter(Moment == "mean")

png(filename = paste0("MEDIAN & MEAN plots/ETHNICITYMEAN.png"))
ggplot(lcfethmean, aes(x = Year, y = MeanMedian, color = Wellbeing))+
  geom_point(size = 3, shape = 15)+
  facet_wrap(~Ethnicity)+
  ylim(0,10)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  labs(y = "Mean")
dev.off()

################ DEPENDENT CHILDREN ############################################

lcfdepchts <- do.call(cbind.data.frame, lcflist$`Summary Statistics of those With/Without Dependent Children`)%>%
  dplyr::select(contains("median"), contains("mean"), `2014.HASDEP`)%>%
  dplyr::rename(HASDEP = `2014.HASDEP`)%>%
  tidyr::pivot_longer(!HASDEP, names_to = "YearMomentsWellbeing", values_to = "MeanMedian")%>%
  dplyr::mutate(Year = str_sub(YearMomentsWellbeing, start = 1, end = 4),
                Moment = str_sub(YearMomentsWellbeing, start = -4, end = -1),
                Wellbeing = str_sub(YearMomentsWellbeing, start = 9, end = 13))%>%
  dplyr::mutate(Wellbeing = case_when(Wellbeing == "Satis" | Wellbeing == "is.we" ~ "Satisfaction",
                                      Wellbeing == "Worth" | Wellbeing == "th.we" ~ "Worthwhile",
                                      Wellbeing == "Happy" | Wellbeing == "py.we" ~ "Happiness",
                                      Wellbeing == "Anxio" | Wellbeing == "ious." ~ "Anxiousness",
                                      TRUE ~ as.character(Wellbeing)))

lcfdepchmedian <- lcfdepchts%>%
  dplyr::filter(Moment == "dian")

png(filename = paste0("MEDIAN & MEAN plots/DEPCHILDMEDIAN.png"))
ggplot(lcfdepchmedian, aes(x = Year, y = MeanMedian, color = Wellbeing))+
  geom_point(size = 3, shape = 15)+
  facet_wrap(~HASDEP)+
  ylim(0,10)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  labs(y = "Median")
dev.off()

lcfdepchmean <- lcfdepchts%>%
  dplyr::filter(Moment == "mean")

png(filename = paste0("MEDIAN & MEAN plots/DEPCHILDMEAN.png"))
ggplot(lcfdepchmean, aes(x = Year, y = MeanMedian, color = Wellbeing))+
  geom_point(size = 3, shape = 15)+
  facet_wrap(~HASDEP)+
  ylim(0,10)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  labs(y = "Mean")
dev.off()

################ NUMBER OF DEPENDENT CHILDREN ##################################

lcfnodepchts <- do.call(cbind.data.frame, lcflist$`Summary Statistics by Number of Dependent Children`)%>%
  dplyr::select(contains("median"), contains("mean"), `2014.Number of Dependent Children`)%>%
  dplyr::rename(NODEPCH = `2014.Number of Dependent Children`)%>%
  tidyr::pivot_longer(!NODEPCH, names_to = "YearMomentsWellbeing", values_to = "MeanMedian")%>%
  dplyr::mutate(Year = str_sub(YearMomentsWellbeing, start = 1, end = 4),
                Moment = str_sub(YearMomentsWellbeing, start = -4, end = -1),
                Wellbeing = str_sub(YearMomentsWellbeing, start = 9, end = 13))%>%
  dplyr::mutate(Wellbeing = case_when(Wellbeing == "Satis" | Wellbeing == "is.we" ~ "Satisfaction",
                                      Wellbeing == "Worth" | Wellbeing == "th.we" ~ "Worthwhile",
                                      Wellbeing == "Happy" | Wellbeing == "py.we" ~ "Happiness",
                                      Wellbeing == "Anxio" | Wellbeing == "ious." ~ "Anxiousness",
                                      TRUE ~ as.character(Wellbeing)))


lcfnodepchmedian <- lcfnodepchts%>%
  dplyr::filter(Moment == "dian")

png(filename = paste0("MEDIAN & MEAN plots/NODEPCHMEDIAN.png"))
ggplot(lcfnodepchmedian, aes(x = Year, y = MeanMedian, color = Wellbeing))+
  geom_point(size = 3, shape = 15)+
  facet_wrap(~NODEPCH)+
  ylim(0,10)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  labs(y = "Median")
dev.off()

lcfnodepchmean <- lcfnodepchts%>%
  dplyr::filter(Moment == "mean")

png(filename = paste0("MEDIAN & MEAN plots/NODEPCHMEAN.png"))
ggplot(lcfnodepchmean, aes(x = Year, y = MeanMedian, color = Wellbeing))+
  geom_point(size = 3, shape = 15)+
  facet_wrap(~NODEPCH)+
  ylim(0,10)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  labs(y = "Mean")
dev.off()

################ MARITAL STATUS WITH DEPENDENT CHILDREN ########################

lcfmardepts <- do.call(cbind.data.frame, lcflist$`Summary Statistics by Marital Status of those with Dependent Children`)%>%
  dplyr::select(contains("median"), contains("mean"), `2014.Marital Status`)%>%
  dplyr::rename(MARSTA = `2014.Marital Status`)%>%
  tidyr::pivot_longer(!MARSTA, names_to = "YearMomentsWellbeing", values_to = "MeanMedian")%>%
  dplyr::mutate(Year = str_sub(YearMomentsWellbeing, start = 1, end = 4),
                Moment = str_sub(YearMomentsWellbeing, start = -4, end = -1),
                Wellbeing = str_sub(YearMomentsWellbeing, start = 9, end = 13))%>%
  dplyr::mutate(Wellbeing = case_when(Wellbeing == "Satis" | Wellbeing == "is.we" ~ "Satisfaction",
                                      Wellbeing == "Worth" | Wellbeing == "th.we" ~ "Worthwhile",
                                      Wellbeing == "Happy" | Wellbeing == "py.we" ~ "Happiness",
                                      Wellbeing == "Anxio" | Wellbeing == "ious." ~ "Anxiousness",
                                      TRUE ~ as.character(Wellbeing)))

lcfmardepmedian <- lcfmardepts%>%
  dplyr::filter(Moment == "dian")

png(filename = paste0("MEDIAN & MEAN plots/MARSTADEPCHMEDIAN.png"))
ggplot(lcfmardepmedian, aes(x = Year, y = MeanMedian, color = Wellbeing))+
  geom_point(size = 3, shape = 15)+
  facet_wrap(~MARSTA)+
  ylim(0,10)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  labs(y = "Median")
dev.of()

lcfmardepmean <- lcfmardepts%>%
  dplyr::filter(Moment == "mean")

png(filename = paste0("MEDIAN & MEAN plots/MARSTADEPCHMEAN.png"))
ggplot(lcfmardepmean, aes(x = Year, y = MeanMedian, color = Wellbeing))+
  geom_point(size = 3, shape = 15)+
  facet_wrap(~MARSTA)+
  ylim(0,10)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  labs(y = "Mean")
dev.off()

#################### SEX, MARITAL STATUS WITH DEPENDENT CHILDREN ###############

lcfsexmardepts <- do.call(cbind.data.frame, lcflist$`Summary Statistics by Marital Status and Sex of those with Dependent Children`)%>%
  dplyr::select(contains("mean"), contains("median"), `2014.Sex and Marrital Status with Dependent Children`)%>%
  dplyr::rename(SEXMARDEP = `2014.Sex and Marrital Status with Dependent Children`)%>%
  tidyr::pivot_longer(!SEXMARDEP, names_to = "YearMomentsWellbeing", values_to = "MeanMedian")%>%
  dplyr::mutate(Year = str_sub(YearMomentsWellbeing, start = 1, end = 4),
                Moment = str_sub(YearMomentsWellbeing, start = -4, end = -1),
                Wellbeing = str_sub(YearMomentsWellbeing, start = 9, end = 13))%>%
  dplyr::mutate(Wellbeing = case_when(Wellbeing == "Satis" | Wellbeing == "is.we" ~ "Satisfaction",
                                      Wellbeing == "Worth" | Wellbeing == "th.we" ~ "Worthwhile",
                                      Wellbeing == "Happy" | Wellbeing == "py.we" ~ "Happiness",
                                      Wellbeing == "Anxio" | Wellbeing == "ious." ~ "Anxiousness",
                                      TRUE ~ as.character(Wellbeing)))

lcfsexmardepmedian <- lcfsexmardepts%>%
  dplyr::filter(Moment == "dian")

png(filename = paste0("MEDIAN & MEAN plots/SEXMARSTADEPCHMEDIAN.png"))
ggplot(lcfsexmardepmedian, aes(x = Year, y = MeanMedian, color = Wellbeing))+
  geom_point(size = 3, shape = 15)+
  facet_wrap(~SEXMARDEP)+
  ylim(0,10)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  labs(y = "Median")
dev.off()

lcfsexmardepmean <- lcfsexmardepts%>%
  dplyr::filter(Moment == "mean")

png(filename = paste0("MEDIAN & MEAN plots/SEXMARSTADEPCHMEAN.png"))
ggplot(lcfsexmardepmean, aes(x = Year, y = MeanMedian, color = Wellbeing))+
  geom_point(size = 3, shape = 15)+
  facet_wrap(~SEXMARDEP)+
  ylim(0,10)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  labs(y = "Mean")
dev.off()

################## MARITAL STATUS ##############################################

lcfmarts <- do.call(cbind.data.frame, lcflist$`Summary Statistics by Marital Status`)%>%
  dplyr::select(contains("median"), contains("mean"), `2014.Marital Status`)%>%
  dplyr::rename(MARSTA = `2014.Marital Status`)%>%
  tidyr::pivot_longer(!MARSTA, names_to = "YearMomentsWellbeing", values_to = "MeanMedian")%>%
  dplyr::mutate(Year = str_sub(YearMomentsWellbeing, start = 1, end = 4),
                Moment = str_sub(YearMomentsWellbeing, start = -4, end = -1),
                Wellbeing = str_sub(YearMomentsWellbeing, start = 9, end = 13))%>%
  dplyr::mutate(Wellbeing = case_when(Wellbeing == "Satis" | Wellbeing == "is.we" ~ "Satisfaction",
                                      Wellbeing == "Worth" | Wellbeing == "th.we" ~ "Worthwhile",
                                      Wellbeing == "Happy" | Wellbeing == "py.we" ~ "Happiness",
                                      Wellbeing == "Anxio" | Wellbeing == "ious." ~ "Anxiousness",
                                      TRUE ~ as.character(Wellbeing)))

lcfmarmedian <- lcfmarts%>%
  dplyr::filter(Moment == "dian")

png(filename = paste0("MEDIAN & MEAN plots/MARSTAMEDIAN.png"))
ggplot(lcfmarmedian, aes(x = Year, y = MeanMedian, color = Wellbeing))+
  geom_point(size = 3, shape = 15)+
  facet_wrap(~MARSTA)+
  ylim(0,10)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  labs(y = "Median")
dev.of()

lcfmarmean <- lcfmarts%>%
  dplyr::filter(Moment == "mean")

png(filename = paste0("MEDIAN & MEAN plots/MARSTAMEAN.png"))
ggplot(lcfmarmean, aes(x = Year, y = MeanMedian, color = Wellbeing))+
  geom_point(size = 3, shape = 15)+
  facet_wrap(~MARSTA)+
  ylim(0,10)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  labs(y = "Mean")
dev.off()


################################################################################