#Packages Used
library(stringr)
library(plyr)
library(dplyr)

#Download File
con <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"

download.file(con, "stormdata.csv.bz2")

#Load data
raw.storm.data <- read.csv("stormdata.csv.bz2", header = TRUE, stringsAsFactors = FALSE)            

#Process Data Section 
##Subset Data
NameList <- c("EVTYPE", "FATALITIES", "PROPDMG", "CROPDMG", "PROPDMGEXP", "CROPDMGEXP")
col.num <- which(colnames(raw.storm.data) %in% NameList)
subsetted.data <- raw.storm.data[,col.num]

##Remove Extra spaces from variables
subsetted.data$EVTYPE <- str_trim(subsetted.data$EVTYPE, side = "both")

##Change variable names to lower case letters
names(subsetted.data) <- tolower(names(subsetted.data))

##Change evtype to lower case letters
subsetted.data$evtype <- tolower(subsetted.data$evtype)

##Change PROPDMG and CROPDMG to standard currency units
#Property Damage Multiplier
unique.propdmg.values <- unique(subsetted.data$propdmgexp)
subsetted.data$propmultiplier <- as.numeric(mapvalues(subsetted.data$propdmgexp, 
                                           unique.propdmg.values,c(1000,1000000,NA,1000000000,
                                                                   1000000,NA,NA,NA,NA,NA,NA,NA,
                                                                   NA,NA,NA,100,NA,NA,NA)))
#Create cropdmg multiplier
unique.cropdmg.values <- unique(subsetted.data$cropdmgexp)
subsetted.data$cropmultiplier <- as.numeric(mapvalues(subsetted.data$cropdmgexp, 
                                                      unique.cropdmg.values,c(NA,1000000,1000,
                                                                              1000000,1000000000,
                                                                              NA,NA,1000,NA)))

#Create final cost of property and crop damage with consistent units
subsetted.data$finalpropdmg <- with(subsetted.data,propdmg * propmultiplier)
subsetted.data$finalcropdmg <- with(subsetted.data, cropdmg * cropmultiplier)

#Create a data frame with final cleaned data used for analysis
clean.data <- with(subsetted.data,data.frame(evtype,fatalities, finalpropdmg, finalcropdmg))
rm(raw.storm.data)
rm(subsetted.data)

#Results Section
#Create Top 10 Most Dangerous Event types by fatalities
fatalities.data <- clean.data %>% group_by(evtype) %>% summarize_each(funs(sum),
                                                                      -c(finalpropdmg, finalcropdmg))
fatalities.plot.data <- head(fatalities.data[order(fatalities.data$fatalities,
                                                   decreasing = TRUE),], n = 10, na.rm = TRUE)
#To reset evtype factor levels from 977 to 10
fatalities.plot.data$evtype <- as.character(fatalities.plot.data$evtype)
fatalities.plot.data$evtype <- as.factor(fatalities.plot.data$evtype)
rm(fatalities.data)

#Top 10 Fatalities Plot
with(fatalities.plot.data,plot(evtype,fatalities, type = "p", pch = 20,col = "black",
                            main = "Top 10 Fatalities by Environment Type",
                            xlab = "Envrionment Type", ylab = "Fatalities"))

#Create Top 10 Property and Crop Damage Events
economic.cost.data <- clean.data %>% group_by(evtype) %>% summarize_each(funs(sum),
                                                                         -c(fatalities))
prop.cost.data <- head(economic.cost.data[order(economic.cost.data$finalpropdmg,
                                             decreasing = TRUE),], n = 10, na.rm = TRUE)
prop.cost.data$evtype <- as.character(prop.cost.data$evtype)
prop.cost.data$evtype <- as.factor(prop.cost.data$evtype)
crop.cost.data <- head(economic.cost.data[order(economic.cost.data$finalcropdmg,
                                                decreasing = TRUE),], n = 10, na.rm = TRUE)
crop.cost.data$evtype <- as.character(crop.cost.data$evtype)
crop.cost.data$evtype <- as.factor(crop.cost.data$evtype)

prop.cost.data
crop.cost.data

#Plot Cost Data
par(mfrow = c(1,2))
with(prop.cost.data,plot(evtype,finalpropdmg, type = "p", pch = 20,col = "black",
                               main = "Top 10 Property Damage Environment Types",
                               xlab = "Environment Type", ylab = "Cost (USD dollars)"))
with(crop.cost.data,plot(evtype,finalcropdmg, type = "p", pch = 20,col = "black",
                         main = "Top 10 Crop Damage Environment Types",
                         xlab = "Environment Type", ylab = "Cost (USD dollars)"))
