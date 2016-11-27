#Packages Used

#Download File
con <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"

download.file(con, "stormdata.csv.bz2")

#Load data
raw.storm.data <- read.csv("stormdata.csv.bz2")            

