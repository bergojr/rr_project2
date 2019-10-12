library(dplyr)

dataURL <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
fileZip <- "stormData.bz2"
dirData <- "./data"
fileCSV <- "stormData.csv"

if (!file.exists(fileZip)){
        download.file(dataURL,fileZip)
}

# if(!file.exists(fileCSV)){
#         bunzip2(fileZip, fileCSV, remove = FALSE, skip = TRUE)
# }
# 


st_data <- read.csv("stormData.bz2", na.strings =c("","NA"), stringsAsFactors = FALSE)

# https://rstudio-pubs-static.s3.amazonaws.com/58957_37b6723ee52b455990e149edde45e5b6.html

evtype <- st_data$EVTYPE

st_data$PROPDMGEXP <- toupper(st_data$PROPDMGEXP)
st_data$CROPDMGEXP <- toupper(st_data$CROPDMGEXP)

evtype <- gsub("^ *","", st_data$EVTYPE)%>%
        toupper()
evtype <- gsub("^SUMMARY",NA, evtype)
        
