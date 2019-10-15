library(dplyr)
library(ggplot2)
library(gridExtra)
library(taRifx)
library(lattice)

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

labels_mult <- sort(unique(c(st_data$PROPDMGEXP,st_data$CROPDMGEXP)))
values_mult <- c(1,1,1,rep(10,9),1E9,100,1E3,1E6)


mult_props <- as.data.frame(cbind(labels_mult,values_mult)) %>%
  remove.factors()%>%
  rbind(c(NA,1,1))

names(mult_props) <- c("labels_mult","values_multprops")

mult_crops <- mult_props
names(mult_crops) <- c("labels_mult","values_multcrops")

st_data2 <- merge(x = st_data, y=mult_props, by.x = "PROPDMGEXP", 
                by.y = "labels_mult")

st_data2 <- merge(x = st_data2, y=mult_crops, by.x = "CROPDMGEXP", 
                  by.y = "labels_mult")%>%
  transform(values_multcrops = as.numeric(values_multcrops))%>%
  transform(values_multprops = as.numeric(values_multprops))%>%
  transform(PROPDMGCOST=PROPDMG*values_multprops)%>%
  transform(CROPDMGCOST=CROPDMG*values_multcrops)

st_data2$values_multcrops <- as.numeric(st_data2$values_multcrops)
st_data2$values_multprops <- as.numeric(st_data2$values_multprops)


transform(st_data2,MULTIPROP=PROPDMG*values_multprops)
transform(st_data2,MULTICROP=CROPDMG*values_multcrops)

evtype <- gsub("^ *","", st_data$EVTYPE)%>%
        toupper()
evtype <- gsub("^SUMMARY",NA, evtype)
        
