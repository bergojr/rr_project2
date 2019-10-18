library(dplyr)
library(ggplot2)
library(gridExtra)
library(taRifx)
library(lattice)
library(reshape2)

dataURL <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
fileZip <- "stormData.bz2"
dirData <- "./data"
fileCSV <- "stormData.csv"

eventlist <- c("Astronomical Low Tide","Avalanche","Blizzard","Coastal Flood","Cold/Wind Chill","Debris Flow","Dense Fog","Dense Smoke","Drought","Dust Devil","Dust Storm",
               "Excessive Heat","Extreme Cold/Wind Chill","Flash Flood","Flood","Freezing Fog","Frost/Freeze","Funnel Cloud","Hail","Heat","Heavy Rain","Heavy Snow","High Surf",
               "High Wind","Hurricane/Typhoon","Ice Storm","Lakeshore Flood","Lake-Effect Snow","Lightning","Marine Hail","Marine High Wind","Marine Strong Wind","Marine Thunderstorm Wind",
               "Rip Current","Seiche","Sleet","Storm Tide","Strong Wind","Thunderstorm Wind","Tornado","Tropical Depression","Tropical Storm","Tsunami","Volcanic Ash","Waterspout",
               "Wildfire","Winter Storm","Winter Weather")%>%
  toupper()


if (!file.exists(fileZip)){
        download.file(dataURL,fileZip)
}


st_data <- read.csv("stormData.bz2", na.strings =c("","NA"), stringsAsFactors = FALSE)
st_data2 <- st_data

# Tratamento da coluna de eventos

evtype <- st_data2$EVTYPE

evtype <- gsub("^ *","", st_data2$EVTYPE)%>%
  toupper()
evtype <- gsub("^SUMMARY",NA, evtype)

st_data2$EVTYPE_Treated <- NA

for (event in eventlist){
  val_exp <- paste("*",event,"*", sep="")
  filtered_events <- grep(val_exp,evtype)
  st_data2$EVTYPE_Treated[filtered_events]<- event
}

# eventos TSTM são 226202 casos distribuídos em 26 categorias!!!!
# Rotina especial para tratamento TSTM
filtro_TSTM <- grep("*TSTM*", st_data2$EVTYPE)
st_data2$EVTYPE_Treated[filtro_TSTM]<- "THUNDERSTORM WIND"




#  Tratamento dos valores de danos em propriedade e plantações

st_data2$PROPDMGEXP <- toupper(st_data2$PROPDMGEXP)
st_data2$CROPDMGEXP <- toupper(st_data2$CROPDMGEXP)

labels_mult <- sort(unique(c(st_data2$PROPDMGEXP,st_data2$CROPDMGEXP)))
values_mult <- c(1,1,1,rep(10,9),1E9,100,1E3,1E6)


mult_props <- as.data.frame(cbind(labels_mult,values_mult)) %>%
  remove.factors()%>%
  rbind(c(NA,1,1))

names(mult_props) <- c("labels_mult","values_multprops")

mult_crops <- mult_props
names(mult_crops) <- c("labels_mult","values_multcrops")

st_data2 <- merge(x = st_data2, y=mult_props, by.x = "PROPDMGEXP", 
                by.y = "labels_mult")

st_data2 <- merge(x = st_data2, y=mult_crops, by.x = "CROPDMGEXP", 
                  by.y = "labels_mult")%>%
  transform(values_multcrops = as.numeric(values_multcrops))%>%
  transform(values_multprops = as.numeric(values_multprops))%>%
  transform(PROPDMGCOST=PROPDMG*values_multprops)%>%
  transform(CROPDMGCOST=CROPDMG*values_multcrops)

st_data2$values_multcrops <- as.numeric(st_data2$values_multcrops)
st_data2$values_multprops <- as.numeric(st_data2$values_multprops)

st_data2 <- transform(st_data2,MULTIPROP=PROPDMG*values_multprops)%>%
  transform(MULTICROP=CROPDMG*values_multcrops)

# transform(st_data2,MULTIPROP=PROPDMG*values_multprops)
# transform(st_data2,MULTICROP=CROPDMG*values_multcrops)


# Preparing data to plot cost damage #

costdata <- st_data2[,c("EVTYPE_Treated","PROPDMGCOST","CROPDMGCOST")]
names(costdata) <-c("Event", "TypeDamage","Value")

melt_cost <- melt(costdata, variable.name = "TypeDamage", 
                  value.name = "DMGValue")

gr_melt_cost <- group_by(melt_cost,Event)
sum_costDMG <- summarize(gr_melt_cost, sum(DMGValue))
names(sum_costDMG) <-c("Event","DMGValue")
na_event <- is.na(sum_costDMG$Event)
sum_costDMG[na_event,"Event"] <- "MISSING VALUE (NA)"

cut_event <- quantile(sum_costDMG$DMGValue, probs = c(0, 0.8, 1))
rl_event <- filter(sum_costDMG, DMGValue > cut_event[2])



damage_plot <- ggplot(rl_event, aes(x=reorder(Event,DMGValue), y= DMGValue))
damage_plot <- damage_plot  + geom_col()
damage_plot <- damage_plot  + coord_flip()
damage_plot <- damage_plot  + labs(title = "Economic Losses by Weather Events in USA 1950 - 2011")
damage_plot <- damage_plot  + theme(plot.title = element_text(hjust = 0.5))
damage_plot <- damage_plot  + labs(y = "Main losses (US$)" , x = "Event type")

print(damage_plot)

# Preparing data to plot cost damage #

injurydata <- st_data2[,c("EVTYPE_Treated","FATALITIES","INJURIES")]
names(injurydata) <-c("Event", "TypeDamage","Count")

melt_injury <- melt(injurydata, variable.name = "TypeDamage", 
                  value.name = "Count")

gr_melt_injury <- group_by(melt_injury,Event)
sum_CountInjury <- summarize(gr_melt_injury, sum(Count))
names(sum_CountInjury) <-c("Event","Count")
na_event <- is.na(sum_CountInjury$Event) 
sum_CountInjury[na_event,"Event"] <- "MISSING VALUE (NA)"

cut_injury <- quantile(sum_CountInjury$Count, probs = c(0, 0.8, 1))
rl_injury <- filter(sum_CountInjury, Count > cut_injury [2])



damage_injury <- ggplot(rl_injury, aes(x=reorder(Event,Count), y= Count))
damage_injury <- damage_injury  + geom_col()
damage_injury <- damage_injury  + coord_flip()
damage_injury <- damage_injury  + labs(title = "Health Losses by Weather Events in USA 1950 - 2011")
damage_injury <- damage_injury  + theme(plot.title = element_text(hjust = 0.5))
damage_injury <- damage_injury  + labs(y = "Main Injury and Death" , x = "Event type")
print(damage_injury)

hist(st_data2$EVTYPE_Treated, )

st_data2$BGN_DATE <- st_data$BGN_DATE
st_data2$BGN_DATE <- as.POSIXct(st_data2$BGN_DATE, tz="", "%m/%d/%Y %H:%M:%OS")

# anos <- year(st_data2$BGN_DATE)
# validade <- is.na(st_data2$EVTYPE_Treated)
# anos_na <- anos[validade]
# 
# dados_anuais <- as.data.frame(cbind(anos,st_data2$EVTYPE_Treated))
# dados_anuais$anos <- factor(dados_anuais$anos)
# 
# df_anos <- tbl_df(table(dados_anuais[!validade,]))
# df_anos_na <- tbl_df(table(dados_anuais[validade,]))

validade <- is.na(st_data2$EVTYPE_Treated)

anos <- year(st_data2$BGN_DATE)
anos <- factor(anos)

anos_na <- anos[validade]

# Analise dos dados entre os períodos 

df_anos<-tbl_df(table(anos))
df_anos_na<-tbl_df(table(anos_na))

ratio <- (df_anos_na$n/df_anos$n)*100

# Useful links:
#
# This link explains how works the damage exponents
#
# https://rstudio-pubs-static.s3.amazonaws.com/58957_37b6723ee52b455990e149edde45e5b6.html
# 
# This is a good link to melt data
# 
# https://seananderson.ca/2013/10/19/reshape/
#
# Link to Coursera forum 
#
# https://www.coursera.org/learn/reproducible-research/discussions/weeks/4/threads/38y35MMiEeiERhLphT2-QA
#