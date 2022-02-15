library(tidyverse)
library(rvest)
library(lubridate)


##Load data
############################## State Water Project

inUrl1  <- "https://pasta.lternet.edu/package/data/eml/edi/1056/1/ebcf5ce43e12876706d8ece935874f0d" 
infile1 <- tempfile()
try(download.file(inUrl1,infile1,method="curl"))
if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")


dt1 <-read.csv(infile1,header=F 
               ,skip=1
               ,sep=","  
               ,quot='"' 
               , col.names=c(
                 "ID",     
                 "SampleDate",     
                 "ForkLength",     
                 "Julian",     
                 "GeneticID",     
                 "PosProb",     
                 "Ots28",     
                 "LengthByDate"    ), check.names=TRUE)

unlink(infile1)

# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

if (class(dt1$ID)!="factor") dt1$ID<- as.factor(dt1$ID)                                   
# attempting to convert dt1$SampleDate dateTime string to R date structure (date or POSIXct)                                
tmpDateFormat<-"%Y-%m-%d"
tmp1SampleDate<-as.Date(dt1$SampleDate,format=tmpDateFormat)
# Keep the new dates only if they all converted correctly
if(length(tmp1SampleDate) == length(tmp1SampleDate[!is.na(tmp1SampleDate)])){dt1$SampleDate <- tmp1SampleDate } else {print("Date conversion failed for dt1$SampleDate. Please inspect the data and do the date conversion yourself.")}                                                                    
rm(tmpDateFormat,tmp1SampleDate) 
if (class(dt1$ForkLength)=="factor") dt1$ForkLength <-as.numeric(levels(dt1$ForkLength))[as.integer(dt1$ForkLength) ]               
if (class(dt1$ForkLength)=="character") dt1$ForkLength <-as.numeric(dt1$ForkLength)
if (class(dt1$Julian)=="factor") dt1$Julian <-as.numeric(levels(dt1$Julian))[as.integer(dt1$Julian) ]               
if (class(dt1$Julian)=="character") dt1$Julian <-as.numeric(dt1$Julian)
if (class(dt1$GeneticID)!="factor") dt1$GeneticID<- as.factor(dt1$GeneticID)
if (class(dt1$PosProb)=="factor") dt1$PosProb <-as.numeric(levels(dt1$PosProb))[as.integer(dt1$PosProb) ]               
if (class(dt1$PosProb)=="character") dt1$PosProb <-as.numeric(dt1$PosProb)
if (class(dt1$Ots28)!="factor") dt1$Ots28<- as.factor(dt1$Ots28)
if (class(dt1$LengthByDate)!="factor") dt1$LengthByDate<- as.factor(dt1$LengthByDate)

# Convert Missing Values to NA for non-dates

dt1$LengthByDate <- as.factor(ifelse((trimws(as.character(dt1$LengthByDate))==trimws("NA")),NA,as.character(dt1$LengthByDate)))

SWP_data<-dt1
remove(dt1)


############################## Central Valley Project


inUrl1  <- "https://pasta.lternet.edu/package/data/eml/edi/1049/2/7a8e220e83f9c9facce6ecd9d3933a9a" 
infile1 <- tempfile()
try(download.file(inUrl1,infile1,method="curl"))
if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")


dt1 <-read.csv(infile1,header=F 
               ,skip=1
               ,sep=","  
               ,quot='"' 
               , col.names=c(
                 "ID",     
                 "SampleDate",     
                 "ForkLength",     
                 "Julian",     
                 "GeneticID",     
                 "PosProb",     
                 "Ots28",     
                 "LengthByDate"    ), check.names=TRUE)

unlink(infile1)

# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

if (class(dt1$ID)!="factor") dt1$ID<- as.factor(dt1$ID)                                   
# attempting to convert dt1$SampleDate dateTime string to R date structure (date or POSIXct)                                
tmpDateFormat<-"%Y-%m-%d"
tmp1SampleDate<-as.Date(dt1$SampleDate,format=tmpDateFormat)
# Keep the new dates only if they all converted correctly
if(length(tmp1SampleDate) == length(tmp1SampleDate[!is.na(tmp1SampleDate)])){dt1$SampleDate <- tmp1SampleDate } else {print("Date conversion failed for dt1$SampleDate. Please inspect the data and do the date conversion yourself.")}                                                                    
rm(tmpDateFormat,tmp1SampleDate) 
if (class(dt1$ForkLength)=="factor") dt1$ForkLength <-as.numeric(levels(dt1$ForkLength))[as.integer(dt1$ForkLength) ]               
if (class(dt1$ForkLength)=="character") dt1$ForkLength <-as.numeric(dt1$ForkLength)
if (class(dt1$Julian)=="factor") dt1$Julian <-as.numeric(levels(dt1$Julian))[as.integer(dt1$Julian) ]               
if (class(dt1$Julian)=="character") dt1$Julian <-as.numeric(dt1$Julian)
if (class(dt1$GeneticID)!="factor") dt1$GeneticID<- as.factor(dt1$GeneticID)
if (class(dt1$PosProb)=="factor") dt1$PosProb <-as.numeric(levels(dt1$PosProb))[as.integer(dt1$PosProb) ]               
if (class(dt1$PosProb)=="character") dt1$PosProb <-as.numeric(dt1$PosProb)
if (class(dt1$Ots28)!="factor") dt1$Ots28<- as.factor(dt1$Ots28)
if (class(dt1$LengthByDate)!="factor") dt1$LengthByDate<- as.factor(dt1$LengthByDate)

CVP_data<-dt1
remove(dt1)


###################### Combine Data and Calculate Loss
Data_combined<-bind_rows(CVP_data,SWP_data) %>% mutate(Count=1, Facility = case_when(
  grepl("CVP", ID) ~ "CVP",
  grepl("SWP", ID) ~ "SWP",
  )) %>% group_by(SampleDate, Facility, GeneticID) %>% summarise(Count=sum(Count))


##################### Load Salvage Count Data from SacPAS

pull_salvage <- function(salvageURL = "http://www.cbr.washington.edu/sacramento/data/query_loss_detail.html") {
  startingSession <- session(salvageURL)
  startingForm <- html_form(startingSession)[[1]]
  
  df <- lapply(startingForm$fields$year$options, function(x) {
    filledForm <- set_values(startingForm,
                             year = x,
                             species = "1:f")
    
    submittedFormURL <- suppressMessages(submit_form(session = startingSession, 
                                                     form = filledForm, POST = salvageURL)$url)
    
    csvLink <- submittedFormURL
    
    if (length(csvLink) == 0) {
      return(NULL)
    } else {
      csvDownload <- csvLink
    }
    
    df <- csvDownload %>% 
      read_csv() %>% filter(!is.na(nfish)) }) %>%
      bind_rows() 
  df
}

salvage_data <- suppressWarnings(pull_salvage())

#Rename some of the data columns, summarize by date, join DNA data
str(salvage_data)

salvage_data_daily <- salvage_data %>% 
  rename(SampleTime='Sample Time',LAD_Race='LAD Race',SampleFraction='Sample Fraction',ExpandedSalvage='Expanded Salvage',LAD_Loss='LAD Loss') %>%
  mutate(SampleDate = as.Date(SampleTime)) %>% group_by(SampleDate,Facility,LAD_Race) %>% 
  summarise(nfish=sum(nfish),ExpandedSalvage=sum(ExpandedSalvage),LAD_Loss=sum(LAD_Loss)) %>% filter(year(SampleDate)>=2010)
  #left_join(Data_combined)

Data_combined_with_salvage<- full_join(salvage_data_daily %>% filter(LAD_Race == "Winter"), Data_combined %>% filter(GeneticID == "Winter"))

#Export
write.csv(Data_combined_with_salvage,file="Winter-run_LAD_vs_Genetic_2010.csv",row.names = F)
