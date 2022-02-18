library(tidyverse)
library(rvest)
library(lubridate)
library(splitstackshape)
library(data.table)

##Load data
##################### Load Salvage Count Data from SacPAS

#Function adjusted from Trinh's code to pull salvage datasets from SacPAS
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

#Run actual function to load data
salvage_data <- suppressWarnings(pull_salvage())


#Rename columns to make it easier to work in R and divide Loss + Expanded Salvage by nfish
salvage_data_adjusted<- salvage_data %>%
  rename(SampleTime='Sample Time',LAD_Race='LAD Race',SampleFraction='Sample Fraction',ExpandedSalvage='Expanded Salvage',LAD_Loss='LAD Loss') %>%
  mutate(ExpandedSalvage=ExpandedSalvage/nfish, LAD_Loss=LAD_Loss/nfish)


#Multiply rows by nfish
salvage_data_adjusted<- setDT(expandRows(salvage_data_adjusted, "nfish")) 

salvage_data_adjusted<- salvage_data_adjusted%>%
  # build grouping by combination of variables
  dplyr::group_by(SampleTime, LAD_Race, Length) %>%
  # add row number which works per group due to prior grouping
  dplyr::mutate(duplicateID = dplyr::row_number()) %>%
  # ungroup to prevent unexpected behaviour down stream
  dplyr::ungroup()

str(salvage_data_adjusted)
#Adjust Sample Time to the proper format
salvage_data_adjusted$SampleTime <- as.POSIXlt(salvage_data_adjusted$SampleTime,format='%Y-%m-%d %H:%M:%S')

str(salvage_data_adjusted)

#################################### Read genetic data

genetic_data_CVP <-read.csv("CVP-sizebydate-2010-21.csv")
genetic_data_SWP <-read.csv("SWP-sizebydate-2010-21.csv")

genetic_data_combined<-bind_rows(genetic_data_CVP,genetic_data_SWP) %>% rename(SampleTime=SampleDate2)

genetic_data_combined$SampleTime <- as.POSIXlt(genetic_data_combined$SampleTime,format='%m/%d/%Y %H:%M')

genetic_data_combined<- genetic_data_combined%>%
  # add facility
  dplyr::mutate(Facility = case_when(
    grepl("CVP",ID) ~ "CVP",
    grepl("SWP",ID) ~ "SWP",
  )) %>%
  # build grouping by combination of variables
  dplyr::group_by(SampleTime, Facility, ForkLength) %>%
  # add row number which works per group due to prior grouping
  dplyr::mutate(duplicateID = dplyr::row_number()) %>%
  # ungroup to prevent unexpected behaviour down stream
  dplyr::ungroup() %>%
  dplyr::rename(Length=ForkLength)

str(genetic_data_combined)

#######Combine the two data sets
combined_data<-salvage_data_adjusted %>% left_join(genetic_data_combined)

unpaired_genetic_data<-full_join(salvage_data_adjusted,genetic_data_combined) %>% filter(is.na(LAD_Race))

paired_genetic_data<-combined_data %>% filter(!is.na(GeneticID))


write.csv(unpaired_genetic_data,file="Unpaired_Genetic_data_2022-02-18.csv",row.names = F)

write.csv(combined_data,file="Total_Salvage_data_2022-02-18.csv",row.names = F)
