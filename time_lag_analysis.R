
# Libraries
xfun::pkg_attach("tidyverse", "lubridate", "pbapply")

# Lists
data <- list()

data$joinedDF <- readRDS("joinedDF.rds")

# The function ------------------------------------------------------------

spatTempAgg <- function(df = data$joinedDF, region, subregion, station, study, 
                        variable, stat = NULL, 
                        spatAggFun = c("mean", "median", "sum", "min", "max"),
                        returnDF = F,
                        timeAggLength,
                        timeAggFun = c("mean", "median", "sum", "min", "max"),
                        timeLag,
                        dateEnd = NULL) {
  
  # This captures the args before any more variables are created; for error if no data is pulled in df1
  arguments <- c(as.list(environment()))
  
  # Starting with study; however, this is used to only find station/region/subregions only; its a deadend 
  if (!missing(study)) {
    message("Use the following spatial options to continue running the function.")
    return(df %>% 
             filter(study == !!study) %>% 
             distinct(station, region, subregion) %>% 
             print(n = Inf))
  }
  
  if (missing(region) & missing(station) & missing(subregion)) {
    studiesWithManyStations <- df %>% 
      distinct(station, study) %>% 
      separate(study, c("study1", "study2"), remove = F, fill = "right") %>% 
      arrange(study2, study1) %>% 
      group_by(study) %>% 
      tally() %>% 
      filter(n > 3) %>% 
      pull(study)
    
    message(paste0("The available ", dQuote("region"), " options are:"))
    
    df %>% 
      distinct(region) %>% 
      arrange(region) %>% 
      pull(region) %>% 
      print()
    
    message(paste0("The available ", dQuote("subregion"), " options are:"))
    
    df %>% 
      distinct(subregion) %>% 
      arrange(subregion) %>% 
      pull(subregion) %>% 
      print()
    
    message("The most immediately available ", dQuote("station"), " options are:")
    
    df %>% 
      filter(!study %in% studiesWithManyStations) %>% 
      distinct(station) %>% 
      arrange(station) %>% 
      pull(station) %>% 
      print()
    
    message("For ", dQuote("station"), " within the studies below, use the ", dQuote("study"), " argument to first find the station of interest.")
    return(studiesWithManyStations)
  }
  
  if (!missing(region) & !missing(subregion) & !missing(station)) {
    findSpatAgg <- data.frame(spatialAreas = c("region", "subregion", "station"),
                              value = c(region, subregion, station)) %>% 
      filter(!is.na(value))
    spatAgg <- pull(findSpatAgg, value)
    spatAggArg <- sym(pull(findSpatAgg, spatialAreas))
  } else {
    if (!missing(region)) {
      spatAgg <- region
      spatAggArg <- sym("region")
    } else {
      if (!missing(subregion)) {
        spatAgg <- subregion
        spatAggArg <- sym("subregion")
      } else {
        spatAgg <- station
        spatAggArg <- sym("station")
      }
    }
  }
  
  # Assigning the spatial agg variable
  
  if (missing(variable)) {
    message(paste0("Available ", dQuote("variable"), " options for ", dQuote(spatAggArg), " are:"))
    return(filter(df, !!spatAggArg %in% spatAgg) %>% 
             distinct(variable) %>% 
             arrange(variable) %>% 
             pull(variable) %>% 
             print())
  }
  
  # Now to actually start pulling the DF
  # First need to deal with region/station issues
  df1 <- df %>% 
    filter(!!spatAggArg %in% spatAgg,
           # Then deal with variable that you want
           variable %in% !!variable)
  
  # Is there data...? If not, then you might of used the wrong arguments
  if (nrow(df1) == 0) {
    arguments <- arguments[which(names(arguments) %in% c("region", "subregion", 
                                                         "station", "variable", 
                                                         "stat", "spatAggFun", 
                                                         "timeAggLength", 
                                                         "timeAggFun", "timeLag"))]
    
    stop("\nNo data found. Arguments for this round were: ", 
         paste0(ifelse(nchar(names(arguments)) > 0, 
                       paste0(names(arguments), " = "), ""), 
                unlist(arguments), 
                collapse = ", "),
         call. = F)
  }
  
  # Does the variable have a stat? What stat do you want?
  if (length(unique(df1$stat)) > 1 & is.null(stat)) {
    message("This variable has more than one stat. Please specify which one you want.")
    return(print(unique(df1$stat)))
  }
  
  if (!is.null(stat)) {
    if (!is.na(stat)) {
      df1 <- df1 %>% 
        filter(stat %in% !!stat)
      statName <- unique(df1$stat)
    } else statName <- "Inst"
  } else statName <- "Inst"
  
  # Getting it into date, station, region, variable format
  df1 <- df1 %>% 
    transmute(date,
              variable,
              value,
              !!spatAggArg)
  
  regionColName <- str_remove(spatAgg, " ")
  
  # Add region and stat to variable name
  df1 <- df1 %>% 
    mutate(variable = paste0(variable, "_", regionColName, "_stat", str_to_title(statName)))
  
  if (returnDF) {
    message("Returning DF prior to application of the specified function to the spatial and temporal aggregation steps.")
    return(df1)
  }
  
  # Need these for the newVarName later
  spatAggFunName <- spatAggFun
  timeAggFunName <- timeAggFun
  
  # NOTE: I am supressing warnings here for min/max; there are going to be missing data
  # and using min/max(NA, na.rm = T) returns an Inf warning; all these Inf labels are
  # replaced with NAs anyways
  if (length(spatAggFun) == 1) {
    if (spatAggFun == "mean") {
      spatAggFun <- function(x) mean(x, na.rm = T)
    } else {
      if (spatAggFun == "median") {
        spatAggFun <- function(x) median(x, na.rm = T)
      } else {
        if (spatAggFun == "sum") {
          spatAggFun <- function(x) sum(x, na.rm = T)
        } else {
          if (spatAggFun == "min") {
            spatAggFun <- function(x) suppressWarnings(min(x, na.rm = T))
          } else {
            if (spatAggFun == "max") {
              spatAggFun <- function(x) suppressWarnings(max(x, na.rm = T))
            }
          }
        }
      }
    }
  } else {
    if (!isTRUE(returnDF)) stop("spatAggFun should be mean, median, sum, min, or max.", call. = F)
  }
  
  if (length(timeAggFun) == 1) {
    if (timeAggFun == "mean") {
      timeAggFun <- function(x) mean(x, na.rm = T)
    } else {
      if (timeAggFun == "median") {
        timeAggFun <- function(x) median(x, na.rm = T)
      } else {
        if (timeAggFun == "sum") {
          timeAggFun <- function(x) sum(x, na.rm = T)
        } else {
          if (timeAggFun == "min") {
            timeAggFun <- function(x) suppressWarnings(min(x, na.rm = T))
          } else {
            if (timeAggFun == "max") {
              timeAggFun <- function(x) suppressWarnings(max(x, na.rm = T))
            }
          }
        }
      }
    }
  } else {
    if (!isTRUE(returnDF)) stop("timeAggFun should be mean, median, sum, min, or max.", call. = F)
  }
  
  # Want to use entire range of df1. This will be used later to lag correctly
  # fullDateString <- seq(min(df1$date), max(df1$date), by = 1)
  if (is.null(dateEnd)) {
    fullDateString <- seq(min(df1$date), as.Date(Sys.time()), by = 1)
  } else {
    fullDateString <- seq(min(df1$date), as.Date(dateEnd), by = 1)
  }
  
  # Working on spatAgg now
  # For most variables, this will simply be mean of; but for some, you may want a min/max/sum
  # of the area? 
  
  # Pulling variable name; will be used later to make dynamic name
  newVarName <- unique(df1$variable) %>% 
    paste0(., "_sp", str_to_title(spatAggFunName))
  
  # Now applying the spatial aggregation
  df2 <- df1 %>% 
    arrange(date) %>% 
    group_by(date, variable, !!spatAggArg) %>% 
    summarise(value = spatAggFun(value), .groups = "drop")
  
  df3 <- df2 %>% 
    complete(date = fullDateString) %>% 
    arrange(date)
  
  if (timeAggLength > 1) {
    # Need to basically lag each value X number of times, where x = timeAggLength duration to get time expansion
    # The timeAggLength - 1 here is intended. This is b/c the first datapoint is the current day's data
    df4 <- lapply(1:(timeAggLength - 1), function(x) {
      df <- df3 %>% 
        transmute(date,
                  "valueLag_{x}" := lag(value, x))
    }) %>% 
      reduce(full_join, by = "date") %>% 
      full_join(df3, by = "date") %>%
      rowwise() %>% 
      mutate(value = timeAggFun(c_across(contains("value")))) %>% 
      select(names(df3)) %>% 
      # Remove rowwise to lag later
      ungroup()
    
    # Renaming variable to include the temporal Agg, rolling window here
    newVarName <- paste0(newVarName, "_tm", str_to_title(timeAggFunName), timeAggLength, "d")
    
  } else {
    df4 <- df3
    
    warning("A timeAggLength = 1 day was used. Only your spatial aggregation was applied.", call. = F)
    # Employing the Inst label here too; although it may be spatially summarized, it is not temporally
    newVarName <- paste0(newVarName, "_tm1dInst")
  }
  
  # Formulating newVarName before finalizing
  newVarName <- paste0(newVarName, "Lag", timeLag, "d")
  
  dfLagged <- df4 %>% 
    # Final df will be just date and the variable of interest
    transmute(date,
              value,
              valueTimeLagged = lag(value, timeLag)) %>% 
    # check here if you want to see if the lag worked or not. The next step will remove NAs
    # # THIS STEP will remove entries where an NA was recorded too; so not just "padded" dates, but will
    # # also remove actual sampling date with NA value
    # # This is why nrow(returnDF) may not equal the fin DF here
    # filter(!is.na(valueTimeLagged)) %>% # Doesn't seem like Jereme want them removed
    # Replacing all NaN, -Inf, and Inf with NA
    mutate(valueTimeLagged = ifelse(is.nan(valueTimeLagged) | is.infinite(valueTimeLagged),
                                    NA, valueTimeLagged)) %>% 
    transmute(date,
              "{newVarName}" := valueTimeLagged)
  dfLagged
}


# Wrapper func ------------------------------------------------------------

spatTempAgg_makeDF <- function(file, list = F, pbar = T) {
  inputs <- read_csv(file, col_types = 'ccccccdcd')
  
  if (pbar) {
    if(!require(pbapply)) stop("Please install pbapply package for progress bar display or use pbar = F.")
    
    df <- pbmapply(spatTempAgg, 
                   region = inputs$region,
                   subregion = inputs$subregion,
                   station = inputs$station,
                   variable = inputs$variable,
                   stat = inputs$stat,
                   spatAggFun = inputs$spatAggFun,
                   timeAggLength = inputs$timeAggLength,
                   timeAggFun = inputs$timeAggFun,
                   timeLag = inputs$timeLag,
                   SIMPLIFY = F)
  } else {
    # Default of mapply `SIMPLIFY` = T
    df <- mapply(spatTempAgg, 
                 region = inputs$region,
                 station = inputs$station,
                 variable = inputs$variable,
                 stat = inputs$stat,
                 spatAggFun = inputs$spatAggFun,
                 timeAggLength = inputs$timeAggLength,
                 timeAggFun = inputs$timeAggFun,
                 timeLag = inputs$timeLag,
                 SIMPLIFY = F)
  }
  
  if (list) return(df)
  
  dfFin <- df %>% 
    reduce(full_join, by = c("date"))
  
  dfFin
}

stop()


##################################################
#My code starts here 
##################################################
#Load data
#Only LAD winter-run with no ad-clip, expanded salvage
raw_df <- spatTempAgg_makeDF(file = "aggFuncInputs_2021_07_23_bm.csv")

#Only keep year 2009-2019 from Dec-March, add year term (for water year)
custom_df <- raw_df %>% filter(date>='2008-12-01'&date<='2019-05-31') %>%filter(month(date) %in% c(12,1,2,3,4,5)) %>% mutate(Year=ifelse(month(date)>=10,year(date)+1,year(date)))
#Fill N/A with 0
custom_df[is.na(custom_df)] <- 0

#Store covariate names
cov_names<- colnames(custom_df[3:32])
cov_names

#convert data to integer
#custom_df[, c(2:16)] <- sapply(custom_df[, c(2:16)], as.integer)

##Evaluation of whether OLS or Poisson regression is better. Both are messy, but OLS provides R-squared value that's more intuitive
#for managers, so let's go with OLS
test1<-lm(data=custom_df,expandedSalvage_WRLAD_wild_full_salvage_statInst_spMean_tm1dInstLag0d~expandedSalvage_WRLAD_wild_full_salvage_statInst_spMean_tm1dInstLag1d)
summary(test1)
#plot(test1)

#test2<-glm(data=custom_df,expandedSalvage_WRLAD_wild_full_salvage_statInst_spMean_tm1dInstLag0d~expandedSalvage_WRLAD_wild_full_salvage_statInst_spMean_tm1dInstLag1d,family=poisson())
#summary(test2)

#If we want to get adjusted r squared and coefficient
summary(test1)$adj.r.squared
summary(test1)$coefficients[2,1]

#Split data per year
df_split <- split(custom_df, f = custom_df$Year)


#Do the loop to get adjusted r squared for each timestep for each year
model_results<-list()
for(i in 1:length(df_split)){
  model_results[[i]]<-data.frame(Rsquared=as.vector(sapply(df_split[[i]][, c(cov_names)], function(x) summary(lm(df_split[[i]]$expandedSalvage_WRLAD_wild_full_salvage_statInst_spMean_tm1dInstLag0d~x))$adj.r.squared)),
                                 Year=2008+i,
                                 Lag=1:30)
}

#Combine list
model_results_combined <- do.call("rbind", model_results)

plot(model_results_combined$Rsquared~model_results_combined$Lag)

#Replace negative r squared with 0
model_results_combined$Rsquared<-ifelse(model_results_combined$Rsquared<0,0,model_results_combined$Rsquared)


#Figure
jitter_plot<- model_results_combined %>%
  # Add a new column called 'bin': cut the initial 'carat' in bins
  mutate(lag_factor=as.factor(Lag)) %>%
  # plot
  ggplot(aes(x=lag_factor, y=Rsquared) ) +
  theme_bw()+
  geom_boxplot(fill="#69b3a2") +  
  ylim(0, 0.6)+
  labs(y=expression(italic(R)^2),x="Time lag in days")+
  theme(axis.title.x = element_text(size = 22, angle = 00), 
        axis.title.y = element_text(size = 22, angle = 90),
        axis.text.x= element_text(size = 16, angle = 00),
        axis.text.y= element_text(size = 16, angle = 00),
        strip.text = element_text(size = 20),
        legend.text = element_text(size=18),
        legend.key.size = unit(1.5, 'cm'),
        plot.title = element_text(size=22))
jitter_plot


png(filename="BoxPlot_salvage_autocorrelation_r2.png", 
    type="cairo",
    units="in", 
    width=14*1, 
    height=9*1, 
    pointsize=14, 
    res=300)
jitter_plot
dev.off()


tiff(filename="PACF_salvage_Winterrun_2009-2019_dec-may.tiff",
     type="cairo",
     units="in", 
     width=4, #10*1, 
     height=3, #22*1, 
     pointsize=5, #12, 
     res=300,
     compression="lzw")
pacf(custom_df$expandedSalvage_WRLAD_wild_full_salvage_statInst_spMean_tm1dInstLag0d,main="Partial auto-correlation function analysis for continuous data from Dec 2018 to May 2019")
dev.off()



tiff(filename="ACF_salvage_Winterrun_2009-2019_dec-may.tiff",
     type="cairo",
     units="in", 
     width=4, #10*1, 
     height=3, #22*1, 
     pointsize=5, #12, 
     res=300,
     compression="lzw")
acf(custom_df$expandedSalvage_WRLAD_wild_full_salvage_statInst_spMean_tm1dInstLag0d,main="Auto-correlation function analysis for continuous data from Dec 2018 to May 2019")
dev.off()
