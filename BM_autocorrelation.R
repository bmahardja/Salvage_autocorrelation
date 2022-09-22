
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


salvage_data_full<-spatTempAgg(df = data$joinedDF,station="salvage",variable="expandedSalvage_WRLAD_wild_full", spatAggFun = "mean",
                  timeAggLength = 1,
                  timeAggFun = "mean",
                  timeLag = 0)

salvage_data_edit<- salvage_data_full %>% filter(year(date)>=2009&year(date)<=2019) %>%filter(month(date) %in% c(12,1,2,3,4,5))

salvage_data_edit<- na.omit(salvage_data_edit)


acf(salvage_data_edit$expandedSalvage_WRLAD_wild_full_salvage_statInst_spMean_tm1dInstLag0d)
acf(salvage_data_edit$expandedSalvage_WRLAD_wild_full_salvage_statInst_spMean_tm1dInstLag0d, type="correlation")
pacf(salvage_data_edit$expandedSalvage_WRLAD_wild_full_salvage_statInst_spMean_tm1dInstLag0d)


tiff(filename="Figure_ACF_expanded_salvage_Winterrun_2009-2019_dec-may.tiff",
     type="cairo",
     units="in", 
     width=4, #10*1, 
     height=3, #22*1, 
     pointsize=5, #12, 
     res=300,
     compression="lzw")
acf(salvage_data_edit$expandedSalvage_WRLAD_wild_full_salvage_statInst_spMean_tm1dInstLag0d, main="")
dev.off()


tiff(filename="Figure_PACF_expanded_salvage_Winterrun_2009-2019_dec-may.tiff",
     type="cairo",
     units="in", 
     width=4, #10*1, 
     height=3, #22*1, 
     pointsize=5, #12, 
     res=300,
     compression="lzw")
pacf(salvage_data_edit$expandedSalvage_WRLAD_wild_full_salvage_statInst_spMean_tm1dInstLag0d, main="")
dev.off()

#Export data for Mike
write.csv(custom_df,row.names=F,file = "WR_wild_salvage_2009-2019_with_lag.csv")
