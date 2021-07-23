
#==========================================================================#
#  Script created by Trinh Nguyen, CDFW Water Branch in February 3, 2021   #
#  Last modified by TN on April 13, 2021                                   #
#==========================================================================#

# The purpose of this script is to create a function that will aggregate a specific predictor from
# the joined DF spatially and temporally, with a temporal lag variable. An input wrapper is included

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

# Testing now. These should step the user through what the function ------
# Here, you can choose station or region
spatTempAgg(df = data$joinedDF)
# Interested in a study
spatTempAgg(df = data$joinedDF, study = "CDECTemp")
# Available variables for that station/region
spatTempAgg(df = data$joinedDF, region = "South Delta")
# Station or region works as the spatial agg
spatTempAgg(df = data$joinedDF, station = "salvage")
# For subregion
spatTempAgg(df = data$joinedDF, subregion = "South Delta Facilities")

# If that variable has different summary stats, then this will show it
spatTempAgg(df = data$joinedDF, station = "salvage", variable = "FL_WRDNA")
# This gives an error to specify spatAggFun
spatTempAgg(df = data$joinedDF, station = "salvage", variable = "FL_WRDNA", stat = "mean")
# This gives an error to specify timeAggFun
spatTempAgg(df = data$joinedDF, station = "salvage", variable = "FL_WRDNA", 
            stat = "mean", spatAggFun = "mean")
# This gives an error about missing timeAggLength
spatTempAgg(df = data$joinedDF, station = "salvage", variable = "FL_WRDNA", 
            stat = "mean", spatAggFun = "mean", timeAggFun = "mean")
# This gives an error about missing timeLag
spatTempAgg(df = data$joinedDF, station = "salvage", variable = "FL_WRDNA", 
            stat = "mean", spatAggFun = "mean", timeAggFun = "mean",
            timeAggLength = 1)
# Not going to give timeLag a default of 0 since it may take a while to calculate; rather have
# user know exactly what they want before investing computing time

# Testing aggregation over region and time --------------------------------
# Starting with day first
spatTempAgg(df = data$joinedDF, region = "South Delta", 
            variable = "waterTemp",
            stat = "mean",
            spatAggFun = "mean",
            timeAggFun = "mean",
            timeAggLength = 1,
            timeLag = 1)

# A diff lag
spatTempAgg(df = data$joinedDF, region = "South Delta", 
            variable = "waterTemp",
            stat = "mean",
            spatAggFun = "mean",
            timeAggLength = 1,
            timeAggFun = "mean",
            timeLag = 0)

# A diff length
spatTempAgg(df = data$joinedDF, region = "South Delta", 
            variable = "waterTemp",
            stat = "mean",
            spatAggFun = "mean",
            timeAggLength = 5,
            timeAggFun = "mean",
            timeLag = 5)

##### PLEASE NOTE: timeAggLength considers the day of a day, so 10-04, 5 days out = 10-08; 
# However, timeLag DOES NOT consider the day of a day, so 10-08, 5 days ago would be 10-03 NOT 10-04
# I can easily make it consider day of a day, but inuitively when I ask myself "what is the
# value 5 days ago, I think of the 3rd and not the 4th. It can make intuitive sense for the
# rolling Avg to include day of, but not the Agg for me.

# Another var/stat?
# Is naming right?
spatTempAgg(df = data$joinedDF, station = "salvage", 
            variable = "waterTemp",
            stat = "mean",
            spatAggFun = "mean",
            timeAggLength = 150,
            timeAggFun = "mean",
            timeLag = 0)

# Another function? 
spatTempAgg(df = data$joinedDF, station = "salvage", 
            variable = "waterTemp",
            stat = "mean",
            spatAggFun = "max",
            timeAggLength = 150,
            timeAggFun = "mean",
            timeLag = 0)
# Note that min/max may give warnings about missing values and -Inf and Inf

# Using RBDD instead, with aggFnc as max
spatTempAgg(df = data$joinedDF, station = "RBDD", 
            variable = "waterTemp",
            spatAggFun = "max",
            timeAggLength = 14,
            timeAggFun = "mean",
            timeLag = 14)

# Using RBDD instead, with both max
spatTempAgg(df = data$joinedDF, station = "RBDD", 
            variable = "waterTemp",
            spatAggFun = "max",
            timeAggLength = 14,
            timeAggFun = "max",
            timeLag = 14)

# Median
# Using RBDD instead, with both max
spatTempAgg(df = data$joinedDF, station = "RBDD", 
            variable = "waterTemp",
            spatAggFun = "median",
            timeAggLength = 14,
            timeAggFun = "median",
            timeLag = 21)

# Min
spatTempAgg(df = data$joinedDF, station = "RBDD", 
            variable = "waterTemp",
            spatAggFun = "min",
            timeAggLength = 14,
            timeAggFun = "min",
            timeLag = 21)

# Sum
spatTempAgg(df = data$joinedDF, station = "RBDD", 
            variable = "waterTemp",
            spatAggFun = "sum",
            timeAggLength = 14,
            timeAggFun = "sum",
            timeLag = 21)

# Seems ok for these simpler functions.

# For functions that may be too complex, can ask for the function to return the DF right b4
# the function is applied to do it manually
spatTempAgg(df = data$joinedDF, station = "salvage", 
            variable = "FL_WRDNA",
            stat = "mean",
            returnDF = T)

# Testing dataframe that Jereme set up
data.frame(date = c(as.Date("2021-03-01"), as.Date("2021-03-04"), as.Date("2021-03-12"), as.Date("2021-03-15")),
           station_A = c(11, NA, NA, 12.5),
           station_B = c(NA, 15, NA, NA),
           station_C = c(NA, NA, 14, NA)) %>% 
  transmute(date, 
            region = "try", variable = "waterTemp", station = NA,
            value = coalesce(station_A, station_B, station_C)) %>% 
  spatTempAgg(df = ., region = "try",
              variable = "waterTemp",
              spatAggFun = "mean",
              timeAggLength = 5,
              timeAggFun = "mean",
              timeLag = 0) %>% 
  print(n = Inf)

data.frame(date = c(as.Date("2021-03-01"), as.Date("2021-03-04"), as.Date("2021-03-12"), as.Date("2021-03-15")),
           station_A = c(11, NA, NA, 12.5),
           station_B = c(NA, 15, NA, NA),
           station_C = c(NA, NA, 14, NA)) %>% 
  transmute(date, 
            # Fake columns to satisfy integrated DF format
            region = "try", variable = "waterTemp", station = NA,
            value = coalesce(station_A, station_B, station_C)) %>% 
  spatTempAgg(df = ., region = "try",
              variable = "waterTemp",
              spatAggFun = "mean",
              timeAggLength = 5,
              timeAggFun = "mean",
              timeLag = 5) %>% 
  print(n = Inf)

# Restricting it to a date now
data.frame(date = c(as.Date("2021-03-01"), as.Date("2021-03-04"), as.Date("2021-03-12"), as.Date("2021-03-15")),
           station_A = c(11, NA, NA, 12.5),
           station_B = c(NA, 15, NA, NA),
           station_C = c(NA, NA, 14, NA)) %>% 
  transmute(date, 
            # Fake columns to satisfy integrated DF format
            region = "try", variable = "waterTemp", station = NA,
            value = coalesce(station_A, station_B, station_C)) %>% 
  spatTempAgg(df = ., region = "try",
              variable = "waterTemp",
              spatAggFun = "mean",
              timeAggLength = 5,
              timeAggFun = "mean",
              timeLag = 5,
              dateEnd = "2021-03-15") %>% 
  print(n = Inf)
# OK
# Try with a larger agg
data.frame(date = c(as.Date("2021-03-01"), as.Date("2021-03-04"), as.Date("2021-03-12"), as.Date("2021-03-15")),
           station_A = c(11, NA, NA, 12.5),
           station_B = c(NA, 15, NA, NA),
           station_C = c(NA, NA, 14, NA)) %>% 
  transmute(date, 
            # Fake columns to satisfy integrated DF format
            region = "try", variable = "waterTemp", station = NA,
            value = coalesce(station_A, station_B, station_C)) %>% 
  spatTempAgg(df = ., region = "try",
              variable = "waterTemp",
              spatAggFun = "mean",
              timeAggLength = 14,
              timeAggFun = "mean",
              timeLag = 0) %>% 
  print(n = Inf)
# Overshoot?
data.frame(date = c(as.Date("2021-03-01"), as.Date("2021-03-04"), as.Date("2021-03-12"), as.Date("2021-03-15")),
           station_A = c(11, NA, NA, 12.5),
           station_B = c(NA, 15, NA, NA),
           station_C = c(NA, NA, 14, NA)) %>% 
  transmute(date, 
            # Fake columns to satisfy integrated DF format
            region = "try", variable = "waterTemp", station = NA,
            value = coalesce(station_A, station_B, station_C)) %>% 
  spatTempAgg(df = ., region = "try",
              variable = "waterTemp",
              spatAggFun = "mean",
              timeAggLength = 21,
              timeAggFun = "mean",
              timeLag = 0) %>% 
  print(n = Inf)
# As of 03.29, this stops at 03.29, current date, as designed. OK

# Testing wrapper ---------------------------------------------------------

# Need to provide a matrix of what you want...

spatTempAgg(df = data$joinedDF, subregion = "Suisun Marsh")

check <- spatTempAgg_makeDF(file = "aggFuncInputs_try.csv")
names(check)

# NOTE: the min and max variables are rolling min/maxes. you may get min/max values that are
# larger/smaller than their mean counterpart for the same day IF the aggregation for the mean
# counterpart is longer
 
# Eg of this:
data.frame(date = c(as.Date("2021-03-01"), as.Date("2021-03-04"), as.Date("2021-03-12"), as.Date("2021-03-15")),
           station_A = c(11, NA, NA, 12.5),
           station_B = c(NA, 15, NA, NA),
           station_C = c(NA, NA, 14, NA)) %>% 
  transmute(date, 
            # Fake columns to satisfy integrated DF format
            region = "try", variable = "waterTemp", station = NA,
            value = coalesce(station_A, station_B, station_C)) %>% 
  spatTempAgg(df = ., region = "try",
              variable = "waterTemp",
              spatAggFun = "mean",
              timeAggLength = 14,
              timeAggFun = "mean",
              timeLag = 0) %>% 
  full_join(data.frame(date = c(as.Date("2021-03-01"), as.Date("2021-03-04"), as.Date("2021-03-12"), as.Date("2021-03-15")),
                       station_A = c(11, NA, NA, 12.5),
                       station_B = c(NA, 15, NA, NA),
                       station_C = c(NA, NA, 14, NA)) %>% 
              transmute(date, 
                        # Fake columns to satisfy integrated DF format
                        region = "try", variable = "waterTemp", station = NA,
                        value = coalesce(station_A, station_B, station_C)) %>% 
              spatTempAgg(df = ., region = "try",
                          variable = "waterTemp",
                          spatAggFun = "max",
                          timeAggLength = 7,
                          timeAggFun = "max",
                          timeLag = 0),
            by = "date") %>% 
  full_join(data.frame(date = c(as.Date("2021-03-01"), as.Date("2021-03-04"), as.Date("2021-03-12"), as.Date("2021-03-15")),
                       station_A = c(11, NA, NA, 12.5),
                       station_B = c(NA, 15, NA, NA),
                       station_C = c(NA, NA, 14, NA)) %>% 
              transmute(date, 
                        value = coalesce(station_A, station_B, station_C)),
            by = "date") %>% 
  mutate(check = ifelse(waterTemp_try_statInst_spMean_tmMean14dLag0d > waterTemp_try_statInst_spMax_tmMax7dLag0d,
                        "check", "ok")) %>% 
  full_join(data.frame(date = c(as.Date("2021-03-01"), as.Date("2021-03-04"), as.Date("2021-03-12"), as.Date("2021-03-15")),
                       station_A = c(11, NA, NA, 12.5),
                       station_B = c(NA, 15, NA, NA),
                       station_C = c(NA, NA, 14, NA)) %>% 
              transmute(date, 
                        # Fake columns to satisfy integrated DF format
                        region = "try", variable = "waterTemp", station = NA,
                        value = coalesce(station_A, station_B, station_C)) %>% 
              spatTempAgg(df = ., region = "try",
                          variable = "waterTemp",
                          spatAggFun = "mean",
                          timeAggLength = 7,
                          timeAggFun = "mean",
                          timeLag = 0),
            by = "date") %>% 
  View()
# The 3 rows of "check" here demonstrates this problem, where the expansion of the 14.0 value
# from 3-12 for 14 days creates a larger mean at these 3 rows than the 7 day max variant; since
# the 14.0 value does not extend that far in the 7 days variant, the max is simply 12.5 and not
# max(c(12.5, 14)), giving a lower value than the mean. The mean values for the 7 day variant 
# shows that these 3 rows are == to the max since it's the only value there.
