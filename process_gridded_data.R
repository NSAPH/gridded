# TODO
# o write a function to get the summary statistics
# o use two years to get the correct median and quantiles
# o 'filename' as a parameter
# o explain how to merge with former New England bounding box

library(data.table)
library(R.matlab)

sites <- fread("ne_grids.csv")

##----- A comparison with the bounding box we defined

old_sites <- data.table(data.frame(readMat("~/Dropbox/HEI_MeasError/QD_Gpred/matlab_files/USAODGridSite.mat")))
setnames(old_sites, names(old_sites), names(sites))

ne_sites <- old_sites[which((old_sites$Lat >= summary(sites$Lat)[1] &
                               old_sites$Lat <= summary(sites$Lat)[6]) 
                            & (old_sites$Lon < summary(sites$Lon)[6] &
                                 old_sites$Lon > summary(sites$Lon)[1])), ]

M <- merge(ne_sites, sites, by = c("Lat", "Lon"), all.x = TRUE)
M

##----- Calculate descriptives: mean, median, quantiles

calculate_descriptives <- function(data = D, only_mean = FALSE, name = "Quantity") {
  tData <- data.table(t(data))
  tData <- tData[1:(nrow(tData) - 2), ] # remove 'Date' and 'Season'
  tData <- tData[, lapply(.SD, as.numeric)] # convert back to numeric
  Aver <- data.table(Aver = rowMeans(tData, na.rm = TRUE))
  if (!only_mean) {
    Quantiles <- data.table(t(apply(tData, 1, quantile, c(0.1, 0.5, 0.9), na.rm = TRUE)))
    setnames(Quantiles, names(Quantiles), c("Q10", "Median", "Q90"))
  }
  if (only_mean) {
    result <- Aver
  } else if (!only_mean) {
    result <- cbind(Aver, Quantiles)
  } else {
    stop("Unknown argument for parameter 'name'.")
  }
  setnames(result, names(result), paste(name, names(result), sep = "_"))
  return(result)
}

##----- Add Date and Season to dataset

add_date_and_season <- function(data = D, year = 2001) {
  begin_january <- paste0(year, "-01-01")
  end_december <- paste0(year, "-12-31")
  begin_summer <- paste0(year, "-06-01")
  end_summer <- paste0(year, "-09-01")
  end_february <- paste0(year, "-03-01")
  begin_december <- paste0(year, "-12-01")
  
  Date <- seq.Date(from = as.Date(begin_january), to = as.Date(end_december), by = 1)
  data[, Date := Date]
  data[, Season := ""]
  data[Date >= begin_summer & Date < end_summer, Season := "summer"]
  # Winter is trickier: December and January/February belong to different files
  # We have to stack years later on
  data[Date >= begin_january & Date < end_february, Season := "winter_janfeb"] # '<' to keep the same code in leap years
  data[Date >= begin_december & Date <= end_december, Season := "winter_dec"]
  
  return(data)
}

##----- Create filename

create_filename <- function(quantity = "air.cf", year = 2000) {
  if (!quantity %in% c("air.cf", "apcp", "dlwrf", "evap", "lhtfl", "prate",
                       "shtfl", "shum.2m", "snowc", "soilm", "tcdc", "ulwrf",
                       "uwnd.10m", "vwnd.10m", "weasd"))
    stop("Wrong 'quantity' argument")
  else
    filename <- paste0("REANALYSIS_", quantity, "_DailyMean_ne_grids_", year, "0101_", year, "1231.mat")
  return(filename)
}

##----- Example: air.sfc for year 2001

process_gridded_data <- function(quantity = "air.cf", year = 2001, seasonal = TRUE) {
  
  if (year > 2000) {
    year_before <- year - 1
    previous_year <- TRUE
  }
  
  filename <- create_filename(quantity = quantity, year = year)
  raw_data <- data.table(data.frame(readMat(filename)))
  D <- add_date_and_season(data = raw_data, year = year)
  
  if (previous_year) {
    filename_year_before <- create_filename(quantity = quantity, year = year_before)
    raw_data_year_before <- data.table(data.frame(readMat(filename_year_before)))
    D_year_before <- add_date_and_season(data = raw_data_year_before, year = year_before)
  }
  
  # Annual dataset
  Annual <- calculate_descriptives(data = D, only_mean = FALSE, name = "Annual")
  
  if (seasonal) {
    # Summer and Winter datasets, with December from the year before
    D_Summer <- D[Season == "summer"]
    if (previous_year)
      D_Winter <- rbind(D_year_before[Season == "winter_dec"], D[Season == "winter_janfeb"])
    else
      D_Winter <- D[Season == "winter_janfeb"]
    # Summer
    Summer <- calculate_descriptives(data = D_Summer, only_mean = FALSE, name = "Summer")
    # Winter
    Winter <- calculate_descriptives(data = D_Winter, only_mean = FALSE, name = "Winter")
  }
  
  if (seasonal)
    result <- cbind(Year = year, Annual, Summer, Winter)
  else
    result <- cbind(Year = year, Annual)
  
  return(result)
}

air.sfc_2000 <- process_air.sfc(year = 2000)
air.sfc_2001 <- process_air.sfc(year = 2001, seasonal = FALSE)
air.sfc_2002 <- process_air.sfc(year = 2002)


