#' Calculate descriptives: mean, median, percentiles
#'
#' @param data a data table of dimensions (days x monitors).
#' @param only_mean logical.  If TRUE, onthe the annual mean is returned.
#'                  If FALSE, the mean, median, 1%, 5%, 10%, 90%, 95%, 99%
#'                  percentiles are returned.
#' @param name the character that will be used a prefix in column names
#'
#' @return a (monitor x number of descriptives) data table

calculate_descriptives <- function(data = D, only_mean = FALSE, name = "Quantity") {
  tData <- data.table(t(data))
  tData <- tData[1:(nrow(tData) - 2), ] # remove 'Date' and 'Season'
  tData <- tData[, lapply(.SD, as.numeric)] # convert back to numeric
  Aver <- data.table(Aver = rowMeans(tData, na.rm = TRUE))
  if (!only_mean) {
    Quantiles <- data.table(t(apply(tData,
                                    1,
                                    quantile, c(0.05, 0.1, 0.5, 0.9, 0.95),
                                    na.rm = TRUE)))
    setnames(Quantiles,
             names(Quantiles),
             c("Q05", "Q10", "Median", "Q90", "Q95"))
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

#' Add Date and Season to a data table
#'
#' @param data a data table read from a year of reanalysis data
#' @param year the corresponding reanalysis year
#'
#' @return the data with two added rows: date and season

add_date_and_season <- function(data = D, year = 2001) {

  begin_january   <- paste0(year, "-01-01")
  begin_february  <- paste0(year, "-02-01")
  begin_march     <- paste0(year, "-03-01")
  begin_april     <- paste0(year, "-04-01")
  begin_may       <- paste0(year, "-05-01")
  begin_june      <- paste0(year, "-06-01")
  begin_july      <- paste0(year, "-07-01")
  begin_august    <- paste0(year, "-08-01")
  begin_september <- paste0(year, "-09-01")
  begin_october   <- paste0(year, "-10-01")
  begin_november  <- paste0(year, "-11-01")
  begin_december  <- paste0(year, "-12-01")
  end_december    <- paste0(year, "-12-31")

  Date <- seq.Date(from = as.Date(begin_january), to = as.Date(end_december), by = 1)
  data[, Date := Date]
  data[, Season := ""]
  data[Date >= begin_january & Date < begin_march, Season := "winter"]
  data[Date >= begin_december & Date <= end_december, Season := "winter"]
  data[Date >= begin_march & Date < begin_june, Season := "spring"]
  data[Date >= begin_june & Date < begin_september, Season := "summer"]
  data[Date >= begin_september & Date < begin_december, Season := "fall"]

  return(data)
}

#' Create filename
#'
#' @param quantity the quantity to be processed
#' @param year the corresponding reanalysis year

create_filename <- function(quantity = "air.sfc", year = 2000) {
  if (!quantity %in% c("air.sfc", "apcp", "dlwrf", "evap", "lhtfl", "prate",
                       "shtfl", "shum.2m", "snowc", "soilm", "tcdc", "ulwrf",
                       "uwnd.10m", "vwnd.10m", "weasd"))
    stop("Wrong 'quantity' argument")
  else
    filename <- paste0("REANALYSIS_", quantity, "_DailyMean_ne_grids_", year, "0101_", year, "1231.mat")
  return(filename)
}

#' Process gridded data
#'
#' @param quantity the quantity to be processed
#' @param year the corresponding reanalysis year
#' @param seasonal logical.
#' @param only_mean logical.  If TRUE, onthe the annual mean is returned.
#'                  If FALSE, the mean, median, 5%, 10%, 90%, 95%
#'                  percentiles are returned.
#' @param folder the folder where the reanalysis data is downloaded
#'
#' @return a data table (monitor x number of descriptives)

process_gridded_data <- function(quantity = "air.sfc", year = 2001,
                                 seasonal = TRUE, only_mean = FALSE,
                                 folder = "~/Documents/Tmp/MeasurementError") {

  filename <- file.path(folder, create_filename(quantity = quantity, year = year))
  raw_data <- data.table(data.frame(readMat(filename)))
  D <- add_date_and_season(data = raw_data, year = year)

  # Annual dataset
  Annual <- calculate_descriptives(data = D, only_mean = only_mean, name = "Annual")

  if (seasonal) {
    # Season datasets
    D_Winter <- D[Season == "winter"]
    D_Spring <- D[Season == "spring"]
    D_Summer <- D[Season == "summer"]
    D_Fall   <- D[Season == "fall"]
    Winter <- calculate_descriptives(data = D_Winter, only_mean = only_mean, name = "Winter")
    Spring <- calculate_descriptives(data = D_Spring, only_mean = only_mean, name = "Spring")
    Summer <- calculate_descriptives(data = D_Summer, only_mean = only_mean, name = "Summer")
    Fall   <- calculate_descriptives(data = D_Fall, only_mean = only_mean, name = "Fall")
  }

  if (seasonal)
    result <- cbind(Year = year, Annual, Winter, Spring, Summer, Fall)
  else
    result <- cbind(Year = year, Annual)

  return(result)
}

list_reanalysis_files <- function() {
  quantities <- c("air.sfc", "apcp", "dlwrf", "evap", "lhtfl", "prate",
                  "shtfl", "shum.2m", "snowc", "soilm", "tcdc", "ulwrf",
                  "uwnd.10m", "vwnd.10m", "weasd")
  years <- 2000:2012
  processes <- expand.grid(quantities, years)
  names(processes) <- c("quantities", "years")
  processes$process <- 0:(nrow(processes) - 1)
  return(processes)
}

# negrid <- read.csv("~/Documents/Tmp/MeasurementError/ne_grids.csv")
# devtools::use_data(negrid)
