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
  data[Date >= begin_january & Date < end_february, Season := "winter"]
  data[Date >= begin_december & Date <= end_december, Season := "winter"]

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
    # Summer and Winter datasets
    D_Summer <- D[Season == "summer"]
    D_Winter <- D[Season == "winter"]
    # Summer
    Summer <- calculate_descriptives(data = D_Summer, only_mean = only_mean, name = "Summer")
    # Winter
    Winter <- calculate_descriptives(data = D_Winter, only_mean = only_mean, name = "Winter")
  }

  if (seasonal)
    result <- cbind(Year = year, Annual, Summer, Winter)
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
