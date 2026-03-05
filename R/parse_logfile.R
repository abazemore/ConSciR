#' Parse datalogger files
#'
#' @description
#' Extracts temperature and humidity data from a directory of logfiles.
#'
#' @param directory A directory of files from a single brand, .csv or .xls only for Rotronic.
#' @param Site Character string specifying site name when not recoverable from the file.
#'   Default is "Site".
#' @param brand The logger brand as a string.
#'
#' @returns dat, a data frame containing the raw TRH data, with columns
#' for Site, sensor, date, temperature, and relative humidity.
#' @export
#'
#' @examples parse_brand(dir("logfiles/tinytag", full.names = TRUE), "Anonymous Library", "tinytag")
#'
parse_brand <- function (directory,
                         Site = "Site",
                         brand = FALSE,
                         sheet = "Hanwell",
                         ...) {
  datalist <- dir(directory, full.names = TRUE)

  if (brand %in% c(
    "hanwell",
    "meaco",
    "miniclima",
    "previous",
    "rotronic",
    "tandd",
    "tinytag",
    "trend"
  ))
  {
    datalist <-   switch(
      brand,
      "hanwell" = lapply(datalist, tidy_Hanwell, Site = Site, sheet = sheet),
      "meaco" = lapply(datalist, parse_meaco),
      "miniclima" = lapply(datalist, parse_miniClima, Site = Site),
      "previous" = lapply(datalist, parse_previous, Site = Site),
      "rotronic" = lapply(datalist, parse_rotronic, Site = Site),
      "tandd" = lapply(datalist, parse_tandd, Site = Site),
      "tinytag" = lapply(datalist, parse_tinytag, Site = Site),
      "trend" = lapply(datalist, parse_trendBMS, Site = Site)
    )
  } else {
    stop('Brand not supported')
  }
  datalist <- datalist[is.vector(datalist)]

  dat <- combine_data(datalist)

  dat
}

#' Parse Hanwell file
#'
#' @param filepath .csv logfile containing a summary of TRH data.
#'
#' @returns dat, a dataframe in a standard format used by other functions in this package
#'
#' @noRd
# parse_hanwell <- function(filepath) {
#   message("Parsing as Hanwell")
#
#   dat
# }

#' Parse Meaco file
#'
#' @description
#' Extracts temperature and humidity data from Meaco logfile.
#'
#' @param filepath path to logfile as a string
#'
#' @inherit parse_miniClima returns
#'

#' @noRd
parse_meaco <- function(filepath) {
  message("Parsing as Meaco")
  file_head <- readr::read_csv(filepath, n_max = 1)
  if ('LUX' %in% names(file_head)) {
    return("Temperature and humidity logs only")
  }
  # Pre-Gingerbread export structure
  else if ("DATE" %in% names(file_head)) {
    dat <- readr::read_csv(filepath) |>
      dplyr::mutate(
        Site = as.character(RECEIVER),
        Sensor = as.character(TRANSMITTER),
        Date = as.POSIXct(DATE),
        Temp = as.numeric(TEMPERATURE),
        RH = as.numeric(HUMIDITY),
        .keep = "none"
      )
  }
  else if (ncol(file_head) == 1 &&
           stringr::str_detect(names(file_head), ' - ID')) {
    # Logger info in first line in format "Site - Sensor - ID:00"
    Receiver <- stringr::str_extract(colnames(file_head), '^.*?(?= - )')
    Sensor <- stringr::str_extract(colnames(file_head), '(?<= - ).*?(?= - )')

    dat <- readr::read_csv(filepath, skip = 1) |>
      dplyr::mutate(
        Site = as.character(Receiver),
        Sensor = as.character(Sensor),
        Date = lubridate::parse_date_time(Timestamp, orders = c('dmy HM', 'dmy HMS')),
        Temp = as.numeric(Temperature),
        RH = as.numeric(Humidity),
        .keep = "none"
      )

  }
  if (all(is.na(dat$Date)) || all(is.na(dat$Temp))) {
    warning("May not be Meaco file")
  }
  dat
}

#' Parse miniClima file
#'
#' @description
#' Extracts temperature and humidity data from miniClima logfile.
#'
#' @param filepath path to logfile as a string
#' @param Site name of the site
#'
#' @returns dat, a data frame containing the raw TRH data, with columns
#' for Site, sensor, date, temperature, and relative humidity.
#'

#' @noRd
parse_miniClima <- function(filepath, Site = "Site") {
  message("Parsing as miniClima")

  # Logger information not stored in file, try filename
  if (stringr::str_detect(filepath, " EBC")) {
    Sensor <- stringr::str_extract(filepath, "[A-Za-z0-9 ]+(?= EBC)") |>
      stringr::str_replace_na("Unknown")
  }

  dat <- readr::read_csv2(
    filepath,
    col_names = c(
      "Date",
      "Temp",
      "RH",
      "setpoint",
      "alarm_min",
      "alarm_max",
      "timediff"
    ),
    skip = 1
  ) |>
    dplyr::mutate(
      Site = as.character(Site),
      Sensor = Sensor,
      Date = lubridate::parse_date_time(Date, orders = "dmy HMS"),
      Temp = Temp,
      RH = RH,
      .before = Date,
      .keep = "none"
    )

  if (all(is.na(dat$Date)) || all(is.na(dat$Temp))) {
    warning("May not be miniClima file")
  }
  dat
}

#' Parse Rotronic file
#'
#' @description
#' Extracts temperature and humidity data from Rotronic logfile.
#'
#' @inherit parse_miniClima params returns
#'
#' @noRd
parse_rotronic <- function(filepath, Site = "Site") {
  message("Parsing as Rotronic")
  # Extract first few rows containing logger information
  if (stringr::str_detect(filepath, ".xls$")) {
    file_head <- readr::read_delim(filepath,
                                   col_names = c("Date", "Time", "RH", "Temp"),
                                   delim = "\t")
    file_data <- readr::read_delim(
      filepath,
      col_names = c("Date", "Time", "RH", "Temp"),
      delim = "\t",
      skip = 23
    )
  }

  if (stringr::str_detect(filepath, ".csv$")) {
    file_head <- readr::read_csv(filepath, col_names = c("Date"), n_max = 5)
    file_data <- readr::read_csv(filepath,
                                 col_names = c("Date", "Time", "RH", "Temp"),
                                 skip = 23)
  }
  if (all(is.na(file_data$Date)) || all(is.na(file_data$Temp))) {
    warning("May not be Rotronic file")
  }
  # Rest of file is observations
  dat <-  file_data |>
    dplyr::mutate(
      Site = as.character(Site),
      Sensor = as.character(file_head$Date[2]),
      Date = lubridate::parse_date_time(paste(Date, Time), orders = "dmy HMS"),
      Temp = as.numeric(
        stringr::str_extract_all(Temp, "[:digit:]+\\.?[:digit:]+")
      ),
      RH = as.numeric(stringr::str_extract_all(RH, "[:digit:]+\\.?[:digit:]+")),
      .before = "Date",
      .keep = "none"
    )
  dat
}

#' Parse T&D file
#'
#' @description
#' Extracts temperature and humidity data from T&D logfile.
#'
#' @inherit parse_miniClima params returns

#' @noRd
parse_tandd <- function(filepath, Site = "Site") {
  message("Parsing as T&D")
  message("Keeping TRH data only")

  # Assumes name includes name and serial starting with F8 which may not be accurate
  if (stringr::str_detect(filepath, 'F8')) {
    Sensor <- stringr::str_extract(filepath, "([A-Za-z0-9 ])+(?= F8)")
  }
  else {
    Sensor <- "Sensor unknown"
    message('Sensor name not recoverable')
  }

  # Rest of file is observations
  dat <- readr::read_csv(
    filepath,
    col_names = c(
      "Date",
      "time",
      "lux",
      "UV",
      "Temp",
      "RH",
      "luxhours",
      "UVhours"
    ),
    col_types = "cccccccc",
    skip = 3
  ) |>
    dplyr::mutate(
      Site = as.character(Site),
      Sensor = as.character(Sensor),
      Date = lubridate::parse_date_time(Date, orders = "ymd HMS"),
      Temp = as.numeric(Temp),
      RH = as.numeric(RH),
      .before = Date,
      .keep = "none"
    )

  if (all(is.na(dat$Date)) || all(is.na(dat$Temp))) {
    warning("May not be T&D file")
  }
  dat
}

#' Parse TinyTag logfile
#'
#' @description
#' Extracts temperature and humidity data from TinyTag logfile.
#'
#' @inherit parse_miniClima params returns
#'
#' @noRd
parse_tinytag <- function(filepath, Site = "Site") {
  message("Parsing as TinyTag")
  file_head <- readr::read_csv(
    filepath,
    col_names = c("id", "Date", "Temp", "RH"),
    col_types = "cccc",
    n_max = 5
  )

  dat <- readr::read_csv(filepath,
                         col_names = c("id", "Date", "Temp", "RH"),
                         skip = 5) |>
    dplyr::mutate(
      Site = as.character(Site),
      Sensor = as.character(file_head$Temp[4]),
      Date = lubridate::parse_date_time(Date, orders = c("ymd HMS", "dmy HMS", "dmy HM")),
      Temp = as.numeric(
        stringr::str_extract_all(Temp, "[:digit:]+\\.?[:digit:]+")
      ),
      RH = as.numeric(stringr::str_extract_all(RH, "[:digit:]+\\.?[:digit:]+")),
      .before = Date,
      .keep = "none"
    )
  if (all(is.na(dat$Date)) || all(is.na(dat$Temp))) {
    warning("May not be T&D file")
  }
  dat
}



#' Parse Trend BMS file
#'
#' @description
#' Extracts temperature and humidity data from a Trend BMS logfile.
#'
#' @inherit parse_miniClima params return
#'
#' @noRd
parse_trendBMS <- function(filepath, Site = "Site") {
  message("Parsing as Trend BMS")
  file_head <- readr::read_csv(filepath,
                               col_names = c("Date", "obs"),
                               n_max = 1)
  # Extract first few rows containing logger information
  # Chcek whether the file is temperature or humidity and set column name
  temp_or_RH <- dplyr::if_else(stringr::str_detect(file_head$obs[1], "Temp"), "Temp", "RH")
  Sensor <- stringr::str_extract(file_head$obs[1], "(?<=\\[).*?(?= Space)")
  #Rest of file is observations
  dat <- readr::read_csv(filepath,
                         col_names = c("Date", temp_or_RH),
                         skip = 1) |>

    dplyr::mutate(
      Site = as.character(Site),
      Sensor = Sensor,
      Date = lubridate::parse_date_time(Date, orders = "dmy HMS"),
      .before = Date
    )
  if (all(is.na(dat$Date)) || all(is.na(dat$Temp))) {
    warning("May not be Trend BMS file")
  }
  dat
}

#' Combine TRH data from a list
#'
#' @description
#' Combines a list of parsed logfiles and returns a
#'
#'
#' @param datalist A list of parsed dataframes
#'
#' @returns A dataframe containing all timestamped rows in `datalist`
#' @export
#'
#' @examples
#' \donttest{
#' all_data <- combine_data(datalist)
#' }
#'
combine_data <- function(datalist) {
  message("Combining files")

  dat <- dplyr::bind_rows(datalist) |> dplyr::distinct()

  dat <-  dplyr::mutate(
    dat,
    Site = Site,
    Sensor = Sensor,
    Date = Date,
    Temp = Temp,
    RH = RH,
    .keep = "none"
  ) |>
    tidyr::drop_na(any_of("Date"))
  dat
}
