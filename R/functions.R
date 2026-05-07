# Global variables ----
.DATASET_DIR <- here::here("data-raw/nurses-stress/")


#' Read in one nurses' stress data file.
#'
#' @param file_path Path to the data file.
#' @param max_rows Number of rows to read in.
#'
#' @returns Outputs a data.table.
#'
read <- function(file_path, max_rows = Inf) {
  dt <- file_path |>
    data.table::fread(nrows = max_rows)
  data.table::setnames(dt, snakecase::to_snake_case)
  return(dt)
}


#' Function reading all files within the nurses-stress stress subfolder
#'
#' @param filename Specifies the file type you want to load
#'
#' @returns a data.table
#' @export
#'
#' @examples
read_all <- function(filename, max_rows = Inf) {
  files <- .DATASET_DIR |>
    fs::dir_ls(regexp = filename, recurse = TRUE)

  data <- files |>
    lapply(\(file) read(file, max_rows = max_rows)) |>
    data.table::rbindlist(idcol = "source")

  return(data)
}


#' Extracts participant id from the file path
#'
#' @param data used data frame
#'
#' @returns returns the data frame with the file path column converted to an id coloumn
#' @export
#'
#' @examples
get_participant_id <- function(data) {
  data_with_id <- data |>
    dplyr::mutate(
      id = stringr::str_extract(
        source,
        "(?<=/stress/)[:alnum:]{2}(?=/)"
      ),
      .before = source
    ) |>
    dplyr::select(-source)
  return(data_with_id)
}


#' Summarise datetime to minutes
#'
#' @param data The dataset used
#' @param unit the date time rounding unit
#' @param fns the functions to summarise with in dplyr summarise
#'
#' @returns returns a new datset with mean, median and sd by id and collection_datetime
summarise_by_datetime <- function(data, fns, unit = "minutes") {
  summarised_data <- data |>
    dplyr::mutate(
      collection_datetime = lubridate::round_date(
        collection_datetime,
        unit = unit
      )
    ) |>
    dplyr::summarise(
      dplyr::across(
        dplyr::where(is.numeric),
        fns
      ),
      .by = c(id, collection_datetime)
    )
  return(summarised_data)
}

#' Combinning the reading, cleaning of id and summarising in one go
#'
#' @param filename the file extensions to read
#' @param max_rows number of rows to read
#' @param unit the date time rounding unit
#' @param fns the functions to summarise with in dplyr summarise
#'
#' @returns the read data frame
#'
read_sensor_data <- function(filename, max_rows = Inf, fns, unit = "minute") {
  data <- read_all(filename, max_rows = max_rows) |>
    get_participant_id() |>
    summarise_by_datetime(fns = fns, unit = unit)
  return(data)
}


#' Tidying survey data to make it ready for combining with the other data sets
#'
#' @param data survey data
#'
#' @returns tidy data
#'
tidy_survey_dates <- function(data) {
  tidied <- data |>
    dplyr::mutate(
      date = lubridate::mdy(date),
      start_datetime = lubridate::as_datetime(paste(date, start_time)),
      end_datetime = lubridate::as_datetime(paste(date, end_time)),
      datetime_id = start_datetime,
      .before = start_time
    ) |>
    dplyr::select(-c(date, start_time, end_time, duration))
  return(tidied)
}


#' Takes the survey data and pivots longer
#'
#' @param data The survey data
#'
#' @returns The survey data in long form
#'
survey_to_long <- function(data) {
  longer <- data |>
    dplyr::select(id, datetime_id, start_datetime, end_datetime) |>
    tidyr::pivot_longer(c(start_datetime, end_datetime), names_to = NULL, values_to = "collection_datetime") |>
    dplyr::group_by(dplyr::pick(-collection_datetime)) |>
    tidyr::complete(collection_datetime = seq(min(collection_datetime),
                                              max(collection_datetime),
                                              by = 60
    )) |>
    dplyr::ungroup()
  return(longer)
}
