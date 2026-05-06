# Global variables ----
.DATASET_DIR <- here::here("data-raw/nurses-stress/")


#' Read in one nurses' stress data file.
#'
#' @param file_path Path to the data file.
#' @param n_rows Number of rows to read in.
#'
#' @returns Outputs a data.table.
#'
read <- function(file_path, n_rows = 100) {
  dt <- file_path |>
    data.table::fread(nrows = n_rows)
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
read_all <- function(filename, n_rows = 100) {
  files <- .DATASET_DIR |>
    fs::dir_ls(regexp = filename, recurse = TRUE)

  data <- files |>
    lapply(\(file) read(file, n_rows = n_rows)) |>
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
#'
#' @returns returns a new datset with mean, median and sd by id and collection_datetime
summarise_by_datetime <- function(data) {
  summarised_data <- data |>
    dplyr::mutate(
      collection_datetime = lubridate::round_date(
        collection_datetime,
        unit = "minute"
      )
    ) |>
    dplyr::summarise(
      dplyr::across(
        dplyr::where(is.numeric),
        list(mean = mean, sd = sd, median = median)
      ),
      .by = c(id, collection_datetime)
    )
  return(summarised_data)
}
