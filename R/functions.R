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
