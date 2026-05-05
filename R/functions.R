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
