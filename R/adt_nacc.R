# function to get information from nacc

#' Get NACC Data
#'
#' @param path The path where the nacc data stored
#' @return a table with nacc data
#' @export
#'
#' @examples
#' \dontrun{
#' path = "/Users/name/Documents/R/adbiomarker/Data/NACC"
#' dt_nacc <- get_nacc(path)
#' }
get_nacc <- function(path){
  dt_nacc <- read.csv(file = path, header = TRUE)
  return(dt_nacc)
}
