#' Get NACC Data
#'
#' Load NACC data from CSV files and convert the column names to be consistent
#' with other data sources
#'
#' 
#' @param csv_fname CSV filename for the NACC data
#'
#' @return A dataframe with NACC data
#'
#' @examples
#' \dontrun{
#' fname= "/Users/name/Documents/R/adbiomarker/Data/NACC"
#' dt_nacc <- get_nacc(path)}
#'
#' @export
#'
adt_get_nacc <- function(csv_fname) {
    dt_nacc <- read.csv(file = csv_fname, header = TRUE)
    return(dt_nacc)
}
