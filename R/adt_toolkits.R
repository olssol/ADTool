
#' Get Subtable
#'
#' @param table_code Code of subtable ("Cognitive" as "COG")
#' @param file_names List of all files name in path
#' @param path Location where data stored
#' @param par_file Parameters dictionary (could be modified if needed)
#'
#' @return
#'
#' @examples
#' \dontrun{
#' dat_cog <- get_table("COG", file_names, par_file, path)
#' }
#'
#' @export

get_table <- function(table_code, file_names, par_file, path) {
    key_name <- filter(par_file, file_code == table_code)[["file_name"]]
    real_name <- file_names[grep(key_name, file_names)]
    location <- paste(path, real_name, sep = "")
    start_row <- filter(par_file, file_code == table_code)[["start_row"]]
    dat <- read.xls(xls = location, skip = start_row - 1)
    return(dat)
}

#' Map Variables
#'
#' @param data_name0 Dataset where the variable from
#' @param table_code0 Table where the variable from
#' @param var_name Name of the variable
#' @param var_file Variables mapping file
#'
#' @return The real name in raw data
#'
#' @examples
#' \dontrun{
#' map_var("BIOCARD", "CSF", "date", var_file)
#' }
#'
#'
#' @export
#' 
map_var <- function(data_name0 = "BIOCARD", table_code0, var_name, var_file) {
    var_file <- data.frame(var_file)
    res <- filter(var_file, data_name == data_name0 &
                            table_code == table_code0 &
                            variables == var_name)[["label"]]
    res <- gsub(" ", "\\.", res)
    res <- gsub("\\(", "\\.", res)
    res <- gsub("\\)", "\\.", res)
    return(res)
}


#' Map Biomarkers Data to Diagnosis Visits
#'
#' Map all marker data to diagnosis visits for each diagnosis visit, assign the
#' closest markers within a 730-day-each-side two-sided window if no marker
#' satisfies assignment criteria, mark as missing if two marker satisfy
#' assignment criteria, take first
#' 
#' @param xid Id in diagnosis visit table
#' @param xdate Date of visit
#' @param dat Table includes biomarkers
#' @param yidname Id in the table includes biomarkers
#' @param ydatename Name of date in the table includes biomarkers
#'
#' @return The matched biomarker data to diagnosis visits
#'
#'
#' @examples
#' \dontrun{
#' tcog <- mymatch(xid = x[map_var("BIOCARD", "DIAG", "subject_id", var_file)],
#' xdate = x$date,
#' dat = dat_cog, yidname = COG$id, ydatename = "date")
#' }
#'
#' @export
#' 
mymatch <- function(xid, xdate, dat, yidname, ydatename) {
    idtemp <- dat[dat[[yidname]] == xid, ]
    datediff <- as.numeric(idtemp[[ydatename]] - xdate)
    if (length(datediff[!is.na(datediff)]) != 0) {
        if (any(abs(datediff) <= 730)) {
            temp <- idtemp[which(abs(datediff) == min(abs(datediff))),
                         , drop = F][1, ]
        } else {
            temp <- dat[1, ]
            temp[yidname] <- xid
            temp[setdiff(names(temp), yidname)] <- NA
        }
    } else {
        temp <- dat[1, ]
        temp[yidname] <- xid
        temp[setdiff(names(temp), yidname)] <- NA
    }
    return(temp)
}

