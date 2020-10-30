#' Read data from external files
#'
#' @param code       Code of subtable ("Cognitive" as "COG")
#' @param file_names List of all files name in path
#' @param dict_par   Parameters dictionary (could be modified if needed)
#'
#' @return
#'
a_read_file <- function(code, file_names, dict_par) {
    key_start  <- dict_par %>%
        filter(file_code == code) %>%
        select(file_name, start_row)

    if (1 != nrow(key_start))
        stop("Number of matching record is not 1.")

    real_name <- file_names[grepl(key_start[1, "file_name"],
                                  file_names)]
    dat       <- read.xls(xls = real_name,
                          skip = key_start[1, "start_row"] - 1)

    return(dat)
}

#' Map Variables
#'
#' @param data_name0  Dataset where the variable from
#' @param table_code0 Table where the variable from
#' @param var_name    Name of the variable
#' @param var_file    Variables mapping file
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
a_map_var <- function(data_name0 = "BIOCARD", table_code0, var_name, var_file) {
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
#' @param xid       Id in diagnosis visit table
#' @param xdate     Date of visit
#' @param dat       Table includes biomarkers
#' @param yidname   Id in the table includes biomarkers
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
a_match <- function(dat_se, date_se, dat_marker, m_date, window, duplist) {
    dat_se %>%
        select(subject_id, date_se) %>%
        group_by(subject_id) %>%
        mutate(date_upr = (eval(parse(text = date_se)) - lag(eval(parse(text = date_se)))) / 2) %>%
        mutate(date_lwr = (eval(parse(text = date_se)) - lead(eval(parse(text = date_se)))) / 2) %>%
        mutate(date_upr = replace(date_upr, is.na(date_upr), window)) %>%
        mutate(date_lwr = replace(date_lwr, is.na(date_lwr), -window)) %>%
        ungroup() %>%
        left_join(dat_marker, by = "subject_id", suffix = c("_marker", "")) %>%
        mutate(date_diff = eval(parse(text = date_se)) - eval(parse(text = m_date))) %>%
        select(-names(dat_marker)[which(names(dat_marker) %in% duplist)]) %>%
        group_by(subject_id) %>%
        filter(date_diff < date_upr & date_diff > date_lwr) %>%
        ungroup() %>%
        select(-c("date_diff", "date_upr", "date_lwr"))
}
