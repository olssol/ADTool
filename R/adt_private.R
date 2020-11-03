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

#' Define time window
#'
#'
#'
#' @export
#'
a_window <- function(dat, v_date, window, window_overlap, v_id = "subject_id") {
    g_win <- function(d1, d2, left) {
        if (is.na(d2)){
            d2 <- window
        }
        if (window_overlap) {
            d <- window / 2
        } else {
            d <- min(as.numeric(d2), window / 2, na.rm = T) # use window/2 or just window?
        }

        d1 + left * d
    }

    dat %>%
        mutate(subject_id = !!as.name(v_id),
               date       = !!as.name(v_date)) %>%
        select(subject_id, date) %>%
        group_by(subject_id) %>%
        arrange(date, .by_group = T) %>%
        mutate(date_left  = (date - lag(date, order_by = subject_id)) / 2,
               date_right = lead(date_left, order_by = subject_id)) %>%
        rowwise() %>%
        mutate(date_left  = g_win(date, date_left,  -1),
               date_right = g_win(date, date_right, 1))

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
a_match <- function(dat_se, dat_marker, m_date, duplist) {

    exc_cols   <- names(dat_marker)[which(names(dat_marker) %in% duplist)]
#    dat_marker <- dat_marker %>%
#        mutate(m_date = !!as.name(m_date))

    dat_match <- dat_se %>%
        select(subject_id, date, date_left, date_right) %>%
        left_join(dat_marker %>%
                  select(subject_id, m_date),
                  by = "subject_id") %>%
        filter(!!as.name(m_date) >= date_left &
                   !!as.name(m_date) < date_right) %>%
        mutate(diff = abs(date - !!as.name(m_date))) %>%
        group_by(subject_id, date) %>%
        arrange(diff, .by_group = T) %>%
        filter(row_number() == 1) %>%
        select(subject_id, date, m_date)

    dat_se %>%
        left_join(dat_match, by = c("subject_id", "date")) %>%
        left_join(dat_marker, by = c("subject_id", m_date)) %>%
        select(- exc_cols)
}
