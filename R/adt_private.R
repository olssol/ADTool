#' Read data 
#'
#' Read specific subtable. The default start column is 1. But could be costomized by editing the "dict_tbl" dictionary.
#'
#' @param code       Code of subtables ("Cognitive" as "COG"). The options are "COG" (Cognitive), "DIAG" (Diagnosis), "CSF" (FINAL_CSF), 
#' "DEMO" (Demographics), "HIPPO" (Hippocampus), "AMY" (Amygdala), "EC" (Entorhinal), "GE" (Genetics), "LIST_A" (list of patients not enrolled), 
#' "LIST_B" (list of patients impaired). 
#' @param file_names List the names of all subtables in the path (all tables should from the same path).
#' @param dict_tbl   A path of data file (.csv) of the parameters dictionary. The default dictionary is appended in the package. 
#' Since the parameters in tables may vary, it could be modified by using a costomized parameter dictionary. The format can refer to the 
#' appended dictionary.
#' 
#' @return A dataset
#' 
#' @example 
#' \dontrun{
#' a_read_file("COG", file_names, dict_tbl)
#' }
#'
a_read_file <- function(code, file_names, dict_tbl) {
    key_start  <- dict_tbl %>%
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
#' Get the original column names. Since the column names in tables may vary, it could be modified by using a costomized column names dictionary. The format can refer to the 
#' appended dictionary.
#'
#' @param table_code0 A string indicating the code of table where the variable (column name) from.
#' @param var_name    A string indicating the name of the variable (column name).
#' @param dict_col_name   A path of data file (.csv) of the variables dictionary. The default dictionary is appended in the package. 
#' Since the column names in tables may vary, it could be modified by using a costomized column names dictionary. The format can refer to the 
#' appended dictionary.
#' @param data_name A string indicating the source of data. Default "BIOCARD."
#'
#' @return The real name in the original data
#'
#' @examples
#' \dontrun{
#' a_map_var("BIOCARD", "CSF", "date", dict_col_name)
#' }
#'
#' @export
#'
a_map_var <- function(data_name = "BIOCARD", table_code0, var_name, dict_col_name) {
    dict_col_name <- data.frame(dict_col_name)
    res <- filter(dict_col_name, data_source == data_name &
                            table_code == table_code0 &
                            col_name == var_name)[["old_col_name"]]
    res <- gsub(" ", "\\.", res)
    res <- gsub("\\(", "\\.", res)
    res <- gsub("\\)", "\\.", res)
    return(res)
}

#' Define the Time Window
#'
#' For most data, the time windows are calculated from the baseline time 
#' (left window = the midpoint between current and the previous time point; right window = the midpoint between current and the next time point).
#' If the time windows are longer than the maximum acceptable length, then force to select biomarker within the maximum window length we set. 
#' For the first and last baseline time, since there is no "previous" or "next" time point availiable, use the maximum acceptable window length instead.
#'
#' @param dat     The baseline time dataset.
#' @param v_date  A string indicating the date name of the baseline time.
#' @param window  An integer (unit of days) indicating the maximum acceptable length of time interval between baseline time and biomarker test time.
#' @param window_overlap  A logical value indicating the time window setting. Default "False." If true, all time windows will set from 1/2 "window" days before  
#' the baseline time to the 1/2 "window" days after baseline. In this case, the time windows may overlap, which means some biomarkers may be 
#' merged into multiple baseline data. If false, the windows will be calculated from the blaseline times. The left window is set to be the midpoint 
#' between the current and the previous time point. The right window is set to be the midpoint between the current and the next time point. 
#' For the first and last baseline time, since there is no "previous" or "next" time point available, 
#' use the maximum acceptable window length (set by the parameter "window") instead. 
#' @param v_id    A string indicating the id name of baseline dateset. Default is "subject_id" (may varied if using customized column names dictionary).
#'
#' @export
#'
a_window <- function(dat, v_date, window, window_overlap, v_id = "subject_id") {
    g_win <- function(d1, d2, left) {
        if (window_overlap) {
            d <- window / 2
        } else {
            d <- min(as.numeric(d2), window / 2, na.rm = T) 
            d <- min(as.numeric(d2),
                     window / 2,
                     na.rm = T)
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

#' Map Biomarkers Data to The Baseline Visits
#'
#' Map all marker data to baseline visits. For each visit, assign the
#' closest markers within the setted window. If no marker
#' satisfies assignment criteria, mark as missing. If two marker satisfy
#' assignment criteria, take first one.
#'
#' @param dat_se     A dataset that will be merged.
#' @param dat_marker A data set including new biomarker that will be merged.
#' @param m_date     A string indicating the name of measure date in dat_marker.
#' @param duplist    A list of duplicated columns names.
#'
#' @return The matched (larger) dateset including newly added biomarker data.
#'
#'
#' @examples
#' \dontrun{
#' dat_se <- a_match(dat_se, dat_dx, "date_dx", duplist)
#' }
#'
a_match <- function(dat_se, dat_marker, m_date, duplist) {
    exc_cols  <- names(dat_marker)[which(names(dat_marker) %in% duplist)]
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
        left_join(dat_match,  by = c("subject_id", "date")) %>%
        left_join(dat_marker, by = c("subject_id", m_date)) %>%
        select(-exc_cols)
}
