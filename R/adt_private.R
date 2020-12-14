#' Remove all special characters
#'
#'
a_gsub <- function(vec_name, lower_case = TRUE) {
    rst <- sapply(vec_name,
                  function(x) gsub("[^[:alnum:]]", "", x))

    if (lower_case)
        rst <- tolower(rst)

    rst
}

#' Get the column name in the source data file.
#'
#' Since the column names in tables may vary, it could be modified by using a
#' costomized column names dictionary. The format can refer to the appended
#' dictionary.
#'
#' @inheritParams parameters
#'
#' @return The real name in the source data file
#'
#'
a_map_var <- function(src_type = c("BIOCARD", "NACC", "ADNI"),
                      tbl_code, col_name,
                      dict_src_tables = NULL) {

    if (is.null(dict_src_tables))
        dict_src_tables <- apt_get_dict(dict = "src_tables")

    res <- filter(dict_col_name,
                  src_type       == src_type &
                  adt_table_code == table_code &
                  adt_col_name   == col_name)


    rst <- a_gsub(res[["src_col_name"]])
    rst
}


#' Read data
#'
#' Read specific subtable. The default start column is 1. But could be
#' costomized by editing the "dict_tbl" dictionary.
#'
#' @param code Code of subtables ("Cognitive" as "COG"). The options are "COG"
#'     (Cognitive), "DIAG" (Diagnosis), "CSF" (FINAL_CSF), "DEMO"
#'     (Demographics), "HIPPO" (Hippocampus), "AMY" (Amygdala), "EC"
#'     (Entorhinal), "GE" (Genetics), "LIST_A" (list of patients not enrolled),
#'     "LIST_B" (list of patients impaired).
#' @param file_names List the names of all subtables in the path (all tables
#'     should from the same path).
#'
#' @param dict_tbl A path of data file (.csv) of the parameters dictionary. The
#'     default dictionary is appended in the package. Since the parameters in
#'     tables may vary, it could be modified by using a costomized parameter
#'     dictionary. The format can refer to the appended dictionary.
#'
#' @return A dataset
#'
#' @example
#' \dontrun{
#' a_read_file("COG", file_names, dict_tbl)
#' }
#'
a_read_file <- function(code, file_names, dict_src_tables) {

    key_start  <- dict_src_tables %>%
        filter(adt_table_code == code) %>%
        select(src_key_words, src_start_row)

    if (1 != nrow(key_start))
        stop("Number of matching record is not 1.")

    finx      <- grep(key_start[1, "src_key_words"],
                      file_names)

    if (1 != length(finx))
        stop(paste("There is not an unique file name that
                    matches with the keyword",
                   key_start[1, "src_key_words"]))

    real_name <- file_names[finx]

    print(sprintf("Loading source file %s ...", basename(real_name)))
    dat <- read.xls(xls = real_name,
                    skip = key_start[1, "start_row"] - 1)

    colnames(dat) <- a_gsub(colnames(dat))


    return(dat)
}


#' Define the Time Window
#'
#' For most data, the time windows are calculated from the baseline time (left
#' window = the midpoint between current and the previous time point; right
#' window = the midpoint between current and the next time point). If the time
#' windows are longer than the maximum acceptable length, then force to select
#' biomarker within the maximum window length we set. For the first and last
#' baseline time, since there is no "previous" or "next" time point availiable,
#' use the maximum acceptable window length instead.
#'
#' @param dat The baseline time dataset.
#' @param v_date A string indicating the date name of the baseline time.
#' @param v_id A string indicating the id name of baseline dateset. Default is
#'     "subject_id" (may varied if using customized column names dictionary).
#'
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

#' Update Dictionary
#'
#'
a_update_dict <- function(dat_dict, dat_csv, key_cols) {

    if (is.null(dat_csv) |
        is.null(key_cols))
        return(dat_dict)

    colnames(dat_csv) <- tolower(dat_csv)
    if (!identical(sort(colnames(dat_dict)),
                   sort(colnames(dat_csv)))) {
        stop("The column names in the CSV file are not compatible with
              the dictionary to be updated")
    }


    ##   XINYU PLEASE ADD:
    ##   loop through dat_csv
    ##   for each row in dat_csv match its key_cols with dat_dict
    ##   once matched, update all the non-key-columns
    ##   similar like what you did here

    ## if (!is.null(dict_col_update)) {
    ##     dict_update <- read_xlsx(dict_col_update)
    ##     dict_src_tables <- adt_get_dict("col_name") %>%
    ##         left_join(dict_update, by = c("old_col_name", "table_code")) %>%
    ##         mutate(old_col_name = ifelse(is.na(new_col_name), old_col_name, new_col_name)) %>%
    ##         select(col_name, data_source, table_code, old_col_name)
    ## }
    ## else

}


## check data
a_check_src <- function(dict_data, dict_src_tables, dict_src_files) {
    isin <- function(var_name, data){
        res <- gsub(" ", "\\.", var_name)
        res <- gsub('\\s*[].()@!#$%^&"]\\s*', "\\.", res)
        return(res %in% names(eval(as.name(data))))
    }
    dict_src_files_sub <- dict_src_files %>%
        select(table_code, inter_name, key_words)
    default_names <- dict_src_tables %>%
        filter(data_source == "BIOCARD") %>%
        select(col_name, table_code, old_col_name) %>%
        left_join(dict_src_files_sub, by = "table_code") %>%
        left_join(dict_data, by = "col_name") %>%
        rowwise() %>%
        mutate(table_name = table_list[grep(key_words, table_list)]) %>%
        mutate(nedt = isin(old_col_name, inter_name))
    edit_names <- default_names %>%
        filter(nedt == "FALSE") %>%
        unite(description, info, values, range, sep = "; ") %>%
        select(old_col_name, description, table_code, table_name) %>%
        mutate(new_col_name = NA) %>%
        arrange(table_name)
    res <- list(default_names, edit_names)
    return(res)
}


## Error messages
a_err_msg <- function(msg) {
    biocard_load_error <- 'Please fill the new_col_name in file
                           "col_name_report.csv". \n
                           The "old_col_name", "description", and "table_name"
                           can be used as references (These
                           values cannot be changed). \n
                           After updating, rerun the function with: \n
                           adt_get_biocard(..., dict_col_update
                            = "col_name_report.xlsx")'

    get(msg)
}
