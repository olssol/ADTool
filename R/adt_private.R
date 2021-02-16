#' Remove all special characters
#'
#' Remove all special charachers in a string except "_",
#' and change all characters to the lower cases is set lower_case = TRUE.
#'
#' @inheritParams parameters
#'
#' @return
#'
#' Returned a string with no special characters except "_"
#'
#' @examples
#' \dontrun{
#' vec <- "ad*&_ae20"
#' a_gsub(vec, lower_case = TRUE)
#' }
#'
#'
a_gsub <- function(vec_name, lower_case = TRUE) {
    rst <- sapply(vec_name,
                  function(x) gsub("[^[:alnum:]\\_]", "", x), USE.NAMES = F)

    if (lower_case)
        rst <- tolower(rst)

    return(rst)
}

#' Print out message
#'
#'
#'
a_print <- function(msg, verbose = TRUE, ...) {
    if (verbose)
        print(msg, ...)
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
        dict_src_tables <- adt_get_dict(dict = "src_tables")

    res <- filter(dict_src_tables,
                  src_type       == src_type &
                  adt_table_code == tbl_code &
                  adt_col_name   == col_name)


    rst <- a_gsub(res[["src_col_name"]])
    return(rst)
}


#' Read data
#'
#' Read specific subtable. The default start column is 1. But could be
#' costomized by editing the "dict_tbl" dictionary.
#'
#' @inheritParams parameters
#'
#' @return A dataset
#'
#' @examples
#' \dontrun{
#' a_read_file("COG", file_names, dict_src_files)
#' }
#'
a_read_file <- function(table_code, file_names, dict_src_files,
                        verbose = TRUE) {

    key_start  <- dict_src_files %>%
        filter(adt_table_code == table_code) %>%
        select(src_key_words, src_start_row)

    if (1 != nrow(key_start))
        stop("Number of matching record is not 1.")

    finx <- grep(key_start[1, "src_key_words"],
                 file_names)

    if (1 != length(finx))
        stop(paste("There is not an unique file name that
                    matches with the keyword",
                   key_start[1, "src_key_words"]))

    real_name <- file_names[finx]

    a_print(sprintf("Loading source file %s ...", basename(real_name)),
            verbose)

    dat <- read.xls(xls = real_name,
                    skip = key_start[1, "src_start_row"] - 1)

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
#' @inheritParams parameters
#'
#' @return A dataset with time window for biomarkers
#'
#' @examples
#' \dontrun{
#' a_window(dat, v_date, window, window_overlap, v_id = "subject_id")
#' }
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
#' @inheritParams parameters
#'
#' @return The matched (larger) dataset including newly added biomarker data.
#'
#'
#' @examples
#' \dontrun{
#' dat_se <- a_match(dat_se, dat_dx, "date_dx")
#' }
#'
a_match <- function(dat_se, dat_marker, m_date) {
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
        left_join(dat_marker, by = c("subject_id", m_date)) 
}

#' Update Dictionary
#'
#' Update the old dictionary with a new one.
#' Input both the old and new dictionary,
#' merged them by the index, update all columns with "src"
#'
#' @inheritParams parameters
#'
#' @return The updated dataset.
a_update_dict <- function(rst_dict, csv_dict) {

    if (is.null(csv_dict)) {
        return(rst_dict)
    }

    if (!identical(sort(colnames(rst_dict)),
                   sort(colnames(csv_dict)))) {
        stop("The column names in the CSV file are not compatible
              with the dictionary to be updated.")
    }

    if (! all(csv_dict$index %in% rst_dict$index)) {
        stop("Invalid index in the CSV file.")
    }

    src_csv_dict <- csv_dict %>%
        select(index, starts_with("src"))

    ## only keep the columns with names starting with src in the csv file
    m_dict <- rst_dict %>%
        left_join(src_csv_dict, by = "index", suffix = c(".rp", "")) %>%
        select(!ends_with(".rp"))

    return(m_dict)
}


## Check data
#'
#' Check the source data whether include all the variable needed
#'
#' @inheritParams parameters
#'
#' @return A dataset includes missing variables information.
#' @export
#'
#' @examples
#' \dontrun{
#' a_check_src("DIAG", dict_src_tables, dat_diag)
#' }
#'
a_check_src <- function(table_code, cur_dat, dict_src_tables) {

    f_isin <- function(var_name) {
        res <- a_gsub(var_name)
        return(res %in% cur_names)
    }

    cur_names <- colnames(cur_dat)

    default_names <- dict_src_tables %>%
        filter(adt_table_code == table_code) %>%
        select(adt_col_name, src_type, adt_table_code, src_col_name) %>%
        mutate(nedt = f_isin(src_col_name)) %>%
        filter(nedt == FALSE)
    return(default_names)
}

## Error messages
a_err_msg <- function(msg_id) {

    biocard_load_error <- 'Some variable(s) not found.\n
                           Missing of ID and date variables may cause problem when merging data.\n
                           Missing of other variables may cause problem when querying. \n
                           To edit the variable(s), please edit the "src_col_name" in "dict_src_tables" file. \n
                           To get the file, use adt_get_dict("src_tables"), \n
                           please save it as the Excel form. \n
                           After updating, rerun the function with: \n
                           adt_get_biocard(..., src_tables = "The updated Excel file")'

    get(msg_id)
}

#' Map categorical variables
#'
#' @return A dictionary with categorical code
#' @export
#'
a_dict_map <- function() {
    ppl <- "x$var = recode(x$var, map)"
    dict_data <- adt_get_dict("ana_data")
    dict_cmd <- dict_data %>%
        filter(grepl("','", values)) %>%
        rowwise() %>%
        mutate(cmd = gsub("var", adt_col_name, ppl)) %>%
        mutate(cmd = gsub("map", values, cmd)) %>% 
        select(adt_col_name, description, values, cmd)
    return(dict_cmd)
}

#' Generate Information Table
#'
#' @inheritParams parameters
#'
#' @return A matrix with number of participants in different categories.
#' @export
#'
#' @examples
#' \dontrun{
#' a_gen_tbl('sex', 'age')
#' }
a_gen_tbl <- function(pat, group, distn) {
    info <- as.data.frame(pat %>% 
                              count(!!as.name(distn), !!as.name(group))) %>% 
        na.omit()
    tbl <- reshape(info, idvar = distn, timevar = group, direction = 'wide', sep = '_') %>% 
        replace(., is.na(.), 0) %>% 
        mutate(All = rowSums(across(where(is.numeric))))
    return(tbl)
}
