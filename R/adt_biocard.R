#' Import BIOCARD Data
#'
#' Import BIOCARD data from source files and generate the analysis dataset.
#'
#' @param path Directory of the BIOCARD data files. Default value is the working
#'     directory,
#'
#' @param merge_by A character string indicating the source baseline time for
#'     aligning the BIOCARD data when merging multiple files. Options include
#'     "diagnosis", "cognitive", "csf", "hippocampus", "amydata" and
#'     "entorhinal".
#'
#' @param window An integer (unit of days) indicating the maximum acceptable gap
#'     time for merging a biomarker test to base data. Default is 730 days. For
#'     most data, the time windows are calculated from the baseline time (left
#'     window = the midpoint between current and the previous time point; right
#'     window = the midpoint between current and the next time point). If the
#'     time windows are longer than the maximum acceptable length, then force to
#'     select biomarker within the maximum window length we set. For the first
#'     and last baseline time, since there is no "previous" or "next" time point
#'     availiable, use the maximum acceptable window length instead.
#'
#' @param window_overlap A logical value indicating the time window setting.
#'     Default "False." If true, all time windows will set from 1/2 "window"
#'     days before the baseline time to the 1/2 "window" days after baseline. In
#'     this case, the time windows may overlap, which means some biomarkers may
#'     be merged into multiple baseline data. If false, the windows will be
#'     calculated from the blaseline times. The left window is set to be the
#'     midpoint between the current and the previous time point. The right
#'     window is set to be the midpoint between the current and the next time
#'     point. For the first and last baseline time, since there is no "previous"
#'     or "next" time point available, use the maximum acceptable window length
#'     (set by the parameter "window") instead.
#'
#' @param pattern A string indicating the pattern of all the data files. Default
#'     is "*.xls" (should work for both .xls and .xlsx). This pattern is used to
#'     read all table names from the path.
#'
#' @param src_files Dictionary file for source file features. See
#'     \link{\code{dict_src_files}} for more details.
#'
#' @param src_tables Dictionary file for source table features. See
#'     \link{\code{dict_src_tables}} for more details.
#'
#' @return
#'
#' The returned value is a list with the following items:
#'
#' \itemize{
#'
#'     \item data: The data includes patients' ids, baseline times,
#'     corresponding biomarkers, and biomarker test times.
#'
#'     \item exid: The subject_ids of patients that are either not enrolled or
#'     impaired at baseline.
#' }
#'
#' @examples
#' \dontrun{
#' # with default unoverlaped window
#' dt_biocard <- get_biocard(path, merge_by = "dx")
#' # with costomized window
#' dt_biocard <- get_biocard(path, merge_by = "dx", window = 365,
#'                           window_overlap = TRUE)
#' # with costomized dictionary
#' dt_biocard <- get_biocard(path, merge_by = "dx",
#'                           dict_tbl = "\BIOCARD\DATA\dict_tbl.csv")
#' }
#'
#' @export
#'
#'

adt_get_biocard <- function(path = ".",
                            merge_by = c("diagnosis", "cognitive", "csf",
                                         "hippocampus", "amydata",
                                         "entorhinal"),
                            window  = 730, window_overlap = FALSE,
                            pattern = "*.xls",
                            src_files  = NULL,
                            src_tables = NULL) {


    ## convert date
    f_date <- function(code, date_name, dta) {
        mvar <- a_map_var("BIOCARD", code, date_name, dict_src_tables)
        dfmt <- filter(dict_src_files,
                       adt_table_code == code)[["src_date_format"]]

        dta[date_name] <- as.Date(dta[, mvar], dfmt)
        dta            <- dta %>% select(-mvar)
        dta
    }

    ## map var
    f_map <- function(code, var, dta, fc = NULL) {
        mvar     <- a_map_var("BIOCARD", code, var, dict_src_tables)
        dta[var] <- dta[mvar]
        if (!mvar==var) {
            dta  <- dta %>% select(-mvar)
        }
        
        if (!is.null(fc))
            dta[var] <- lapply(dta[var], fc)

        dta
    }


    ## --------- prepare pars -------------------------------------

    ## source data set of dates for combining data
    merge_by <- match.arg(merge_by)
    
    ## switch ##
    merge_by <- tolower(switch(merge_by,
                       diagnosis = "DIAG", 
                       cognitive = "COG", 
                       csf = "CSF", 
                       hippocampus = "HIPPO", 
                       amydata = "AMY", 
                       entorhinal = "EC"))

    dup_list <- c("JHUANONID", "LETTERCODE",
                  "NIHID", "VISITNO", "Diagnosis.at.last.scan",
                  "Scan", "De.Identified.Subject.ID",
                  "Subject.NIHID", "Age.at.Scan",
                  "Diagnosis..at.last.scan", "Consensus.Diagnosis",
                  "intracranial_vol", "X")

    ## list all file names matching the pattern
    file_names <- list.files(path = path, pattern = pattern, full.names = TRUE)

    ## dictionary of src files
    dict_src_files  <- adt_get_dict("src_files", csv_fname  = src_files)
    dict_src_tables <- adt_get_dict("src_tables", csv_fname = src_tables)
    dict_data       <- adt_get_dict("ana_data")


    ## --------- read tables -------------------------------------
    vec_tbls <- c("COG", "DIAG", "CSF", "DEMO",
                  "HIPPO", "AMY", "EC", "GE")

    chk_all <- NULL
    #def_all <- NULL
    # table_list <- list.files(path = path, pattern = pattern, full.names = FALSE)
    for (i in vec_tbls) {
        cur_dat  <- a_read_file(i, file_names, dict_src_files)
        cur_chk  <- a_check_src(i, dict_src_tables, cur_dat)
        
        #cur_def <- cur_chk[[1]]
        #cur_err <- cur_chk[[2]]

        assign(paste("dat_", tolower(i), sep =""), cur_dat)
        chk_all <- rbind(chk_all, cur_chk)
        #chk_all <- rbind(chk_all, cur_err)
        #def_all <- rbind(def_all, cur_def)
    }

    if (dim(chk_all)[1]>0) {
        print("err_list:")
        err_list <- chk_all
        print(chk_all)
        err_msg <- a_err_msg("biocard_load_error")
        stop(err_msg)
    }

    dat_lsta  <- a_read_file("LIST_A", file_names, dict_src_files)
    dat_lstb  <- a_read_file("LIST_B", file_names, dict_src_files)


    ## ----------  manipulation ----------------------------------
    print("Formatting data ...")

    dat_cog   <- f_date("COG",  "date_cog",   dat_cog)
    dat_cog   <- f_map("COG",   "subject_id", dat_cog)

    dat_diag  <- f_date("DIAG", "date_diag",    dat_diag)
    dat_diag  <- f_map("DIAG",  "subject_id", dat_diag)
    
    dat_csf   <- f_date("CSF",  "date_csf",   dat_csf)
    dat_csf   <- f_map("CSF",   "abeta",      dat_csf)
    dat_csf   <- f_map("CSF",   "subject_id", dat_csf)

    dat_demo  <- f_map("DEMO", "subject_id", dat_demo)
    dat_demo  <- dat_demo[, -which(names(dat_demo)
                                  %in% tolower(c("JHUANONID",
                                         "LETTERCODE",
                                         "NIHID")))]

    dat_hippo <- f_date("HIPPO", "date_hippo", dat_hippo)
    dat_hippo <- f_map("HIPPO",  "subject_id", dat_hippo)

    dat_hippo <- f_map("HIPPO",  "intracranial_vol_hippo",
                       dat_hippo, as.numeric)

    dat_hippo <- f_map("HIPPO",  "l_hippo", dat_hippo, as.numeric)
    dat_hippo <- f_map("HIPPO",  "r_hippo", dat_hippo, as.numeric)
    dat_hippo$bi_hippo <- (dat_hippo$l_hippo + dat_hippo$r_hippo) / 2

    ## MRI amygdala
    dat_amy <- f_date("AMY", "date_amy",        dat_amy)
    dat_amy <- f_map("AMY", "subject_id",       dat_amy)

    dat_amy <- f_map("AMY", "intracranial_vol_amy",
                     dat_amy, as.numeric)

    dat_amy <- f_map("AMY", "l_amy", dat_amy, as.numeric)
    dat_amy <- f_map("AMY", "r_amy", dat_amy, as.numeric)
    dat_amy$bi_amy <- (dat_amy$l_amy + dat_amy$r_amy) / 2

    ## MRI EC volume
    dat_ec  <- f_date("EC", "date_ec",         dat_ec)
    dat_ec  <- f_map("EC", "subject_id",       dat_ec)
    dat_ec  <- f_map("EC", "intracranial_vol_ec", dat_ec, as.numeric)
    dat_ec  <- f_map("EC", "l_ec_vol",         dat_ec, as.numeric)
    dat_ec  <- f_map("EC", "r_ec_vol",         dat_ec, as.numeric)
    dat_ec  <- f_map("EC", "l_ec_thick",       dat_ec, as.numeric)
    dat_ec  <- f_map("EC", "r_ec_thick",       dat_ec, as.numeric)
    dat_ec$bi_ec_vol   <- (dat_ec$l_ec_vol   + dat_ec$r_ec_vol)   / 2
    dat_ec$bi_ec_thick <- (dat_ec$l_ec_thick + dat_ec$r_ec_thick) / 2

    ## race
    dat_ge <- dat_ge[, -which(names(dat_ge)
                                  %in% tolower(c("JHUANONID",
                                         "LETTERCODE",
                                         "NIHID")))]
    dat_ge <- f_map("GE", "subject_id", dat_ge)

    ## exclude subjects from list A and list B
    id_name <- a_map_var("BIOCARD", "LIST_A", "subject_id", dict_src_tables)
    exid <- c(dat_lsta[[id_name]],
              dat_lstb[[id_name]])

    ## ------------- prepare bases of dates -----------------------------
    print("Merging analysis dataset...")
    dat_se <- a_window(dat    = get(paste("dat_", merge_by, sep = "")),
                       v_date = paste("date_", merge_by, sep = ""),
                       window,
                       window_overlap)

    ## ------------- combine data --------------------------------------
    dat_se <- a_match(dat_se, dat_diag,    "date_diag",    dup_list)
    dat_se <- a_match(dat_se, dat_cog,   "date_cog",   dup_list)
    dat_se <- a_match(dat_se, dat_csf,   "date_csf",   dup_list)
    dat_se <- a_match(dat_se, dat_hippo, "date_hippo", dup_list)
    dat_se <- a_match(dat_se, dat_amy,   "date_amy",   dup_list)
    dat_se <- a_match(dat_se, dat_ec,    "date_ec",    dup_list)

    dat_diag_sub <- dat_diag %>%
        select(tolower(c("subject_id", "JHUANONID", "LETTERCODE", "NIHID",
                 "VISITNO", "date_diag")))

    dat_se <- dat_se %>%
        left_join(dat_demo, by = c("subject_id")) %>%
        left_join(dat_ge, by = c("subject_id")) %>%
        select(- c("date", "date_left", "date_right")) %>%
        left_join(dat_diag_sub, by = c("subject_id", "date_diag"))

    ## load ApoE-4
    dat_se$apoe <- as.numeric(dat_se[["apoecode"]] %in% c(3.4, 4.4))
    dat_se$apoe[dat_se["apoecode"] == 2.4] <- NA
    
    ## drop duplicates
    dat_se <- dat_se %>%
        select(!(ends_with(".x")|ends_with(".y")))
    
    ## add exid
    dat_se <- dat_se %>%
        rowwise() %>%
        mutate(exclude = subject_id %in% exid)

    print("Done.")

    ## return
    s <- dat_se
    # class(s) <- "biocard"
    return(s)
}
