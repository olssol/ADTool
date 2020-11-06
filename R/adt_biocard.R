#' Get BIOCARD Data
#'
#' Load all BIOCARD data from a folder, generate the corresponding analysis
#' dataset with column names consistent with other data sources
#'
#' @param path The location where the biocard data stored. All the subtables need to be in the same location.
#' @param merge_by A character string indicating the baseline time for merging. If select "dx" (diagnosis), 
#' all the biomarkers will be selected and merged according to the diagnosis date.
#' Possible values for the baseline time are "dx" (diagnosis), "cog" (cognitive), "csf", 
#' "hippo" (hippocampus), "amy" (amygdala), "ec" (entorhinal).
#' @param window An integer (unit of days) indicating the maximum acceptable length of time interval between baseline time and biomarker test time.
#' The defaule is set to be 730 days. For most data, the time windows are calculated from the baseline time 
#' (left window = the midpoint between current and the previous time point; right window = the midpoint between current and the next time point).
#' If the time windows are longer than the maximum acceptable length, than force to select biomarker within the maximum window length we set. 
#' For the first and last baseline time, since there is no "previous" or "next" time point availiable, use the maximum acceptable window length instead.
#' @param window_overlap A logical value idicating the time window setting. Default "False". If true, all time windows will set from 1/2 "window" days before
#' the baseline time to the 1/2 "window" days after the baseline time. In this case, the time windows may overlap, which means some biomarkers may be 
#' merged to multiple baseline data. If false, will use the windows calculated from the blaseline times. The left window is set to be the midpoint 
#' between current and the previous time point. The right window is set to be the midpoint between current and the next time point. 
#' For the first and last baseline time, since there is no "previous" or "next" time point availiable, 
#' use the maximum acceptable window length (set by the parameter "window") instead. 
#' @param pattern A string indicating the pattern of all the data files. Default is "*.xls" (should work for both .xls and .xlsx). 
#' This pattern is used to read all table names from the path.
#' @param dict_par A path of data file (.csv) of the parameters dictionary. The default dictionary is appended in the package. 
#' Since the parameters in tables may vary, it could be modified by using costomized parameters dictionary. The format can refer to the 
#' appended dictionary.
#' @param dict_vars A path of data file (.csv) of the variables dictionary. The default dictionary is appended in the package. 
#' Since the variables in tables may vary, it could be modified by using costomized variables dictionary. The format can refer to the 
#' appended dictionary.
#' 
#' @return The returned value is a list with the following items:
#' \itemize{
#'    \item data: The data includes patients' ids, baseline times, corresponding biomarkers, and biomarker test times.
#'    \item exid: The subject_ids fo patients that are either not enrolled or impaired at baseline. 
#' }
#'
#' @examples
#' \dontrun{
#' # with default unoverlaped window
#' dt_biocard <- get_biocard(path, merge_by = "dx")
#' # with costomized window
#' dt_biocard <- get_biocard(path, merge_by = "dx", window = 365, window_overlap = TRUE)
#' # with costomized dictionary
#' dt_biocard <- get_biocard(path, merge_by = "dx", dict_par = "\BIOCARD\DATA\dict_par.csv")
#' }
#'
#' @export
#'
adt_get_biocard <- function(path = ".",
                            merge_by = c("dx", "cog", "csf",
                                         "hippo", "amy", "ec"),
                            window  = 730, window_overlap = FALSE,
                            pattern = "*.xls",
                            dict_par = NULL, dict_vars = NULL) {
    
    ## --------- functions -------------------------------------
    ## convert date
    f_date <- function(code, date_name, dta) {
        mvar <- a_map_var("BIOCARD", code, date_name, dict_vars)
        dfmt <- filter(dict_par,
                       file_code == code)[["date_format"]]
        
        dta[date_name] <- as.Date(dta[, mvar], dfmt)
        dta            <- dta[, -grep(mvar, names(dta))]
        dta
    }
    
    ## map var
    f_map <- function(code, var, dta, fc = NULL) {
        mvar     <- a_map_var("BIOCARD", code, var, dict_vars)
        dta[var] <- dta[mvar]
        dta      <- dta[, -grep(mvar, names(dta))]
        
        if (!is.null(fc))
            dta[var] <- lapply(dta[var], fc)
        
        dta
    }
    
    ## --------- prepare pars -------------------------------------
    
    ## source data set of dates for combining data
    merge_by <- match.arg(merge_by)
    
    dup_list <- c("JHUANONID", "LETTERCODE",
                  "NIHID", "VISITNO", "Diagnosis.at.last.scan",
                  "Scan", "De.Identified.Subject.ID",
                  "Subject.NIHID", "Age.at.Scan",
                  "Diagnosis..at.last.scan", "Consensus.Diagnosis",
                  "intracranial_vol")
    
    ## list all file names matching the pattern
    file_names <- list.files(path = path, pattern = pattern, full.names = TRUE)
    
    if (is.null(dict_par))
        dict_par <- adt_get_dict("par")
    else
        dict_par <- adt_get_dict("par", csv_fname = dict_vars)
    
    if (is.null(dict_vars))
        dict_vars <- adt_get_dict("vars")
    else
        dict_vars <- adt_get_dict("vars", csv_fname = dict_vars)
    
    ## --------- read tables -------------------------------------
    dat_cog   <- a_read_file("COG",    file_names, dict_par)
    dat_dx    <- a_read_file("DIAG",   file_names, dict_par)
    dat_csf   <- a_read_file("CSF",    file_names, dict_par)
    dat_demo  <- a_read_file("DEMO",   file_names, dict_par)
    dat_hippo <- a_read_file("HIPPO",  file_names, dict_par)
    dat_amy   <- a_read_file("AMY",    file_names, dict_par)
    dat_ec    <- a_read_file("EC",     file_names, dict_par)
    dat_race  <- a_read_file("GE",     file_names, dict_par)
    dat_lsta  <- a_read_file("LIST_A", file_names, dict_par)
    dat_lstb  <- a_read_file("LIST_B", file_names, dict_par)
    
    ## ----------  manipulation ----------------------------------
    dat_cog   <- f_date("COG",  "date_cog",   dat_cog)
    dat_cog   <- f_map("COG",   "subject_id", dat_cog)
    
    dat_dx    <- f_date("DIAG", "date_dx",    dat_dx)
    dat_dx    <- f_map("DIAG",  "subject_id", dat_dx)
    
    dat_csf   <- f_date("CSF",  "date_csf",   dat_csf)
    dat_csf   <- f_map("CSF",   "abeta",      dat_csf)
    dat_csf   <- f_map("CSF",   "subject_id", dat_csf)
    
    dat_demo   <- f_map("DEMO", "subject_id", dat_demo)
    dat_demo <- dat_demo[, -which(names(dat_demo)
                                  %in% c("JHUANONID",
                                         "LETTERCODE",
                                         "NIHID"))]
    
    dat_hippo <- f_date("HIPPO", "date_hippo",       dat_hippo)
    dat_hippo <- f_map("HIPPO",  "subject_id",       dat_hippo)
    
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
    dat_race <- dat_race[, -which(names(dat_race)
                                  %in% c("JHUANONID",
                                         "LETTERCODE",
                                         "NIHID"))]
    dat_race <- f_map("GE", "subject_id", dat_race)
    
    ## exclude subjects from list A and list B
    id_name <- a_map_var("BIOCARD", "LIST_A", "subject_id", dict_vars)
    exid <- c(dat_lsta[[id_name]],
              dat_lstb[[id_name]])
    
    ## ------------- prepare bases of dates -----------------------------
    dat_se <- a_window(dat    = get(paste("dat_", merge_by, sep = "")),
                       v_date = paste("date_", merge_by, sep = ""),
                       window,
                       window_overlap)
    
    ## ------------- combine data --------------------------------------
    dat_se <- a_match(dat_se, dat_dx,    "date_dx",    duplist)
    dat_se <- a_match(dat_se, dat_cog,   "date_cog",   duplist)
    dat_se <- a_match(dat_se, dat_csf,   "date_csf",   duplist)
    dat_se <- a_match(dat_se, dat_hippo, "date_hippo", duplist)
    dat_se <- a_match(dat_se, dat_amy,   "date_amy",   duplist)
    dat_se <- a_match(dat_se, dat_ec,    "date_ec",    duplist)
    
    dat_se <- dat_se %>%
        left_join(dat_demo, by = c("subject_id")) %>%
        left_join(dat_race, by = c("subject_id"))
    
    ## load ApoE-4
    dat_se$apoe <- as.numeric(dat_se[["APOECODE"]] %in% c(3.4, 4.4))
    dat_se$apoe[dat_se["APOECODE"] == 2.4] <- NA
    
    ## return
    return(list(data = dat_se,
                exid = exid))
}
