#' Import BIOCARD Data
#'
#' Import BIOCARD data from source files and generate the analysis dataset.
#'
#' @inheritParams parameters
#'
#' @return
#'
#' Returned the analysis dataset with: patients' ids, baseline times,
#' corresponding biomarkers, biomarker test times, etc.
#'
#' @examples
#' \dontrun{
#' ## with default unoverlaped window
#' dt_biocard <- get_biocard(path, merge_by = "diagnosis")
#'
#' ## with costomized window
#' dt_biocard <- get_biocard(path, merge_by = "diagnosis",
#'                           window = 365,
#'                           window_overlap = TRUE)
#'
#' ## with dictionary provided by user
#' dt_biocard <- get_biocard(path, merge_by = "dx",
#'                           src_tables = "dict_src_tables.xlsx")
#' }
#'
#' @export
#'
#'
adt_get_biocard <- function(path     = ".",
                            merge_by = c("diagnosis", "cognitive", "csf",
                                         "hippocampus", "amydata",
                                         "entorhinal"),
                            window  = 730, window_overlap = FALSE,
                            pattern    = "*.xls",
                            src_files  = NULL,
                            src_tables = NULL,
                            par_apoe = list(levels = c(3.4, 4.4, 2.4),
                                            labels = c(1, 2, NA)),
                            verbose    = TRUE) {

    ## --------- internal functions -------------------------------------
    ## convert date
    f_date <- function(code, date_name, dta) {
        mvar <- a_map_var("BIOCARD", code, date_name, dict_src_tables)
        dfmt <- dict_src_files %>%
            filter(adt_table_code == code)

        stopifnot(1 == nrow(dfmt))

        dta %>%
            mutate(!!date_name :=
                       as.Date(!!as.name(mvar),
                               dfmt[1, "src_date_format"])) %>%
            select(- all_of(mvar))
    }

    ## map var
    f_map <- function(code, var, dta, fc = NULL) {
        mvar <- a_map_var("BIOCARD", code, var, dict_src_tables)
        dta  <- dta %>%
            rename(!!var := mvar)

        if (!is.null(fc))
            dta[var] <- lapply(dta[var], fc)

        dta
    }

    ## --------- prepare pars -------------------------------------

    ## source data set of dates for combining data
    merge_by <- match.arg(merge_by)

    ## switch
    merge_by <- switch(merge_by,
                       diagnosis   = "diag",
                       cognitive   = "cog",
                       csf         = "csf",
                       hippocampus = "hippo",
                       amydata     = "amy",
                       entorhinal   = "ec")

    dup_list <- c("jhuanonid", "lettercode",
                  "nihid", "visitno", "diagnosis.at.last.scan",
                  "scan", "de.identified.subject.id",
                  "subject.nihid", "age.at.scan",
                  "diagnosis..at.last.scan", "consensus.diagnosis",
                  "intracranial_vol", "x")

    ## --------- prepare files  -------------------------------------

    ## list all file names matching the pattern
    file_names <- list.files(path       = path,
                             pattern    = pattern,
                             full.names = TRUE)

    ## dictionary of src files
    dict_src_files  <- adt_get_dict("src_files",  csv_fname = src_files)
    dict_src_tables <- adt_get_dict("src_tables", csv_fname = src_tables)
    dict_data       <- adt_get_dict("ana_data")

    ## --------- read tables -------------------------------------
    vec_tbls <- c("COG", "DIAG", "CSF", "DEMO",
                  "HIPPO", "AMY", "EC", "GE")

    chk_all <- NULL
    for (i in vec_tbls) {
        cur_dat  <- a_read_file(i, file_names, dict_src_files, verbose)
        cur_chk  <- a_check_src(i, cur_dat,    dict_src_tables)

        assign(paste("dat_", tolower(i), sep = ""),
               cur_dat)

        chk_all <- rbind(chk_all, cur_chk)
    }

    if (dim(chk_all)[1] > 0) {

        ## XINYU: IS THIS MESSAGE THE SAME AS a_msg("biocard_load_error")
        err_msg <- a_msg("biocard_load_error")
        message(err_msg)
        print(chk_all)

        ## message("Some variable(s) not found.
        ##       Missing of ID and date variables may cause problem when merging data.
        ##       Missing of other variables may cause problem when querying.
        ##       (The analysis dateset can still be generated if no ID and date variables are missing.)
        ##       Missing information is shown below:")

        ## XINYU: I SUGGEST WE LET THE PROCESS CONTINUE
        ## skip <- readline(prompt = "Continue to generate the analysis dataset? (Y/n)?")
        ## if ("n" == skip) {
        ##}
    }

    dat_lsta  <- a_read_file("LIST_A", file_names, dict_src_files, verbose)
    dat_lstb  <- a_read_file("LIST_B", file_names, dict_src_files, verbose)


    ## ----------  manipulation ----------------------------------
    a_print("Formatting data ...", verbose)

    dat_cog   <- f_date("COG",  "date_cog",   dat_cog)
    dat_cog   <- f_map("COG",   "subject_id", dat_cog)

    dat_diag  <- f_date("DIAG", "date_diag",  dat_diag)
    dat_diag  <- f_map("DIAG",  "subject_id", dat_diag)

    dat_csf   <- f_date("CSF",  "date_csf",   dat_csf)
    dat_csf   <- f_map("CSF",   "abeta",      dat_csf)
    dat_csf   <- f_map("CSF",   "subject_id", dat_csf)

    dat_demo  <- f_map("DEMO", "subject_id", dat_demo)
    dat_demo  <- dat_demo %>%
        select(-c("jhuanonid", "lettercode", "nihid"))

    dat_hippo <- f_date("HIPPO", "date_hippo", dat_hippo)
    dat_hippo <- f_map("HIPPO",  "subject_id", dat_hippo)

    dat_hippo <- f_map("HIPPO",  "intracranial_vol_hippo",
                       dat_hippo, as.numeric)

    dat_hippo <- f_map("HIPPO",  "l_hippo", dat_hippo, as.numeric)
    dat_hippo <- f_map("HIPPO",  "r_hippo", dat_hippo, as.numeric)

    ## MRI amygdala
    dat_amy <- f_date("AMY", "date_amy",        dat_amy)
    dat_amy <- f_map("AMY", "subject_id",       dat_amy)

    dat_amy <- f_map("AMY", "intracranial_vol_amy",
                     dat_amy, as.numeric)

    dat_amy <- f_map("AMY", "l_amy", dat_amy, as.numeric)
    dat_amy <- f_map("AMY", "r_amy", dat_amy, as.numeric)


    ## MRI EC volume
    dat_ec  <- f_date("EC", "date_ec",         dat_ec)
    dat_ec  <- f_map("EC", "subject_id",       dat_ec)
    dat_ec  <- f_map("EC", "intracranial_vol_ec", dat_ec, as.numeric)
    dat_ec  <- f_map("EC", "l_ec_vol",         dat_ec, as.numeric)
    dat_ec  <- f_map("EC", "r_ec_vol",         dat_ec, as.numeric)
    dat_ec  <- f_map("EC", "l_ec_thick",       dat_ec, as.numeric)
    dat_ec  <- f_map("EC", "r_ec_thick",       dat_ec, as.numeric)

    ## process data
    dat_hippo$bi_hippo <- (dat_hippo$l_hippo + dat_hippo$r_hippo) / 2
    dat_amy$bi_amy     <- (dat_amy$l_amy     + dat_amy$r_amy)     / 2
    dat_ec$bi_ec_vol   <- (dat_ec$l_ec_vol   + dat_ec$r_ec_vol)   / 2
    dat_ec$bi_ec_thick <- (dat_ec$l_ec_thick + dat_ec$r_ec_thick) / 2

    ## race
    dat_ge <- dat_ge %>%
        select(-c("jhuanonid", "lettercode", "nihid"))

    dat_ge <- f_map("GE", "subject_id", dat_ge)

    ## exclude subjects from list A and list B
    id_name <- a_map_var("BIOCARD", "LIST_A", "subject_id", dict_src_tables)

    exid <- c(dat_lsta[[id_name]],
              dat_lstb[[id_name]])

    ## ------------- prepare bases of dates -----------------------------
    a_print("Merging analysis dataset...", verbose)
    dat_se <- a_window(dat    = get(paste("dat_", merge_by, sep = "")),
                       v_date = paste("date_", merge_by, sep = ""),
                       window,
                       window_overlap)

    ## ------------- combine data --------------------------------------
    dat_se <- a_match(dat_se, dat_diag,  "date_diag",  dup_list)
    dat_se <- a_match(dat_se, dat_cog,   "date_cog",   dup_list)
    dat_se <- a_match(dat_se, dat_csf,   "date_csf",   dup_list)
    dat_se <- a_match(dat_se, dat_hippo, "date_hippo", dup_list)
    dat_se <- a_match(dat_se, dat_amy,   "date_amy",   dup_list)
    dat_se <- a_match(dat_se, dat_ec,    "date_ec",    dup_list)

    dat_diag_sub <- dat_diag %>%
        select(c("subject_id", "jhuanonid", "lettercode", "nihid",
                 "visitno", "date_diag"))

    dat_se <- dat_se %>%
        left_join(dat_demo, by = c("subject_id")) %>%
        left_join(dat_ge, by = c("subject_id")) %>%
        select(- c("date", "date_left", "date_right")) %>%
        left_join(dat_diag_sub, by = c("subject_id", "date_diag"))

    ## load ApoE-4
    ## XINYU: I SUGGEST WE MAKE THIS A FUNCTION SINCE NACC OR ANDI MAY USE IT TOO
    dat_se$apoe <- apt_apoe(dat_se$apoecode)

    ## drop duplicates
    dat_se <- dat_se %>%
        select(!(ends_with(".x") | ends_with(".y")))

    ## add exid
    dat_se <- dat_se %>%
        rowwise() %>%
        mutate(exclude = subject_id %in% exid)

    a_print("Done.", verbose)

    ## return

    ## XINYU: WHY ARE WE NOT ASSIGNING CLASS ANY MORE?
    s <- dat_se
    # class(s) <- "biocard"
    return(s)
}
