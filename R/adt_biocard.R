#' Get BIOCARD Data
#'
#' Load all BIOCARD data from a folder, generate the corresponding analysis
#' dataset with column names consistent with other data sources
#' 
#' 
#' @param path Location where the biocard data stored
#' @param pattern Pattern of the data files
#' @param parameter_file Parameters dictionary (could be modified if needed)
#' @param variable_file Variables dictionary (could be modified if needed)
#'
#' @return A list with the following items
#' 
#'
#' @examples
#' \dontrun{
#' dt_biocard <- get_biocard(path)
#' }
#'
#' @export
#'
adt_get_biocard <- function(path = ".", pattern = "*.xls",
                            dict_par = NULL, dict_vars = NULL, window = 730) {

    ## --------- functions -------------------------------------
    ## convert date
    f_date <- function(code, dta) {
        mvar <- a_map_var("BIOCARD", code, "date", dict_vars)
        dfmt <- filter(dict_par,
                       file_code == code)[["date_format"]]

        dta["date"] <- as.Date(dta[, mvar], dfmt)
        dta         <- dta[, -grep(mvar, names(dta))]
        dta
    }

    ## map var
    f_map <- function(code, var, dta, fc = NULL) {
        mvar     <- a_map_var("BIOCARD", code, var, dict_vars)
        dta[var] <- dta[mvar]
        dta      <- dta[, -grep(mvar, names(dta))]

        if (!is.null(fc))
            dta[var] <- fc(dta[var])

        dta
    }

    ## --------- prepare pars -------------------------------------
    dup_list <- c("JHUANONID", "LETTERCODE",
                  "NIHID", "VISITNO", "date",
                  "Scan", "De.Identified.Subject.ID",
                  "Subject.NIHID", "Age.at.Scan",
                  "Diagnosis.at.last.scan", "Consensus.Diagnosis",
                  "intracranial_vol")

    ## list all file names matching the pattern
    file_names <- list.files(path = path, pattern = pattern, full.names = TRUE)

    if (is.null(dict_par))
        dict_par <- apt_get_dict("par")
    else
        dict_par <- apt_get_dict("par", csv_fname = dict_vars)

    if (is.null(dict_vars))
        dict_vars <- apt_get_dict("vars")
    else
        dict_vars <- apt_get_dict("vars", csv_fname = dict_vars)

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
    dat_cog   <- f_date("COG",  dat_cog)
    dat_cog   <- f_map("COG", "subject_id", dat_cog)

    dat_dx    <- f_date("DIAG", dat_dx)
    dat_dx    <- f_map("DIAG", "subject_id", dat_dx)

    dat_csf   <- f_date("CSF",  dat_csf)
    dat_csf   <- f_map("CSF",   "abeta", dat_csf)
    dat_csf   <- f_map("CSF",   "subject_id", dat_csf)

    dat_hippo <- f_date("HIPPO", dat_hippo)
    dat_hippo <- f_map("HIPPO", "subject_id",       dat_hippo)
    dat_hippo <- f_map("HIPPO", "intracranial_vol", dat_hippo, as.numeric)
    dat_hippo <- f_map("HIPPO", "l_hippo",,         dat_hippo, as.numeric)
    dat_hippo <- f_map("HIPPO", "r_hippo",,         dat_hippo, as.numeric)
    dat_hippo$bi_hippo <- (dat_hippo$l_hippo + dat_hippo$r_hippo) / 2

    ## MRI amygdala
    dat_amy <- f_date("AMY", dat_amy)
    dat_amy <- f_map("AMY", "subject_id",       dat_amy)
    dat_amy <- f_map("AMY", "intracranial_vol", dat_amy, as.numeric)
    dat_amy <- f_map("AMY", "l_amy",            dat_amy, as.numeric)
    dat_amy <- f_map("AMY", "r_amy",            dat_amy, as.numeric)
    dat_amy$bi_amy <- (dat_amy$l_amy + dat_amy$r_amy) / 2

    ## MRI EC volume
    dat_ec  <- f_date("EC", dat_ec)
    dat_ec  <- f_map("EC", "subject_id",       dat_ec)
    dat_ec  <- f_map("EC", "intracranial_vol", dat_ec, as.numeric)
    dat_ec  <- f_map("EC", "l_ec_vol",         dat_ec, as.numeric)
    dat_ec  <- f_map("EC", "r_ec_vol",         dat_ec, as.numeric)
    dat_ec  <- f_map("EC", "l_ec_thick",       dat_ec, as.numeric)
    dat_ec  <- f_map("EC", "r_ec_thick",       dat_ec, as.numeric)
    dat_ec$bi_ec_vol   <- (dat_ec$l_ec_vol   + dat_ec$r_ec_vol)   / 2
    dat_ec$bi_ec_thick <- (dat_ec$l_ec_thick + dat_ec$r_ec_thick) / 2

    ## race
    dat_race <- dat_race[, - which(names(dat_race)
                                   %in% c("JHUANONID",
                                          "LETTERCODE",
                                          "NIHID"))]

    ## exclude subjects from list A and list B
    exid <- c(dta_lsta[LIST_A$id],
              dta_lstb[LIST_B$id])

    ## ------------- combine data --------------------------------------
    tcog   <- a_match(dat_diag, dat_cog,   window, dup_list)
    tcsf   <- a_match(dat_diag, dat_csf,   window, dup_list)
    thippo <- a_match(dat_diag, dat_hippo, window, dup_list)
    tamy   <- a_match(dat_diag, dat_amy,   window, dup_list)
    tec    <- a_match(dat_diag, dat_ec,    window, dup_list)

    out <- merge(dat_diag, tcog,   by = "subject_id")
    out <- merge(out,      tcsf,   by = "subject_id")
    out <- merge(out,      thippo, by = "subject_id")
    out <- merge(out,      tamy,   by = "subject_id")
    out <- merge(out,      tec,    by = "subject_id")
    dat <- merge(out,
                 dat_demo[, -which(names(dat_demo) %in% dup_list)],
                 by = "subject_id")


    ## load ApoE-4
    dat <- merge(dat, dat_race, by = GE$id)
    dat$apoe <- as.numeric(dat[, "APOECODE"] %in% c(3.4, 4.4))
    dat$apoe[dat["APOECODE"] == 2.4] <- NA

    ##
    return(list(data = dat,
                exid = exid))

}
