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
            #dta[var] <- fc(dta[var])
            dta[var] <- lapply(dta[var], fc)

        dta
    }

    ## --------- prepare pars -------------------------------------
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
    dat_hippo <- f_map("HIPPO",  "intracranial_vol_hippo", dat_hippo, as.numeric)
    dat_hippo <- f_map("HIPPO",  "l_hippo",          dat_hippo, as.numeric)
    dat_hippo <- f_map("HIPPO",  "r_hippo",          dat_hippo, as.numeric)
    dat_hippo$bi_hippo <- (dat_hippo$l_hippo + dat_hippo$r_hippo) / 2
    
    ## MRI amygdala
    dat_amy <- f_date("AMY", "date_amy",        dat_amy)
    dat_amy <- f_map("AMY", "subject_id",       dat_amy)
    dat_amy <- f_map("AMY", "intracranial_vol_amy", dat_amy, as.numeric)
    dat_amy <- f_map("AMY", "l_amy",            dat_amy, as.numeric)
    dat_amy <- f_map("AMY", "r_amy",            dat_amy, as.numeric)
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

    ## ------------- combine data --------------------------------------
    tcog   <- a_match(dat_dx, dat_cog,   "date_cog",   window, dup_list)
    tcsf   <- a_match(dat_dx, dat_csf,   "date_csf",   window, dup_list)
    thippo <- a_match(dat_dx, dat_hippo, "date_hippo", window, dup_list)
    tamy   <- a_match(dat_dx, dat_amy,   "date_amy",   window, dup_list)
    tec    <- a_match(dat_dx, dat_ec,    "date_ec",    window, dup_list)
    
    out <- merge(dat_dx, tcog,     by = "subject_id")
    out <- merge(out,    tcsf,     by = "subject_id")
    out <- merge(out,    thippo,   by = "subject_id")
    out <- merge(out,    tamy,     by = "subject_id")
    out <- merge(out,    tec,      by = "subject_id")
    dat <- merge(out,    dat_demo, by = "subject_id")

    ## load ApoE-4
    dat <- merge(dat, dat_race, by = "subject_id")
    dat$apoe <- as.numeric(dat[, "APOECODE"] %in% c(3.4, 4.4))
    dat$apoe[dat["APOECODE"] == 2.4] <- NA

    ##
    return(list(data = dat,
                exid = exid))

}
