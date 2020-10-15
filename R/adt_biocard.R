#' Get BIOCARD Data
#'
#' @param path Location where the biocard data stored
#' @param pattern Pattern of the data files
#' @param parameter_file Parameters dictionary (could be modified if needed)
#' @param variable_file Variables dictionary (could be modified if needed)
#'
#' @return a table with merged biocard data
#' @export
#' @examples
#' \dontrun{
#' path = "/Users/name/Documents/R/adbiomarker/Data/BIOCARD"
#' dt_biocard <- get_biocard(path)
#' }
#'

get_biocard <- function(path = "/Users/name/Data/BIOCARD",
                        pattern = "*.xls",
                        parameter_file = "par_file.xlsx",
                        variable_file = "variables.xlsx"
                        ) {
    file_names <- list.files(path = path, pattern = pattern)
    path <- paste(path, "/", sep = "")
    par_file <- read_xlsx(paste(path, parameter_file, sep = ""))
    var_file <- read_xlsx(paste(path, variable_file, sep = ""))
                                        # par_file <- read_xlsx(parameter_file)
                                        # var_file <- read_xlsx(variable_file)

    dat_cog <- get_table("COG", file_names, par_file, path)
    dat_dx <- get_table("DIAG", file_names, par_file, path)
    dat_csf <- get_table("CSF", file_names, par_file, path)
    dat_demo <- get_table("DEMO", file_names, par_file, path)

    dat_cog["date"] <- as.Date(dat_cog[, map_var("BIOCARD",
                                                 "COG",
                                                 "date",
                                                 var_file)],
                               filter(par_file,
                                      file_code == "COG")[["date_format"]])
    dat_cog <- dat_cog[, -grep(map_var("BIOCARD",
                                       "COG",
                                       "date",
                                       var_file), names(dat_cog))]
    dat_dx["date"] <- as.Date(dat_dx[, map_var("BIOCARD",
                                               "DIAG",
                                               "date",
                                               var_file)],
                              filter(par_file,
                                     file_code == "DIAG")[["date_format"]])
    dat_dx <- dat_dx[, -grep(map_var("BIOCARD",
                                     "DIAG",
                                     "date",
                                     var_file), names(dat_dx))]
    dat_csf["date"] <- as.Date(dat_csf[, map_var("BIOCARD",
                                                 "CSF",
                                                 "date",
                                                 var_file)],
                               filter(par_file,
                                      file_code == "CSF")[["date_format"]])
    dat_csf <- dat_csf[, -grep(map_var("BIOCARD", "CSF", "date", var_file),
                               names(dat_csf))]
    dat_csf["abeta"] <- dat_csf[, map_var("BIOCARD", "CSF", "abeta", var_file)]
    dat_csf <- dat_csf[, -grep(map_var("BIOCARD", "CSF", "abeta", var_file),
                               names(dat_csf))]

                                        # MRI hippocampus
    hippo_dat <- get_table("HIPPO", file_names, par_file, path)
    hippo_dat$date <- as.Date(hippo_dat[, map_var("BIOCARD",
                                                  "HIPPO",
                                                  "date",
                                                  var_file)],
                              filter(par_file,
                                     file_code == "HIPPO")[["date_format"]])
    hippo_dat <- hippo_dat[, -grep(map_var("BIOCARD",
                                           "HIPPO",
                                           "date",
                                           var_file),
                                   names(hippo_dat))]
    hippo_dat$intracranial_vol <- as.numeric(hippo_dat[, map_var("BIOCARD",
                                                                 "HIPPO",
                                                                 "intracranial_vol",
                                                                 var_file)])
    hippo_dat <- hippo_dat[, -grep(map_var("BIOCARD",
                                           "HIPPO",
                                           "intracranial_vol",
                                           var_file),
                                   names(hippo_dat))]
    hippo_dat$l_hippo <- as.numeric(hippo_dat[, map_var("BIOCARD",
                                                        "HIPPO",
                                                        "l_hippo",
                                                        var_file)])
    hippo_dat <- hippo_dat[, -grep(map_var("BIOCARD",
                                           "HIPPO",
                                           "l_hippo",
                                           var_file),
                                   names(hippo_dat))]
    hippo_dat$r_hippo <- as.numeric(hippo_dat[, map_var("BIOCARD",
                                                        "HIPPO",
                                                        "r_hippo",
                                                        var_file)])
    hippo_dat <- hippo_dat[, -grep(map_var("BIOCARD",
                                           "HIPPO",
                                           "r_hippo",
                                           var_file),
                                   names(hippo_dat))]
    hippo_dat$bi_hippo <- (hippo_dat$l_hippo + hippo_dat$r_hippo) / 2

                                        # MRI amygdala
    amy_dat <- get_table("AMY", file_names, par_file, path)
    amy_dat$date <- as.Date(amy_dat[, map_var("BIOCARD",
                                              "AMY",
                                              "date",
                                              var_file)],
                            filter(par_file, file_code == "AMY")[["date_format"]])
    amy_dat <- amy_dat[, -grep(map_var("BIOCARD", "AMY", "date", var_file),
                               names(amy_dat))]
    amy_dat$intracranial_vol <- as.numeric(amy_dat[, map_var("BIOCARD",
                                                             "AMY",
                                                             "intracranial_vol",
                                                             var_file)])
    amy_dat <- amy_dat[, -grep(map_var("BIOCARD", "AMY", "intracranial_vol",
                                       var_file), names(amy_dat))]
    amy_dat$l_amy <- as.numeric(amy_dat[, map_var("BIOCARD", "AMY", "l_amy",
                                                  var_file)])
    amy_dat <- amy_dat[, -grep(map_var("BIOCARD", "AMY", "l_amy", var_file),
                               names(amy_dat))]
    amy_dat$r_amy <- as.numeric(amy_dat[, map_var("BIOCARD", "AMY", "r_amy",
                                                  var_file)])
    amy_dat <- amy_dat[, -grep(map_var("BIOCARD", "AMY", "r_amy", var_file),
                               names(amy_dat))]
    amy_dat$bi_amy <- (amy_dat$l_amy + amy_dat$r_amy) / 2

                                        # MRI EC volume
    ec_dat <- get_table("EC", file_names, par_file, path)
    ec_dat$date <- as.Date(ec_dat[, map_var("BIOCARD", "EC", "date", var_file)],
                           filter(par_file, file_code == "EC")[["date_format"]])
    ec_dat <- ec_dat[, -grep(map_var("BIOCARD", "EC", "date", var_file),
                             names(ec_dat))]
    ec_dat$intracranial_vol <- as.numeric(ec_dat[, map_var("BIOCARD",
                                                           "EC",
                                                           "intracranial_vol",
                                                           var_file)])
    ec_dat <- ec_dat[, -grep(map_var("BIOCARD",
                                     "EC",
                                     "intracranial_vol",
                                     var_file),
                             names(ec_dat))]
    ec_dat$l_ec_vol <- as.numeric(ec_dat[, map_var("BIOCARD",
                                                   "EC",
                                                   "l_ec_vol",
                                                   var_file)])
    ec_dat <- ec_dat[, -grep(map_var("BIOCARD", "EC", "l_ec_vol", var_file),
                             names(ec_dat))]
    ec_dat$r_ec_vol <- as.numeric(ec_dat[, map_var("BIOCARD",
                                                   "EC",
                                                   "r_ec_vol",
                                                   var_file)])
    ec_dat <- ec_dat[, -grep(map_var("BIOCARD", "EC", "r_ec_vol", var_file),
                             names(ec_dat))]
    ec_dat$bi_ec_vol <- (ec_dat$l_ec_vol + ec_dat$r_ec_vol) / 2
    ec_dat$l_ec_thick <- as.numeric(ec_dat[, map_var("BIOCARD",
                                                     "EC",
                                                     "l_ec_thick",
                                                     var_file)])
    ec_dat <- ec_dat[, -grep(map_var("BIOCARD", "EC", "l_ec_thick", var_file),
                             names(ec_dat))]
    ec_dat$r_ec_thick <- as.numeric(ec_dat[, map_var("BIOCARD",
                                                     "EC",
                                                     "r_ec_thick",
                                                     var_file)])
    ec_dat <- ec_dat[, -grep(map_var("BIOCARD", "EC", "r_ec_thick", var_file),
                             names(ec_dat))]
    ec_dat$bi_ec_thick <- (ec_dat$l_ec_thick + ec_dat$r_ec_thick) / 2


    dat <- data.frame()
    for (i in seq_len(nrow(dat_dx))) {
        x <- dat_dx[i, ]
        tcog <- mymatch(xid = x[map_var("BIOCARD", "DIAG", "subject_id", var_file)],
                        xdate = x$date,
                        dat = dat_cog, yidname = COG$id, ydatename = "date")
        tcsf <- mymatch(xid = x[map_var("BIOCARD", "DIAG", "subject_id", var_file)],
                        xdate = x$date,
                        dat = dat_csf, yidname = CSF$id, ydatename = "date")
        thippo <- mymatch(xid =
                              x[map_var("BIOCARD", "DIAG", "subject_id", var_file)],
                          xdate = x$date,
                          dat = hippo_dat, yidname = HIPPO$id, ydatename = "date")
        tamy <- mymatch(xid = x[map_var("BIOCARD", "DIAG", "subject_id", var_file)],
                        xdate = x$date,
                        dat = amy_dat, yidname = AMY$id, ydatename = "date")
        tec <- mymatch(xid = x[map_var("BIOCARD", "DIAG", "subject_id", var_file)],
                       xdate = x$date,
                       dat = ec_dat, yidname = EC$id, ydatename = "date")
                                        # merge data for output
        dup_list <- c("JHUANONID", "LETTERCODE",
                      "NIHID", "VISITNO", "date",
                      "Scan", "De.Identified.Subject.ID",
                      "Subject.NIHID", "Age.at.Scan",
                      "Diagnosis.at.last.scan", "Consensus.Diagnosis",
                      "intracranial_vol")
        x <- x[, -which(names(x) %in% dup_list)]
        out <- merge(x, tcog,
                     by = map_var("BIOCARD", "DIAG", "subject_id", var_file))
        tcsf <- tcsf[, -which(names(tcsf) %in% dup_list)]
        out <- merge(out, tcsf,
                     by.x = map_var("BIOCARD", "DIAG", "subject_id", var_file),
                     by.y = map_var("BIOCARD", "CSF", "subject_id", var_file))
        thippo <- thippo[, -which(names(thippo) %in% dup_list)]
        out <- merge(out, thippo,
                     by.x = map_var("BIOCARD", "DIAG", "subject_id", var_file),
                     by.y = map_var("BIOCARD", "HIPPO", "subject_id", var_file))
        tamy <- tamy[, -which(names(tamy) %in% dup_list)]
        out <- merge(out, tamy,
                     by.x = map_var("BIOCARD", "DIAG", "subject_id", var_file),
                     by.y = map_var("BIOCARD", "AMY", "subject_id", var_file))
        tec <- tec[, -which(names(tec) %in% dup_list)]
        out <- merge(out, tec,
                     by.x = map_var("BIOCARD", "DIAG", "subject_id", var_file),
                     by.y = map_var("BIOCARD", "EC", "subject_id", var_file))
        dat <- rbind(dat, out)
    }

    dat <- merge(dat, dat_demo[, -which(names(dat_demo) %in% dup_list)],
                 by = map_var("BIOCARD", "DIAG", "subject_id", var_file))

                                        # exclude subjects from list A and list B
    list_a <- get_table("LIST_A", file_names, par_file, path)

    list_b <- get_table("LIST_B", file_names, par_file, path)

    exid <- c(list_a[LIST_A$id], list_b[LIST_B$id])

                                        # load ApoE-4
    dat_race <- get_table("GE", file_names, par_file, path)
    dat_race <- dat_race[, -which(names(dat_race)
                                  %in% c("JHUANONID", "LETTERCODE", "NIHID"))]
    dat <- merge(dat, dat_race, by = GE$id)
    dat$apoe <- as.numeric(dat[, "APOECODE"] %in% c(3.4, 4.4))
    dat$apoe[dat["APOECODE"] == 2.4] <- NA

    ##
    return(list(dat, exid))

}
