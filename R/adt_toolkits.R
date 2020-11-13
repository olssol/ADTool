#' Query data frames
#'
#' Get the dictionary or dictionary data for creating new dictionary and checking variables meanings.
#'
#' @param data        A dataset. Possible choices are "dict_tbl": the dictionary for the structure of tables, 
#' "dict_cil_name": the dictionary maps the original column names (old_col_name) to the column names of generated analysis dataset,
#' "dict_data": This dictionary includes all the variables names in the analysis dataset. This dictionary could be used to check 
#' the location and description of each variable. 
#' @param str         String to search. Default value is NULL. If NULL, this function is used to check the entire dictionary.
#' If input a string (the variable name of the analysis dataset), this function is used to check the location and description of the variable. 
#' @param csv_fname   A string indicating the CSV filename to be saved (saved to the default location). Default is NULL. The saved file is in 
#' .scv format.
#'
#' @return The search result (either an entire dataset or the location and description of searched variable).
#' 
#' @examples
#' \dontrun{
#' adt_tk_query(dict_tbl)
#' adt_tk_query(dict_col_name, csv_fname = "col_name.csv")
#' adt_tk_query(dict_data, "waissmac")
#' adt_tk_query(dict_data, "MCIN1EX")}
#' 
#' @export
#'
adt_tk_query <- function(data, str = NULL, csv_fname = NULL) {

    if (is.null(str)) {
        rst <- data
    } else {
        lst_inx <- sapply(colnames(data),
                          function(x) grep(str, data[[x]]))

        inx <- NULL
        for (i in seq_len(length(lst_inx))) {
            if (length(lst_inx[[i]]) > 0)
                inx <- c(inx, lst_inx[[i]])
        }
        inx <- sort(unique(inx))

        if (0 == length(inx))
            rst <- NULL
        else
            rst <- data[inx, ]
    }

    if (is.null(rst)) {
        print("No records found.")
        return(NULL)
    } else {
        if (is.null(csv_fname))
            return(rst)
        else
            write.csv(rst, file = csv_fname)
    }
}

#' Get dictionary 
#' 
#' Get the needed dictionary data for leading subtables and mapping variables.
#'
#' @param dict      A string indicating the name of the dictionary. 
#'                  The available options are: "tbl": the dictionary for table structure, 
#'                  "col_name": the dictionary for mapping column names, "adni": the dictionary for ADNI data.
#' @param csv_fname The path of external dictionary. If changes of default dictionaries needed, then the costomized dictionary could be added here. 
#' However, the format of the costomized dictionary should follow the default one.
#'
#' @return The dictionary dateset.
#'
#' @examples
#' \dontrun{
#' dict_par <- adt_get_dict(dict = "tbl")
#' dict_par <- adt_get_dict(dict = "tbl", csv_fname = "foo_tbl.csv")}
#'
#' @export
#'
adt_get_dict <- function(dict = c("tbl", "col_name", "adni"), csv_fname = NULL) {
    dict     <- match.arg(dict)
    rst_dict <- get(paste("dict_", dict, sep = ""))

    if (!is.null(csv_fname)) {
        csv_dict <- read.csv(file = csv_fname)
        if (!identical(names(rst_dict), names(csv_dict)))
            stop("Data structure in the CSV file is wrong.
              Check the dictionary provided in the package.")

        rst_dict <- csv_dict
    }

    rst_dict
}
