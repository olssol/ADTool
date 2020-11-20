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
                          function(x) grep(str, data[[x]], ignore.case = TRUE))

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
adt_get_dict <- function(dict = c("tbl", "col_name", "adni", "data"), csv_fname = NULL) {
    dict     <- match.arg(dict)
    rst_dict <- get(paste("dict_", dict, sep = ""))

    if (!is.null(csv_fname)) {
        csv_dict <- read.csv(file = csv_fname, row.names = 1)
        if (!identical(names(rst_dict), names(csv_dict)))
            stop("Data structure in the import CSV file is wrong.
              Please match the columns as the dictionary provided in the package using.")
        if (dict == "tbl") {
            if(!identical(csv_dict$file_code, rst_dict$file_code)) {
                stop("Please use the same file_code in the imported dict_tbl as 
                     the dictionary provided in the package using.")
            }
            if(!identical(csv_dict$file_name, rst_dict$file_name)) {
                stop("Please use the same file_name in the imported dict_tbl as 
                     the dictionary provided in the package using.")
            }
        }
        if (dict == "col_name") {
            if(!identical(csv_dict$col_name, rst_dict$col_name)) {
                stop("Please use the same col_name in the imported dict_col_name as 
                     the dictionary provided in the package using.")
            }
            if(!identical(csv_dict$data_source, rst_dict$data_source)) {
                stop("Please use the same data_source in the imported dict_col_name as 
                     the dictionary provided in the package using.")
            }
            if(!identical(csv_dict$table_code, rst_dict$table_code)) {
                stop("Please use the same table_code in the imported dict_col_name as 
                     the dictionary provided in the package using.")
            }
        }
        

        rst_dict <- csv_dict
    }

    rst_dict
}

#' Query Function
#'
#' @param biocard the object
#'
#' @return
#' @export
#'
#' @examples
adt_query <- function(biocard) {
    UseMethod("biocard")
}

#' Search description from dictionary
#'
#' @param biocard the object
#' @param var The variable name to query
#' @param opt Searching options
#'
#' @return
#' @export
#'
#' @examples
adt_query <- function(biocard, var, opt = c("general", "variable", "sources")) {
    opt <- match.arg(opt)
    dict_data <- biocard$dict_data
    if (opt == "general") {
        res <- adt_tk_query(dict_data, var)
    }
    if (opt == "variable") {
        res <- adt_tk_query(dict_data, var) %>%
                filter(col_name == var)
    }
    if (opt == "sources") {
        res <- adt_tk_query(dict_data, var) %>%
                filter(grepl(var, location, ignore.case = TRUE))
    }
    if (dim(res)[1] == 0) {
        print("No records found.")
        return(NULL)
    }
    else {
       return(res)
    }
}





