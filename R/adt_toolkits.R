#' Query data frames
#'
#' Get the dictionary or dictionary data for creating new dictionary and
#' checking variables meanings.
#'
#' @inheritParams parameters
#'
#' @return The search result (either an entire dataset or the location and
#'     description of searched variable).
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

#' Get dictionary dataset
#'
#' Get the dictionary dataset as a dataframe or save as a csv file.
#'
#' @inheritParams parameters
#'
#' @return The dictionary dataset.
#'
#' @examples
#' \dontrun{
#' dict_par <- adt_get_dict(dict = "tbl")
#' dict_par <- adt_get_dict(dict = "tbl", csv_fname = "foo_tbl.csv")}
#'
#' @export
#'
adt_get_dict <- function(dict = c("src_files", "src_tables",
                                  "ana_data", "adni"),
                         csv_fname = NULL) {

    dict     <- match.arg(dict)
    rst_dict <- get(paste("dict_", dict, sep = ""))

    if (!is.null(csv_fname)) {
        csv_dict <- read.xls(xls = csv_fname)
    }
    else{
        csv_dict <- NULL
    }

    rst_dict <- a_update_dict(rst_dict, csv_dict)
    if (dict=="src_tables") {
        rst_dict <- rst_dict %>%
            drop_na()
        }
    rst_dict
}
