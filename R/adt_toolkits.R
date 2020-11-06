#' Query data frames
#'
#' @param data        data sets
#' @param str         String to search
#' @param csv_fname   CSV filename to be saved
#' @param ignore_case Ignore case during matching
#'
#' @export
#'
adt_tk_query <- function(data, str = NULL, csv_fname = NULL,
                         ignore_case = TRUE) {

    if (is.null(str)) {
        rst <- data
    } else {
        lst_inx <- sapply(colnames(data),
                          function(x) grep(str, data[, x]))

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
#' @param dict      Name of the dictionary. 
#'                  The available options are: "tbl", "col_name", "adni"
#' @param csv_fname External dictionary. If change of default setting needed
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
