#' Search description from dictionary
#'
#' @param biocard the object
#' @param var The variable name to query
#' @param opt Searching options
#'
#' @return
#'
#' @examples
#' \dontrun{
#' adt_query(biovard, "EDUC", "general")
#' }
#'
#' @export
#'
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
