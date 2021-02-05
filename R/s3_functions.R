#' Summary for class adtool
#'
#' @param adtool 
#' @param ... 
#' @param vars A list of strings indicating categorical variables name
#'
#' @return
#' @export
#'
#' @examples
summary.adtool <- function(adtool, vars = NULL, ...) {
  x <- adtool$ana_dt
  pat <- x %>% distinct(subject_id, .keep_all = TRUE)
  n_pat <- dim(pat)[1]
  n_var <- dim(pat)[2]
  
  cat('This analysis dataset is created from', 
      toupper(adtool$dat_ty), 'using', 
      adtool$merge_by, 'time as the baseline for merging.\n')
  
  if(adtool$overlap) {
    cat('The time windows (may overlap) are before/after ', 
                         adtool$window / 2, ' days per baseline time.')
  }
  else {
    cat('The time windows are set by the midpoint between the previous and next baseline time.\n')
    cat('If the length of windows are longer than', 
           adtool$window / 2, 'days,\nthen force to select biomarkers within', 
           adtool$window / 2, 'days before/after baseline time.\n')
  }
  cat('There are entire', n_pat, 'patients and',
      n_var, 'variables included in this dataset.\n\n')
  if(!is.null(vars)) {
    for (i in vars) {
      adt_query_cate(x, i)
      cat('\n')
    }
  }
}

#' Plot for class adtool
#'
#' @param adtool 
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
plot.adtool <- function(adtool, ...) {
  
  
}