#' Summary for class ad_ana_data
#' 
#' @inheritParams parameters
#' @return The summary of this class
#' @export
#'
summary.ad_ana_data <- function(dat, vars = NULL, ...) {
  x <- dat$ana_dt
  pat <- x %>% distinct(subject_id, .keep_all = TRUE)
  n_pat <- dim(pat)[1]
  n_var <- dim(pat)[2]
  
  cat('Data source:',               toupper(dat$dat_ty), 
      '\nNumber of participants:', n_pat, 
      '\nMerged by:',               'subject_id', 
      '\nVisit time reference:',    dat$merge_by, 'date', 
      '\nWindow length: (-/+)',     dat$window/2, 'days', 
      '\nOverlap:',                 dat$overlap)
  
  if(!is.null(vars)) {
    for (i in vars) {
      info <- adt_query_cate(x, i)
      cat(info[[1]]$adt_col_name, ':', info[[1]]$description, 
          '\nvalues:', info[[1]]$values, '\nstatistics:\n')
      print(info[[2]])
      cat('\n')
    }
  }
}

#' Plot for class ad_ana_data
#'
#' @inheritParams parameters
#'
#' @return
#' @export
#'
#' @examples
plot.ad_ana_data <- function(dat, 
                             distn, # extend ...
                             group, 
                             baseline = TRUE, 
                             ...) {
  x <- dat$ana_dt
  #--------------- only first visit ------------------
  pat <- x %>% 
    select(subject_id, distn, group)

  if (baseline) {
    pat <- pat %>% 
      distinct(subject_id, .keep_all = TRUE)
  }
  
  tbl <- a_gen_tbl(pat, group, distn)
  p <- ggplot(pat) + 
    theme_bw() + 
    geom_bar(aes(x = !!as.name(distn), color = !!as.name(group), fill = !!as.name(group)), 
             position = 'stack', alpha = 0.9) + 
    labs(x = distn, y = 'Number of Subjects', title = 'Participant Distribution') + 
    theme(plot.title = element_text(size = 12, face = 'bold', hjust = 0.5))
  
  grid.arrange(tableGrob(tbl), p, 
               ncol = 2, as.table = TRUE, widths = c(1, 1.2))
}