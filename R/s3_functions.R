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
  
  cat('This analysis dataset is created by merging data files from the', 
      toupper(dat$dat_ty), 'study based on subjectid in the "long" format.', 
      '\nBiomarkers measured at different time are grouped into visits, \nusing', 
      dat$merge_by, 'measurement time as the reference for each visit time.\n')
  
  if(dat$overlap) {
    cat('Biomarkers measured prior or after', 
        dat$window / 2, 'days are grouped into the same visit.\n', 
        'Note, it is possible for the same biomarker measurements to be grouped into two visits.')
  }
  else {
    cat('The time window are set by the midpoints of the previous-current and current-next measurements time.', 
        '\nBiomarkers measured within this window are grouped into the same visit.\n')
    cat('If the length of windows are longer than', 
        dat$window / 2, 'days,\nthen force to select biomarkers within', 
        dat$window / 2, 'days prior or after the measurement time.\n')
  }
  cat('There are entire', n_pat, 'patients and',
      n_var, 'variables included in this dataset.\n\n')
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
#' @param dat 
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
plot.ad_ana_data <- function(dat, 
                             distn = c('age', 'visit', 'year'), 
                             stack_by = c('diagnosis', 'sex'), 
                             ...) {
  x <- dat$ana_dt
  #--------------- only first visit ------------------
  pat <- x %>% 
    select(subject_id, date_cog, startyear, birthyear, sex, diagnosis) %>% 
    mutate(sex = recode(sex, '1'='Male','2'='Female')) %>% 
    mutate(age =  startyear - birthyear) %>% 
    mutate(age = case_when(age <  10            ~ 'under 10', 
                                age >= 10 & age < 20 ~ '10-19', 
                                age >= 20 & age < 30 ~ '20-29', 
                                age >= 30 & age < 40 ~ '30-39', 
                                age >= 40 & age < 50 ~ '40-49', 
                                age >= 50 & age < 60 ~ '50-59', 
                                age >= 60 & age < 70 ~ '60-69', 
                                age >= 70 & age < 80 ~ '70-79', 
                                age >= 80 & age < 90 ~ '80-89', 
                                age >= 90            ~ 'above 89')) %>% 
    group_by(subject_id) %>% 
    mutate(visit = row_number()) %>% 
    mutate(year = if_else(as.numeric(format(date_cog, '%Y')) - startyear < 0, 
                          0, as.numeric(format(date_cog, '%Y')) - startyear)) %>% 
    ungroup()
  
  if (distn == 'age') {
    pat_sub <- pat %>% 
      distinct(subject_id, .keep_all = TRUE) %>% 
      select(sex, diagnosis, age)
  }  
  else if (distn == 'visit') {
    pat_sub <- pat %>% select(sex, diagnosis, visit)
  }
  else if (distn == 'year') {
    pat_sub <- pat %>% 
      group_by(subject_id) %>% 
      distinct(year, .keep_all = TRUE) %>% 
      ungroup() %>% 
      select(sex, diagnosis, year)
  }
  
  tbl <- a_gen_tbl(pat_sub, stack_by, distn)
  p <- ggplot(pat_sub) + 
    theme_bw() + 
    geom_bar(aes(x = !!as.name(distn), color = !!as.name(stack_by), fill = !!as.name(stack_by)), 
             position = 'stack', alpha = 0.9) + 
    labs(x = distn, y = 'Number of Subjects', title = 'Participant Distribution') + 
    theme(plot.title = element_text(size = 12, face = 'bold', hjust = 0.5))
  
  grid.arrange(tableGrob(tbl), p, 
               ncol = 2, as.table = TRUE, widths = c(1, 1.2))
}