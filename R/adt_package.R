#' The 'ADTool' package.
#'
#' @docType package
#' @name    adt_package
#' @aliases adt
#'
#' @importFrom grDevices colors
#' @importFrom graphics axis box legend lines par plot points text arrows grid rect
#' @importFrom parallel detectCores
#' @importFrom readxl read_xlsx
#' @importFrom gdata read.xls
#' @import dplyr 
#'
#' @importFrom stats approxfun as.formula binomial cov density ecdf glm
#'     integrate optim predict quantile sd var
#'
#' @description
#'
#' @section Background:
#'
#' Sharp increases in Alzheimer's disease (AD) cases, deaths, and costs are
#' stressing the U.S. health care system and caregivers. Without effective
#' interventions, the costs and unpaid care are projected to increase 4-fold by
#' 2050 [1]. The current scientific consensus indicated that the preclinical
#' stage, when patients are still cognitively normal, is the crucial window of
#' opportunity for early interventions before irreversible brain structure
#' changes occur [2, 3]. However, because the preclinical stage is typically
#' asymptomatic, it is important to focus on AD biomarkers, quantify their
#' ordering in time and magnitude of the signal, and estimate their evolution
#' during the preclinical progression of AD.
#'
#' @section Data Sources:
#' 
#' There are several major AD data sources exist which allows researchers to
#' conduct their research.
#' 
#' The BIOCARD study is a longitudinal, observational study initiated in 1995,
#' and designed to identify biomarkers associated with progression from
#' cognitively normal (CN) to mild cognitive impairment (MCI) or dementia. It
#' enrolled 349 cognitively normal individuals who were primarily middle-aged
#' (mean age=57.1) at enrollment. Most participants by design had a first-degree
#' relative with dementia. Clinical assessments and cognitive testing were
#' completed annually; MRI scans, cerebrospinal fluid, and blood specimens were
#' collected approximately every 2 years. The cohort remains being followed
#' currently with an administrative gap between 2005 and 2009. As of May 31,
#' 2018, 80 participants have cognitive symptom onset. Study characteristics are
#' shown in table 1. This study provides rare and valuable information of
#' longitudinal biomarker measurements over 20 years (median follow up 13.7
#' years) and mostly before subjects have cognitive decline – the crucial
#' preclinical time window for understanding the AD biomarker cascade.
#'
#' The ADNI study is a multicenter observation study launched in 2004, to
#' collect clinical, imaging, genetic and biospecimen biomarkers from cohorts of
#' different clinical states at baseline. It could be characterized as a
#' cross-sectional study with longitudinal follow-up in that participants in
#' diagnostic groups were very different from each other at baseline. The
#' initial phase of ADNI (2004, 5 years) enrolled 200 CN elderly, 400 MCI, and
#' 200 AD. This cohort was followed and augmented in three later phases, ADNI-GO
#' (2009, 2 years), ADNI-2 (2011, 5 years) and ADNI-3 (2016, 5 years), with over
#' 1000 additional participants enrolled in existing cohorts as well as in new
#' cohorts of non-MCI but with Significant Memory Concern (SMC), early MCI, and
#' late MCI. We will only use ADNI subjects that are not demented at baseline in
#' our analysis. This study uses a similar protocol for biomarker measurements
#' as the BIOCARD study. It complements the BIOCARD data on a larger cohort and
#' a wider spread of the AD progression continuum beyond the preclinical phase.
#'
#' The NACC UDS data is a collection of data reflecting the total enrollment
#' since 2005 across 34 Alzheimer’s Disease Centers and includes subjects with a
#' range of cognitive status. The majority of these subjects are followed
#' longitudinally with assessments obtained annually. A standard protocol is
#' used to collect clinical information, neuropsychological test results, CSF
#' markers, and MRI markers when available. This protocol will use a sub-cohort
#' of UDS data that included subjects who had at least one MRI evaluation and
#' had volumetric MRI biomarkers calculated, N= 1317 as of May 2018. This cohort
#' mostly does not have CSF or PET imaging information, but provides information
#' on a large population with intensive cognitive and MRI measures. 
#'
#' @section Objectives:
#'
#' In this package, we establish AD data standards and data dictionaries in this
#' package that define the formats and organization structures of the AD data
#' across multiple data sources. R Functions are provided for data analysts to
#' integrate data from multiple data sources and create their analysis dataset.
#' 
#' @references
#'
NULL

#' Dictionary of columns 
#'
#' @name dict_par
#'
#' @docType data
#'
#' @usage data(grav)
#' #'
#' @format A dataframe with the following variables:
#' \itemize{
#'   \item{file_code}{}
#'   \item{file_name}{}
#' }
NULL

#' Dictionary of variables 
#'
#' @name dict_vars
#'
#' @format A dataframe with the following variables:
#' \itemize{
#'   \item{file_code}{}
#'   \item{file_name}{}
#' }
NULL

#' ADNI Dictionary data
#'
#' Dictionary for ADNI files 
#' 
#' @name dict_adni
#'
#' @format A dataframe with the following variables:
#' \itemize{
#'   \item{file_label}{}
#'   \item{file_name}{}
#' }
NULL

