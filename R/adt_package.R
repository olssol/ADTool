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
#' @importFrom plyr revalue
#' @import dplyr
#' @import tidyr
#' @import xlsx
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

#' Dictionary of Column Names
#'
#' This dictionary maps the original column names (old_col_name) to the column
#' names of generated analysis dataset. In the raw data, the same variable may
#' have different names. This dictionary could be used to uniform variable
#' names. Also, as some column names in the raw data may be complicated
#' (including space, unit, special symbols, etc.), this dictionary is used to
#' simplify the column names.
#'
#' @name dict_src_tables
#'
#' @docType data
#'
#' @usage data(dict_src_tables)
#'
#' @format A dataframe with the following variables (all variables are in string
#'     type):
#'
#' \itemize{
#' 
#' \item{index}{The index for the dictionary}
#'
#' \item{adt_col_name}{The  internal column names used in merging
#'     datesets.}
#'
#' \item{src_col_name}{The source column names used in the analysis
#'     dateset.}
#'
#' \item{src_type}{The source of the data, such as "BIOCARD."}
#' 
#' \item{adt_table_code}{The code of subtable. The options are "COG"
#'     (Cognitive), "DIAG" (Diagnosis), "CSF" (FINAL_CSF), "DEMO"
#'     (Demographics), "HIPPO" (Hippocampus), "AMY" (Amygdala), "EC"
#'     (Entorhinal), "GE" (Genetics), "LIST_A" (list of patients not enrolled),
#'     "LIST_B" (list of patients impaired). }
#'
NULL

#' Dictionary of Source files Features
#'
#' This dictionary includes the basic setting of subtables, such as "file_code",
#' "file_name", "start_row", and "date_format." In the raw data, the starting
#' row and date type may vary for different subtables. This dictionary could be
#' used to store these settings. Also, since the subtables will have different
#' names (maybe with time as a suffix), this dictionary is used to simplify the
#' subtable names.
#'
#' @name dict_src_files
#'
#' @format A dataframe with the following variables:
#'
#' \itemize{
#'
#' \item{index}{The index for the dictionary}
#' 
#' \item{src_type}{The source of the data, such as "BIOCARD."} 
#'
#' \item{adt_table_code}{The code of subtable. The options are "COG" (Cognitive),
#'     "DIAG" (Diagnosis), "CSF" (FINAL_CSF), "DEMO" (Demographics), "HIPPO"
#'     (Hippocampus), "AMY" (Amygdala), "EC" (Entorhinal), "GE" (Genetics),
#'     "LIST_A" (list of patients not enrolled), "LIST_B" (list of patients
#'     impaired). }
#'
#' \item{src_key_words}{The identifiable keywords in the subtable names.}
#'
#' \item{src_start_row}{Integers indicating the starting row when reading
#'     subtables. The default value is 1.}
#'
#' \item{src_date_format}{The format of the date in subtables. The default is
#'     "%Y-%m-%d."} 
#'
#' }
NULL

#' Dictionary of Data
#'
#' This dictionary includes all the variables names in the analysis dataset.
#' This dictionary could be used to check the location and discription of each
#' variable.
#'
#' @name dict_data
#'
#' @format A dataframe with the following variables:
#' \itemize{
#'
#' \item{Column_Name}{The name of variables in the analysis dataset.}
#'
#' \item{Location}{The location where this variable from.}
#'
#' \item{Description}{A description introduces the data type, range, meaning,
#' and mapping (if use one hot encoding).} }
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


#' Parameters
#'
#' List of function parameters in the \code{ADTool} package
#'
#' @name parameters
#' 
#' @param path Directory of the BIOCARD data files. Default value is the working
#'     directory,
#'
#' @param merge_by A character string indicating the source baseline time for
#'     aligning the BIOCARD data when merging multiple files. Options include
#'     "diagnosis", "cognitive", "csf", "hippocampus", "amydata" and
#'     "entorhinal".
#'
#' @param window An integer (unit of days) indicating the maximum acceptable gap
#'     time for merging a biomarker test to base data. Default is 730 days. For
#'     most data, the time windows are calculated from the baseline time (left
#'     window = the midpoint between current and the previous time point; right
#'     window = the midpoint between current and the next time point). If the
#'     time windows are longer than the maximum acceptable length, then force to
#'     select biomarkers within the maximum window length we set. For the first
#'     and last baseline time, since there is no "previous" or "next" time point
#'     available, use the maximum acceptable window length instead.
#'
#' @param window_overlap A logical value indicating the time window setting.
#'     Default "False." If true, all time windows will set from 1/2 "window"
#'     days before the baseline time to the 1/2 "window" days after baseline. In
#'     this case, the time windows may overlap, which means some biomarkers may
#'     be merged into multiple baseline data. If false, the windows will be
#'     calculated from the baseline times. The left window is set to be the
#'     midpoint between the current and the previous time point. The right
#'     window is set to be the midpoint between the current and the next time
#'     point. For the first and last baseline time, since there is no "previous"
#'     or "next" time point available, use the maximum acceptable window length
#'     (set by the parameter "window") instead.
#'
#' @param pattern A string indicating the pattern of all the data files. Default
#'     is "*.xls" (should work for both .xls and .xlsx). This pattern is used to
#'     read all table names from the path.
#'
#' @param src_files Updated dictionary file for source file features. See
#'     \code{\link{dict_src_files}} for more details.
#'
#' @param src_tables Updated dictionary file for source table features. See
#'     \code{\link{dict_src_tables}} for more details.
#'
#' @param vec_name A character string indicating the vector needed to be removed special characters.
#' 
#' @param lower_case A logical value indicating the lower case setting.
#'     Default "False." If true, all characters will to convert to lower cases. 
#'     
#' @param src_type A string indicating the source of data. Options include
#'     "BIOCARD", "NACC" and "ADNI"
#' 
#' @param col_name A string indicating a column name in the source data file.
#' 
#' @param dict_src_tables A dictionary file for the parameters. The
#'     default dictionary is appended in the package. Since the parameters in
#'     tables may vary, it could be modified by using a costomized parameter
#'     dictionary. The format can refer to the appended dictionary.
#'     See \code{\link{dict_src_tables}} for more details.
#'     
#' @param table_code Code of subtables ("Cognitive" as "COG"), which can be mapped
#'     to the source data files and is used for internal data manipulation. 
#'     The options are "COG" (Cognitive), "DIAG" (Diagnosis), "CSF" (FINAL_CSF), "DEMO"
#'     (Demographics), "HIPPO" (Hippocampus), "AMY" (Amygdala), "EC"
#'     (Entorhinal), "GE" (Genetics), "LIST_A" (list of patients not enrolled),
#'     "LIST_B" (list of patients impaired).
#'     
#' @param file_names List the names of all subtables in the path (all tables
#'     should from the same path).
#'
#' @param dict_src_files A path of data file (.csv) of the parameters dictionary. The
#'     default dictionary is appended in the package. Since the parameters in
#'     tables may vary, it could be modified by using a costomized parameter
#'     dictionary. The format can refer to the appended dictionary.
#'     See \code{\link{dict_src_files}} for more details.
#'
#' @param dat The baseline time dataset.
#' 
#' @param v_date A string indicating the date name of the baseline time.
#' 
#' @param v_id A string indicating the id name of baseline dateset. Default is
#'     "subject_id" (may varied if using customized column names dictionary).
#'     
#' @param dat_se     A dataset that will be merged.
#' 
#' @param dat_marker A data set including new biomarker that will be merged.
#' 
#' @param m_date     A string indicating the name of measure date in dat_marker.
#' 
#' @param duplist    A list of duplicated columns names.
#' 
#' @param rst_dict A dataset that waiting to be updated.
#' 
#' @param csv_dict A dataset with updated information.
#' 
#' @param cur_dat A dataset indicating current loaded subtable.
#' 
#' @param data A dataset. Possible choices are "dict_tbl": the dictionary for
#'     the structure of tables, "dict_cil_name": the dictionary maps the
#'     original column names (old_col_name) to the column names of generated
#'     analysis dataset, "dict_data": This dictionary includes all the variables
#'     names in the analysis dataset. This dictionary could be used to check the
#'     location and description of each variable.
#'     
#' @param str String to search. Default value is NULL. If NULL, this function is
#'     used to check the entire dictionary. If input a string (the variable name
#'     of the analysis dataset), this function is used to check the location and
#'     description of the variable.
#'     
#' @param csv_fname A string indicating the CSV filename to be saved (saved to
#'     the default location). Default is NULL. The saved file is in .scv format.
#'     If changes of default dictionaries needed, then the costomized dictionary
#'     could be added here. However, the format of the costomized dictionary 
#'     should follow the default one.
#'     
#' @param dict A string indicating the name of the dictionary. The available
#'     options are: "src_files": the dictionary for table structure, "src_tables": the
#'     dictionary for mapping column names, "ana_data": the dictionary for query, "adni": the dictionary for ADNI
#'     data.
#'     
#' @param apoecode A string indicating the APOE. The available
#'     options are: 3.4, 4.4, 2.4
#' 
#' @param par_apoe A list indicating the map for apoecode. Default
#'     value is list(levels = c(3.4, 4.4, 2.4),labels = c(1, 2, NA))).
#'
#'
NULL
