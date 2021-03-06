% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/adt_private.R
\name{a_check_src}
\alias{a_check_src}
\title{Check the source data whether include all the variable needed}
\usage{
a_check_src(table_code, cur_dat, dict_src_tables)
}
\arguments{
\item{table_code}{Code of subtables ("Cognitive" as "COG"), which can be mapped
to the source data files and is used for internal data manipulation. 
The options are "COG" (Cognitive), "DIAG" (Diagnosis), "CSF" (FINAL_CSF), "DEMO"
(Demographics), "HIPPO" (Hippocampus), "AMY" (Amygdala), "EC"
(Entorhinal), "GE" (Genetics), "LIST_A" (list of patients not enrolled),
"LIST_B" (list of patients impaired).}

\item{cur_dat}{A dataset indicating current loaded subtable.}

\item{dict_src_tables}{A dictionary file for the parameters. The
default dictionary is appended in the package. Since the parameters in
tables may vary, it could be modified by using a costomized parameter
dictionary. The format can refer to the appended dictionary.
See \code{\link{dict_src_tables}} for more details.}
}
\value{
A dataset includes missing variables information.
}
\description{
Check the source data whether include all the variable needed
}
\examples{
\dontrun{
a_check_src("DIAG", dict_src_tables, dat_diag)
}

}
