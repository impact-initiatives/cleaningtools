% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/review_cleaning.R
\name{create_cleaning_log}
\alias{create_cleaning_log}
\title{Compares 2 dataset and logs differences}
\usage{
create_cleaning_log(
  raw_dataset,
  raw_dataset_uuid_column = "uuid",
  clean_dataset = clean_dataset,
  clean_dataset_uuid_column = "uuid",
  check_for_deletion_log = T,
  columns_not_to_check = NULL,
  check_for_variable_name = T
)
}
\arguments{
\item{raw_dataset}{Raw dataset}

\item{raw_dataset_uuid_column}{uuid column in the raw dataset. Default is "uuid".}

\item{clean_dataset}{Clean dataset}

\item{clean_dataset_uuid_column}{uuid column in the raw dataset. Default is "uuid".}

\item{check_for_deletion_log}{TRUE to flag the removed survey}

\item{columns_not_to_check}{Columns to exclude from the checks}

\item{check_for_variable_name}{TRUE to flag the removed variables}
}
\value{
Cleaning log
}
\description{
Compares 2 dataset and logs differences
}
\examples{
\dontrun{
create_cleaning_log(
  raw_dataset = cleaningtools::cleaningtools_raw_data, raw_dataset_uuid_column = "X_uuid",
  clean_dataset = cleaningtools::cleaningtools_clean_data, clean_dataset_uuid_column = "X_uuid",
  check_for_deletion_log = TRUE, check_for_variable_name = TRUE
)
}
}
