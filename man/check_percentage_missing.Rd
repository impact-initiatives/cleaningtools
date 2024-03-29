% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/check_percentage_missing.R
\name{check_percentage_missing}
\alias{check_percentage_missing}
\title{Check the percentages of missing value}
\usage{
check_percentage_missing(
  dataset,
  uuid_column = "uuid",
  column_to_check = "percentage_missing",
  strongness_factor = 2,
  log_name = "percentage_missing_log"
)
}
\arguments{
\item{dataset}{a dataset to be check as a dataframe or a list with the
dataframe stored as "checked_dataset".}

\item{uuid_column}{uuid column in the dataset. Default is "uuid".}

\item{column_to_check}{string character with the name of the columns to check. Default is
"percentage_missing"}

\item{strongness_factor}{Strongness factor define how strong your outliers will be. The default is 3.}

\item{log_name}{name of the log of flagged value, default is percentage_missing_log}
}
\value{
return a list with the dataset checked stored as checked_dataset and
a dataframe with the flagged values log
}
\description{
The function will flag if a survey for its missing values. The missing values column can be created
with add_percentage_missing and the values are flagged with check_outliers.
}
\examples{
# Adding the percentage missing first
data_example <- data.frame(
  uuid = letters[1:3],
  col_1 = c(1:3),
  col_2 = c(NA, NA, "expenditures"),
  col_3 = c("with need", NA, "with need"),
  col_4 = c("food health school", NA, "food"),
  col_4.food = c(1, NA, 1),
  col_4.health = c(1, NA, 0),
  col_4.school = c(1, NA, 0)
)
data_example <- data_example \%>\%
  add_percentage_missing()
data_example \%>\% check_percentage_missing()

# With a dataset that already has a percentage missing
data_example2 <- data.frame(
  uuid = letters,
  any_cols = LETTERS,
  any_number = 1:26,
  percentage_missing = c(rep(.05, 25), .99)
)
data_example2 \%>\% check_percentage_missing()

}
\seealso{
\code{\link[=add_percentage_missing]{add_percentage_missing()}}, \code{\link[=check_outliers]{check_outliers()}}
}
