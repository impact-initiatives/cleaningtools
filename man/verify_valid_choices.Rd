% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{verify_valid_choices}
\alias{verify_valid_choices}
\title{Verify if the Kobo choices dataframe is valid}
\usage{
verify_valid_choices(kobo_choices)
}
\arguments{
\item{kobo_choices}{A dataframe representing the Kobo choices sheet.}
}
\value{
Logical. Returns \code{TRUE} if the dataframe meets all the criteria. Otherwise, it returns \code{FALSE}.
}
\description{
This function checks whether the provided Kobo choices dataframe meets certain criteria:
\enumerate{
\item It is a dataframe.
\item It is not empty.
\item It contains columns named "list_name" and "name".
}
}
\examples{
# Assume df_valid_choices is a valid Kobo choices dataframe
df_valid_choices <- data.frame(list_name = c("ChoiceA", "ChoiceB"), name = c("Option1", "Option2"))
verify_valid_choices(df_valid_choices) # should return TRUE

# Assume df_invalid_choices lacks the required columns
df_invalid_choices <- data.frame(column1 = c("ChoiceA", "ChoiceB"),
                                 column2 = c("Option1", "Option2"))
verify_valid_choices(df_invalid_choices) # should return FALSE
}
