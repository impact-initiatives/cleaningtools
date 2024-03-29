% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/check_audits.R
\name{create_duration_from_audit_with_start_end}
\alias{create_duration_from_audit_with_start_end}
\title{Calculate duration from audit between 2 questions}
\usage{
create_duration_from_audit_with_start_end(
  audit_file,
  start_question,
  end_question
)
}
\arguments{
\item{audit_file}{a dataframe with a single audit file,
it needs start, end, node column}

\item{start_question}{character vector use for the starting question}

\item{end_question}{character vector use for the ending question}
}
\value{
A dataframe with the duration in ms and duration in minutes.
}
\description{
The function will calculate time difference between the start of the
start_question and the end of the end_question.
}
\details{
In case the node appear several time (if the value was changed or with a
select multiple) it will take the minimum for the start and the maximum for
the end.

If a value is missing (skip logic or question not found), it will return -Inf
}
\examples{
some_audit <- data.frame(
  event = c("form start", rep("question", 5)),
  node = c("", paste0("/xx/question", 1:5)),
  start = c(
    1661415887295, 1661415887301, 1661415890819,
    1661415892297, 1661415893529, 1661415894720
  ),
  end = c(
    NA, 1661415890790, 1661415892273,
    1661415893506, 1661415894703, 1661415896452
  )
)
create_duration_from_audit_with_start_end(some_audit, "question1", "question3")

}
