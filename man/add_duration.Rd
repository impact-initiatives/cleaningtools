% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/add_duration.R
\name{add_duration}
\alias{add_duration}
\title{Creates a duration variable using the start and end time of the survey}
\usage{
add_duration(
  dataset,
  duration_column = "duration",
  uuid_column = "uuid",
  start_column = "start",
  end_column = "end"
)
}
\arguments{
\item{dataset}{dataset to add the duration}

\item{duration_column}{the name of the duration variable that will be created.}

\item{uuid_column}{uuid column in the dataset. Default is uuid.}

\item{start_column}{start time of the survey, in KOBO format (e.g "2021-07-14T14:02:24.955+03:00")}

\item{end_column}{end time of the survey, in KOBO format (e.g "2021-07-14T14:03:28.955+03:00")}
}
\value{
the clean dataset with 6 new columns: duration, start_date, start_time, end_date, end_time, days_diff
}
\description{
Creates a duration variable using the start and end time of the survey
}
\examples{
test_data <- data.frame(
  start = c(
    "2021/07/13 11:25:49", "2021/07/13 12:36:16",
    "2021/07/13 10:21:10", "2021/07/13 10:54:07", "2021/07/13 11:18:45"
  ),
  end = c(
    "2021/07/13 12:02:39", "2021/07/13 13:20:17",
    "2021/07/13 10:53:42", "2021/07/13 11:28:58", "2021/07/13 11:55:24"
  ),
  uuid = 1:5
)

test_data_w_duration <- test_data \%>\%
  add_duration()

test_data_kobo_time <- data.frame(
  `X.U.FEFF.start` = c(
    "2021-07-13T11:25:49.543+03:00",
    "2021-07-13T12:36:16.316+03:00",
    "2021-07-13T10:21:10.337+03:00",
    "2021-07-13T10:54:07.394+03:00",
    "2021-07-13T11:18:45.521+03:00"
  ),
  end = c(
    "2021-07-14T12:02:39.269+03:00",
    "2021-07-13T13:20:17.815+03:00",
    "2021-07-13T10:53:42.662+03:00",
    "2021-07-13T11:28:58.295+03:00",
    "2021-07-13T11:55:24.366+03:00"
  ),
  uuid_column = 1:5
)
test_data_kobo_time_w_duration <- test_data_kobo_time \%>\%
  add_duration(start_column = "X.U.FEFF.start")
}
