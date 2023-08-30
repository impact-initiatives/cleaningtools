## add_duration
#' Creates a duration variable using the start and end time of the survey
#'
#' @param dataset dataset to add the duration
#' @param duration_column the name of the duration variable that will be created.
#' @param uuid_column uuid column in the dataset. Default is uuid.
#' @param start_column start time of the survey, in KOBO format (e.g "2021-07-14T14:02:24.955+03:00")
#' @param end_column end time of the survey, in KOBO format (e.g "2021-07-14T14:03:28.955+03:00")
#'
#' @return the clean dataset with 6 new columns: duration, start_date, start_time, end_date, end_time, days_diff
#' @export
#'
#' @examples
#' test_data <- data.frame(
#'   start = c(
#'     "2021/07/13 11:25:49", "2021/07/13 12:36:16",
#'     "2021/07/13 10:21:10", "2021/07/13 10:54:07", "2021/07/13 11:18:45"
#'   ),
#'   end = c(
#'     "2021/07/13 12:02:39", "2021/07/13 13:20:17",
#'     "2021/07/13 10:53:42", "2021/07/13 11:28:58", "2021/07/13 11:55:24"
#'   ),
#'   uuid = 1:5
#' )
#'
#' test_data_w_duration <- test_data %>%
#'   add_duration()
#'
#' test_data_kobo_time <- data.frame(
#'   `X.U.FEFF.start` = c(
#'     "2021-07-13T11:25:49.543+03:00",
#'     "2021-07-13T12:36:16.316+03:00",
#'     "2021-07-13T10:21:10.337+03:00",
#'     "2021-07-13T10:54:07.394+03:00",
#'     "2021-07-13T11:18:45.521+03:00"
#'   ),
#'   end = c(
#'     "2021-07-14T12:02:39.269+03:00",
#'     "2021-07-13T13:20:17.815+03:00",
#'     "2021-07-13T10:53:42.662+03:00",
#'     "2021-07-13T11:28:58.295+03:00",
#'     "2021-07-13T11:55:24.366+03:00"
#'   ),
#'   uuid_column = 1:5
#' )
#' test_data_kobo_time_w_duration <- test_data_kobo_time %>%
#'   add_duration(start_column = "X.U.FEFF.start")
add_duration <- function(dataset, duration_column = "duration",
                         uuid_column = "uuid", start_column = "start", end_column = "end") {
  # make the dataset a dataframe
  dataset <- as.data.frame(dataset)

  # if start_column column doesn exist, error. same for end_column [test 1]
  if (!(start_column %in% names(dataset))) {
    stop("data needs to have a column start for this function work")
  }
  if (!(end_column %in% names(dataset))) {
    stop("data needs to have a column end for this function work")
  }

  # if the duration_column column already exist [test 2]
  if (duration_column %in% names(dataset)) {
    stop("There is already a variable called duration_column in your dataset, please input another duration_column")
  }

  # the function only runs if start_column and end_column are in the KOBO format "date T hour"
  if ((all(grepl("T", dataset[[start_column]]) | is.na(dataset[[start_column]])) & all(grepl("T", dataset[[end_column]]) | is.na(dataset[[end_column]]))) |
    all(grepl("^\\d{4}/\\d{2}/\\d{2}\\s\\d{2}:\\d{2}:\\d{2}$", dataset[[start_column]])) & all(grepl("^\\d{4}/\\d{2}/\\d{2}\\s\\d{2}:\\d{2}:\\d{2}$", dataset[[end_column]]))) {
    # split the columns start_column and end_column
    if (all(grepl("^\\d{4}/\\d{2}/\\d{2}\\s\\d{2}:\\d{2}:\\d{2}$", dataset[[start_column]])) & all(grepl("^\\d{4}/\\d{2}/\\d{2}\\s\\d{2}:\\d{2}:\\d{2}$", dataset[[end_column]]))) {
      dataset <- dataset %>%
        tidyr::separate(!!rlang::sym(start_column), c("start_date", "start_time"), " ", remove = F) %>%
        tidyr::separate(!!rlang::sym(end_column), c("end_date", "end_time"), " ", remove = F)
    } else {
      dataset <- dataset %>%
        tidyr::separate(!!rlang::sym(start_column), c("start_date", "start_time"), "T", remove = F) %>%
        tidyr::separate(!!rlang::sym(end_column), c("end_date", "end_time"), "T", remove = F)
    }

    # test 3: warning if there is not a start_column and end_column for each survey
    if (sum(is.na(dataset[[start_column]])) > 0 | sum(is.na(dataset[[end_column]])) > 0) {
      warning("There are some observations for which either start or end is missing. The duration will not be computed for these")
    }

    # compute days diff and create days count
    dataset$days_diff <- difftime(as.POSIXct(dataset$end_date),
      as.POSIXct(dataset$start_date),
      units = "days"
    )
    # round the time
    dataset$start_time <- round(as.difftime(dataset$start_time,
      units = "mins"
    ), 2)
    dataset$end_time <- round(as.difftime(dataset$end_time,
      units = "mins"
    ), 2)
    # compute the duration
    dataset[[duration_column]] <- as.numeric(dataset$end_time -
      dataset$start_time) +
      (as.numeric(dataset$days_diff) * 24 * 60)

    # return in list format
    return(dataset)
  } else {
    stop("The dates are not in the correct format, the duration cannot be computed")
  }
}
