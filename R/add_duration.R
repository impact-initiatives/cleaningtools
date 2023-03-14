##add_duration
#' Creates a duration variable using the start and end time of the survey
#'
#' @param .dataset the clean dataframe
#' @param duration_var_name the name of the duration variable that will be created.
#' @param uuid the unique identifier of the survey
#' @param start start time of the survey, in KOBO format (e.g "2021-07-14T14:02:24.955+03:00")
#' @param end end time of the survey, in KOBO format (e.g "2021-07-14T14:03:28.955+03:00")
#'
#' @return the clean dataset with 6 new columns: duration, start_date, start_time, end_date, end_time, days_diff
#' @export
#'
#' @examples

add_duration <- function(.dataset, duration_var_name="duration",
                         uuid="_uuid", start="start", end="end") {

  #make the dataset a dataframe
  .dataset <- as.data.frame(.dataset)

  # if start column doesn exist, error. same for end [test 1]
  if (!(start %in% names(.dataset)))
    stop("data needs to have a column start for this function work")
  if (!(end %in% names(.dataset)))
    stop("data needs to have a column end for this function work")

  #if the duration_var_name column already exist [test 2]
  if (duration_var_name %in% names(.dataset))
    stop("There is already a variable called duration_var_name in your dataset, please input another duration_var_name")

  #the function only runs if start and end are in the KOBO format "date T hour"
  if(all(grepl("T", .dataset[[start]]) | is.na(.dataset[[start]])) & all(grepl("T", .dataset[[end]]) | is.na(.dataset[[end]])))
    {
  #split the columns start and end
  .dataset <- .dataset %>%
    tidyr::separate(!!sym(start), c("start_date","start_time"), "T", remove=F) %>%
    tidyr::separate(!!sym(end), c("end_date", "end_time"), "T", remove=F)

  #test 3: warning if there is not a start and end for each survey
  if(sum(is.na(clean_data[[start]]))>0 | sum(is.na(clean_data[[end]]))>0){
     warning("There are some observations for which either start or end is missing. The duration will not be computed for these")
  }

  #compute days diff and create days count
  .dataset$days_diff <- difftime(as.POSIXct(.dataset$end_date),
                                         as.POSIXct(.dataset$start_date), units = "days")
  #round the time
  .dataset$start_time <- round(as.difftime(.dataset$start_time,
                                            units = "mins"), 2)
  .dataset$end_time <- round(as.difftime(.dataset$end_time,
                                          units = "mins"), 2)
  #compute the duration
  .dataset[[duration_var_name]] <- as.numeric(.dataset$end_time -
                                        .dataset$start_time)+
                        (as.numeric(.dataset$days_diff)*24*60)

  #return in list format
  return(.dataset)

  }
  else {
    stop("The dates are not in the correct format, the duration cannot be computed")
  }
}


#for now only works with dates in Kobo format (day T hour). to change



