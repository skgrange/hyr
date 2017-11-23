# From threadr
# No export
parse_date_arguments <- function(date, type, tz = "UTC") {
  if (lubridate::is.Date(date) || lubridate::is.POSIXt(date)) 
    date <- as.character(date)
  if (is.na(date)) {
    date <- lubridate::ymd(Sys.Date(), tz = tz)
  }
  else {
    date_system <- lubridate::ymd(Sys.Date(), tz = tz)
    if (type == "start") {
      if (stringr::str_count(date) == 4) 
        date <- stringr::str_c(date, "-01-01")
      date <- ifelse(is.na(date), as.character(lubridate::floor_date(date_system, 
                                                                     "year")), date)
    }
    if (type == "end") {
      if (stringr::str_count(date) == 4) 
        date <- stringr::str_c(date, "-12-31")
      date <- ifelse(is.na(date), as.character(lubridate::ceiling_date(date_system, 
                                                                       "year")), date)
    }
    date <- lubridate::parse_date_time(date, c("ymd", "dmy"), 
                                       tz = tz)
  }
  return(date)
}