#' Function to load HYSPLIT data files produced with \code{\link{hyr_run}}. 
#' 
#' @param file Vector of file names. 
#' 
#' @param verbose Should the function give messages? 
#' 
#' @return Tibble. 
#' 
#' @author Stuart K. Grange
#' 
#' @export
read_hyr <- function(file, verbose = FALSE) {
  purrr::map_dfr(file, read_hyr_worker, verbose = verbose)
}


read_hyr_worker <- function(file, verbose) {
  
  if (verbose) message(date_message(), file, "...")
  
  # Read as text
  text <- readr::read_lines(file)
  
  # Empty files
  if (length(text) <= 7) return(tibble())
  
  # Where does the header end? 
  to_skip <- grep("PRESSURE", text)
  
  # Read and drop useless variables
  df <- readr::read_table(
    text, 
    skip = to_skip, 
    col_names = FALSE, 
    col_types = readr::cols()
  ) %>% 
    select(-c(1, 2, 7, 8)) %>% 
    setNames(
      c(
        "year", "month", "day", "hour", "hours_offset", "latitude", "longitude", 
        "height", "pressure"
      )
    )
  
  # Clean up dates and format return
  df <- df %>% 
    mutate(year = if_else(year < 50, year + 2000, year + 1900),
           date_trajectory = stringr::str_c(year, month, day, hour, sep = " "),
           date_trajectory = lubridate::ymd_h(date_trajectory, tz = "UTC"),
           date_arrival = date_trajectory - lubridate::hours(hours_offset)) %>% 
    select(date_arrival,
           date_trajectory,
           everything()) %>% 
    select(-year,
           -month,
           -day,
           -hour)
  
  return(df)
  
}
