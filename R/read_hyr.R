#' Function to load HYSPLIT data files produced with \code{\link{hyr_run}}. 
#' 
#' @param file Vector of file names produced by \code{\link{hyr_run}}. 
#' 
#' @param verbose Should the function give messages? 
#' 
#' @param progress Should a progress bar be displayed? 
#' 
#' @return Tibble. 
#' 
#' @author Stuart K. Grange
#' 
#' @seealso \code{\link{hyr_run}}
#' 
#' @examples 
#' 
#' \dontrun{
#' 
#' # Get file list
#' list_files <- list.files("~/Desktop/hysplit_outputs", full.names = TRUE)
#' 
#' # Load files
#' data_hysplit <- read_hyr(list_files)
#' 
#' # Or load files and rename variables for use in openair
#' data_hysplit_openair <- read_hyr(list_files) %>% 
#'   hyr_rename_for_openair()
#' 
#' }
#' 
#' @export
read_hyr <- function(file, verbose = FALSE, progress = FALSE) {
  
  file %>% 
    purrr::map(read_hyr_worker, verbose = verbose, .progress = progress) %>% 
    purrr::list_rbind()
  
}


read_hyr_worker <- function(file, verbose) {
  
  if (verbose) {
    cli::cli_alert_info("{cli_date()} {.path {file}}...")
  }
  
  # Read as text
  text <- readr::read_lines(file, progress = FALSE)
  
  # For empty files
  if (length(text) <= 7) {
    return(tibble())
  }
  
  # Where does the header end? 
  to_skip <- stringr::str_which(text, "PRESSURE")
  
  # Read and drop useless variables
  df <- readr::read_table(
    text, 
    skip = to_skip, 
    col_names = FALSE, 
    show_col_types = FALSE
  ) %>% 
    select(-c(1, 2, 7, 8)) %>% 
    purrr::set_names(
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
    relocate(date_arrival,
             date_trajectory) %>% 
    select(-year,
           -month,
           -day,
           -hour)
  
  return(df)
  
}
