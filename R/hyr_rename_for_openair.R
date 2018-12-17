#' Function to rename variables in a data frame containing trajectory data for
#' use in \strong{openair}. 
#' 
#' @param df Data frame from \code{\link{read_hyr}}. 
#' 
#' @return Tibble. 
#' 
#' @author Stuart K. Grange
#' 
#' @export
hyr_rename_for_openair <- function(df) {
  
  # Simple rename
  df %>% 
    rename(date = date_arrival,
           date2 = date_trajectory,
           hour.inc = hours_offset,
           lat = latitude,
           lon = longitude)
  
}
