#' Function to rename variables in a tibble created by \code{\link{read_hyr}} to
#' enable immediate use with \strong{openair}.
#' 
#' @param df Tibble from \code{\link{read_hyr}}. 
#' 
#' @return Tibble. 
#' 
#' @author Stuart K. Grange
#' 
#' @seealso \code{\link{read_hyr}}
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
