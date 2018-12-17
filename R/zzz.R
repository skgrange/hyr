#' Function to squash R check's global variable notes. 
#' 
if (getRversion() >= "2.15.1") {
  
  # What variables are causing issues?
  variables <- c(
    "date_arrival", "date_trajectory", "hours_offset", "latitude", "longitude"
  )
  
  # Squash the note
  utils::globalVariables(variables)
  
}
