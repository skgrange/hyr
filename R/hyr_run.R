#' Function to command HYSPLIT (Hybrid Single Particle Lagrangian Integrated 
#' Trajectory Model) from R. 
#' 
#' \code{run_hysplit} will return a data frame which is ready for use in 
#' \strong{openair}'s \code{traj*} functions. 
#' 
#' @param df Data 
#' 
#' @param directory_exec Location of HYSPLIT's executable files. Ensure executable
#' permisions have been set for the \code{hyts_std} application if on a Unix 
#' system. HYSPLIT's \code{bdyfiles} directory with the \code{ASCDATA.CFG} will
#' also need to be in the correct location. 
#' 
#' @param directory_input Location of input meteorological files for HYSPLIT. 
#' 
#' @param directory_output Location of where HYSPLIT should write its trajectory 
#' outputs while working. 
#' 
#' @param verbose Should the function give messages on what trajectory is being
#' processed. 
#' 
#' @seealso \code{\link[openair]{importTraj}}, \code{\link[openair]{trajPlot}}, 
#' \href{http://ready.arl.noaa.gov/HYSPLIT.php}{HYSPLIT home}
#' 
#' @author Stuart K. Grange
#' 
#' @export
hyr_run <- function(df, directory_exec = "exec/", directory_input, 
                    directory_output, verbose = TRUE) {
  
  # Parse arguments
  # Expand paths
  directory_exec <- path.expand(directory_exec)
  directory_input <- path.expand(directory_input)
  directory_output <- path.expand(directory_output)
  
  # Add final path separator
  directory_exec <- str_c(directory_exec, .Platform$file.sep)
  directory_input <- str_c(directory_input, .Platform$file.sep)
  directory_output <- str_c(directory_output, .Platform$file.sep)
  
  # Check if source directory exists, often not because of external drive use
  if (!dir.exists(directory_input)) 
    stop("`directory_input` does not exist.", call. = FALSE)
  
  # Check for
  if (!dir.exists(directory_output)) {
    
    message(
      date_message(), 
      directory_output, 
      " does not exist, create it?"
    )
    
    if (menu(choices = c("Yes", "No")) == 1) {
      
      dir.create(directory_output, showWarnings = FALSE, recursive = TRUE)
      message(date_message(), directory_output, " created...")
      
    }
    
  }
  
  # # Dates
  # # Start and end dates
  # df$start <- parse_date_arguments(df$start, "start")
  # df$end <- parse_date_arguments(df$end, "end")
  
  # Receptor location and starting height
  coordinates <- str_c(df$latitude, df$longitude, df$start_height, sep = " ")
  
  # Set up where the control file is to be written
  control_file <- file.path(directory_exec, "CONTROL")
  
  # Store working directory because this will be changed
  directory_current_working <- getwd()
  
  # Create date sequence
  if (df$interval == "3 hour") df$end <- df$end + hours(21)
  date_sequence <- seq(df$start, df$end, by = df$interval)
  
  # Apply function which runs the model multiple times
  if (verbose) {
    
    message(
      date_message(),
      "Running ", 
      length(date_sequence), 
      " HYSPLIT trajectories..."
    )
    
  }
  
  # Run model
  purrr::walk(
    date_sequence, 
    hyr_run_worker, 
    directory_exec = directory_exec, 
    directory_input = directory_input, 
    directory_output = directory_output, 
    control_file = control_file, 
    coordinates = coordinates, 
    runtime = df$runtime, 
    model_height = df$model_height, 
    verbose = verbose
  )
  
  # Change working directory back to original after system calls
  setwd(directory_current_working)
  
  # No return
  
}


# Define the function which creates a control file and calls the hy_std 
# application. 
# 
# No export
hyr_run_worker <- function(date, directory_exec, directory_input, 
                           directory_output, control_file, coordinates, runtime, 
                           model_height, verbose) {
  
  # Get pieces of the date
  date_year <- year(date)
  date_month <- month(date)
  date_day <- day(date)
  date_hour <- hour(date)
  
  # Pad zeros
  date_month <- str_pad(date_month, width = 2, pad = "0")
  date_day <- str_pad(date_day, width = 2, pad = "0")
  date_hour <- str_pad(date_hour, width = 2, pad = "0")
  
  # Format for control file
  date_control <- str_c(date_year, date_month, date_day, date_hour, sep = " ")
  
  # Use date to create a file name
  file_name_export <- str_c(
    str_replace_all(date_control, " ", ""), 
    "_hyr_output.txt"
  )
  
  # Get date string
  current_month_pattern <- str_c(date_year, date_month)
  past_month_pattern <- get_year_and_month(current_month_pattern, - 1)
  future_month_pattern <- get_year_and_month(current_month_pattern, 1)
  
  # Add other pieces of the file names
  past_month_pattern <- str_c("RP", past_month_pattern , ".gbl")
  current_month_pattern <- str_c("RP", current_month_pattern , ".gbl")
  future_month_pattern <- str_c("RP", future_month_pattern , ".gbl")
  
  # Create the file and directory list for the control file
  # Do not use future month here, to-do enhance later
  file_list_input <- c(
    past_month_pattern, 
    current_month_pattern, 
    future_month_pattern
  )
  
  # # For back trajectories, do not use the future month
  # # Also needed for final run of the month
  # if (runtime < 0) {
  # 
  #   # But only if not the final day of the month, otherwise a stop error occurs
  #   if (!floor_date(date, "day") == ceiling_date(date, "month", change_on_boundary = FALSE))
  #     file_list_input <- file_list_input[1:2]
  # 
  # }
  
  # Add directory to file names
  file_list_input <- str_c(directory_input, file_list_input, sep = "\n")
  
  # Write control file
  # This will replace the contents of the current file if it exists
  write_to_control_file(control_file, date_control, append = FALSE)
  
  # Starting locations
  write_to_control_file(control_file, "1")
  
  # Write coordinates and starting height of model
  write_to_control_file(control_file, coordinates)
  
  # Write runtime of model, hours forward or backwards for trajectories
  write_to_control_file(control_file, runtime)
  
  # Vertical motion option, top of model, and input grids (number of files)
  write_to_control_file(
    control_file,
    str_c("0\n", model_height, "\n", length(file_list_input))
  )
  
  # Write input directory and file names
  write_to_control_file(control_file, file_list_input)
  
  # Output directory
  write_to_control_file(control_file, directory_output)
  
  # Output file
  write_to_control_file(control_file, file_name_export)
  
  # cat("\n", readLines(control_file))
  
  # Change working directory to hysplit application
  setwd(directory_exec)
  
  # Message file name
  if (verbose) message(date_message(), file_name_export, "...")
  
  # System call to run hysplit
  processx::run("./hyts_std", spinner = TRUE)
  
  # No return
  
}


# Three files are needed for the model to create a back trajectory,
# the current month, previous month, and future month
get_year_and_month <-  function(pattern, difference = 1) {
  
  # Do some date things
  date <- ymd(str_c(pattern, "01")) + months(difference)
  year <- year(date)
  month <- month(date)
  month <- str_pad(month, 2, pad = "0")
  
  # Combine
  pattern <- str_c(year, month)
  
  return(pattern)
  
}


# Write table function
write_to_control_file <- function(file, x, append = TRUE) {
  
  # Write to file
  write.table(
    x, 
    file, 
    col.names = FALSE, 
    row.names = FALSE, 
    quote = FALSE, 
    append = append
  )
  
}


# # Function to read hysplit files
# # 
# # Taken from openair manual to keep bound trajectory files in the same format
# # as the openair package. Have changed tz to UTC rather than GMT though. 
# # 
# # No export
# read_hysplit_file <- function(file, drop) {
#   
#   # Load file, error catching is for when two or three input met files are used
#   # and results in a different length file header
#   df <- tryCatch({
#     
#     read.table(file, header = FALSE, skip = 6)
#     
#   }, error = function(e) {
#     
#     read.table(file, header = FALSE, skip = 7)
#     
#   })
#   
#   # Drop
#   df <- subset(df, select = -c(V2, V7, V8))
#   
#   # Rename
#   df <- plyr::rename(df, c(V1 = "receptor", V3 = "year", V4 = "month", V5 = "day",
#                            V6 = "hour", V9 = "hour.inc", V10 = "lat", V11 = "lon",
#                            V12 = "height", V13 = "pressure"))
#   
#   # Clean two digit years
#   df$year <- ifelse(df$year < 50, df$year + 2000, df$year + 1900)
#   
#   # Transform pieces of date to date
#   df$date2 <- with(df, ISOdatetime(year, month, day, hour, min = 0, sec = 0, 
#                                    tz = "UTC"))
#   
#   # Drop variables
#   if (drop) df <- subset(df, select = -c(year, 
#                                          month, 
#                                          day, 
#                                          hour, 
#                                          receptor))
#   
#   # Transform arrival time, minus hours from hour.inc variable
#   df$date <- df$date2 - 3600 * df$hour.inc
#   
#   return(df)
#   
# }
