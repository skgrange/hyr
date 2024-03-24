#' Function to command HYSPLIT (Hybrid Single Particle Lagrangian Integrated 
#' Trajectory Model) from R. 
#' 
#' @param df Data frame/tibble which contains a set of variables. See examples 
#' for an example of what variables need to be included. 
#' 
#' @param directory_exec Location of HYSPLIT's executable files. Ensure executable
#' permissions have been set for the \code{hyts_std} application if on a Unix 
#' system. 
#' 
#' @param directory_input Location of input meteorological files for HYSPLIT. 
#' 
#' @param directory_output Location of where HYSPLIT should write its trajectory 
#' outputs to. 
#' 
#' @param verbose Should the function give messages?
#' 
#' @param progress Should a progress bar be displayed? 
#' 
#' @seealso \code{\link[openair]{importTraj}}, \code{\link[openair]{trajPlot}}, 
#' \href{http://ready.arl.noaa.gov/HYSPLIT.php}{HYSPLIT home}, 
#' \code{\link{read_hyr}}
#' 
#' @author Stuart K. Grange
#' 
#' @examples 
#' 
#' \dontrun{
#' 
#' # Create a tibble with all the things the hyr_run function requires
#' data_receptor <- tribble(
#'   ~latitude, ~longitude, ~runtime, ~interval, ~start_height, ~model_height, ~start, ~end,            
#'   36.150735, -5.349437, -120, "24 hour", 10, 10000, "2018-06-25", "2018-07-02"
#'   ) %>%
#'  mutate(start = as.POSIXct(start),
#'         end = as.POSIXct(end))
#'       
#' # Run hysplit, directories will be different on your system
#' hyr_run(
#'   data_receptor,
#'   directory_exec = "~/programmes/hysplit/trunk/exec",
#'   directory_input = "/media/storage/data/hysplit",
#'   directory_output = "~/Desktop/hysplit_outputs",
#'   verbose = TRUE
#' )
#'
#' }
#' 
#' @export
hyr_run <- function(df, directory_exec = "exec/", directory_input, 
                    directory_output, verbose = FALSE, progress = FALSE) {
  
  # Parse arguments
  # Expand paths
  directory_exec <- fs::path_expand(directory_exec)
  directory_input <- fs::path_expand(directory_input)
  directory_output <- fs::path_expand(directory_output)
  
  # Add final path separator, for hyspit files
  directory_exec <- str_c(directory_exec, .Platform$file.sep)
  directory_input <- str_c(directory_input, .Platform$file.sep)
  directory_output <- str_c(directory_output, .Platform$file.sep)
  
  # Check if source directory exists, often not because of external drive use
  if (!dir.exists(directory_input)) {
    cli::cli_abort("`directory_input` does not exist.")
  }
  
  # Check for output directory and create if desired
  if (!fs::dir_exists(directory_output)) {
    cli::cli_alert_info("{cli_date()} {directory_output} does not exist, should it be created?")
    if (menu(choices = c("Yes", "No")) == 1) {
      fs::dir_create(directory_output, recursive = TRUE)
      cli::cli_alert_info("{cli_date()} {directory_output} has been created...")
    }
  }
  
  # Receptor location and starting height
  coordinates <- str_c(df$latitude, df$longitude, df$start_height, sep = " ")
  
  # Set up where the control file is to be written
  control_file <- fs::path(directory_exec, "CONTROL")
  
  # Store working directory because this will be changed when calling the 
  # programme
  directory_current_working <- getwd()
  
  # Create date sequence
  if (df$interval == "3 hour") df$end <- df$end + hours(21)
  date_sequence <- seq(df$start, df$end, by = df$interval)
  
  if (verbose | progress) {
    cli::cli_alert_info(
      "{cli_date()} {length(date_sequence)} HYSPLIT trajectories to be run..."
    )
  }
  
  # Apply function which runs the model multiple times
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
    verbose = verbose,
    .progress = progress
  )
  
  # Change working directory back to original after system calls
  setwd(directory_current_working)
  
  return(invisible(df))
  
}


# Define the function which creates a control file and calls the hy_std 
# application. 
hyr_run_worker <- function(date, directory_exec, directory_input, 
                           directory_output, control_file, coordinates, runtime, 
                           model_height, verbose) {
  
  # Message to user
  if (verbose) {
    cli::cli_alert_info(
      "{cli_date()} Calculating trajectory starting at {format(date, '%Y-%m-%d %H:%M:%S')}..."
    )
  }
  
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
    str_remove_all(date_control, " "), 
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
  file_list_input <- c(past_month_pattern, current_month_pattern, future_month_pattern)
  
  # For back trajectories, do not use the future month
  # Also needed for final run of the month
  # But only if not the final day of the month, otherwise a stop error occurs
  if (runtime < 0 && !date_day == day(ceiling_date(date, "month") - 1L)) {
    file_list_input <- head(file_list_input, 2)
  }
  
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
  
  # Change working directory to hysplit application
  setwd(directory_exec)
  
  # System call to run hysplit
  list_run <- processx::run("./hyts_std")
  
  return(invisible(list_run))
  
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
  
  write.table(
    x, 
    file, 
    col.names = FALSE, 
    row.names = FALSE, 
    quote = FALSE, 
    append = append
  )
  
}
