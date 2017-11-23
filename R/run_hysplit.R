#' Function to command HYSPLIT (Hybrid Single Particle Lagrangian Integrated 
#' Trajectory Model) from R. 
#' 
#' \code{run_hysplit} will return a data frame which is ready for use in 
#' \strong{openair}'s \code{traj*} functions. 
#' 
#' @param latitude Latitude of a receptor location. 
#' 
#' @param longitude Longitude of a receptor location. 
#' 
#' @param start What is the start date of data to be returned? Ideally, the 
#' date format should be \code{yyyy-mm-dd}, but the UK locale convention of 
#' \code{dd/mm/yyyy} will also work. Years as strings or integers work too and
#' will floor-rounded. 
#' 
#' @param end What is the end date of data to be returned? Ideally, the 
#' date format should be \code{yyyy-mm-dd}, but the UK locale convention of 
#' \code{dd/mm/yyyy} will also work. Years as strings or integers work too and 
#' will be ceiling-rounded. 
#' 
#' @param interval What interval should the trajectories be run at? Default is 
#' \code{"3 hour"}, as in \strong{openair}.
#' 
#' @param runtime How many hours from time zero should the trajectory be run? 
#' Default is \code{-96}, i.e., a back trajectory of 96 hours, as in 
#' \strong{openair}. 
#' 
#' @param start_height Height of receptor location. Default is \code{10} metres, 
#' as in \strong{openair}. 
#' 
#' @param model_height Height of model domain. Default is \code{10000} metres,
#' as in \strong{openair}. 
#' 
#' @param hysplit_exec Location of HYSPLIT's executable files. Ensure executable
#' permisions have been set for the \code{hyts_std} application if on a Unix 
#' system. HYSPLIT's \code{bdyfiles} directory with the \code{ASCDATA.CFG} will
#' also need to be in the correct location. 
#' 
#' @param hysplit_input Location of input meteorological files for HYSPLIT. 
#' 
#' @param hysplit_output Location of where HYSPLIT should write its trajectory 
#' outputs while working. 
#' 
#' @param delete Should temporary files be silently deleted before and after the
#' model runs? Default is \code{TRUE}. 
#' 
#' @param drop Should the \code{"year"}, \code{"month"}, \code{"day"}, 
#' \code{"hour"}, and \code{"receptor"} variables be dropped from the bound
#' object? Default is \code{TRUE}. 
#' 
#' @param site An optional site string to be added to the returned data frame. 
#' 
#' @param source An optional source string to be added to the returned data frame. 
#' 
#' @param verbose Should the function give messages on what trajectory is being
#' processed. Default is \code{FALSE} and will estimate the time until 
#' completion.
#' 
#' @seealso \code{\link{importTraj}}, \code{\link{trajPlot}}, 
#' \href{http://ready.arl.noaa.gov/HYSPLIT.php}{HYSPLIT home}
#' 
#' @author Stuart K. Grange
#' 
#' @import lubridate
#' @import stringr
#' 
#' @examples 
#' \dontrun{
#' 
#' # Run back trajectories for 2010 for Lerwick, Shetland Islands
#' data_lerwick <- run_hysplit(
#'   latitude = 60.13922, 
#'   longitude = -1.185319, 
#'   runtime = -96, 
#'   start_height = 10, 
#'   model_height = 10000, 
#'   start = 2010,
#'   end = 2010,
#'   hysplit_exec = "/AQprojects/hysplit/exec", 
#'   hysplit_input = "/AQprojects/Hysplit/TrajData", 
#'   hysplit_output = "/AQprojects/hysplit/hysplit_output",
#'   site = "lerw"
#' )
#'   
#' # Run back trajectories for 2015 at 1500 metres and extended run time for
#' # analysis of air aloft or Gibraltar
#' data_gibraltar <- run_hysplit(
#'   latitude = 36.134, 
#'   longitude = -5.347, 
#'   runtime = -240, 
#'   start_height = 1500, 
#'   model_height = 10000, 
#'   start = 2015,
#'   end = 2015,
#'   hysplit_exec = "~/Dropbox/ricardo/hysplit/exec", 
#'   hysplit_input = "/media/stuartg/MOBILE_II/data_objects/hysplit_met_data", 
#'   hysplit_output = "~/Dropbox/ricardo/hysplit/hysplit_output",
#'   site = "gibraltar"
#' )
#'   
#' # On a Windows system with a default hysplit installation
#' data_gibraltar_standard <- run_hysplit(
#'   latitude = 36.134, 
#'   longitude = -5.347, 
#'   runtime = -240, 
#'   start_height = 10, 
#'   model_height = 10000, 
#'   start = 2015,
#'   end = 2015,
#'   hysplit_exec = "C:/hysplit4/exec", 
#'   hysplit_input = "D:/data/hysplit_met_data", 
#'   hysplit_output = "C:/hysplit4/output",
#'   site = "gibraltar"
#' )
#'   
#' }
#' 
#' @export
run_hysplit <- function(latitude = 51.5, 
                        longitude = -0.1, 
                        start = NA, 
                        end = NA, 
                        interval = "3 hour", 
                        runtime = -96, 
                        start_height = 10, 
                        model_height = 10000,
                        hysplit_exec = "exec/", 
                        hysplit_input, 
                        hysplit_output = "hysplit_output/",
                        delete = TRUE,
                        drop = TRUE,
                        site = NA, 
                        source = NA, 
                        verbose = FALSE) {
  
  # Parse arguments
  # Expand paths
  hysplit_exec <- path.expand(hysplit_exec)
  hysplit_input <- path.expand(hysplit_input)
  hysplit_output <- path.expand(hysplit_output)
  
  # Add final path separator
  hysplit_exec <- str_c(hysplit_exec, .Platform$file.sep)
  hysplit_input <- str_c(hysplit_input, .Platform$file.sep)
  hysplit_output <- str_c(hysplit_output, .Platform$file.sep)
  
  # Check if source directory exists, often not because of external drive use
  if (!dir.exists(hysplit_input)) 
    stop("'hysplit_input' does not exist.", call. = FALSE)
  
  # Selection after testing if directory is empty
  files_old <- list.files(hysplit_output, full.names = TRUE)
  
  # Delete old files
  if (delete) {
    
    # Delete
    if (verbose) message("Deleting old files...")
    file.remove(files_old)
    
  } else {
    
    # Selection control
    while (length(files_old) > 1) {
      
      # Get input
      input <- readline("There are files in the output directory. Should these be deleted? \n")
      
      # Parse
      input <- str_to_upper(input)
      input <- ifelse(
        input %in% c("YES", "Y", "T", "TRUE", "TR", "TRU"), TRUE, FALSE)
      
      if (input) {
        
        message("Deleting old HYSPLIT files...")
        file.remove(files_old)
        break
        
      } else {
        
        break
        
      }
      
    }
    
  }
    
  # Dates
  # Start and end dates
  start <- parse_date_arguments(start, "start")
  end <- parse_date_arguments(end, "end")
  
  # Receptor location and starting height
  coordinates <- str_c(latitude, longitude, start_height, sep = " ")
  
  # Set up where the control file is to be written
  control_file <- file.path(hysplit_exec, "CONTROL")
  
  # Store working directory because this will be changed
  wd <- getwd()
  
  # Create date sequence
  if (interval == "3 hour") end <- end + hours(21)
  date_sequence <- seq(start, end, interval)
  
  # Apply function which runs the model multiple times
  if (verbose) 
    message(str_c("Running ", length(date_sequence), " HYSPLIT trajectories..."))
  
  # For plyr's progress bar
  progress <- ifelse(verbose, "time", "none")
  
  # Run model
  plyr::l_ply(
    date_sequence, 
    run_trajectory, 
    hysplit_exec = hysplit_exec, 
    hysplit_input = hysplit_input, 
    hysplit_output = hysplit_output, 
    control_file = control_file, 
    coordinates = coordinates, 
    runtime = runtime, 
    model_height = model_height, 
    verbose = verbose,
    .progress = progress
  )
  
  # Change working directory back to original after system calls
  setwd(wd)
  
  # Bind output files
  if (verbose) message("Binding HYSPLIT files...")
  
  # Get file list
  file_list <- list.files(hysplit_output, "hysplit_output.txt", full.name = TRUE)
  
  # Load files as in openair, but drop some things usually
  df <- plyr::ldply(file_list, read_hysplit_file, drop = drop)
  
  # Add variables which are not in openair's files
  df$start_height <- start_height
  if (!is.na(site)) df$site <- site
  if (!is.na(source)) df$source <- source
  
  # Delete files
  if (delete) file.remove(file_list)
  
  return(df)
  
}


# Define the function which creates a control file and calls the hy_std 
# application. 
# 
# No export
run_trajectory <- function(date, 
                           hysplit_exec, 
                           hysplit_input, 
                           hysplit_output, 
                           control_file, 
                           coordinates, 
                           runtime, 
                           model_height, 
                           verbose) {
  
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
    str_replace_all(date_control, " ", ""), "_hysplit_output.txt")
  
  # Get date string
  current_month_pattern <- str_c(date_year, date_month)
  past_month_pattern <- get_year_and_month(current_month_pattern, - 1)
  future_month_pattern <- get_year_and_month(current_month_pattern, 1)
  
  # Add other pieces of the file names
  past_month_pattern <- str_c("RP", past_month_pattern , ".gbl")
  current_month_pattern <- str_c("RP", current_month_pattern , ".gbl")
  future_month_pattern <- str_c("RP", future_month_pattern , ".gbl")
  
  # Create the file and directory list for the control file
  file_list <- c(past_month_pattern, current_month_pattern, future_month_pattern)
  
  # For back trajectories, do not use the future month
  if (runtime < 0) {
    
    # But only if not the final day of the month, otherwise a stop error occurs
    if (!floor_date(date, "day") == ceiling_date(date, "month") - days(1)) 
      file_list <- file_list[1:2]
    
  }
  
  # Add directory to file names
  file_dir_list <- str_c(hysplit_input, file_list, sep = "\n")
  
  # Write control file
  # This will replace the contents of the current file if it exists
  write_to_control_file(date_control, control_file, append = FALSE)
  
  # Starting locations
  write_to_control_file("1", control_file)
  
  # Write coordinates and starting height of model
  write_to_control_file(coordinates, control_file)
  
  # Write runtime of model, hours forward or backwards for trajectories
  write_to_control_file(runtime, control_file)
  
  # Vertical motion option, top of model, and input grids (number of files)
  write_to_control_file(str_c("0\n", model_height, "\n", length(file_list)),
                        control_file)
  
  # Write input directory and file names
  write_to_control_file(file_dir_list, control_file)
  
  # Output directory
  write_to_control_file(hysplit_output, control_file)
  
  # Output file
  write_to_control_file(file_name_export, control_file)
  
  # Change working directory to hysplit application
  setwd(hysplit_exec)
  
  # Message file name
  if (verbose) message(str_c("\n", file_name_export))
  
  # System call, quiet arguments are os specific
  if (.Platform$OS.type == "windows") {
    
    # Run
    system("./hyts_std", show.output.on.console = FALSE)
    
  } else {
    
    # Run, ensure executable permissions are set
    system("./hyts_std", ignore.stdout = TRUE)
    
  }
  
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
  
  # Return
  pattern 
  
}


# Write table function
write_to_control_file <- function(string, file, append = TRUE) {
  
  # Write to file
  write.table(
    string, 
    file, 
    col.names = FALSE, 
    row.names = FALSE, 
    quote = FALSE, 
    append = append
  )
  
}


# Function to read hysplit files
# 
# Taken from openair manual to keep bound trajectory files in the same format
# as the openair package. Have changed tz to UTC rather than GMT though. 
# 
# No export
read_hysplit_file <- function(file, drop) {
  
  # Load file, error catching is for when two or three input met files are used
  # and results in a different length file header
  df <- tryCatch({
    
    read.table(file, header = FALSE, skip = 6)
    
  }, error = function(e) {
    
    read.table(file, header = FALSE, skip = 7)
    
  })
  
  # Drop
  df <- subset(df, select = -c(V2, V7, V8))
  
  # Rename
  df <- plyr::rename(df, c(V1 = "receptor", V3 = "year", V4 = "month", V5 = "day",
                           V6 = "hour", V9 = "hour.inc", V10 = "lat", V11 = "lon",
                           V12 = "height", V13 = "pressure"))
  
  # Clean two digit years
  df$year <- ifelse(df$year < 50, df$year + 2000, df$year + 1900)
  
  # Transform pieces of date to date
  df$date2 <- with(df, ISOdatetime(year, month, day, hour, min = 0, sec = 0, 
                                   tz = "UTC"))
  
  # Drop variables
  if (drop) df <- subset(df, select = -c(year, 
                                         month, 
                                         day, 
                                         hour, 
                                         receptor))
  
  # Transform arrival time, minus hours from hour.inc variable
  df$date <- df$date2 - 3600 * df$hour.inc
  
  return(df)
  
}
