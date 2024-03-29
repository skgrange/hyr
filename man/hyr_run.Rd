% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hyr_run.R
\name{hyr_run}
\alias{hyr_run}
\title{Function to command HYSPLIT (Hybrid Single Particle Lagrangian Integrated 
Trajectory Model) from R.}
\usage{
hyr_run(
  df,
  directory_exec = "exec/",
  directory_input,
  directory_output,
  verbose = FALSE,
  progress = FALSE
)
}
\arguments{
\item{df}{Data frame/tibble which contains a set of variables. See examples 
for an example of what variables need to be included.}

\item{directory_exec}{Location of HYSPLIT's executable files. Ensure executable
permissions have been set for the \code{hyts_std} application if on a Unix 
system.}

\item{directory_input}{Location of input meteorological files for HYSPLIT.}

\item{directory_output}{Location of where HYSPLIT should write its trajectory 
outputs to.}

\item{verbose}{Should the function give messages?}

\item{progress}{Should a progress bar be displayed?}
}
\description{
Function to command HYSPLIT (Hybrid Single Particle Lagrangian Integrated 
Trajectory Model) from R.
}
\examples{

\dontrun{

# Create a tibble with all the things the hyr_run function requires
data_receptor <- tribble(
  ~latitude, ~longitude, ~runtime, ~interval, ~start_height, ~model_height, ~start, ~end,            
  36.150735, -5.349437, -120, "24 hour", 10, 10000, "2018-06-25", "2018-07-02"
  ) \%>\%
 mutate(start = as.POSIXct(start),
        end = as.POSIXct(end))
      
# Run hysplit, directories will be different on your system
hyr_run(
  data_receptor,
  directory_exec = "~/programmes/hysplit/trunk/exec",
  directory_input = "/media/storage/data/hysplit",
  directory_output = "~/Desktop/hysplit_outputs",
  verbose = TRUE
)

}

}
\seealso{
\code{\link[openair]{importTraj}}, \code{\link[openair]{trajPlot}}, 
\href{http://ready.arl.noaa.gov/HYSPLIT.php}{HYSPLIT home}, 
\code{\link{read_hyr}}
}
\author{
Stuart K. Grange
}
