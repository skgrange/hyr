% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_hyr.R
\name{read_hyr}
\alias{read_hyr}
\title{Function to load HYSPLIT data files produced with \code{\link{hyr_run}}.}
\usage{
read_hyr(file, verbose = FALSE, progress = FALSE)
}
\arguments{
\item{file}{Vector of file names produced by \code{\link{hyr_run}}.}

\item{verbose}{Should the function give messages?}

\item{progress}{Should a progress bar be displayed?}
}
\value{
Tibble.
}
\description{
Function to load HYSPLIT data files produced with \code{\link{hyr_run}}.
}
\examples{

\dontrun{

# Get file list
list_files <- list.files("~/Desktop/hysplit_outputs", full.names = TRUE)

# Load files
data_hysplit <- read_hyr(list_files)

# Or load files and rename variables for use in openair
data_hysplit_openair <- read_hyr(list_files) \%>\% 
  hyr_rename_for_openair()

}

}
\seealso{
\code{\link{hyr_run}}
}
\author{
Stuart K. Grange
}
