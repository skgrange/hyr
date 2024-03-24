# **hyr**

**hyr** is an R package to command the [Hybrid Single Particle Lagrangian Integrated Trajectory Model (HYSPLIT)](https://www.ready.noaa.gov/HYSPLIT.php) with R. 

# Installation

The development version of the **hyr** package can be easily installed with the [**remotes**](https://github.com/r-lib/remotes) package: 

```
# Install hyr
remotes::install_packages("skgrange/hyr")
```

# Setting-up HYSPLIT

There are a few components which need to be set-up to run HYSPLIT trajectories with **hyr**. 

  1. The HYSPLIT programme needs to be acquired then installed or complied from [here](https://www.ready.noaa.gov/HYSPLIT.php). You will need some knowledge of the directory structure to point **hyr**'s functions to where the executable can be called. On my (Ubuntu) system the directory structure looks like this: 

<!-- # tree hysplit/  -L 3 -P 'CONTROL|hyts_std' --charset ascii --> 

```
hysplit/
`-- trunk
    |-- bdyfiles
    |-- cluster
    |   |-- archive
    |   |-- endpts
    |   |-- example
    |   `-- working
    |-- cmaq
    |-- data2arl
    |   |-- api2arl
    |   `-- arw2arl
    |-- datem
    |-- document
    |-- examples
    |   |-- volcano
    |   `-- wildfire
    |-- exec
    |   |-- CONTROL
    |   `-- hyts_std
    |-- gisprog
    |   |-- arlshapes
    |   `-- dbf
    |-- graphics
    |-- guicode
    |-- html
    |-- library
    |   |-- fcsubs
    |   `-- hysplit
    |-- qwikcode
    |-- scripts
    |-- source
    |-- testing
    `-- working
```

The `exec` directory is the most important directory to be aware of because it contains the HYSPLIT programme (`hyts_std`) and the `CONTROL` file which is `hyts_std`'s configuration file. When using the main function in **hyr**, `hyr_run`, `CONTROL` files are generated from R inputs, then `hyts_std` programme is called, and this is repeated for all trajectories which are desired. If you are on a Unix system, ensure that `hyts_std` has executable permissions before attempting to run trajectories. 

  2. HYSPLIT also needs meteorological input files to calculate trajectories. There are many datasets which could potentially be used (a list is [here](https://www.ready.noaa.gov/archives.php)). However, a commonly used dataset are found within the [National Centers for Environmental Prediction/National Center for Atmospheric Research (NCEP/NCAR) Reanalysis](https://www.ready.noaa.gov/gbl_reanalysis.php) product. Data files are provided for each month, start in 1948, and cover the globe. New months' files are added every month, but lag behind the current month by about two months which means they are not suitable for future forecast trajectories. These files can be accessed from an [`ftp` server](ftp://arlftp.arlhq.noaa.gov/pub/archives/reanalysis). On my systems, I store a subset of these data files in a directory like this: 

<!-- tree --charset ascii hysplit/ | head -n 20 --> 

```
hysplit/
|-- RP198501.gbl
|-- RP198502.gbl
|-- RP198503.gbl
|-- RP198504.gbl
|-- RP198505.gbl
|-- RP198506.gbl
|-- RP198507.gbl
|-- RP198508.gbl
|-- RP198509.gbl
|-- RP198510.gbl
|-- RP198511.gbl
|-- RP198512.gbl
|-- RP198601.gbl
|-- RP198602.gbl
|-- RP198603.gbl
|-- RP198604.gbl
|-- RP198605.gbl
```

# **hyr** usage

## Running HYSPLIT

After you have set-up the HYSPLIT programme and have input met files, **hyr** can be used to run trajectories with the `hyr_run` function. `hyr_run` takes an input data frame/tibble with the required information to calculate a run and three directory_* arguments to point the function to the location of HYSPLIT `exec` directory, where the meteorological files are stored, and where to export the HYSPLIT trajectory data. For example, below runs daily back trajectories for Gibraltar: 

``` 
# Create a tibble with all the things the hyr_run function requires
data_receptor <- tribble(
  ~latitude, ~longitude, ~runtime, ~interval, ~start_height, ~model_height, ~start, ~end,            
  36.150735, -5.349437, -240, "24 hour", 10, 10000, "2018-06-25", "2018-07-02"
  ) %>%
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
```

The `verbose` argument will give you messages on the process which is recommended if you are running many trajectories. 

## Reading the HYSPLIT output files

Once trajectories have been run, the data need to be loaded as a data frame/tibble for analysis. The `read_hyr` function does this easily: 

```
# Read output from hyr_run
# Get list of files
file_list <- list.files("~/Desktop/hysplit_outputs", full.names = TRUE)

# Load all files
data_hyr <- read_hyr(file_list, verbose = FALSE)
```

The names of the tibble are not the same of those used in the **openair** package. To rename the variables for immediate use in **openair**'s functions, use the `hyr_rename_for_openair` function:

```
# Rename for openair functions
data_hyr_openair <- hyr_rename_for_openair(data_hyr)
```

# To-do 

  - Determine what the best way is to implement a HYSPLIT run for many receptors/runtimes/intervals/start heights 

  - Test a more complete set of forward trajectories usage scenarios
