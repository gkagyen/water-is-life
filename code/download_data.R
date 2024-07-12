library(fs)
library(dataRetrieval)
library(readr)
library(glue)

# make a function to download data, defaults to current date for end

# function: dates as "YYYY-MM-DD"
get_daily_flow <- function(gage_no){
  
  # create folder to save data
  fs::dir_create("data_raw")
  
  # set parameters to download data
  siteNo <- gage_no # The USGS gage number
  pCode <- "00060" # 00060 is discharge parameter code
  
  # get NWIS daily data: CURRENT YEAR
  dat <- readNWISdv(siteNumbers = siteNo,
                    parameterCd = pCode)
  # add water year
  dat <- addWaterYear(dat)
  # rename the columns
  dat <- renameNWISColumns(dat)
  
  # save out
  write_csv(dat,
            file =
              glue("data_raw/nfa_updated_{Sys.Date()}.csv"))
}

# RUN with:
#siteNo <- "11427000"# NF American River
# get_daily_flow(siteNo)