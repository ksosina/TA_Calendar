
# How to use
# 1 Install the following packages c("data.table", "dplyr","chron", "googlesheets4", "googledrive"). A code is provided below for google stuff

# install.packages("devtools")
# devtools::install_github("tidyverse/googledrive")
# devtools::install_github("tidyverse/googlesheets4")

# 2 To use the google sheets you'd need to sign in
# 3 In the google sheets file, you must have info for all TAs. 1 means present and 0 means absent.
#   In addition, have two columns identifying the date range where people might be absent (see line 13 in create_620s_cl.R )
# 4 You need to know the start and end date, the dates where no labs are held, and the dates where TAs are doubled
# 5 optionally you might need to know the experienced TAs so you can double up in the first week. The code is not here
# 6 Run all codes from line 39 to 312 until you get a "TRUE" result. 
# 7 Save and upload to google calendar for the TA account

ss <- "https://docs.google.com/spreadsheets/d/1X1r6uZvP4GxOdz87CdvAXsvOSnDe_cSVIzr-4_uS2BE/edit#gid=0"



# parameters --------------------------------------------------------------

first_week_start_date <- "09/05/19"
first_week_end_date <- "09/06/19"

start_date <- "01/27/20"
end_date <- "03/11/20"

no_TA_dates <- c("02/18/20", "02/19/20", "03/12/20")

double_days <- c("01/29/20", "01/30/20", "02/03/20", "02/12/20", "02/13/20", "02/17/20",
                 "02/26/20", "02/27/20", "03/02/20", "03/09/20", "03/10/20", "03/11/20")
ta_location <- c("TA Office Hour = W2009", "STATA Office Hour = W3025")


start_times <- c("12:15:00 PM","2:30:00 PM") # OH start time and stata start time
end_times <- c("1:20:00 PM","3:20:00 PM")


source("https://raw.githubusercontent.com/ksosina/TA_Calendar/master/create_cal_func.R")
create_620s_cl(ss, start_date, end_date, no_TA_dates, double_days, ta_location, start_times, end_times)
