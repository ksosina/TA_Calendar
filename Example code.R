


ss <- "https://docs.google.com/spreadsheets/d/1X1r6uZvP4GxOdz87CdvAXsvOSnDe_cSVIzr-4_uS2BE/edit#gid=0"
sheets_get(ss)


# Load file
my_dt <- read_sheet(ss, sheet = 1) %>% data.table
my_dt_missing <- my_dt[!is.na(Comments), .(TA, Comments)]
my_dt_missing$from <- "01/27/20"
my_dt_missing$to <- "01/31/20"


# parameters --------------------------------------------------------------

ta_names<- my_dt$TA


first_week_start_date <- "09/05/19"
first_week_end_date <- "09/06/19"

start_date <- "01/27/20"
end_date <- "03/11/20"

no_TA_dates <- c("02/18/20", "02/19/20", "03/12/20")

double_days <- c("01/29/20", "01/30/20", "02/03/20", "02/12/20", "02/13/20", "02/17/20",
                 "02/26/20", "02/27/20", "03/02/20", "03/09/20", "03/10/20", "03/11/20")
ta_location <- c("TA Office Hour = W2009", "STATA Office Hour = W3025")
# lab_location<-"W3031"

start_times <- c("12:15:00 PM","2:30:00 PM") # OH start time and stata start time
end_times <- c("1:20:00 PM","3:20:00 PM")


create_620s_cl(ss, start_date, end_date, no_TA_dates, double_days, ta_location, start_times, end_times)
