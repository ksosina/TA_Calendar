# How to use
# 1 Install the following packages c("data.table", "dplyr","chron", "googlesheets4", "googledrive"). A code is provided below for google stuff

# install.packages("devtools")
# devtools::install_github("tidyverse/googledrive")
# devtools::install_github("tidyverse/googlesheets4")

# 2 To use the google sheets you'd need to sign in
# 3 In the google sheets file, you must have info for all TAs. 1 means present and 0 means absent.
#   In addition, have two columns identifying the date range where people might be absent (see lines 33 and 34 )
# 4 You need to know the start and end date, the dates where no labs are held, and the dates where TAs are doubled
# 5 optionally you might need to know the experienced TAs so you can double up in the first week. The code is not here
# 6 Run all codes from line 39 to 312 until you get a "TRUE" result. 
# 7 Save and upload to google calendar for the TA account


# load libs ---------------------------------------------------------------
packs <- c("data.table", "dplyr","chron", "googlesheets4", "googledrive")
sapply(packs, library, character.only = T)



# Load Sheets -------------------------------------------------------------

# Google Sheet with TA conflicts (will need to be updated)
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
end_times <- c("1:20:00 PM","3:20:00 PM") # OH end time and stata end time


# Conflicts ---------------------------------------------------------------

dts <- seq.dates(start_date,end_date)
weekdts <- weekdays(dts)
dates <- dts[weekdts!="Sat"&weekdts!="Sun"&!as.character(dts)%in%no_TA_dates]
weekdts <- weekdays(dates)

# temp_wk <- rep(weekdts, each = 2)
# temp_dates <- rep(dates, each = 2)
temp_wk <- paste0(weekdts[1:5])
OH_idx <- seq(2, 11, by = 2)
STATA_idx <- seq(3, 11, by = 2)

avails <- lapply(1:nrow(my_dt), function(i){
  dt <- my_dt[i, ]
  
  
  # Deal with absent days
  if(dt$TA %in% my_dt_missing$TA){
    m_dt <- my_dt_missing[TA %in% dt$TA]
    abs_dt <- seq.dates(m_dt$from, m_dt$to)
    
    my_dates <- dates[!paste(dates) %in% abs_dt]
    
  }else{
    my_dates <- dates
  }
  
  
  
  idx <- dt[, OH_idx, with = F] %>%  unlist
  idx <- which(idx == 1)
  temp_days_oh <- temp_wk[idx]
  
  idx <- dt[, STATA_idx, with = F] %>%  unlist
  idx <- which(idx == 1)
  temp_days_stata <- temp_wk[idx]
  
  n1 <- length(temp_days_oh);n2 <- length(temp_days_stata)
  
  if(n1 > 0 & n2 > 0 ){
    avail <- c(paste0(my_dates[temp_wk %in% temp_days_oh], "_OH"),
               paste0(my_dates[temp_wk %in% temp_days_stata], "_STATA"))
  }else if(n1 > 0 & !n2 > 0){
    avail <- paste0(my_dates[temp_wk %in% temp_days_oh], "_OH")
  }else if(!n1 > 0 & n2 > 0){
    avail <- paste0(my_dates[temp_wk %in% temp_days_stata], "_STATA")
  }else{
    stop(paste0("Missing dates for", dt$TA))
  }
  
  
  out <- data.table("TA" = dt$TA, "Avail" = avail)
  
  return(out)
})
avails <- do.call(rbind, avails)


avails[, `:=`(Type = sapply(strsplit(Avail, "_"), "[", 2),
              dates = as.Date(sapply(strsplit(Avail, "_"), "[", 1), format = "%m/%d/%y"))]

# Sort by amount of dates available per TA
n_con <- avails[, .N, by = TA][order(N)]
avails <- inner_join(n_con[, .(TA)], avails) %>% data.table

fwrite(avails, "avails.txt")



# Create bin dates --------------------------------------------------------

dts <- seq.dates(start_date,end_date)
weekdts <- weekdays(dts)
dates <- dts[weekdts!="Sat"&weekdts!="Sun"&!as.character(dts)%in%no_TA_dates]
dates <- c(dates,double_days)
dates <- sort(c(dates))


# Randomly fill the dates -------------------------------------------------

# Get count of events per TA
len_dates <- 2*length(dates)
len_tas <- length(ta_names)
mult <- floor(len_dates/len_tas) # No of events per TA

ta_names <- n_con$TA


# For each TA select 2/3 STATA events and 3/2 OH events starting with the person with the most conflicts


avail_date_oh <- data.table(dates = dates)
avail_date_stata <- data.table(dates = dates)
avail_date_oh <- avail_date_oh[, .N, by = dates]
avail_date_stata <- avail_date_stata[, .N, by = dates]

avail_date_oh[, `:=`(dates = as.Date(dates, format = "%m/%d/%y"),
                     N = N)]
avail_date_stata[, `:=`(dates = as.Date(dates, format = "%m/%d/%y"),
                        N = N)]

rand_dates <- lapply(ta_names, function(nm){
  
  
  # Remove pool of exisiting dates, select TA and type
  
  oh <- avails[TA == nm & Type == "OH" & dates %in% avail_date_oh[N > 0, dates]]
  stata <- avails[TA == nm & Type == "STATA" & dates %in% avail_date_stata[N > 0, dates]]
  
  # Size is obtained by dividing mult by 2
  if(nrow(oh) > 0 & nrow(stata) > 0){
    s1 <- min(ifelse(nrow(oh) > nrow(stata), ceiling(mult/2), floor(mult/2)), nrow(oh))
    s2 <- min(nrow(stata), (mult - s1))
  }else if( !nrow(oh) > 0 & nrow(stata) > 0){
    s1 <- 0
    s2 <- mult
  }else if(nrow(oh) > 0 & !nrow(stata) > 0){
    s1 <- mult
    s2 <- 0
  }
 
  out_oh <- sample(oh$dates, size = s1, replace = F)
  out_stata <- sample(stata$dates, size = s2, replace = F)
  
  
  
  avail_date_oh$N <<- avail_date_oh$N - (avail_date_oh$dates %in% out_oh)
  avail_date_stata$N <<- avail_date_stata$N - (avail_date_stata$dates %in% out_stata)
  
  
  out <- rbind(avails[TA == nm & dates %in% out_oh & Type == "OH"],
               avails[TA == nm & dates %in% out_stata & Type == "STATA"])
  
  
  
  my_csv <- data.table(Subject = out$TA,
                       "Start Date" = out$dates,
                       'Start Time' = rep(start_times, times = c(s1, s2)),
                       'End Date' = out$dates,
                       'End Time' = rep(end_times, times = c(s1, s2)),
                       "All Day Event" = "False",
                       Description = "Biostat 621 TA Schedule",
                       Location = rep(c("W2009", "W3025"), times = c(s1, s2)),
                       Private = "False")
  
  my_csv
})

rand_dates <- do.call(rbind, rand_dates) %>% unique


# Obtain Dates for office hours (OH) that haven't been assigned yet
rm_avail_oh <-  avails[Type == "OH" & dates %in% avail_date_oh[N > 0, dates] ]
rm_avail_oh <- anti_join(rm_avail_oh,
                         rand_dates[, .(TA = Subject, dates = `Start Date`,
                                        Type = ifelse(`Start Time` == start_times[1], "OH", "STATA"))]) %>% data.table

# Obtain Dates for STATA sessions that haven't been assigned yet
rm_avail_stata <- avails[Type == "STATA" & dates %in% avail_date_stata[N>0, dates] ]
rm_avail_stata <- anti_join(rm_avail_stata,
                            rand_dates[, .(TA = Subject, dates = `Start Date`,
                                           Type = ifelse(`Start Time` == start_times[1], "OH", "STATA"))]) %>% data.table

# Total number of dates left unassigned
rem_oh <- length(dates) - nrow(rand_dates[`Start Time` == start_times[1]])
rem_st <- length(dates) - nrow(rand_dates[`Start Time` == start_times[2]])



# For the remaining dates, select TAs
dts_rm <- as.Date(NA, format = "%m/%d/%y")
rm_avail_oh <- lapply(1:rem_oh, function(i){
  dt <- rm_avail_oh[!dates %in% dts_rm]
  if(nrow(dt)>0){
    times_rep <- dt[, .N, by = TA][, N]
    idx_prob <- times_rep/nrow(dt)
    idx_prob <- rep(idx_prob, time = times_rep)
    
    idx <- sample(1:nrow(dt), size = 1, prob = idx_prob, replace = F)
    out <- dt[idx]
    dts_rm <<- c(na.omit(dts_rm), out$dates)
    out
  }
})


rm_avail_oh <- do.call(rbind, rm_avail_oh)
rm_avail_oh <- rm_avail_oh[order(Type)]
rm_avail_oh_n <- rm_avail_oh[, .N, by = Type]

rm_csv_oh <- data.table(Subject = rm_avail_oh$TA,
                        "Start Date" = rm_avail_oh$dates,
                        'Start Time' = start_times[1],
                        'End Date' = rm_avail_oh$dates,
                        'End Time' = end_times[1],
                        "All Day Event" = "False",
                        Description = "Biostat 621 TA Schedule",
                        Location = "W2009",
                        Private = "False")





dts_rm <- as.Date(NA, format = "%m/%d/%y")

rem_st <- rem_st + rem_oh - nrow(rm_avail_oh)


rm_avail_stata <- lapply(1:rem_st, function(i){
  dt <- rm_avail_stata[!dates %in% dts_rm]
  if(nrow(dt)>0){
    
    times_rep <- dt[, .N, by = TA][, N]
    idx_prob <- times_rep/nrow(dt)
    idx_prob <- rep(idx_prob, time = times_rep)
    
    
    idx <- sample(1:nrow(dt), size = 1, prob = idx_prob, replace = F)
    out <- dt[idx]
    dts_rm <<- c(na.omit(dts_rm), out$dates)
    out
  }
  
})
rm_avail_stata <- do.call(rbind, rm_avail_stata)
rm_avail_stata <- rm_avail_stata[order(Type)]
rm_avail_stata_n <- rm_avail_stata[, .N, by = Type]

rm_csv_st <- data.table(Subject = rm_avail_stata$TA,
                        "Start Date" = rm_avail_stata$dates,
                        'Start Time' = start_times[2],
                        'End Date' = rm_avail_stata$dates,
                        'End Time' = end_times[2],
                        "All Day Event" = "False",
                        Description = "Biostat 621 TA Schedule",
                        Location = "W3025",
                        Private = "False")


res_assigns <- rbind(rand_dates, rm_csv_oh, rm_csv_st)
res_assigns <- res_assigns %>% unique


extr_wrk <- res_assigns[, .N, by = Subject]
extr_wrk$N <- extr_wrk$N - mult

# TAs who will have more assignments than others
extr_wrk[N > 0]


# This MUST BE TRUE; Otherwise keep rerunning
nrow(res_assigns) == length(dates)*2

# Save results
fwrite(res_assigns, "ta_assigns.txt")

# all_assigned <- res_assigns
# all_assigned <- unique(all_assigned)


# Create calendars --------------------------------------------------------

all_assigned_ta <- split(res_assigns, res_assigns$Subject)

sapply(all_assigned_ta, function(dt){
  fwrite(dt, file = paste0(unique(dt$Subject), ".csv"), append = F)
})
