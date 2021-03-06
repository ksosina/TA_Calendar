---
Author: "O.A.S"
Date: "January 25, 2020"
---

# TA_Calendar
Codes and function to create calendar

## Creating a calendar for all TAs

### How to use

1. Install the following packages c("data.table", "dplyr","chron", "googlesheets4", "googledrive"). A code is provided below for google stuff

```{r, eval=FALSE}
install.packages("devtools")
devtools::install_github("tidyverse/googledrive")
devtools::install_github("tidyverse/googlesheets4")
```

2. To use the google sheets you'd need to sign in.

3. In the google sheets file, you must have info for all TAs. 1 means present and 0 means absent.  In addition, have two columns identifying the date range where people might be absent (see line 13 in create_620s_cl.R ). For example

| TA  | M (12:15) | M (2:30) | T (12:15) | T (2:30) | W (12:15) | W (2:30) | Th (12:15) | Th (2:30) | F (12:15) | F (2:30) | from | to |
|:----------:|:---------:|:--------:|:---------:|:--------:|:---------:|:--------:|:----------:|:---------:|:---------:|:--------:|:--------:|:--------:|
| John Smith | 0 | 0 | 0 | 0 | 1 | 0 | 1 | 0 | 1 | 0 |  |  |
| James Bond | 0 | 1 | 1 | 1 | 1 | 0 | 1 | 1 | 1 | 1 | 05/12/18 | 05/16/18 |

where the columns "from" and "to" represent the dates James Bond won't be available to serve as a TA.

4. You need to know the start and end date, the dates where no labs are held, and the dates where TAs are doubled.

5. Optionally you might need to know the experienced TAs so you can double up in the first week. The code is not here.

6. Run all codes in ```Example code.R``` to see how it works. You might need to create your own sheet first. It should be in the format shown above.

7. Save and upload to google calendar for the TA account

