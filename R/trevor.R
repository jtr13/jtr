# Login to https://trevor.myschoolapp.com/
# Click Julian, Schedule, View Full Schedule
# Schedule Reports button, Choose Day Name (Grid)
# Choose dates, set Show Dates to Yes
# Click Export Data to Excel

library(tidyverse)
# may be skip = 3 instead
df <- readxl::read_xls("~/Downloads/trevor2.xls", skip = 2)
colnames(df)[1] <- "Time"
gather(df, value = "Subject", key = "Start Date", -Time) %>%
  filter(!is.na(Subject)) %>%
  mutate(Time = str_replace_all(Time, "AM", " AM"),
         Time = str_replace_all(Time, "PM", " PM")) %>%
  separate(col = "Time", into = c("Start Time", "End Time"),
                  sep = " - \\r\\s?") %>%
  mutate(`Start Date` = str_extract(`Start Date`, "\\d+/\\d+/\\d+")) %>%
  mutate(`End Date` = `Start Date`) %>%
  select(Subject, `Start Date`, `Start Time`, `End Date`, `End Time`) %>%
  mutate(`All day event` = FALSE, Description = "", Location = "") %>%
  filter(!(str_detect(Subject, "Lunch"))) %>%
  filter(!(str_detect(Subject, "Homeroom"))) %>%
  write_csv("~/Downloads/trevorgoogle.csv")

# In Google Calendar, Click Settings, then Import & Export
