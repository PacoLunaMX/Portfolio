# import libraries
library(ggplot2)
library(tidyr)
library(dplyr)
library(readr)
library(janitor)

# load datasets
df_aug_2021 <- read_csv('Data/202108-divvy-tripdata.csv')
df_sep_2021 <- read_csv('Data/202109-divvy-tripdata.csv')
df_oct_2021 <- read_csv('Data/202110-divvy-tripdata.csv')
df_nov_2021 <- read_csv('Data/202111-divvy-tripdata.csv')
df_dec_2021 <- read_csv('Data/202112-divvy-tripdata.csv')
df_jan_2022 <- read_csv('Data/202201-divvy-tripdata.csv')
df_feb_2022 <- read_csv('Data/202202-divvy-tripdata.csv')
df_mar_2022 <- read_csv('Data/202203-divvy-tripdata.csv')
df_apr_2022 <- read_csv('Data/202204-divvy-tripdata.csv')
df_may_2022 <- read_csv('Data/202205-divvy-tripdata.csv')
df_jun_2022 <- read_csv('Data/202206-divvy-tripdata.csv')
df_jul_2022 <- read_csv('Data/202207-divvy-tripdata.csv')

# starting to know our data
View(df_aug_2021)

glimpse(df_aug_2021)

# which type of bikes we have available?
unique(df_aug_2021$rideable_type)

# which type of different users there are?
unique(df_aug_2021$member_casual)

# compare the data type of columns across all the data frames
compare_df_cols(df_aug_2021,df_sep_2021,df_oct_2021,df_nov_2021,df_dec_2021,df_jan_2022, df_feb_2022, df_mar_2022, df_apr_2022, df_may_2022, df_jun_2022, df_jul_2022, return="mismatch")


# combine datasets into a single df
total_trips <- rbind(df_aug_2021,df_sep_2021,df_oct_2021,df_nov_2021,df_dec_2021,df_jan_2022, df_feb_2022, df_mar_2022, df_apr_2022, df_may_2022, df_jun_2022, df_jul_2022)

# removing duplicate trips with the same id
total_trips %>% distinct(ride_id, .keep_all= TRUE)

# calculate the duration of each trip and make it a new column
total_trips$trip_duration <- as.numeric(difftime(total_trips$ended_at,total_trips$started_at, units='mins'))

# checking that all values in the trip_duration column are positive ones and removing the negative ones
total_trips <- total_trips %>%
  filter(trip_duration>0)

# obtaining the weekday at which the trip started
total_trips$weekday <- weekdays(total_trips$started_at, abbreviate=FALSE)


# making a column with the month in which the trip started
total_trips$month <- format(total_trips$started_at, "%m")   


# counting the total of NA values in the station columns
sum(is.na(total_trips$start_station_name))
sum(is.na(total_trips$start_station_id))

sum(is.na(total_trips$end_station_name))
sum(is.na(total_trips$end_station_id))


write.csv(total_trips,'total_trips.csv')

