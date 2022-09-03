# loading the libraries that we need
library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(stringr)

# loading the data
data <- read_csv('/Users/francisco/Desktop/Personal/Portfolio/R-Portfolio/Audible - Explore, Clean, Visualize/audible_uncleaned.csv')


##########################################
######### EXPLORING DATA #################
##########################################
# starting to know our variables and data types
glimpse(data)

# get a look at the first rows of our data
head(data)

# looking for duplicated rows
n_distinct(data)

# looking the number of distinct values in the name column and author column
data %>%
  distinct(name, author)


##########################################
######### CLEANING DATA ##################
##########################################
# 1. Remove the Wrritenby and Narratedby in the author and narrator columns and separate the first from the last name by a blank space.
data$author <- gsub("Writtenby:","",as.character(data$author))
data$narrator <- gsub("Narratedby:","",as.character(data$narrator))

data$author <- gsub('([[:upper:]])', ' \\1', as.character(data$author)) %>%
  str_trim()
data$narrator  <- gsub('([[:upper:]])', ' \\1', as.character(data$narrator ))%>%
  str_trim()


# 2. Remove duplicated rows for name and author columns.
data <- distinct(data, name, author, .keep_all= TRUE)

dim(data)

# 3. Standardize the time column in minutes.
# how many different type of formats we have
unique(nchar(data$time, type = "chars", allowNA = FALSE))


# creating a function that recieves a string value as parameter and returns a numeric value in minutes
clean_time <- function(value){
  if(value=="Less than 1 minute"){
    return(as.numeric(1))
  }
  else{
    if (nchar(value)>7){
      # separate hrs from minutes
      hrs <- str_trim(unlist(strsplit(value, split = "and"))[1])
      mins <- str_trim(unlist(strsplit(value, split = "and"))[2])   
      
      # leave only the numeric value of each variable and multiplying the hour per 60 minutes
      hrs <- as.numeric(gsub(".*?([0-9]+).*", "\\1", hrs)) *60
      mins <- as.numeric(gsub(".*?([0-9]+).*", "\\1", mins))
      
      # returning the sum of hours in minutes plus the original minutes
      return(mins + hrs)
    }
    else {
      if (grepl("h", value, fixed = TRUE)){
        hrs <- as.numeric(gsub(".*?([0-9]+).*", "\\1", value)) *60
        return(hrs)
      }
      else{
        mins <- as.numeric(gsub(".*?([0-9]+).*", "\\1", value))
        return(mins)
      }
      
    }
  }
  
}

# applying the function to the time column
data$time <- lapply(data$time, clean_time)

# converting the column in integer class
data$time <-as.integer(unlist(data$time))

# renaming the column to duration_mins
data <- data %>%
  rename(duration_mins=time)

# 4. Convert releasedate column into date type.
data$releasedate <- as.Date(data$releasedate, "%d-%m-%y")

# 5. Separate the number of starts and number of ratings in the stars column.
# get the unique lengths of the column values
unique(nchar(data$stars, type = "chars", allowNA = FALSE))

# get the unique values for the stars column
data$stars %>%
  unique()


# creating a function to just keep the values in the stars column
clean_stars <- function(value){
  if(value=="Not rated yet"){
    return("-1 -1")
  }
  else{
    # substitute the ratings word with an empty string
    value <- gsub(" ratings", "", value)
    # substitute the our of 5 starts sentence with an empy string
    value <- gsub(" out of 5 stars"," ",value)
    value <- gsub(",","",value)
    return(value)
      
    }
  }

# applying the function to the stars column
data$stars <- lapply(data$stars, clean_stars)

# separate column into two
data <- data %>% separate(stars, c('stars', 'number_ratings'),sep=" ",convert=TRUE,extra="drop")

# 6. Convert the price column into numeric type

# looking for values that might no be of numeric type
unique(data$price)

# changing the Free value to a 0
data$price[data$price == "Free"] <- 0  

# removing commas for some values
remove_comas <- function(value){
  value <- gsub(",","", value)
  return(value)
}

data$price <- lapply(data$price, remove_comas)


# changing the column type to a numeric one
data$price <- as.double(data$price)


# exporting data to a csv file
write.csv(data,"/Users/francisco/Desktop/Personal/Portfolio/R-Portfolio/Audible - Explore, Clean, Visualize/audible_clean.csv", row.names = TRUE)




