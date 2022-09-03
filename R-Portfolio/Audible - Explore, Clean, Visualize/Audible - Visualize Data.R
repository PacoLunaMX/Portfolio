# importing libraries
library(readr)
library(ggplot2)
library(dplyr)
library(tidyverse)

# load the data set
df <- read_csv('/Users/francisco/Desktop/Personal/Portfolio/R-Portfolio/Audible - Explore, Clean, Visualize/audible_clean.csv')
View(df)
glimpse(df)

# drop null values
clean_df <- df %>%
  filter(stars >-1 ,number_ratings >-1 )
  

# find business questions

#1. There is a correlation between the duration of a book and its ratings or stars?
#2. Which are the top 10 books with better starts? (Taking into account the number of reviews)
#3. What is the percentage of each language from the total?

# visualize the data in order to answer the business questions

#1.There is a correlation between the duration of a book and its ratings or stars?
clean_df %>%
  ggplot(aes(duration_mins,number_ratings)) +
  geom_point(alpha=0.3)+
  labs(title="Duration of books vs number of ratings")

#2. Which are the top 10 books with better starts? (Taking into account the number of reviews)
top_10 <-df %>%
  arrange(-stars,-number_ratings) %>%
  head(10)

top_10 %>%
  ggplot(aes(reorder(stringr::str_wrap(name,10),-number_ratings),number_ratings))+
  geom_bar(stat='identity')+
  labs(title="Top 10 books with more reviews and 5 stars of ratings")+
  theme(axis.title.x = element_blank())

#3. What is the percentage of each language from the total?
top_languages <- df %>%
  count(language)%>%
  arrange(-n) %>%
  head(10)


top_languages  %>%
  arrange(n) %>%
  mutate(Country=factor(language, language)) %>%
  ggplot( aes(reorder(language,-n), y=n) ) +
  geom_segment( aes(x=reorder(language,n) ,xend=language, y=0, yend=n), color="grey") +
  geom_point(size=3, color="#69b3a2") +
  coord_flip() +
  theme(
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_blank(),
    legend.position="none"
  ) +
  xlab("")+  
  labs(title="Top 10 languages more used in audiobooks from Audible")