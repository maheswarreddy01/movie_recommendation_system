# loading required packages

#install.packages("tidyverse")

library(tidyverse)
library(ggplot2)
library(scales)
library(jsonlite)
library(dplyr)
library(purrr)
library(data.table)


# DATA PRE-PROCESSING


# importing meta data of movies
movies_md = read.csv("B:/my-work/kdd/movie_recommendation_system/datasets/movies_metadata.csv")

head(movies_md)

dim(movies_md)

# removing rows that contains
movies_md[movies_md=="[]"] = NA
movies_md = na.omit(movies_md)

dim(movies_md)

# removing duplicate rows

names(movies_md)[6] =  "m_id"
movies_md = distinct(movies_md)

dim(movies_md)

summary(movies_md)

# changing data types for particular columns

movies_md$release_date = as.Date(movies_md$release_date)
movies_md$revenue = as.numeric(movies_md$revenue)
movies_md$budget = as.numeric(movies_md$budget)

summary(movies_md)

#adding a new column year for detailed analysis
movies_md$year = format(movies_md$release_date,format="%Y")
movies_md$year = as.integer(movies_md$year)

glimpse(movies_md)

# Filtering data by removing unwanted columns
movies_filtered = filter(movies_md, revenue > 1000000, budget > 1000, runtime > 0) %>%
  # Removing unwanted columns
  select(-"adult", -"belongs_to_collection", -"homepage", -"overview", -"poster_path", -"status", -"video", -"tagline", -"production_companies", -"production_countries", -"spoken_languages") %>%
  # Adding columns for net return and Return on investment (ROI) to see which movies collected most at box-office
  mutate(net_return = revenue - budget, ROI = net_return / budget * 100) 

movies_filtered = movies_filtered[ !(movies_filtered$m_id %in% c(363093)), ]

glimpse(movies_filtered)


# ANALYSIS OF THE DATA

# Genre-wise distribution of the data

horror          = filter(movies_filtered, grepl('Horror'         , genres))
mystery         = filter(movies_filtered, grepl('Mystery'        , genres))
action          = filter(movies_filtered, grepl('Action'         , genres))
adventure       = filter(movies_filtered, grepl('Adventure'      , genres))
fantasy         = filter(movies_filtered, grepl('Fantasy'        , genres))
comedy          = filter(movies_filtered, grepl('Comedy'         , genres))
thriller        = filter(movies_filtered, grepl('Thriller'       , genres))
documentary     = filter(movies_filtered, grepl('Documentary'    , genres))
animation       = filter(movies_filtered, grepl('Animation'      , genres))  
romance         = filter(movies_filtered, grepl('Romance'        , genres))  
family          = filter(movies_filtered, grepl('Family'         , genres))  
western         = filter(movies_filtered, grepl('Western'        , genres))  
music           = filter(movies_filtered, grepl('Music'          , genres))  
science_fiction = filter(movies_filtered, grepl('Science Fiction', genres))  
crime           = filter(movies_filtered, grepl('Crime'          , genres))  
history         = filter(movies_filtered, grepl('History'        , genres))  
war             = filter(movies_filtered, grepl('War'            , genres))  

# collections-wise distribution of every genre

genre_collections = data.frame(name = c("Horror", "Mystery", "Action", "Adventure", "Fantasy", "Comedy", "Thriller", "Documentary", "Animation",                                         "Romance", "Family", "Western", "Music", "Science Fiction", "Crime", "History", "War"), 
                               average_revenue    = c(mean(horror$revenue), mean(mystery$revenue), mean(action$revenue),
                                                      mean(adventure$revenue), mean(fantasy$revenue), mean(comedy$revenue),
                                                      mean(thriller$revenue), mean(documentary$revenue), mean(animation$revenue),
                                                      mean(romance$revenue), mean(family$revenue), mean(western$revenue),
                                                      mean(music$revenue), mean(science_fiction$revenue), mean(crime$revenue),
                                                      mean(history$revenue), mean(war$revenue)),
                               average_net_return = c(mean(horror$net_return), mean(mystery$net_return), mean(action$net_return),
                                                      mean(adventure$net_return), mean(fantasy$net_return), mean(comedy$net_return),
                                                      mean(thriller$net_return), mean(documentary$net_return), mean(animation$net_return),
                                                      mean(romance$net_return), mean(family$net_return), mean(western$net_return),
                                                      mean(music$net_return), mean(science_fiction$net_return), mean(crime$net_return),
                                                      mean(history$net_return), mean(war$net_return)),
                               average_ROI        = c(mean(horror$ROI), mean(mystery$ROI), mean(action$ROI),
                                                      mean(adventure$ROI), mean(fantasy$ROI), mean(comedy$ROI),
                                                      mean(thriller$ROI), mean(documentary$ROI), mean(animation$ROI),
                                                      mean(romance$ROI), mean(family$ROI), mean(western$ROI),
                                                      mean(music$ROI), mean(science_fiction$ROI), mean(crime$ROI),
                                                      mean(history$ROI), mean(war$ROI)),
                               average_budget     = c(mean(horror$budget), mean(mystery$budget), mean(action$budget),
                                                      mean(adventure$budget), mean(fantasy$budget), mean(comedy$budget),
                                                      mean(thriller$budget), mean(documentary$budget), mean(animation$budget),
                                                      mean(romance$budget), mean(family$budget), mean(western$budget),
                                                      mean(music$budget), mean(science_fiction$budget), mean(crime$budget),
                                                      mean(history$budget), mean(war$budget)))

# Average Revenue by Genre
ggplot(data = genre_collections, mapping = aes(x = reorder(name, -average_revenue), y = average_revenue)) + geom_bar(color = "black", fill = "#AAAFFF", stat='identity') + labs(title = "Average Revenue by Genre", y = "Average Revenue", x = "Genre") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# Average Net Return by Genre
ggplot(data = genre_collections, mapping = aes (x = reorder(name, -average_net_return), y = average_net_return)) + geom_bar(color = "black", fill = "#AAAFFF", stat='identity') + labs(title = "Average Net Return by Genre", y = "Average Net Return", x = "Genre") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 

# Average ROI by Genre
ggplot(data = genre_collections, mapping = aes (x = reorder(name, -average_ROI), y = average_ROI)) + geom_bar(color = "black", fill = "#AAAFFF", stat='identity') + labs(title = "Average ROI by Genre", y = "Average ROI", x = "Genre") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# Runtime vs net-return

ggplot(data = movies_filtered, mapping = aes (x = runtime, y = revenue)) + geom_point( stat='identity') + geom_smooth() + labs(title = "Net Return by Runtime", y = "Net Return", x = "Runtime")


# tmdb ratings vs net-return
ggplot(data = filter(movies_filtered, vote_count > 50), mapping = aes(x = vote_average, y = net_return)) + geom_point(stat='identity') + geom_smooth() + labs(title = "Net Return by Average Rating", y = "Net Return", x = "Average Rating on TMDB")
# we can see if ratings increases returns are also increasing


# TOP 10 MOVIES RECOMMENDATION using their collections at box-office
# this is not user-preference based model


top_10 = movies_filtered %>% arrange(desc(revenue)) %>% slice(1:10)
top_10_titles = data.frame(top_10$original_title)

