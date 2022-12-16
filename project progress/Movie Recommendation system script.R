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
movies_md = read.csv("movies_metadata.csv")

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

#data preparation for content-based filtering approach

movies_df = read.csv("movies.csv")
head(movies_df)
dim(movies_df)

ratings_df = read.csv("ratings_small.csv")
ratings_df = ratings_df[1:3]
head(ratings_df)
dim(ratings_df)

# we are going to make content-based filtering using Genre attribute

genres_df = as.data.frame(movies_df$genres, stringsAsFactors=FALSE)
head(genres_df)
library(data.table)
genres_separated = as.data.frame(tstrsplit(genres_df[,1], '[|]', type.convert=TRUE), stringsAsFactors=FALSE)
colnames(genres_separated) = c(1:7)
genres_separated = subset(genres_separated,select = c(1:7))


genre_list = c("Action", "Adventure", "Animation", "Children", "Comedy", "Crime","Documentary",
               "Drama", "Fantasy","Film-Noir", "Horror", "Musical", "Mystery",
               "Romance","Sci-Fi", "Thriller", "War", "Western")

genre_matrix = matrix(0,62424,18)
genre_matrix[1,] = genre_list

colnames(genre_matrix) = genre_list

for (i in 1:nrow(genres_separated)) {
  for (j in 1:ncol(genres_separated)) {
    genmat_col = which(genre_matrix[1,] == genres_separated[i,j])
    genre_matrix[i+1,genmat_col] = 1
  }
}

genre_matrix2 <- as.data.frame(genre_matrix[-1,], stringsAsFactors=FALSE)
for (c in 1:ncol(genre_matrix2)) {
  genre_matrix2[,c] <- as.integer(genre_matrix2[,c])
}

# Creating Binary class data frame from ratings to see whether user likes movie or not
# rating > 3 is equal to 1 (1 means like) 
# rating <= 3 is equal to -1 (-1 means dislike)
binaryratings = ratings_df
for (i in 1:nrow(binaryratings)){
  if (binaryratings[i,3] > 3){
    binaryratings[i,3] = 1
  }
  else{
    binaryratings[i,3] = -1
  }
}

binaryratings2 = reshape2::dcast(binaryratings, movieId~userId, value.var = "rating", na.rm=FALSE)
for (i in 1:ncol(binaryratings2)){
  binaryratings2[which(is.na(binaryratings2[,i]) == TRUE),i] = 0
}
binaryratings2 = binaryratings2[,-1]


movieIds = (unique(movies_df$movieId)) #62483
ratingmovieIds = (unique(ratings_df$movieId)) #9066
movies2_df = movies_df[-which((movieIds %in% ratingmovieIds) == FALSE),]
rownames(movies2_df) = NULL

genre_matrix3 = genre_matrix2[-which((movieIds %in% ratingmovieIds) == FALSE),]
rownames(genre_matrix3) = NULL

result = matrix(0,18,671)
for (c in 1:ncol(binaryratings2)){
  for (i in 1:ncol(genre_matrix3)){
    result[i,c] = sum((genre_matrix3[,i]) * (binaryratings2[,c]))
  }
}

for(j in 1:ncol(result)){
for (i in 1:nrow(result)){
  if (result[i,j] < 0){
    result[i,j] = 0
  }
  else {
    result[i,j] = 1
  }
}
}

# recommendations for user1

result2 = result[,1]#First user's profile
result2
sim_mat = rbind.data.frame(result2, genre_matrix3)
sim_mat = data.frame(lapply(sim_mat,function(x){as.integer(x)})) 
sim_mat[1,] #user liked genres

library(proxy)
sim_results = dist(sim_mat, method = "Cosine")
sim_results = as.data.frame(as.matrix(sim_results[1:8863]))
rows = which(sim_results == min(sim_results))
#Recommended movies
movies_df[rows,]


# recommendations for user2

result2 = result[,2]#Second user's profile
result2
sim_mat = rbind.data.frame(result2, genre_matrix3)
sim_mat = data.frame(lapply(sim_mat,function(x){as.integer(x)})) 
sim_mat[1,] #user liked genres

library(proxy)
sim_results = dist(sim_mat, method = "Cosine")
sim_results = as.data.frame(as.matrix(sim_results[1:8863]))
rows = which(sim_results == min(sim_results))
#Recommended movies

movies_df[rows,]


# We will build a User based Collaborative Filtering model

# Data preparation
library(reshape2)

ratingmat = dcast(ratings_df, userId~movieId, value.var = "rating", na.rm=FALSE)
ratingmat = as.matrix(ratingmat[,-1])

#install.packages("recommenderlab", repos = "https://mhahsler.r-universe.dev")
#install.packages("matrixStats", type = "binary")
#install.packages('BBmisc')

library(recommenderlab)
library(BBmisc)


ratingmat2 = as(ratingmat, "realRatingMatrix")

#binarize the data
ratingmat_bin = binarize(ratingmat2,minRating=3)

#Creating Recommender Model. "UBCF" stands for User-Based Collaborative Filtering

recommender_model = Recommender(ratingmat_bin, method = "UBCF", param=list(method="Cosine",nn=30))
recom = predict(recommender_model, ratingmat2[1], n=10) #Obtain top 10 recommendations for 1st user in dataset
recom_list = as(recom, "list")

#Obtain recommendations
recom_result = matrix(0,10)
for (i in c(1:10)){
  recom_result[i] = movies_df[as.integer(recom_list[[1]][i]),2]
}

#Recommending movies for 2nd user

recommender_model2 = Recommender(ratingmat_bin, method = "UBCF", param=list(method="Cosine",nn=30))
recom = predict(recommender_model2, ratingmat2[2], n=10) #Obtain top 10 recommendations for 2nd user in dataset
recom_list = as(recom, "list")

#Obtain recommendations
recom_result = matrix(0,10)
for (i in c(1:10)){
  recom_result[i] = movies_df[as.integer(recom_list[[1]][i]),2]
}

#Evaluation of Models

scheme = evaluationScheme(ratingmat2, method = 'cross-validation',k = 5, given = -1,goodRating = 5)

algs = list('popular' = list(name = 'POPULAR',param = NULL),
             'IBCF' = list(name = 'IBCF',parameter = list(method = "Cosine")),
             'UBCF' = list(name = 'UBCF',param = list(nn = 30)))

set.seed(1)
results_list = evaluate(scheme,method = algs,type = 'ratings')

library(dplyr)
labels = c('RMSE','MSE','MAE')
results = avg(results_list) %>% unlist() %>% as.data.frame()
results$label  = labels
results$method = row.names(results)
colnames(results)[1]  = 'RMSE'
library(ggplot2)
results %>% filter(label == 'RMSE') %>% ggplot(aes(reorder(method,-RMSE),RMSE)) + geom_bar(stat = 'identity')


