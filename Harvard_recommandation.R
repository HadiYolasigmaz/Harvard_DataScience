#Use Dataset given by 'Create Test and Validation Sets' information updated 1/18/2019.

# 1.Required packages will be installed.

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")

# 2.From Movielens Website, Movielens 10M.zip ,ratings.dat and movies.dat files will be downloaded and with some data preparetion r code movielens dataset is created.

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- read.table(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                      col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           title = as.character(title),
                                           genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")

# 3.Validation set will be 10% of MovieLens data

set.seed(1)
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# 4.Make sure userId and movieId in validation set are also in edx set

validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# 5.Add rows from validation set that users and movies not in training set, edx,   back into edx set to have at least a value for every user and every movie in validation set. 

removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)

# 6.Now we only have edx and validatation tables to use as train set and test set, other temperary objects are removed. 
#Data is ready to analysis. How is their structure? First data should be understood.

str(edx)

########## Analysis

summary(edx)

#No NA data is observed for numeric columns. Predictions will be done on rating column.
# It is better to start analysis from simple 

#If we were using mean of all rating without any rules declared, what will be RMSE 

average <- mean(edx$rating)
average
first_rmse <- RMSE(validation$rating,average)
rmse_results_table <- data_frame(method = "First - only all data average", RMSE = first_rmse)
rmse_results_table

#First rmse is 1.06. It is far from 0.87750.
# Movie Recommandation System should start by analysing the movie
#First movie and how many times they were rated graph.

edx %>% select(movieId,rating) %>%
  mutate(x=1) %>%
  group_by(movieId) %>%
  summarize( rated=sum(x)) %>%
  filter (rated>100 )%>%
  ggplot (aes(x=movieId, y=rated)) +
  geom_point() +
  geom_bar(stat="identity", fill="skyblue", alpha=0.7) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

#It is showing that some ranges for movies are selected for this analysis. There are some movies rated more even more than 30.000 times. 

#A new column, gb_movie, group by movie will be added by calculating average rating of this movie and saved.

#If we use this new average as an additive to predicting rating. What will be last RMSE.

gb_movie_avgs <- edx %>% group_by(movieId) %>% summarize(gb_movie=mean(rating-average))
gb_movie_avgs
#predict 
movie_predicted_ratings <- average + validation %>% 
  left_join(gb_movie_avgs, by='movieId') %>%
  pull(gb_movie)
#predicted_ratings
gb_movie_rmse <- RMSE(movie_predicted_ratings, validation$rating)
rmse_results_table <- bind_rows(rmse_results_table, data_frame(method="Group by Movie",    RMSE = gb_movie_rmse))
rmse_results_table

#0.944 is good, it is more near to 0.87750. But above .90 is only 5 points that not enough, a new rating should be tried to reach to aim.

###This time, we focus User column. What will be new rating if we include user.

edx %>% select(userId,rating) %>%
  mutate(x=1) %>%
  group_by(userId) %>%
  summarize( rated=sum(x)) %>%
  filter (rated>100 )%>%
  ggplot (aes(x=userId, y=rated)) +
  geom_point() +
  geom_bar(stat="identity", fill="skyblue", alpha=0.7) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

#Users are rating movies that helping us prepare a well defined movie recommandation system.

#What will new average be?

gb_user_avgs <- edx %>% group_by(userId) %>% summarize(gb_user=mean(rating-average))

user_predicted_ratings <- average + validation %>% 
  left_join(gb_user_avgs, by='userId') %>%
  pull(gb_user)
#predicted_ratings
gb_user_rmse <- RMSE(user_predicted_ratings, validation$rating)
rmse_results_table <- bind_rows(rmse_results_table, data_frame(method="Group by User",    RMSE = gb_user_rmse))
rmse_results_table

#It is not made any improvement better than movieID

### what happen if we continue analysing by combination of movieID and userID 

gb_user_avgs <- edx %>% 
  left_join(gb_movie_avgs, by='movieId') %>%
  group_by(userId) %>%
  summarize(gb_user = mean(rating - average - gb_movie))
gb_predicted_ratings <- validation %>% 
  left_join(gb_movie_avgs, by='movieId') %>%
  left_join(gb_user_avgs, by='userId') %>%
  mutate(pred = average + gb_movie + gb_user) %>%
  pull(pred)
gb_movie_user_RMSE <- RMSE(gb_predicted_ratings, validation$rating)
rmse_results_table <- bind_rows(rmse_results_table,    
                                data_frame(method="Combination of Movie and User",  RMSE = gb_movie_user_RMSE))
rmse_results_table

#0.8653488 is below 0.87750. 
#It is done

