#The MovieLens Project
#This file contains the R script generating the movie rating predictions and calculating the RMSE


#Important Notice:
#The title and genres columns of the edx and validation data sets downloaded from
#the grouplens.org website have no useful information (NA's) after loading them with the provided script
#Therefore, the edx and validation data sets used here are provided by edx staff member @wonyoungcheong via the following link
#https://drive.google.com/drive/folders/1IZcBBX0OmL9wu9AdzMBFUG8GoPbGQ38D?usp=sharing
#There are 2 .rds files within the above link, one has the edx data set,
#the other has the validation data set, both recognizable within the file names 
#They are downloaded directly into the computer, specifically in the current working directory to make them easier to load
#The R version used for the project is 4.0
#Some training process could not be performed due to low computer memory space.

#Loading the necessary libraries to work on the movielens data sets
library(dplyr)
library(tidyverse)
library(caret)


#Loading the edx data set
edx<-readRDS("edx.rds")


#The following lines of code set the seed to 1, and generate the edx_train and edx_test sets with 90% for this last one

set.seed(1, sample.kind="Rounding") #Since using R 4.0
test_index <- createDataPartition(edx$rating, times = 1, p = 0.9, list = FALSE)
edx_train<-edx[-test_index,]
edx_test<-edx[test_index,]

#Listing the movie titles along with the average and total ratings in descending order of average rating
edx_train_mra<-unique(edx_train %>% group_by(title) %>% summarize(avg_rating=mean(rating),total_rating=n()) %>% arrange(desc(avg_rating)))

#Listing the movie genres along with the average and total ratings in descending order of average rating
edx_train_gra<-unique(edx_train %>% group_by(genres) %>% summarize(avg_rating=mean(rating),total_rating=n()) %>% arrange(desc(avg_rating)))

#Listing the users along with the average and total ratings in descending order of average rating
edx_train_ura<-unique(edx_train %>% group_by(userId) %>% summarize(avg_rating=mean(rating),total_rating=n()) %>% arrange(desc(avg_rating)))

#A view at the above listings
view(edx_train_gra)
view(edx_train_mra)
view(edx_train_ura)

#Showing the boxplot of ratings for different single genres
edx_train %>% filter(genres==c("Animation","Action","Adventure","Comedy","Thriller","Sci-Fi","Crime","Film-Noir","Children","Mystery","War","Western","Drama","IMAX")) %>% group_by(genres) %>% ggplot(aes(genres,rating))+geom_boxplot()

#Ploting the genres rating for userId 644 (adjusting the axis labels)
edx_train %>% filter(userId==644) %>% ggplot(aes(genres,rating))+geom_point()+theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

#Ploting the genres rating for userId 7365 (adjusting the axis labels)
edx_train %>% filter(userId==7365) %>% ggplot(aes(genres,rating))+geom_point()+theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

#Ploting the genres rating for userId 48518 (adjusting the axis labels)
edx_train %>% filter(userId==48518) %>% ggplot(aes(genres,rating))+geom_point()+theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


#Filtering user with average rating higher than 4.6 and total rating higher than 100
edx_train_ura %>% filter(avg_rating>4.6 & total_rating>100) %>% select(userId,avg_rating,total_rating)

#Ploting the genres rating for userId 32463 (adjusting the axis labels)
edx_train %>% filter(userId==32463) %>% ggplot(aes(genres,rating))+geom_point()+theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

#Showing that a user can give multiple levels of rating to the exact same genres of movies
edx_train %>% filter(userId==32463 & genres==c("Action|Adventure|Sci-Fi")) %>% ggplot(aes(genres,rating))+geom_point()+theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

#Defining the principal component data set in edx_train_pca then summarising it to see the proportion of variance explained
edx_train_pca<- edx_train %>% select(userId,movieId) %>% prcomp(.)
summary(edx_train_pca)

#Training with glm first
glmfit<-train(rating ~ userId+movieId, data=edx_train, method="glm")
glmfit

#Training with knn
knnfit<-train(rating ~ userId+movieId, data=edx_train, method="knn")

#Training with rf
rffit<-train(rating ~ userId+movieId, data=edx_train, method="rf")


#Training with the lda model 
ldafit<-train(rating ~ userId+movieId, data=edx_train, method="lda")
ldafit


#Training with the qda model
qdafit<-train(rating ~ userId+movieId, data=edx_train, method="qda")
qdafit

#Performing training with the other predictors
glmfit<-train(rating ~ userId+movieId+timestamp+genres, data=edx_train, method="glm")
ldafit<-train(rating ~ userId+movieId+timestamp+genres, data=edx_train, method="lda")
qdafit<-train(rating ~ userId+movieId+timestamp+genres, data=edx_train, method="qda")

#Calculating RMSE with the glm model on the edx_test set
RMSE(predict(glmfit,edx_test),edx_test$rating)

#Loading the validation data set to test our final model
validation<-readRDS("validation.rds")

#Testing our final training model on the validation set
mean(predict(glmfit,validation)==validation$rating)

#Calculating RMSE with the glm model on the validation set
RMSE(predict(glmfit,validation),validation$rating)