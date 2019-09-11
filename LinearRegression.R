### LOADING PACKAGES

library(ggplot2)
library(scales)
library(dplyr)
library(grid)

### SET WORKING DIRECTORY

setwd("C:/Users/Admin/Desktop/RIT/Intro to Big Data - Spring 19/News")

#######################################################

### LOADING NEWS DATA

news <- read.csv("News_Final.csv") # DATA ON NEWS ITEMS

### LOADING SOCIAL FEEDBACK DATA

## Social Feedback Data - Facebook
facebook_economy <- read.csv("Facebook_Economy.csv")
facebook_microsoft <- read.csv("Facebook_Microsoft.csv") 
facebook_obama <- read.csv("Facebook_Obama.csv") 
facebook_palestine <- read.csv("Facebook_Palestine.csv") 

## Social Feedback Data - Google+
googleplus_economy <- read.csv("GooglePlus_Economy.csv")
googleplus_microsoft <- read.csv("GooglePlus_Microsoft.csv") 
googleplus_obama <- read.csv("GooglePlus_Obama.csv") 
googleplus_palestine <- read.csv("GooglePlus_Palestine.csv") 

## Social Feedback Data - LinkedIn
linkedin_economy <- read.csv("LinkedIn_Economy.csv")
linkedin_microsoft <- read.csv("LinkedIn_Microsoft.csv") 
linkedin_obama <- read.csv("LinkedIn_Obama.csv") 
linkedin_palestine <- read.csv("LinkedIn_Palestine.csv") 




cols <- as.data.frame(linkedin_palestine[,c(2:145)])
cols_list <- seq(2,145)
mean_value <- colMeans(cols)
plot(mean_value, xlab = "Time Slice Index",main = "LinkedIn_Microsoft (MEAN)")

model1 = lm(mean_value ~ cols_list )
#summary(linearRegression_model1)
abline(model1, lwd = 2, col = "Blue")