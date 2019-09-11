library(tm)
library(wordcloud)
#library(Rgraphviz)
library(RColorBrewer)
library(wordcloud)
library(topicmodels)
library(plyr)
library(ggplot2)
#library(RTextTools)
library(e1071)
library(caret)
install.packages('maxent')


news_full <- read.csv('News_Final.csv')
news_title_topic <- news_full[,c("Title","Topic")]
sample <- sample.int(n = nrow(news_title_topic), size = floor(.8*nrow(news_title_topic)), replace = F)
news_title_topic <- news_title_topic[sample, ]


set.seed(123) # Set Seed so that same sample can be reproduced in future also
# Now Selecting 75% of data as sample from total 'n' rows of the data  
sample <- sample.int(n = nrow(news_title_topic), size = floor(0.1*nrow(news_title_topic)), replace = F)
train <- news_title_topic[sample, ]
test  <- news_title_topic[-sample, ]


#processing for training
source <- VectorSource(train$Title)
corpus <- Corpus(source)

corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, removeWords, stopwords('english'))


mat <- DocumentTermMatrix(corpus)
mat <- removeSparseTerms(mat, 0.98) 
mat4 <- weightTfIdf(mat)
mat4 <- as.matrix(mat4)


#processing for testing
source1 <- VectorSource(test$Title)
corpus1 <- Corpus(source1)

corpus1 <- tm_map(corpus1, content_transformer(tolower))
corpus1 <- tm_map(corpus1, removeNumbers)
corpus1 <- tm_map(corpus1, removePunctuation)
corpus1 <- tm_map(corpus1, stripWhitespace)
#corpus1 <- tm_map(corpus1,stopwords('english'))

mat1 <- DocumentTermMatrix(corpus1)
mat1 <- removeSparseTerms(mat1, 0.98) 

mat5 <- weightTfIdf(mat1)
mat5 <- as.matrix(mat5)



classifier <- naiveBayes(mat4, train$Topic)
inspredicted <- predict(classifier,mat5)
table(as.character(test$Topic), as.character(inspredicted))


compare <- table(test$Topic, inspredicted)
PercentageAccuracy = sum(diag(compare)/sum(compare))*100

print(PercentageAccuracy)

