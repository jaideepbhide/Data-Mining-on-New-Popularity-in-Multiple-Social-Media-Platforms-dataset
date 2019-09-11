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
#install.packages('maxent')


news_full <- read.csv('News_Final.csv')
news_title_topic <- news_full[,c("Title","Topic")]
sample <- sample.int(n = nrow(news_title_topic), size = floor(.75*nrow(news_title_topic)), replace = F)
news_title_topic <- news_title_topic[sample, ]

source <- VectorSource(news_title_topic$Title)
corpus <- Corpus(source)

corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, removeWords, stopwords('english'))


mat <- DocumentTermMatrix(corpus)
mat <- removeSparseTerms(mat, 0.98) 
""

mat4 <- weightTfIdf(mat)
mat4 <- as.matrix(mat4)

container <- create_container(mat, news_title_topic$Topic, trainSize=1:67132,testSize=67133:69929, virgin=FALSE)
model <- train_model(container, 'SVM',kernel='linear')
results <- classify_model(container, model)
table(as.character(x.rand$V1[569:710]), as.character(results[,"SVM_LABEL"]))