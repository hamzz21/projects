## Installing the packages 
install.packages("tm")
install.packages("wordcloud")
install.packages("RColorBrewer")
install.packages("SnowballC")
install.packages("syuzhet")
install.packages("ggplot2")

# Libraries in R Studio
library(tm)
library(wordcloud)
library(RColorBrewer)
library(SnowballC)
library(syuzhet)
library(NLP)
library(ggplot2)

# Reading the CSV files 
tweet_NZNationalParty.df <- read.csv("./Political Parties\\NZNationalParty_tweets.csv")
tweet_NZGreen.df <- read.csv("./Political Parties\\NZGreens_tweets.csv")
tweet_NZLabourParty.df <- read.csv("./Political Parties\\nzlabour_tweets.csv")

## This displays the tweets 
head(tweet_NZNationalParty.df)
head(tweet_NZGreen.df)
head(tweet_NZLabourParty.df)

## Shows the content within the data frame's text field 
## NZNationalParty
head(tweet_NZNationalParty.df$text)
tweet_NZNationalParty.df2<- tweet_NZNationalParty.df$text
##NZGreen
head(tweet_NZGreen.df$text)
tweet_NZGreen.df2<- tweet_NZGreen.df$text
##NZLabourParty
head(tweet_NZLabourParty.df$text)
tweet_NZLabourParty.df2<- tweet_NZLabourParty.df$text

##'find and replace' function used to remove irrelevant material

## NZNationalParty
tweet_NZNationalParty.df2 <-iconv(tweet_NZNationalParty.df2, "UTF-8", "UTF-8", sub = "byte")
tweet_NZNationalParty.df2 <- gsub("http\\S+|www\\.[^\\S]+|#\\S+|@\\S+|<[^>]+>","",tweet_NZNationalParty.df2)
tweet_NZNationalParty.df2 <- gsub("[[:cntrl:]]|[[:punct:]]|[^a-zA-z0-9\\S]"," ",tweet_NZNationalParty.df2)
head(tweet_NZNationalParty.df2)
## NZGreen
tweet_NZGreen.df2 <-iconv(tweet_NZGreen.df2, "UTF-8", "UTF-8", sub = "byte")
tweet_NZGreen.df2 <- gsub("http\\S+|www\\.[^\\S]+|#\\S+|@\\S+|<[^>]+>","",tweet_NZGreen.df2)
tweet_NZGreen.df2 <- gsub("[[:cntrl:]]|[[:punct:]]|[^a-zA-z0-9\\S]"," ",tweet_NZGreen.df2)
head(tweet_NZGreen.df2)
##NZLabourParty
tweet_NZLabourParty.df2 <-iconv(tweet_NZLabourParty.df2, "UTF-8", "UTF-8", sub = "byte")
tweet_NZLabourParty.df2 <- gsub("http\\S+|www\\.[^\\S]+|#\\S+|@\\S+|<[^>]+>","",tweet_NZLabourParty.df2)
tweet_NZLabourParty.df2<- gsub("[[:cntrl:]]|[[:punct:]]|[^a-zA-z0-9\\S]"," ",tweet_NZLabourParty.df2)
head(tweet_NZLabourParty.df2)

## Ensuring to transform data frame into a vector representation 
word_NZNationalParty.df <-as.vector(tweet_NZNationalParty.df2)
word_NZGreen.df <-as.vector(tweet_NZGreen.df2)
word_NZLabourParty.df <-as.vector(tweet_NZLabourParty.df2)

#To evaluate the emotions expressed in tweets, doing sentiment analysis. 
#This helps to score tweets based on the emotional content. 
emotion_NZNationalParty.df <-get_nrc_sentiment(word_NZNationalParty.df)
emotion_NZGreen.df <-get_nrc_sentiment(word_NZGreen.df)
emotion_NZLabourParty.df <-get_nrc_sentiment(word_NZLabourParty.df)

#Combining the scores of each party
emotion_NZNationalParty.df2 <-cbind(tweet_NZNationalParty.df2, emotion_NZNationalParty.df)
head(emotion_NZNationalParty.df2)
emotion_NZGreen.df2 <-cbind(tweet_NZGreen.df2, emotion_NZGreen.df)
head(emotion_NZGreen.df2)
emotion_NZLabourParty.df2 <-cbind(tweet_NZLabourParty.df2,emotion_NZLabourParty.df)
head(emotion_NZLabourParty.df2)

##Evaluating the tweets by classifying them as positive or negative based on the sentiment
sent_NZNationalParty.value <-get_sentiment(word_NZNationalParty.df)
sent_NZGreen.value <-get_sentiment(word_NZGreen.df)
sent_NZLabourParty.value <-get_sentiment(word_NZLabourParty.df)


# Filtering tweets based on their sentiment can be categorized into three groups:
#positive, negative, and neutral

#Positive 
positive_NZNationalParty<- word_NZNationalParty.df[sent_NZNationalParty.value>0]
head(positive_NZNationalParty)
positive_NZGreen <- word_NZGreen.df[sent_NZGreen.value>0]
head(positive_NZGreen)
positive_NZLabourParty <- word_NZLabourParty.df[sent_NZLabourParty.value>0]
head(positive_NZLabourParty)

#Negative 
negative_NZNationalParty <- word_NZNationalParty.df[sent_NZNationalParty.value<0]
head(negative_NZNationalParty)
negative_NZGreen <- word_NZGreen.df[sent_NZGreen.value<0]
head(negative_NZGreen)
negative_NZLabourParty <- word_NZLabourParty.df[sent_NZLabourParty.value<0]
head(negative_NZLabourParty)

#Neutral 
neutral_NZNationalParty <- word_NZNationalParty.df[sent_NZNationalParty.value == 0]
head(neutral_NZNationalParty)
neutral_NZGreen <- word_NZGreen.df[sent_NZGreen.value == 0]
head(neutral_NZGreen)
neutral_NZLabourParty <- word_NZLabourParty.df[sent_NZLabourParty.value == 0]
head(neutral_NZLabourParty)

##Counting the positive, negative and neutral tweets 

#The number of positive tweets
pos_NZNationalParty <- length(positive_NZNationalParty)
pos_NZGreen <- length(positive_NZGreen)
pos_NZLabourParty <- length(positive_NZLabourParty)

#The number of negative tweets
neg_NZNationalParty <- length(negative_NZNationalParty)
neg_NZGreen <- length(negative_NZGreen)
neg_NZLabourParty <- length(negative_NZLabourParty)

#The number of neutral tweets
neu_NZNationalParty <- length(neutral_NZNationalParty)
neu_NZGreen <- length(neutral_NZGreen)
neu_NZLabourParty <- length(neutral_NZLabourParty)

##PIE CHART

colors <- c("pink", "lightblue", "lightgreen")
#########
# Helps with the "figure margins too large" error
par(mar = c(2, 2, 2, 2))
###############
#NZNationalParty
sentiments_NZNationalParty <- c (neg_NZNationalParty,pos_NZNationalParty, neu_NZNationalParty)
names (sentiments_NZNationalParty) <- c ("Negative", "Positive", "Neutral")
pie(sentiments_NZNationalParty, main = "Sentiment Analysis of NZ National Party",col = colors,radius = 1.0)

#NZGreen

sentiments_NZGreen <- c (neg_NZGreen,pos_NZGreen, neu_NZGreen)
names (sentiments_NZGreen) <- c ("Negative", "Positive", "Neutral")
pie(sentiments_NZGreen, main = "Sentiment Analysis of NZ Green Party",col = colors,radius = 1.0)

#NZLabourParty
sentiments_NZLabourParty <- c (neg_NZLabourParty,pos_NZLabourParty ,neu_NZLabourParty)
names (sentiments_NZLabourParty) <- c ("Negative", "Positive", "Neutral")
pie(sentiments_NZLabourParty, main = "Sentiment Analysis of NZ Labour Party",col = colors,radius = 1.0)

##BAR PLOTS

#Selecting colours for the bar graph
bar_colors <-c("lavender","lightyellow","lightcyan")

#NZNationalParty
barplot (c(neg_NZNationalParty,pos_NZNationalParty,neu_NZNationalParty), 
        main="Sentiment Analysis Bar Chart of NZ National Party", xlab="Sentiment", ylab="Count", 
        names.arg=c("Negative", "Positive", "Neutral"), col= bar_colors)

#NZGreen
barplot (c(neg_NZGreen,pos_NZGreen , neu_NZGreen), 
         main="Sentiment Analysis Bar Chart of NZ Green Party", xlab="Sentiment", ylab="Count", 
         names.arg=c("Negative", "Positive", "Neutral"), col= bar_colors)

#NZLabourParty
barplot (c(neg_NZLabourParty,pos_NZLabourParty, neu_NZLabourParty), 
         main="Sentiment Analysis Bar Chart of NZ Labour Party", xlab="Sentiment", ylab="Count", 
         names.arg=c("Negative", "Positive", "Neutral"), col= bar_colors)



# Choosing and presenting the most positive sentiment

Mostpos_NZNationalParty.positive <- word_NZNationalParty.df[sent_NZNationalParty.value == max(sent_NZNationalParty.value)]
Mostpos_NZNationalParty.positive

Mostpos_NZGreen.positive <- word_NZGreen.df[sent_NZGreen.value == max(sent_NZGreen.value)]
Mostpos_NZGreen.positive

Mostpos_NZLabourParty.positive <- word_NZLabourParty.df[sent_NZLabourParty.value == max(sent_NZLabourParty.value)]
Mostpos_NZLabourParty.positive

# Choosing and presenting the most negative sentiment

Mostneg_NZNationalParty.negative <- word_NZNationalParty.df[sent_NZNationalParty.value == min(sent_NZNationalParty.value)]
Mostneg_NZNationalParty.negative

Mostneg_NZGreen.negative <- word_NZGreen.df[sent_NZGreen.value == min(sent_NZGreen.value)]
Mostneg_NZGreen.negative

Mostneg_NZLabourParty.negative <- word_NZLabourParty.df[sent_NZLabourParty.value == min(sent_NZLabourParty.value)]
Mostneg_NZLabourParty.negative

# Neutral Tweets Displayed
head(neu_NZNationalParty)
head(neu_NZGreen)
head(neu_NZLabourParty)


#Generating a word collection from the cleaned tweet data frame

NZNationalParty_corpus <- Corpus(VectorSource(word_NZNationalParty.df))
NZGreen_corpus <- Corpus(VectorSource(word_NZGreen.df))
NZLabourParty_corpus <- Corpus(VectorSource(word_NZLabourParty.df))

#Creating an Term Document Matrix
#NZNationalParty

tdm_NZNationalParty <- TermDocumentMatrix(NZNationalParty_corpus,
                                 control = list(removePunctuation = TRUE, wordLengths=c(5, 15),
                                                stopwords = c("National","party","government","Labour",stopwords("english")),
                                                removeNumbers = TRUE, tolower = TRUE))

tdm_NZNationalParty.matrix <- as.matrix(tdm_NZNationalParty)
NZNationalParty_freqs <- sort(rowSums(tdm_NZNationalParty.matrix), decreasing=TRUE) 

NZNationalParty_dm <- data.frame(word=names(NZNationalParty_freqs), freq=NZNationalParty_freqs)

wordcloud(NZNationalParty_dm$word, NZNationalParty_dm$freq, min.freq = 1, max.words=50,
          random.order=FALSE, colors=brewer.pal(9, "Spectral"))

#NZGreen
tdm_NZGreen <- TermDocumentMatrix(NZGreen_corpus,
                                          control = list(removePunctuation = TRUE, wordLengths=c(5, 15),
                                                         stopwords = c("Party", "government","Green","govt",stopwords("english")),
                                                         removeNumbers = TRUE, tolower = TRUE))

tdm_NZGreen.matrix <- as.matrix(tdm_NZGreen)
NZGreen_freqs <- sort(rowSums(tdm_NZGreen.matrix), decreasing=TRUE) 

NZGreen_dm <- data.frame(word=names(NZGreen_freqs), freq=NZGreen_freqs)

wordcloud(NZGreen_dm$word, NZGreen_dm$freq, min.freq = 2, max.words=50,
          random.order=FALSE, colors=brewer.pal(9, "Spectral"))

#NZLabourParty

tdm_NZLabourParty <- TermDocumentMatrix(NZLabourParty_corpus,
                                          control = list(removePunctuation = TRUE, wordLengths=c(5, 15),
                                                         stopwords = c("Labour", "government","party","tweets","kiwis",stopwords("english")),
                                                         removeNumbers = TRUE, tolower = TRUE))

tdm_NZLabourParty.matrix <- as.matrix(tdm_NZLabourParty)
NZLabourParty_freqs <- sort(rowSums(tdm_NZLabourParty.matrix), decreasing=TRUE) 

NZLabourParty_dm <- data.frame(word=names(NZLabourParty_freqs), freq=NZLabourParty_freqs)

wordcloud(NZLabourParty_dm$word, NZLabourParty_dm$freq, min.freq = 2, max.words=50,
          random.order=FALSE, colors=brewer.pal(9, "Spectral"))



