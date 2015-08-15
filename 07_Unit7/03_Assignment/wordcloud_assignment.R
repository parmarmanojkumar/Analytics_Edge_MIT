rm(list = ls())
library(wordcloud)
tm_pre_process_dtm <- function(data, enaremoveWord = 0){
        library(tm)
        corpusTitle = Corpus(VectorSource(data))
        corpusTitle = tm_map(corpusTitle, tolower)
        corpusTitle = tm_map(corpusTitle, PlainTextDocument)
        corpusTitle = tm_map(corpusTitle, removePunctuation)
        if (enaremoveWord ==1){
                corpusTitle = tm_map(corpusTitle, removeWords, c(stopwords("english"),"apple"))  
        } else {
                corpusTitle = tm_map(corpusTitle, removeWords, stopwords("english"))
        }
        dtmTitle = DocumentTermMatrix(corpusTitle)
        dtmTitle
}
#problem1
tweets = read.csv("tweets.csv", stringsAsFactors = F)
str(tweets)
dtm = tm_pre_process_dtm(tweets$Tweet)
dtm
allTweets = as.data.frame(as.matrix(dtm))
#problem2

wordcloud(colnames(allTweets), colSums(allTweets), scale=c(2, .25))
dtmremoved = tm_pre_process_dtm(tweets$Tweet, 1)
dtmremoved
allTweets = as.data.frame(as.matrix(dtmremoved))
wordcloud(colnames(allTweets), colSums(allTweets), scale=c(2, .25))
?wordcloud
negativeTweets = subset(allTweets, tweets$Avg <= -1)
wordcloud(colnames(negativeTweets), colSums(negativeTweets))
colors=brewer.pal(9, "Blues")
wordcloud(colnames(allTweets), colSums(allTweets), scale=c(2, .25), random.order = F, colors = colors)
