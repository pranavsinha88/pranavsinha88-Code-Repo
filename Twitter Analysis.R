ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages() [, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only=TRUE)
}

tweet_analysis_package <- c("tm" , "NLP" , "twitterMap", "openxlsx", "xlsx", "topicmodels", "lda", "sna", "twitteR",
                            "ROAuth", "ggplot2", "wordcloud", "igraph", "Rgraphviz","sentR", "qdap", "lazy","plyr",
                            "Rcpp")

ipak(tweet_analysis_package)

consumer_key <- "VeHLTFMWhVQjwIWfjHpZpTw0d"
consumer_secret <- "vhx2fR9Ac9Psj0CJruCiJTENBjMVcn5z4k1AbKIRD5cfMRaQyO"
access_token <- "809685705710407680-48HBe2AKpFAnPipBxmSGKZ4eyw2w5KK"
access_secret <- "2ZrjRAdZoywAU39AeeDM6rbIRWSXIAE6hcFE4DloyMcwO"

setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

tweets <- userTimeline("ABBgroupnews", n = 3200)

tweets_df <- do.call("rbind", lapply(tweets, as.data.frame))

write.csv(tweets_df,file="ABBtwitterList.csv")

n.tweet <- length(tweets)

tweets.df <- twListToDF(tweets)

tweets.df[100, c("id", "created", "screenName", "replyToSN", "favoriteCount", "retweetCount", 
                 "longitude", "latitude", "text")]

writeLines(strwrap(tweets.df$text[100], 60))

data.to.clean <- read.csv("ABBtwitterList.csv")
dataworking <- data.to.clean
names(dataworking)

# You need to use one variable at a time
varName = c("text")   ### THIS IS THE VARIABLE(CONTENTS) THAT NEEDS TO BE CLEANED
newVarName=c("cleansed")
removeEmails  = TRUE
removeUrls = TRUE
removePhoneNumber = TRUE
removeNumber = TRUE
removePunctuations = TRUE
removeStopwords = TRUE
stripWhitespaces = TRUE
stemDoc = TRUE

dataHandling <- function(varName, newVarName)
{
  
  # Regular expressions to match 1. Email, 2. URL, 3. Phone number
  #---------------------------------------------------------------
  
  email.expression <- "[A-Za-z0-9-]+[.A-Za-z0-9-]*@[A-Za-z0-9-]+(\\.com|\\.co.in|\\.net|\\.org|\\.info|\\.edu|\\.mil|\\.gov|\\.biz|\\.ws|\\.us|\\.tv|\\.cc|\\.aero|\\.arpa|\\.coop|\\.int|\\.jobs|\\.museum|\\.name|\\.pro)|\\.travel|\\.nato)"
  url.expression <- "(http://|https://|www.)[[:alnum:]~!#$%&+-=?,:/;._]*"
  phonenumber.expression <- "\\+?(\\d{2,3})[- ]?\\(?(\\d{3,5})\\)?[- ]?(\\d{3,5})[- ]?(\\d{4})?"
  
  # To read data from a single csv file and create a dataset of required column
  #----------------------------------------------------------------------------
  varIndex <- which(colnames(dataworking)==varName)
  
  corpus <- tolower(dataworking[,varIndex])
  
  # To remove emails from dataset
  #----------------------------------------------------------------------------
  
  if(removeEmails) {
    corpus <- gsub(email.expression,' ', corpus, ignore.case = TRUE)
  }
  
  # To remove urls from dataset
  #----------------------------------------------------------------------------
  
  if(removeUrls) {
    corpus <- gsub(url.expression,' ', corpus, ignore.case = TRUE)
  }
  
  # To remove phone numbers from dataset
  #----------------------------------------------------------------------------
  
  if(removePhoneNumber) {
    corpus <- gsub(phonenumber.expression,' ', corpus, ignore.case = TRUE)
  }
  
  # split into distinct words
  w <- strsplit( corpus , " " )
  corpus1<-c()
  for(n in 1:length(w)){
    # calculate the length of each word
    x <- nchar( w[[n]] )
    
    # keep only words with length 3 to 200
    y <- w[[n]][ x %in% 3:200 ]
    
    # string 'em back together
    y <- paste( unlist( y ), collapse = " " )
    corpus1<- c(corpus1,y)
  }
  
  
  # To covert dataset into a Corpus; required for executing 'tm_map' functions
  #----------------------------------------------------------------------------
  
  corpus <- Corpus(VectorSource(corpus1))
  
  # To remove stopwords from corpus
  #----------------------------------------------------------------------------
  
  if(removeStopwords) {
    corpus <- tm_map(corpus, removeWords, stopwords("english")[!(stopwords("english") %in% c("no","nor","not"))]) 
  }
  
  # To remove numbers from corpus
  #----------------------------------------------------------------------------
  
  if(removeNumber) {
    corpus <- tm_map(corpus, removeNumbers)
  }
  
  # To remove punctuations from corpus
  #----------------------------------------------------------------------------
  
  if(removePunctuations) {
    corpus <- tm_map(corpus, removePunctuation, preserve_intra_word_dashes = TRUE)
  }
  
  
  # To remove additional whitespaces from corpus
  #----------------------------------------------------------------------------
  
  if(stripWhitespaces) {
    corpus <- tm_map(corpus, stripWhitespace)
  }
  
  # To add data post pre-processing as a new column in the original dataset
  #----------------------------------------------------------------------------
  
  dataSize <- nrow(dataworking)
  newCol <- unlist(corpus[1:dataSize])
  
  
  x=NULL
  i=1
  
  while(i<=length(newCol))
  {
    x[i] = newCol[i]
    i=i+12
    
  }
  
  
  newCol=x[!is.na(x)]
  tmDataSetNew <- as.data.frame(newCol)
  
  newColIndex <- which(colnames(tmDataSetNew)=='newCol')
  
  colnames(tmDataSetNew) = paste(newVarName,varName,sep="_")
  
  # To clear all variable used
  #----------------------------------------------------------------------------
  
  rm(list=c("filePath","fileName","fileLoc", "varName","newVarName","tmDataSet"
            ,"corpus","newCol","newColIndex","varIndex","dataSize"))
  dataworking <- cbind(dataworking,tmDataSetNew)
  assign("dataworking",dataworking,envir=.GlobalEnv)
  
}

dataHandling(varName, newVarName)


dataworking$cleansed_text <- iconv(dataworking$cleansed_text, "latin1", "ASCII", sub="")
write.csv(dataworking,"cleansed_finaldata.csv")


mycorpus <- dataworking
names(mycorpus)

mycorpus$cleansed_text <- tolower(mycorpus$cleansed_text)

corpus <- Corpus(VectorSource(mycorpus$cleansed_text))

dtm_train <- DocumentTermMatrix(corpus)

sparse_tr <- as.matrix(removeSparseTerms(dtm_train, .995))

col_tr <- (colnames(sparse_tr))
col_tr1 <- col_tr

class(corpus)

CorpusCopy <- corpus

corpus <- tm_map(corpus, stemDocument)

writeLines(strwrap(corpus[[100]]$content, 60))

stemCompletion2 <- function(x, dictionary) {
  
  x <- unlist(strsplit(as.character(x), " "))
  
  x <- x[x != ""]
  
  x <- stemCompletion(x, dictionary=dictionary)
  
  x <- paste(x, sep="", collapse=" ")
  
  PlainTextDocument(stripWhitespace(x))
  
}

corpus <- lapply(corpus, stemCompletion2, dictionary=CorpusCopy)

corpus <- Corpus(VectorSource(corpus))

writeLines(strwrap(corpus[[100]]$content, 60))

tdm <- TermDocumentMatrix(corpus, control = list(wordLengths = c(1, Inf)))

freq.terms <- findFreqTerms(tdm, lowfreq = 7)

freq.terms

term.freq <- rowSums(as.matrix(tdm))
term.freq <- subset(term.freq, term.freq >=7)
df <- data.frame (term = names(term.freq), freq = term.freq)


ggplot(df, aes(x=term, y=freq)) + geom_bar(stat =  "identity") + xlab("Terms") +ylab("Count") + coord_flip() +
           theme(axis.text = element_text(size=10))

m <- as.matrix(tdm)
word.freq <- sort(rowSums(m), decreasing = T)

pal <- brewer.pal(9, "BuGn") [-(1:4)]
pal2 <- brewer.pal(8,"Dark2")

wordcloud(words = names(word.freq), freq = word.freq, min.freq = 3, random.order = F, colors = pal2)

findAssocs(tdm, "abb", 0.2)
findAssocs(tdm, "technologies", 0.2)


source("http://bioconductor.org/biocLite.R")
biocLite("Rgraphviz")

plot(tdm, term = freq.terms, corThreshold = 0.1, weighting = T)

dtm <- as.DocumentTermMatrix(tdm)
lda <- LDA(dtm, k = 8)
term <- terms(lda, 7)
(term <- apply(term, MARGIN = 2, paste, collapse = ", "))

topics <- topics(lda)
topics <- data.frame(date=as.Date(tweets.df$created), topic=topics)

ggplot(topics, aes(date, fill = term[topic])) + geom_density(position = "stack")

install.packages(c('devtools','curl'))
library("devtools")
require(devtools)
library(plyr)

install_github("sentiment140","okugami79")

library(sentiment)

library(sentiment)

sentiments <- sentiment(tweets.df$text)

table(sentiments$polarity)

sentiments$score <- 0
sentiments$score[sentiments$polarity == "positive"] <- 1
sentiments$score[sentiments$polarity == "negative"] <- -1
sentiments$date <- as.Date(tweets.df$created)

result <- aggregate(score ~ date, data = sentiments, sum)
plot(result, type = "l")

user <- getUser("ABBgroupnews")
user$toDataFrame()
friends <- user$getFriends()
followers <- user$getFollowers()
#followers2 <- followers[[1]]$getFollowers()

table(tweets.df$retweetCount)

selected <- which(tweets.df$retweetCount >= 38 )

dates <- strptime(tweets.df$created, format="%Y-%m-%d")

plot(x=dates, y=tweets.df$retweetCount, type="l", col="grey", xlab="Date", ylab="Times retweeted")

colors <- rainbow(10)[1:length(selected)]

points(dates[selected], tweets.df$retweetCount[selected], pch=19, col=colors)

text(dates[selected], tweets.df$retweetCount[selected], tweets.df$text[selected], col=colors, cex=.9)
   