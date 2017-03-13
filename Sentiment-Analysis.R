
packages <- c("ggplot2", "lubridate", "RTextTools", "bit64", "stringr", 
              "date", "scales", "textcat", "NLP", "SnowballC", "textcat",
              "data.table", "tm.plugin.dc", "qdapRegex", "wordcloud", "wordcloud2")
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages()))) 
}

library(data.table)
library(textcat)
library(lubridate)
library(ggplot2)
library(bit64)
library(stringr)
library(date)
library(scales)
library(tm)
library(tm.plugin.dc)
library(SnowballC)
library(RTextTools) 
library(qdapRegex)
library(wordcloud)
library(wordcloud2)
#----------------------------------------------------------------
printf <- function(...) cat(sprintf(...))
#----------------------------------------------------------------
load_dataset <- function() {
  #  tweets <- data.table::fread("tweets.csv", na.strings="NA", colClasses=NULL, nrows=4800000)
  tweets <- data.table::fread("tweets.csv", na.strings="NA", colClasses=NULL, nrows=4800000)
  tweets <- cbind(timestamp=as.POSIXct(paste (tweets$tweet_time, tweets$tweet_date, sep = " ") , format="%H:%M:%S %Y-%m-%d"), tweets)
  tweets <- cbind(date=as.POSIXct(tweets$tweet_date, format="%Y-%m-%d"), tweets)
  tweets$tweet_time <- NULL
  tweets$tweet_date <- NULL
  tweets
}
#----------------------------------------------------------------
remove_urls <- function(tweets)
{
  tweets$content <- gsub("?(f|ht)tp(s?)://\\S+\\s*", "", tweets$content)
  tweets
}
#----------------------------------------------------------------
remove_usernames <- function(tweets)
{
  tweets$content <- gsub("?(@)\\S+\\s*", "", tweets$content)
  tweets
}
#----------------------------------------------------------------
remove_hash_tags <- function(tweets)
{
  tweets$content <- gsub("?(#)\\S+\\s*", "", tweets$content)
  tweets
}
#----------------------------------------------------------------
remove_hash_tag_sign <- function(tweets)
{
  tweets$content <- gsub("?(#)", "", tweets$content)
  tweets
}
#----------------------------------------------------------------
remove_numbers <- function(tweets)
{
  tweets$content <- gsub("[[:digit:]]", "", tweets$content)
  tweets
}

#----------------------------------------------------------------
extract_emoticons <- function(tweets)
{
  #tweets = data.frame(content=c("hello saeed 1 :)  and maybe  2 :( and  3 :(( and 4 :)) and 5 :o :))  :("))
  
  tweets$content <- gsub("?:[)][)]", "laugh", tweets$content)
  tweets$content <- gsub("?:[)]", "smile", tweets$content)
  tweets$content <- gsub("?:o", "wonder", tweets$content)
  tweets$content <- gsub("?:[(][(]", "cry", tweets$content)
  tweets$content <- gsub("?:[(]", "sad", tweets$content)
  tweets$conten <- rm_emoticon(tweets$content, extract = FALSE, trim = TRUE, clean = TRUE,
              pattern = "@rm_emoticon", replacement = "", 
              dictionary = getOption("regex.library"))
  tweets
}


#----------------------------------------------------------------
detect_tweets_language <- function(tweets) {
  clean.tweets <- remove_urls(tweets)
  clean.tweets <- remove_usernames(clean.tweets)
  clean.tweets <- remove_hash_tags(clean.tweets)
  clean.tweets <- remove_numbers(clean.tweets)
  
  tweets.language = textcat(clean.tweets$content)
  tweets <- cbind(language=tweets.language , tweets)
  tweets
}
#----------------------------------------------------------------
load_keywords <- function() {
  keywords <- read.csv(file="strings", header=FALSE)
  names(keywords) <- c("keyword")
  keywords
}

#----------------------------------------------------------------
find_tweet_frequency <- function(tweets) {
  
  tweets <- cbind(as.POSIXct(paste (tweets$tweet_time, tweets$tweet_date, sep = " ") , format="%H:%M:%S %Y-%m-%d"), tweets)
  tweets <- cbind(as.POSIXct(tweets$tweet_date, format="%Y-%m-%d"), tweets)
  tweets$tweet_time <- NULL
  tweets$tweet_date <- NULL
  tweets
}
#----------------------------------------------------------------
filter_dataset_by_keyword <- function( tweets, keyword )
{
  # Replace space with ".*" to build a regex expression that will search for all keywords
  keyword.regex <- str_replace_all(keyword," ",".*")
  search.result <- with(tweets , str_detect(tweets$content, keyword.regex))
  tweets[search.result,drop=TRUE]
}

#----------------------------------------------------------------
find_keyword_frequencies <- function( tweets, keywords )
{
  keywords.frequncy = list()
  for(i in 1:length(keywords$keyword)){
    keyword.regex <- str_replace_all(tolower(keywords$keyword[i])," ",".*")
    search.result <- with(tweets , str_detect(tolower(tweets$content), keyword.regex))
    keywords.frequncy[[i]] <- c( keyword = keywords$keyword[i], count = sum( unlist(search.result) ) )
    printf("Counting frequency of keyword:'%s'   \t %s\n", keywords$keyword[i], sum(unlist(search.result)))
  }
  keywords.frequncy
}

#----------------------------------------------------------------
find_frequent_terms <- function( tweets.corpus, lowfreq )
{
  tdm <- TermDocumentMatrix(tweets.corpus, control = list(wordLengths = c(3,Inf)))
  
  freq.term <- findFreqTerms(tdm, lowfreq = lowfreq)
  term.freq <- rowSums(as.matrix(tdm)) 
  term.freq <- subset(term.freq, term.freq >= lowfreq)
  df <- data.frame(term = names(term.freq), freq = term.freq)
  base <- ggplot(df, aes(x = term, y = freq)) + geom_bar(stat = "identity") + 
    xlab("Most Frequent Terms") + 
    ylab("Count") + 
    coord_flip()
  base
}
#----------------------------------------------------------------
create_matrix <- function(textColumns, language="english", minDocFreq=1, minWordLength=3, removeNumbers=TRUE, removePunctuation=TRUE, removeSparseTerms=0, removeStopwords=TRUE, stemWords=FALSE, stripWhitespace=TRUE, toLower=TRUE, weighting=weightTf) {
  
  stem_words <- function(x) {
    split <- strsplit(x," ")
    return(wordStem(split[[1]],language=language))
  }
  
  control <- list(language=language,tolower=toLower,removeNumbers=removeNumbers,removePunctuation=removePunctuation,stripWhitespace=stripWhitespace,minWordLength=minWordLength,stopwords=removeStopwords,minDocFreq=minDocFreq,weighting=weighting)
  
  if (stemWords == TRUE) control <- append(control,list(stemming=stem_words),after=6)
  
  trainingColumn <- apply(as.matrix(textColumns),1,paste,collapse=" ")
  trainingColumn <- sapply(as.vector(trainingColumn,mode="character"),iconv,to="UTF8",sub="byte")
  
  corpus <- Corpus(VectorSource(trainingColumn),readerControl=list(language=language))
  matrix <- DocumentTermMatrix(corpus,control=control);
  if (removeSparseTerms > 0) matrix <- removeSparseTerms(matrix,removeSparseTerms)
  
  gc()
  return(matrix)
}
#----------------------------------------------------------------
classify_emotion <- function(textColumns, algorithm="bayes", prior=1.0, verbose=FALSE,...) {
  matrix <- create_matrix(textColumns,...)
  #lexicon <- read.csv(system.file("data/emotions.csv.gz",package="sentiment"),header=FALSE)
  lexicon <- read.csv(file = "data/emotions.csv.gz",header=FALSE)
  
  counts <- list(anger=length(which(lexicon[,2]=="anger")),disgust=length(which(lexicon[,2]=="disgust")),fear=length(which(lexicon[,2]=="fear")),joy=length(which(lexicon[,2]=="joy")),sadness=length(which(lexicon[,2]=="sadness")),surprise=length(which(lexicon[,2]=="surprise")),total=nrow(lexicon))
  documents <- c()
  
  for (i in 1:nrow(matrix)) {
    if (verbose) print(paste("DOCUMENT",i))
    scores <- list(anger=0,disgust=0,fear=0,joy=0,sadness=0,surprise=0)
    doc <- matrix[i,]
    words <- findFreqTerms(doc,lowfreq=1)
    
    for (word in words) {
      for (key in names(scores)) {
        emotions <- lexicon[which(lexicon[,2]==key),]
        index <- pmatch(word,emotions[,1],nomatch=0)
        if (index > 0) {
          entry <- emotions[index,]
          
          category <- as.character(entry[[2]])
          count <- counts[[category]]
          
          score <- 1.0
          if (algorithm=="bayes") score <- abs(log(score*prior/count))
          
          if (verbose) {
            print(paste("WORD:",word,"CAT:",category,"SCORE:",score))
          }
          
          scores[[category]] <- scores[[category]]+score
        }
      }
    }
    
    if (algorithm=="bayes") {
      for (key in names(scores)) {
        count <- counts[[key]]
        total <- counts[["total"]]
        score <- abs(log(count/total))
        scores[[key]] <- scores[[key]]+score
      }
    } else {
      for (key in names(scores)) {
        scores[[key]] <- scores[[key]]+0.000001
      }
    }
    
    best_fit <- names(scores)[which.max(unlist(scores))]
    if (best_fit == "disgust" && as.numeric(unlist(scores[2]))-3.09234 < .01) best_fit <- NA
    documents <- rbind(documents,c(scores$anger,scores$disgust,scores$fear,scores$joy,scores$sadness,scores$surprise,best_fit))
  }
  
  colnames(documents) <- c("ANGER","DISGUST","FEAR","JOY","SADNESS","SURPRISE","BEST_FIT")
  return(documents)
}
#----------------------------------------------------------------
classify_polarity <- function(textColumns,algorithm="bayes",pstrong=0.5,pweak=1.0,prior=1.0,verbose=FALSE,...) {
  matrix <- create_matrix(textColumns,...)
  lexicon <- read.csv(file = "data/subjectivity.csv.gz",header=FALSE)
  
  
  counts <- list(positive=length(which(lexicon[,3]=="positive")),negative=length(which(lexicon[,3]=="negative")),total=nrow(lexicon))
  documents <- c()
  
  for (i in 1:nrow(matrix)) {
    if (verbose) print(paste("DOCUMENT",i))
    scores <- list(positive=0,negative=0)
    doc <- matrix[i,]
    words <- findFreqTerms(doc,lowfreq=1)
    
    for (word in words) {
      index <- pmatch(word,lexicon[,1],nomatch=0)
      if (index > 0) {
        entry <- lexicon[index,]
        
        polarity <- as.character(entry[[2]])
        category <- as.character(entry[[3]])
        count <- counts[[category]]
        
        score <- pweak
        if (polarity == "strongsubj") score <- pstrong
        if (algorithm=="bayes") score <- abs(log(score*prior/count))
        
        if (verbose) {
          print(paste("WORD:",word,"CAT:",category,"POL:",polarity,"SCORE:",score))
        }
        
        scores[[category]] <- scores[[category]]+score
      }		
    }
    
    if (algorithm=="bayes") {
      for (key in names(scores)) {
        count <- counts[[key]]
        total <- counts[["total"]]
        score <- abs(log(count/total))
        scores[[key]] <- scores[[key]]+score
      }
    } else {
      for (key in names(scores)) {
        scores[[key]] <- scores[[key]]+0.000001
      }
    }
    
    best_fit <- names(scores)[which.max(unlist(scores))]
    ratio <- as.integer(abs(scores$positive/scores$negative))
    if (ratio==1) best_fit <- "neutral"
    documents <- rbind(documents,c(scores$positive,scores$negative,abs(scores$positive/scores$negative),best_fit))
    if (verbose) {
      print(paste("POS:",scores$positive,"NEG:",scores$negative,"RATIO:",abs(scores$positive/scores$negative)))
      cat("\n")
    }
  }
  
  colnames(documents) <- c("POS","NEG","POS/NEG","BEST_FIT")
  return(documents)
}

#----------------------------------------------------------------


perform_sentiment_analysis <- function(tweets, keyword) {
  #------------------ NLP -----------------------
  #------------- Creating Corpus-----------------
  
  # create a subset of tweets, since nlp engine handles datasets larger than 300K rows very slowly
  if (nrow(tweets) > 10000)
    tweets <- tweets[sample(nrow(tweets), 10000), ]
  
  #tweets.corpus <- as.DistributedCorpus( VCorpus( VectorSource( tweets.sample$content ) ) )
  tweets.corpus <- Corpus(VectorSource(tweets$content))
  
  # convert to lowercase
  tweets.corpus <- tm_map(tweets.corpus, content_transformer(tolower) )
  
  # remove punctuation
  tweets.corpus <- tm_map(tweets.corpus, content_transformer(removePunctuation) )
  
  # remove numbers
  tweets.corpus <- tm_map(tweets.corpus, content_transformer(removeNumbers) )
  
  # add three extra stop words: 'via', ...
  tweets.stop.words <- c(stopwords("english"),"via","apear","will","per","get","says")
  
  #remove stopwords from corpus
  tweets.corpus <- tm_map(tweets.corpus, content_transformer(removeWords), tweets.stop.words)
  
  #----------- Langugae Detection ---------------
  tweets <- detect_tweets_language(tweets)
  #View(tweets)
  
  
  #----------- Remove Non-English tweets ---------------
  tweets <- tweets[tweets$language == "english",]
  #View(tweets)
  
  
  test = data.frame(content=c("hello saeed :)  and maybe :( "))
  tweets <- extract_emoticons(tweets = tweets)
  
  #----------- Sensivity Analysis ---------------
  emotions <- classify_emotion(tweets)
  
  emotions <- data.frame(emotions)

  # build a corpus
  emotions.corpus <- Corpus(VectorSource(emotions$BEST_FIT))
  
  # build a term-document matrix
  emotions.corpus.dtm <- TermDocumentMatrix(emotions.corpus)
  
  # inspect the document-term matrix
  emotions.corpus.dtm
  
  # inspect most popular words
  emotions.corpus.dtm.m <- as.matrix(emotions.corpus.dtm)
  emotions.corpus.dtm.v <- sort(rowSums(emotions.corpus.dtm.m),decreasing=TRUE)
  emotions.corpus.dtm.df <- data.frame(word = names(emotions.corpus.dtm.v),freq=emotions.corpus.dtm.v)
  #head(emotions.corpus.dtm.df, 10)
  
  
  
  #----------- Polarity Analysis ---------------
  polarities <- classify_polarity(tweets)
  
  polarities <- data.frame(polarities)
  
  # build a corpus
  polarities.corpus <- Corpus(VectorSource(polarities$BEST_FIT))
  
  # build a term-document matrix
  polarities.corpus.dtm <- TermDocumentMatrix(polarities.corpus)
  
  # inspect the document-term matrix
  polarities.corpus.dtm
  
  # inspect most popular words
  polarities.corpus.dtm.m <- as.matrix(polarities.corpus.dtm)
  polarities.corpus.dtm.v <- sort(rowSums(polarities.corpus.dtm.m),decreasing=TRUE)
  polarities.corpus.dtm.df <- data.frame(word = names(polarities.corpus.dtm.v),freq=polarities.corpus.dtm.v)
  #head(emotions.corpus.dtm.df, 10)
  
  set.seed(1234)
  par(mfrow=c(2,2))
  if (nrow(emotions.corpus.dtm.df) > 2)
  {
    barplot(emotions.corpus.dtm.df[1:nrow(emotions.corpus.dtm.df),]$freq, las = 2, names.arg = emotions.corpus.dtm.df[1:nrow(emotions.corpus.dtm.df),]$word,
          col ="lightblue", main = paste("Tweet's Emotion \n '" , keyword,"'"),
          ylab = "Frequency")
  
    wordcloud(words = emotions.corpus.dtm.df$word, freq = emotions.corpus.dtm.df$freq, min.freq = 1,
            max.words=1000, random.order=FALSE, rot.per=0.35, 
            colors=brewer.pal(8, "Dark2"))
  
  } else {
    printf("Not Enough data to prepare sentminet analysis plot[%d] for keyword '%s' , number of records:%d\n",i , keywords$keyword[i], nrow(emotions.corpus.dtm.df) )
    plot(c(0),main="NO Data", ylab="no data", xlab="no data")
    plot(c(0),main="NO Data", ylab="no data", xlab="no data")
  }
  
  if (nrow(polarities.corpus.dtm.df) > 2)
  {
    barplot(polarities.corpus.dtm.df[1:nrow(polarities.corpus.dtm.df),]$freq, las = 2, names.arg = polarities.corpus.dtm.df[1:nrow(polarities.corpus.dtm.df),]$word,
            col ="lightblue", main = paste("Tweet's Polarity \n '" , keyword,"'"),
            ylab = "Frequency")
    
    wordcloud(words = polarities.corpus.dtm.df$word, freq = polarities.corpus.dtm.df$freq, min.freq = 1,
              max.words=1000, random.order=FALSE, rot.per=0.35, 
              colors=brewer.pal(8, "Dark2"))
    
  } else {
    printf("Not Enough data to prepare polarity analysis plot[%d] for keyword '%s' , number of records:%d\n",i , keywords$keyword[i], nrow(emotions.corpus.dtm.df) )
    plot(c(0),main="NO Data", ylab="no data", xlab="no data")
    plot(c(0),main="NO Data", ylab="no data", xlab="no data")
  }
}

#----------------------------------------------------------------
#--------------- Main Code -------------------


#-------------- Loading Data -----------------

printf("Loading dataset, it might take a while ...\n")
tweets <- load_dataset() 
printf("Dataset sucessfully loaded.\n")

#--------------- Load Keywords ----------------
printf("Loading keywords ...\n")
keywords <- load_keywords()
keywords

#-------------- Text Cleaning -----------------
tweets <- remove_urls(tweets)
tweets <- remove_usernames(tweets)
tweets <- remove_hash_tag_sign(tweets)
tweets <- remove_numbers(tweets)
tweets <- extract_emoticons(tweets)
#tweets <- tweets[tweets$content != NULL,]
#tweets <- subset(tweets, content != NA)

#--------------- Clustering -------------------
printf("Filtering the tweets dataset based on the provided keywords...\n")
segmented.tweets.list = list()
for(i in 1:length(keywords$keyword)){
  segmented.tweets.list[[i]] <- filter_dataset_by_keyword( tweets, keywords$keyword[i] )
  printf("Preparing dataset %d base on '%s' keyword, %d tweets added\n",i , keywords$keyword[i], nrow(segmented.tweets.list[[i]] ))
}

#--------------- Performing Sentiment Analysis -------------------
#draw adummy plot, this isneccessary for saving plots
par( mfrow = c( 2, 2 ) )
plot(c(0),main="NO Data", ylab="no data", xlab="no data")
for(i in 1:length(keywords$keyword)){
  printf("Generating Histogram %d \n",i)
  if ( nrow(segmented.tweets.list[[i]] ) > 0)
  {
    printf("Saving Histogram Image %d \n",i)
    sentiment.analysis.result.filename = paste("plots\\sentiment-analysis-",i,".png",sep = "")
    png(file = sentiment.analysis.result.filename);
    perform_sentiment_analysis(segmented.tweets.list[[i]], keywords$keyword[i])
    dev.off()
  }
  else
  {
    printf("Segment %d is empty\n",i)
  }
}

