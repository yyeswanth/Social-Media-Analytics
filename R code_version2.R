#Data Knights -- Social Media Analytics Using NLP

#**Before executing the code please set your working directory
#Eg: setwd("C:\Spring Semester\Data Analytics using R\Project\")
#Place the given Negative and Positive words files in the Working directory

#TwitteR package includes set of text analytics functions. In this project, this library
#is primarily used to provide interface to Twitter web API.
if(!(require("twitteR")))
{
  install.packages("twitteR", dependencies = TRUE)
  library(twitteR)
}

#TM package is a framework of text mining applications which provides variety of functions
# to process raw tweets into high quality information from text.
if(!(require("tm")))
{
  install.packages("tm", dependencies = TRUE)
  library(tm)
}

#HTTR package is a collection of tools for working with URLs and HTTP while accessing online data.
if(!(require("httr")))
{
  install.packages("httr", dependencies = TRUE)
  library(httr)
}

# This package is used to create wordclouds.
if(!(require("wordcloud")))
{
  install.packages("wordcloud", dependencies = TRUE)
  library(wordcloud)
}


#The stringr package provides string functions for string processing. It provides 
#additional functionalities that is missing in R base package
if(!(require("stringr")))
{
  install.packages("stringr", dependencies = TRUE)
  library(stringr)
}

# PLYR package is used for split-apply-combine (SAC) procedures i.e.applying functions over grouped data.
if(!(require("plyr")))
{
  install.packages("plyr", dependencies = TRUE)
  library(plyr)
}


#topicmodels package is used to perform topic modelling using LDA and CTM algorithms
if(!(require("topicmodels")))
{
  install.packages("topicmodels", dependencies = TRUE)
  library(topicmodels)
}

  
# Set up connection to Twitter search API for downloading tweets.
#Please refer the report for step by step procedure to get the Twitter API keys

# enter the The consumer key provided by Twitter application

api_key = "Enter your api key here"
#enter the consumer secret key supplied by Twitter
api_secret = "Enter your api secret key here"
#enter the access token
access_token = "Enter your Access token here"
#The access secret supplied by Twitter
access_token_secret =  "Enter your access token secret key here"

#Set up authentication using following function that uses consumer keys and access tokens
#setup_twitter_oauth function is part of {twitteR} library  and uses OAuth authentication
#handshake function from httr package for a twitter session 
setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret) 

#function SearchTwitter searches for all tweets matching with search string provided
tweet=function(searchstring,nt){
  
  tweets = searchTwitter(searchstring, n=nt,lang="en")
  
  # This function takes a list of tweets returns a data.frame version of the tweets
  tweets_dataframe = twListToDF(tweets)
  
  #a. remove special characters and retain alpha numeric, '//','(quote)and '@' from tweets text
  # using gsub function we can replace all occurance of first argument with the second argument in the tweets
  # gsub can take regex expressions like [[:digit:]]
  tweets_data = gsub("[^0-9A-Za-z@///' ]", "", tweets_dataframe$text)
  
  #b. while some of the text is from re-tweets, we would certainly like to get rid of 
  #name of the person whose post was re-tweeted. This will remove word 'RT' and the name of person
  #w=mentioned after RT
  tweets_data = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "",tweets_data)
  
  #c. The other form of noise in twitter data is names of people between the text.
  #The function below removes name of people by matching anything that starts with @ symbol as name
  tweets_data = gsub("@\\w+", "", tweets_data)
  
  #d. Since the purpose of sentiment analysis is to analyze words and not numbers 
  # we can remove the numeric digits between the text using function gsub
  tweets_data = gsub("[[:digit:]]", "", tweets_data)
  
  #e. Data obtained from internet usually contains a lot of html entities and links;which gets 
  #embedded in the raw  data. It is important to get rid of these entities as a part of 
  #data sanitization process. The function below matches http links and removes them
  tweets_data = gsub('http\\S+\\s*',"", tweets_data)
  
  
  #Following function converts dataframe to corpus
  tweets_corpus <- Corpus(VectorSource(tweets_data))
  
  # To create a consistent pattern we will convert all words in text to lowercase. for example,
  # frequency functions otherwise would count same word in different case as two different words
  tweets_corpus<- tm_map(tweets_corpus, tolower)
  
  #create a plain text document using content in corpus
  tweets_corpus <- tm_map(tweets_corpus, PlainTextDocument)
  
  #All the punctuation marks between text should be removed
  tweets_corpus <- tm_map(tweets_corpus, removePunctuation)
  
  #While carrying out text analysis driven at the word level, it is required to remove the 
  #commonly occurring words also called stop-words. One approach is to create a create a long 
  #list of stop-words and other is too use a predefined language specific libraries
  # We will apply both approaches: 
  #1) use common stop words for english language 
  tweets_corpus <- tm_map(tweets_corpus, removeWords, stopwords("english"))
  
  # Based on our observation of data, we will create a customized list to be applied on top of
  # the text cleaned by using predefined stop words library
  
  st = c("amp","you'll","you'd","tedcruz","she","yet","hilari","trump","cruz",
         "you", "yourself", "won't", "wont","will","hillari","hilaryclinton",
         "donald","bernie","berniesanders","sen","ted","cruz","hillary",
         "clinton","i", "me", "my", "myself", "we", "our", "ours", "ourselves", 
         "you", "your", "yours", "yourself", "yourselves", "he", "him", "his",
         "himself", "she", "her", "hers", "herself", "it", "its", "itself",
         "they", "them", "their", "theirs", "themselves", "what", "which", 
         "who", "whom", "this", "that", "these", "those", "am", "is", "are",
         "was", "were", "be", "been", "being", "have", "has", "had", "having",
         "do", "does", "did", "doing", "would","should", "could", "ought",
         "i'm", "you're", "he's", "she's", "it's", "we're", "they're", "i've", 
         "you've", "we've", "they've", "i'd", "you'd", "he'd", "she'd", "we'd", 
         "they'd", "i'll", "you'll", "he'll", "she'll", "we'll", "they'll", 
         "isn't", "aren't", "wasn't", "weren't", "hasn't", "haven't", "hadn't",
         "doesn't", "don't", "didn't", "won't", "wouldn't", "shan't", "shouldn't",
         "can't", "cannot", "couldn't", "mustn't", "let's", "that's", "who's", 
         "what's", "here's", "there's", "when's", "where's", "why's", "how's", 
         "a", "an", "the", "and", "but", "if", "or", "because", "as", "until", 
         "while", "of", "at", "by", "for", "with", "about", "against", "between",
         "into", "through", "during", "before", "after", "above", "below", "to", 
         "from", "up", "down", "in", "out", "on", "off", "over", "under", "again",
         "further", "then", "once", "here", "there", "when", "where", "why",
         "how", "all", "any", "both", "each", "few", "more", "most", "other", 
         "some", "such", "no", "nor", "not", "only", "own", "same", "so", "than",
         "too", "very")
  
  tweets_corpus <- tm_map(tweets_corpus, removeWords, st) 
  
  # word stemming means to reduce word to its root form i.e. to remove any tense information
  # or to convert derived words to their root form. TM library functions allows us to stem the words
  #and thus identify words with same meaning which  might otherwise look different to machine 
  tweets_corpus <- tm_map(tweets_corpus,stemDocument)
  
  return(tweets_corpus)
  
}
# This function tweets can be dynamically used to retrieve and process tweets. This avoids 
#writing same processing step four times for each search string

# Extract data using tweet function and save in a clean corpus for four delegates : 
#Hilary Clinton, Donald Trump, Ted Cruz and Bernie Sanders
#If you want to increase the Number of tweets for analysis ,you can increase the count here
#Eg: Increase the count of 200 to your desired number
bs=tweet("berniesanders",200)  
dt=tweet("realDonaldTrump",200) 
hc=tweet("HilaryCLinton",200) 
tc=tweet("tedcruz",200) 

#--------------------------------------------------------------------------------------#

# 6: Retrieve and visualize the most frequent words associates/used for these 4 delegates. 

DTM_hc = DocumentTermMatrix(hc)
DTM_dt = DocumentTermMatrix(dt)
DTM_bs = DocumentTermMatrix(bs)
DTM_tc = DocumentTermMatrix(tc)

#we can remove the sparse terms using removeSparseTerms function
#ex: removeSparseTerms(DocumentTermMatrix,sparsity_probability)

#the sum of columns will give number of times each word has been repeated in all the 
#tweets collected for each delegate
freq_hc <- colSums(as.matrix(DTM_hc))
freq_dt <- colSums(as.matrix(DTM_dt))
freq_bs <- colSums(as.matrix(DTM_bs))
freq_tc <- colSums(as.matrix(DTM_tc))
# create a data frame with two columns word and frequency
DTM_hc <- data.frame(word=names(freq_hc), freq=freq_hc)
DTM_dt <- data.frame(word=names(freq_dt), freq=freq_dt)
DTM_bs <- data.frame(word=names(freq_bs), freq=freq_bs)
DTM_tc <- data.frame(word=names(freq_tc), freq=freq_tc)


# Barplots and wordclouds convey the message associated with numbers derived above by comparing
# the most frequent words associated with each delegate


# minimum frequency can be entered here:
#You can change the frequency of counts here,but be realistic while changing the count
#don`t expect too high frequency of word in a corpus
min_freq =100
# for example you want to plot only words which occur more than 100 times assign 100 to min_freq
barplot(DTM_hc$freq[DTM_hc$freq>min_freq],names.arg=DTM_hc$word[DTM_hc$freq>min_freq],xlab="Words", ylab="Frequency",col="steelblue",las=2)
barplot(DTM_dt$freq[DTM_dt$freq>min_freq],names.arg=DTM_dt$word[DTM_dt$freq>min_freq],xlab="Words", ylab="Frequency",col="steelblue",las=2)
barplot(DTM_bs$freq[DTM_bs$freq>min_freq],names.arg=DTM_bs$word[DTM_bs$freq>min_freq],xlab="Words", ylab="Frequency",col="steelblue",las=2)
barplot(DTM_tc$freq[DTM_tc$freq>min_freq],names.arg=DTM_tc$word[DTM_tc$freq>min_freq],xlab="Words", ylab="Frequency",col="steelblue",las=2)

# we can use par option to compare four word clouds at a time
#worcloud gives warnings if the word could not be fit on page
opar = par()
par(mfrow=c(2,2))

wordcloud(names(freq_hc),freq_hc, min.freq=min_freq,colors=brewer.pal(8, "Dark2"))
wordcloud(names(freq_dt),freq_dt, min.freq=min_freq,colors=brewer.pal(8, "Dark2"))
wordcloud(names(freq_bs),freq_bs, min.freq=min_freq,colors=brewer.pal(8, "Dark2"))
wordcloud(names(freq_tc),freq_tc, min.freq=min_freq,colors=brewer.pal(8, "Dark2"))
# These visualizations will help  perceiving, associations people make with each delegate

par(opar)


# Following function is designed to calculate polarity score. Polarities are denoted by +1 for 
#positive words and -1 for negative word in the text. Polarity score is calculated for each
#tweet as sum of polarities. 
# for example if a tweet has 2 positive words and  negative words and 5 neutral. The polarities
#assigned will be as (1,1,-1,-1,-1,0,0,0 )and the Polarity score will be sum of polarites = -1


polarity_score = function(tweets, positive, negative)
{
  score =laply(tweets, function(tweet, positive, negative)
  {
    #before matching words in each tweet we will need to separate multiple attached 
    #words into individual entities. For example, "brokelaptop" to "broke laptop" 
    #str_split function provides this functionality
    
    words = str_split(tweet,"\\s+")
    # unlist function simplifies a list structure i.e. is a sentence into vector which contains
    #all atomic components present in tweets
    words = unlist(words)
    
    # function matches the words with positive and negative databases and calculates score 
    positive_overlap = match(words, positive)
    negative_overlap= match(words, negative)
    positive_overlap =!is.na(positive_overlap)
    negative_overlap= !is.na(negative_overlap)
    score =sum(positive_overlap) - sum(negative_overlap)
    return(score)
  }, positive, negative)
  scores =data.frame(score=score, text=tweets)
  return(scores)
}

#Access the negative and positive library which will be used in the polarity score above
# Please change the path as per your location of files
positive <- scan('positive-words.txt', what='character', comment.char=';') #file with positive words 
negative <- scan('negative-words.txt', what='character', comment.char=';') #file with negative words

#Before applying function convert corpus to dataframe
hc_df=data.frame(text = sapply(hc, as.character), stringsAsFactors = FALSE)
dt_df=data.frame(text = sapply(dt, as.character), stringsAsFactors = FALSE)
bs_df=data.frame(text = sapply(bs, as.character), stringsAsFactors = FALSE)
tc_df=data.frame(text = sapply(tc, as.character), stringsAsFactors = FALSE)

# Using the function defined above calculate Polarity score for each tweet for all delegates
Sentiment_scores_hc = polarity_score(hc_df$text, positive, negative)
Sentiment_scores_dt = polarity_score(dt_df$text, positive, negative)
Sentiment_scores_bs = polarity_score(bs_df$text, positive, negative)
Sentiment_scores_tc = polarity_score(tc_df$text, positive, negative)


# As a Score less than zero means a Negative polarity and score greater than zero means Positive and neutral if zero
# creating polarity column with negative or positive or neutral as values

Sentiment_scores_hc$polarity=ifelse(Sentiment_scores_hc$score>0,"positive", ifelse(Sentiment_scores_hc$score<0 , "negative", "neutral"))
Sentiment_scores_dt$polarity=ifelse(Sentiment_scores_dt$score>0,"positive", ifelse(Sentiment_scores_dt$score<0 , "negative", "neutral"))
Sentiment_scores_bs$polarity=ifelse(Sentiment_scores_bs$score>0,"positive", ifelse(Sentiment_scores_bs$score<0 , "negative", "neutral"))
Sentiment_scores_tc$polarity=ifelse(Sentiment_scores_tc$score>0,"positive", ifelse(Sentiment_scores_tc$score<0 , "negative", "neutral"))


# we will prepare a table with frequency of negative,neutral and positive tweets
# we have used table function to give the summary data and function t to transpose

x= data.frame(table(Sentiment_scores_hc$polarity),table(Sentiment_scores_bs$polarity),table(Sentiment_scores_dt$polarity),table(Sentiment_scores_tc$polarity))
x = x[,c(2,4,6,8)]
colnames(x)<-c( "Hillary","Bernie","Donald","Tedcruz")
x = t(x)  
colnames(x)<-c("Negative","Neutral", "Positive")

# Bar plot to compare polarities of Hillary,Donald,Bernie,TedCruz
# we have limited the yaxis basing on number tweets downloaded
#so that the legend won't fall on the bars

colors = c("red","green","yellow","blue") 
barplot(x,beside=T, col=colors,ylim=c(0,nrow(hc_df)),ylab="Number of tweets",main="Polarity Comparsion")
legend("top",legend=rownames(x),horiz = TRUE,fill=colors)

# To compare extreme sentiments it will be good to look at the % of negative vs. postive 
# out of the total non-neutral tweets associated with each delegate

# Calcuate the % of positive and negative tweets out of all the non-neutral tweets
#Framing len vector with number of non-neutral tweets for all delegates
#Framing dataframe y  by combining length vector  and matrix x
len = rbind(nrow(Sentiment_scores_hc[Sentiment_scores_hc$score!=0,]),nrow(Sentiment_scores_hc[Sentiment_scores_bs$score!=0,]),nrow(Sentiment_scores_hc[Sentiment_scores_dt$score!=0,]),nrow(Sentiment_scores_hc[Sentiment_scores_tc$score!=0,]))
y= cbind.data.frame(x,len)

#Divide the number of positive and negative score by total number of non-neutral tweets
#converting  and subsetting to get percentages of Negative and positive tweets
y$Negative = y$Negative/y$len
y$Positive = y$Positive/y$len
y= y[,c(1,3)]
y = t(y)

# Bar plot to see what % of tweets are positive vs. negative for each delegate 
colors = c("red",  "green") 
bp = barplot(y,beside=T, col=colors,ylim=c(0,1.5),ylab="Percentages",main="Sentiment Comparsion")
text(bp, 0, round(y,2),cex=1,pos=3)
legend("top",legend=rownames(y),horiz = TRUE,fill=colors)

#Performing Topic Modeling
#Creating an object Document Term Matrix "topic_dtm_hc"
topic_dtm_hc = DocumentTermMatrix(hc)
#Converting the datatype of the DTM to matrix
n=as.matrix(topic_dtm_hc)
#Filtering rows based on the frequency of words in a particluar tweet,below we are considering only tweets
#which contains more than 4 words
n=n[rowSums(n, na.rm = FALSE)>4,]

#K is the important term in LDA,here we should specify the number of topics that we want to generate
# Eg: In the below example we are generating only 2 topics.so, k value is 2
k<-2

#LDA function uses Gibbs method and parameters are not fixed,can be changed 
# For more information please refer the Project report
ldaOut <-LDA(n,k, method="Gibbs", control=list(nstart=5, seed =list(200,5,654,987,4567) , best=TRUE, burnin =1000, iter =1000, thin=50))

View(ldatopics)

#Converting the output of LDA function to a matrix 
ldatopics <- as.matrix(topics(ldaOut))
#small manipulations on the matrix ldatopics to give a better understanding 
row.names(ldatopics) <- paste("Document",c(1:nrow(n)))
colnames(ldatopics) <- "Topic"
#writing the results to a csv file, which will be created in the current working directory
write.csv(ldatopics,"DocsToTopics.csv")

#shows the output of dataframe upto few rows
head(ldatopics)

#Here we should specify the number of terms that we want under each topic
#In the below code we used '6' which gives 6 terms under each topic
ldaterms <- as.matrix(terms(ldaOut,6))
#writing the output to a csv file, which will be created in the current working directory
write.csv(ldaterms,"TopicsToTerms.csv")

#Executing below object gives the topics and the terms under each topic
ldaterms

#Calculating the topic probabilites 
topicProb <- as.data.frame(ldaOut@gamma)
colnames(topicProb) <- c("Topic 1","Topic 2")
#writing the results to a csv file,which will be created in the current working directory
write.csv(topicProb,"TopicProbabilities.csv")

#shows the output of dataframe upto few rows
head(topicProb)


#Performing the topic modeling using other function called CTM which uses VEM method

ctmout<- CTM(n,k, method="VEM",control = NULL,model = NULL )

#Converting the output of CTM function to a matrix 
ctmtopics <- as.matrix(topics(ctmout))
#small manipulations on the matrix CTMtopics to give a better understanding 
row.names(ctmtopics) <- paste("Document",c(1:nrow(n)))
colnames(ctmtopics) <- "Topic"
#writing the results to a csv file,which will be created in the current working directory
write.csv(ctmtopics,"CTMDocsToTopics.csv")

#shows the output of dataframe upto few rows
head(ctmtopics)

#Here we should specify the number of terms that we want under each topic
#In the below code we used '6' which gives 6 terms under each topic
ctmterms <- as.matrix(terms(ctmout,6))
#writing the results to a csv file,which will be created in the current working directory
write.csv(ctmterms,"CTMTopicsToTerms.csv")

#Executing below object gives the topics and the terms under each topic
ctmterms

#Calculating the topic probabilites 
ctmtopicProb <- as.data.frame(ctmout@gamma)
colnames(ctmtopicProb) <- c("Topic 1","Topic 2")
#Writing the results to a csv file,which will be created in the current working directory
write.csv(ctmtopicProb,"CTMTopicProbabilities.csv")

#shows the output of dataframe upto few rows
head(ctmtopicProb)