require(RedditExtractoR)
require(igraph)
require(rgexf) 
require(tm)
require(SnowballC)
require(ggplot2)
require(jpeg)
require(wordcloud)
require(qdap)
require(topicmodels)
require(rvest)
require(dplyr)

cleanCorpus <- function(corpus){
        # alle URLs weg
        removeURL <- function(x) gsub("http[[alnum:]]*", "", x)
        corpus <- tm_map(corpus, removeURL)
        
        # alles lower case maken
        corpus <- tm_map(corpus, content_transformer(tolower))
        
        # alle punctuation weg
        corpus <- tm_map(corpus,removePunctuation)
        
        # alle nummers weg
        corpus <- tm_map(corpus,removeNumbers)
        
        # verwijder stopwoorden  - opgelet: goed kijken naar deze lijst
        myStopwords <- stopwords("english")
        corpus <- tm_map(corpus, removeWords, myStopwords)
        
        # verwijder vreemde tekens
        removeSpecialChars <- function(x) gsub("[^a-zA-Z0-9 ]","",x)
        corpus <- tm_map(corpus, removeSpecialChars)
        
        corpus
}
