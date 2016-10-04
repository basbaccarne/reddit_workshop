#################################################
#################################################
## DIGITAL METHODS WORKSHOP - REDDIT EN MEMES ###
##               BY BAS BACCARNE              ###
#################################################
#################################################

# Bepaal de zoekterm (query) en de subreddit waarop je wil zoeken

query = "obama"
subreddit = "memes"

#################################################
####      verander niets onder dit punt      ####
#### doorloop lijn per lijn met ctrl + enter ####
#################################################

source("installs.R")            # dit moet je maar een keer doen
source("functions_libraries.R")


###### deel 1: analyseren van afbeeldingen ###### 
#################################################

# haal de posts op met jouw zoekterm (query) en kanaal (subreddit)
posts <- GetPosts(query, subreddit)

# voeg IDs toe, afbeeldingsIDs en transformeer de time data naar time data
posts <- CleanData(posts)

# we kijken even wat voor vlees we in de kuip hebben
qplot(posts$date,posts$num_comments)

# voeg hieraan de afbeeldingsurl, titel en auteur toe (+ cleaning)
# opgelet: duurt wel even
posts <- addMetadata(posts)

# waar wil je de afbeeldingen opslaan?
# kies je map via session > set working directory
foldername <- "images_reddit"
dir.create(foldername)
img_folder <- paste(getwd(),"/",foldername,sep="")

# download de afbeeldingen
downloadImages(posts)

# export naar image plot file
clean.posts <- eraseNoIMG(posts)
write.table(clean.posts, "redditposts.txt", sep="\t", row.names=F, quote=F)

# analyseer deze data met imageplot (zie handout)



        #       #       #       #       #



###### deel 2 - analyseren van interacties ###### 
#################################################

# haal de commenter netwerkdata op van Reddit
comments <- GetComments(query, subreddit)

# converteer de commentstructuur naar een relatievector
comments_vector <- vector()
for(i in 1:nrow(comments)){
        comments_vector <- c(comments_vector,comments$user[i], comments$author[i])
}

# converteer de relatievector naar een igraph object
igraph <- graph(comments_vector)

# optioneel: werp eens een blik op dit igraph object (R)
plotNetwork(igraph)

# converteer igraph object naar gexf (om in te kunnen lezen in Gephi)
gexf <- igraph.to.gexf(igraph)
capture.output(print(gexf),
               file = "gephi_data.gexf")

# analyseer deze data met Gephi



#       #       #       #       #



######## deel 3 - analyseren van teksten ######## 
#################################################
# source: http://www.slideshare.net/rdatamining/text-mining-with-r-an-analysis-of-twitter-data #

## A - data cleaning ##

# opbouw van het corpus (alle tekst samen + forced als text vector)
corpus <- Corpus(VectorSource(comments$comment))

# alles lower case maken
corpus <- tm_map(corpus, content_transformer(tolower))

# alle punctuation weg
corpus <- tm_map(corpus,removePunctuation)

# alle nummers weg
corpus <- tm_map(corpus,removeNumbers)

# alle URLs weg
removeURL <- function(x) gsub("http[[alnum:]]*", "", x)
corpus <- tm_map(corpus, removeURL)

# verwijder stopwoorden  - opgelet: goed kijken naar deze lijst
myStopwords <- stopwords("english")
corpus <- tm_map(corpus, removeWords, myStopwords)

# maak een kopie van het corpus (hebben we later nodig)
corpus_copy <- corpus

# stamwoorden
corpus <- tm_map(corpus, stemDocument)

# check even een sample om te zien of je transormaties gelukt zijn
for (i in 1:10){
        cat(paste("[[", i, "]]", sep=""))
        writeLines(corpus[[i]])
}

# stem completion
        # corpus <- tm_map(corpus, stemCompletion,
        #               dictionary = corpus_copy)


# tel hoeveel het woord "crazy" voorkomt
crazyCases <- tm_map(corpus, grep, pattern = "\\<crazy")
sum(unlist(crazyCases))

# voeg woorden die voor jou hetzelfde zijn samen
corpus <- tm_map(corpus, gsub, pattern="crazy", replacement="crazyness")

# aanmaken van de TDM (term document matrix)
corpus_clean <- tm_map(corpus, PlainTextDocument)
tdm <- TermDocumentMatrix(corpus_clean, 
                          control = list(wordLengths = c(1, Inf)))


## B - frequente termen en associaties ##

# meest voorkomende termen
freq_terms <- findFreqTerms(tdm, lowfreq=15)

# frequentie van de termen
term_freq <- rowSums(as.matrix(tdm))
term_freq <- subset(term_freq, term_freq >= 15)
df <- data.frame(term=names(term_freq), freq=term_freq)

library(ggplot2)
ggplot(df, aes(x=term, y=freq)) + geom_bar(stat = "identity") + xlab("terms") + ylab("Count") + coord_flip()

findAssocs(tdm, "trump", 0.2)

install.packages ("wordcloud")
library("wordcloud")
m <- as.matrix(tdm)
word.freq <- sort(rowSums(m), decreasing=T)
wordcloud(words = names(word.freq), freq=word.freq, min.freq=3, random.order = F)

## C - clustering ##
tdm2 <- removeSparseTerms(tdm, sparse=0.95)
m2 <- as.matrix(tdm2)

distMatrix <- dist(scale(m2))
fit <- hclust(distMatrix, method="ward")
plot(fit)
rect.hclust(fit, k=6)

m3 <- t(m2)
set.seed(122)
k <- 6
kmeansResult <- kmeans(m3, k)
round(kmeansResult$centers, digits=3)

for(i in 1:k){
        cat(paste("cluster", i, ": ", sep=""))
        s <- sort (kmeansResult$centers[i,], decreasing = T)
        cat(names(s)[1:5], "\n")
}


## D - Topic Modelling ##

dtm <- as.DocumentTermMatrix(tdm)
rowTotals <- apply(dtm , 1, sum)        #Find the sum of words in each Document
dtm.new   <- dtm[rowTotals> 0, ]        #remove all docs without words


install.packages("topicmodels")
library("topicmodels")

lda <- LDA(dtm.new, k=8)
term <- terms(lda,4)
term

topic <- topics(lda, 1)
dates <- as.Date(strptime(comments$comm_date, "%d-%m-%y"))
dates.new <-dates[rowTotals> 0] 
topics <- data.frame(date=dates.new, topic)
qplot(dates.new, ..count.., data=topics, geom="density", fill=term[topic], position="stack")
