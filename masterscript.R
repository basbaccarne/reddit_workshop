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

# dit moet je maar een keer doen
source("https://raw.githubusercontent.com/basbaccarne/reddit_workshop/master/installs.R")            
source("https://raw.githubusercontent.com/basbaccarne/reddit_workshop/master/functions_libraries.R")


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


# analyseer deze data met Gephi (zie handout)



#       #       #       #       #



######## deel 3 - analyseren van teksten ######## 
#################################################
# source: http://www.slideshare.net/rdatamining/text-mining-with-r-an-analysis-of-twitter-data #

## A - data cleaning ##

# opbouw van het corpus (alle tekst samen + forced als text vector)
corpus <- Corpus(VectorSource(comments$comment))

# cleaning
corpus <- cleanCorpus(corpus)

# maak een kopie van het corpus (hebben we later nodig)
corpus_copy <- corpus

# transformeer naar stamwoorden
corpus <- tm_map(corpus, stemDocument)

# check even een sample om te zien of je transormaties gelukt zijn
for (i in 1:5){
        cat(paste("[[", i, "]]", sep=""))
        writeLines(corpus[[i]])
}

# stem completion
# corpus <- tm_map(corpus, stemCompletion, dictionary = corpus_copy)

# aanmaken van de TDM (term document matrix)
corpus_clean <- tm_map(corpus, PlainTextDocument)
tdm <- TermDocumentMatrix(corpus_clean, 
                          control = list(wordLengths = c(1, Inf)))




## B - frequente termen en associaties ##

# overzicht termen die min 2 keer voorkomen
freq_terms <- findFreqTerms(tdm, lowfreq=2)
freq_terms

# frequentie van de termen die meer dan 5 keer voorkomen
term_freq <- rowSums(as.matrix(tdm))
term_freq <- subset(term_freq, term_freq >= 5)
df <- data.frame(term=names(term_freq), freq=term_freq)
ggplot(df, aes(x=term, y=freq)) + geom_bar(stat = "identity") + xlab("terms") + ylab("Count") + coord_flip()

# associaties zoeken en omzetten naar een gephi bestand
termDocMatrix <- tdm
termDocMatrix <- as.matrix(termDocMatrix)
termDocMatrix[termDocMatrix>=1] <- 1
termDocMatrix2 <- termDocMatrix %*% t(termDocMatrix)

termDocMatrix.g <- graph.adjacency(termDocMatrix2, weighted=TRUE, mode="undirected")
termDocMatrix.g <- simplify(termDocMatrix.g)
V(termDocMatrix.g)$label <- V(termDocMatrix.g)$name
V(termDocMatrix.g)$degree <- degree(termDocMatrix.g)
layout1 <- layout.fruchterman.reingold(termDocMatrix.g)
plot(termDocMatrix.g, layout=layout1, vertex.size=20, 
     vertex.label.color="darkred")

gexf2 <- igraph.to.gexf(termDocMatrix.g)
capture.output(print(gexf2),
               file = "gephi_data_tdm.gexf")

# wordcloud
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

lda <- LDA(dtm.new, k=8)
term <- terms(lda,4)
term

topic <- topics(lda, 1)
dates <- as.Date(strptime(comments$comm_date, "%d-%m-%y"))
dates.new <-dates[rowTotals> 0] 
topics <- data.frame(date=dates.new, topic)
qplot(dates.new, ..count.., data=topics, geom="density", fill=term[topic], position="stack")

