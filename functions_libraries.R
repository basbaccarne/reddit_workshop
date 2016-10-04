require("RedditExtractoR")
require("igraph")
require("rgexf") 
require("tm")
require("SnowballC")
require("ggplot2")
require("jpeg")

GetPosts <- function(query, subreddit){
        reddit_urls(search_terms = query, 
                    subreddit = subreddit,
                    cn_threshold = 1, 
                    page_threshold = 300, 
                    sort_by = "comments",
                    wait_time = 2)
}

CleanData <- function(posts){
        posts$ID <- 1:nrow(posts)
        posts$imgID <- paste(posts$ID, ".jpg", sep="")
        
        posts$date <- strptime(posts$date, "%d-%m-%y")
        posts$timePOSIX <- as.numeric(as.POSIXlt(posts$date))
        
        posts
}

addMetadata <- function(posts){
        for (i in 1:nrow(posts)){
                post_content <- reddit_content(posts$URL[i])
                posts$img_url[i] <- post_content[1,16]
                posts$title[i] <- post_content[1,14]
                posts$author[i] <- post_content[1,9]
                if(grepl("http://imgur.com", posts$img_url[i])){
                        posts$img_url[i] <- paste(posts$img_url[i], ".jpg", sep="")
                        posts$img_url[i] <- gsub("http://imgur.com", "http://i.imgur.com", posts$img_url[i])
                }
                if(grepl("/gallery/", posts$img_url[i])){
                        posts$img_url[i] <- gsub("/gallery/", "/", posts$img_url[i])
                }
                if(grepl("/new.", posts$img_url[i])){
                        posts$img_url[i] <- gsub("/new.", ".", posts$img_url[i])
                }
                print(paste("status: ",round(i/nrow(posts),3)*100,"%", sep=""))
        }
        posts
}

downloadImages <- function(posts){
        for (i in 1:nrow(posts)){
                skip_with_message = simpleError('a file url was corrupt')
                if(grepl(".jpg",posts$img_url[i]) | grepl(".png",posts$img_url[i])){
                        tryCatch(download.file(
                                        posts$img_url[i], 
                                        paste(img_folder,"/",posts$imgID[i],sep=""), 
                                        mode="wb",
                                        quiet=T
                                ),
                                        print(paste("status: ",round(i/nrow(posts),3)*100,"%", sep="")), 
                                error = function(e) skip_with_message
                                )
                }
        }    
}

eraseNoIMG <- function(posts){
        clean.posts <- posts
        for(i in 1:nrow(posts)){
                clean.posts$valid[i] <- file.exists(paste(img_folder, "/", i, ".jpg", sep=""))
                
                if (grepl(".jpg",posts$img_url[i])){
                        if(class(try(readJPEG(paste(img_folder, "/", posts$imgID[i], sep="")), silent=T)) == "try-error"){
                                clean.posts$valid[i] <- FALSE  
                        }
                }
        }
        
        clean.posts[clean.posts$valid==T,]
}

GetComments <- function (query, subreddit){
        get_reddit(search_terms = query, 
                   subreddit=subreddit,
                   cn_threshold = 0, 
                   page_threshold = 100, 
                   sort_by = "comments",
                   wait_time = 2)
}

plotNetwork <- function(igraph){
        plot(igraph, 
                edge.arrow.size=.5, 
                vertex.color="gold", 
                vertex.size=15, 
                vertex.frame.color="gray", 
                vertex.label.color="black", 
                vertex.label.cex=0.8, 
                vertex.label.dist=2, 
                edge.curved=0.2) 
}