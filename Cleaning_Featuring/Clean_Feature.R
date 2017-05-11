




# --------------------------   Links of interest   ------------------------------

# https://rstudio-pubs-static.s3.amazonaws.com/31867_8236987cf0a8444e962ccd2aec46d9c3.html
# http://www.sthda.com/english/wiki/text-mining-and-word-cloud-fundamentals-in-r-5-simple-steps-you-should-know#step-2-install-and-load-the-required-packages
# https://thachtranerc.wordpress.com/2016/04/12/katzs-backoff-model-implementation-in-r/

# --------------------------------------------------------------------------------


# -----------------------------------------------------------------
# ******** Cleaning functions and featuring functions *************
# -----------------------------------------------------------------

## this function removes rows with empty spaces
bigram_clean <- function(bigram){
        ## removing empty rows
        aa <- which(bigram[,1] == "" | bigram[,2] == "")
        if (length(aa)>0){
                mat <- bigram[-aa,]
        }
        
        ## removing row when word1 == word2
        aa <- which(mat[,1] == mat[,2])
        if (length(aa)>0){
                mat <- mat[-aa,]
        }
        
        ## removing NA values
        aa <- which(is.na(mat[,1])|is.na(mat[,2]))
        if (length(aa)>0){
                mat <- mat[-aa,]
        }
        return(mat)
}


## this function removes rows with empty spaces
trigram_clean <- function(trigram){
        ## removing empty rows
        aa <- which(trigram[,1] == "" | trigram[,2] == ""| trigram[,3] == "")
        if (length(aa)>0){
                mat <- trigram[-aa,]
        }
        
        ## removing row when word1 == word2
        aa <- which(mat[,1] == mat[,2] | mat[,2] == mat[,3] | mat[,1] == mat[,3])
        if (length(aa)>0){
                mat <- mat[-aa,]
        }
        
        ## removing NA values
        aa <- which(is.na(mat[,1])|is.na(mat[,2])|is.na(mat[,3]))
        if (length(aa)>0){
                mat <- mat[-aa,]
        }
        return(mat)
}



quadgram_clean <- function(quadgram){
        ## removing empty rows
        aa <- which(quadgram[,1] == "" | quadgram[,2] == ""| quadgram[,3] == ""| quadgram[,4] == "")
        if (length(aa)>0){
                mat <- quadgram[-aa,]
        }
        
        ## removing row when word1 == word2
        aa <- which(mat[,1] == mat[,2] | mat[,2] == mat[,3] | mat[,1] == mat[,3] | mat[,3] == mat[,4])
        if (length(aa)>0){
                mat <- mat[-aa,]
        }
        
        ## removing NA values
        aa <- which(is.na(mat[,1])|is.na(mat[,2])|is.na(mat[,3])|is.na(mat[,4]))
        if (length(aa)>0){
                mat <- mat[-aa,]
        }
        return(mat)
}



## This function selects the 4000 first words most used
bigram_sel <- function(doc, words=4000) {
        
        stpw <- stopwords("english")
        noun <- 5
        
        ## One of the 2 words must not be a stop word
        list1 <- which(doc[,1] %in% stpw)
        list2 <- which(doc[,2] %in% stpw)
        match <- intersect(list1, list2)
        
        if (length(match)>0){doc <- doc[-match,]}
        
        numw <- min(words, nrow(doc))
        
        ## Getting 5 bigrams for each unique first word
        uni <- unique(doc[,1])
        
        fin <- NULL
        words <- min(words, length(uni))
        
        for (i in  1 :words){
                indi <- which(doc[,1] %in% uni[i])
                mw <- min(length(indi), noun)
                if (length(indi)>0){
                        fin <- c(fin, indi[1:mw])  
                }
        }
        
        doc <- doc[fin,]
        return(doc)
        
}




## This function selects the 4000 first words most used
trigram_sel <- function(doc, words=600, words2=100) {
        
        stpw <- stopwords("english")
        noun <- 5
        
        ## One of the 3 words must not be a stop word
        list1 <- which(doc[,1] %in% stpw)
        list3 <- which(doc[,3] %in% stpw)
        match <- intersect(list1, list3)
        if (length(match)>0){doc <- doc[-match,]}
        list3 <- which(doc[,3] %in% stpw)
        if (length(list3)>0){doc <- doc[-list3,]}
        
        numw <- min(words, nrow(doc))
        
        ## Getting 5 bigrams for each unique first word
        uni <- unique(doc[,1])
        fin <- NULL
        ws <- min(words, length(uni))
        uni <- uni[1:words]
        
        a2 <- which(doc[,1] %in% uni)
        doc<- doc[a2,]
        doc2 <- NULL
        
        for (i in  1 :ws){
                print(i)
                indi <- which(doc[,1] %in% uni[i])
                pard <- unique(doc[indi,2])
                w2 <- min(length(pard), words2)
                pard <- pard[1:w2]
                for (j in 1:w2){
                        yindi <- which(doc[,1] %in% uni[i] & doc[,2] %in% pard[j])
                        mw <- min(length(yindi), noun)
                        if (mw>0){
                                doc2 <- rbind(doc2, doc[yindi[1:mw],])  
                        }
                }
                doc <- doc[-indi,]
                
        }
        
        return(doc2)
        
}




## This function selects the 4000 first words most used
quadgram_sel <- function(doc, words=400, words2=80, words3=25) {
        
        stpw <- stopwords("english")
        noun <- 5
        
        ## One of the 3 words must not be a stop word
        list2 <- which(doc[,2] %in% stpw)
        list4 <- which(doc[,4] %in% stpw)
        match <- intersect(list2, list4)
        if (length(match)>0){doc <- doc[-match,]}
        list4 <- which(doc[,4] %in% stpw)
        if (length(list4)>0){doc <- doc[-list4,]}
        
        numw <- min(words, nrow(doc))
        
        ## Getting 5 bigrams for each unique first word
        uni <- unique(doc[,1])
        fin <- NULL
        ws <- min(words, length(uni))
        uni <- uni[1:words]
        
        a2 <- which(doc[,1] %in% uni)
        doc<- doc[a2,]
        doc2 <- NULL
        
        for (i in  1 :ws){
                print(i)
                indi <- which(doc[,1] %in% uni[i])
                pard <- unique(doc[indi,2])
                w2 <- min(length(pard), words2)
                pard <- pard[1:w2]
                for (j in 1:w2){
                        yindi <- which(doc[,1] %in% uni[i] & doc[,2] %in% pard[j])
                        pard2 <- unique(doc[yindi,3])
                        w3 <- min(length(pard2), words3)
                        pard2 <- pard[1:w3]
                        for (k in 1:w3){
                                yindi2 <- which(doc[,1] %in% uni[i] & doc[,2] %in% pard[j] & doc[,3] %in% pard2[k])
                                mw <- min(length(yindi2), noun)
                                if (mw>0){
                                        doc2 <- rbind(doc2, doc[yindi2[1:mw],])  
                                }
                        }
                }
                doc <- doc[-indi,]
                
        }
        
        return(doc2)
        
}

# -------------------------------------------------------------------
# *******************************************************************
# -------------------------------------------------------------------






# ********************  Loading and reading the data sets  *******************************
## ENGLISH data set
### Defining path to files
library(tm)
library(SnowballC)
library(ngramrr)
library(data.table)
library(tidyr)

path <- "./final"

rep2 <- "en_US"
dir2 <- paste(path, rep2, sep="/")

name1 <- "en_US.blogs.txt"
name2 <- "en_US.news.txt"
name3 <- "en_US.twitter.txt"

file_dir1 <- paste(dir2, name1, sep="/")
file_dir2 <- paste(dir2, name2, sep="/")
file_dir3 <- paste(dir2, name3, sep="/")

file1 <- readLines(file_dir1, encoding = "UTF-8", skipNul = TRUE)
file2 <- readLines(file_dir2, encoding = "UTF-8", skipNul = TRUE)
file3 <- readLines(file_dir3, encoding = "UTF-8", skipNul = TRUE)

### Subsampling
set.seed(5231)

perc <- 0.10 ##10% of lines
sfile1 <- sample(file1, size=length(file1)*perc, replace = FALSE)
sfile2 <- sample(file2, size=length(file2)*perc, replace = FALSE)
sfile3 <- sample(file3, size=length(file3)*perc, replace = FALSE)

sfile1 <- strsplit(sfile1, split=" ")

sTotal <- c(sfile1, sfile2, sfile3)
remove(file1, file2, file3,sfile1,sfile2,sfile3)
gc()

#**************** Creating corpus and Cleaning data  *********************
corpus <- Corpus(VectorSource(sTotal))

### Preparing data
corpus<- tm_map(corpus, content_transformer(function(x) iconv(x, to="UTF-8", sub="byte")))

### Remove punctuation
corpus <- tm_map(corpus, removePunctuation)

### Remove numbers
corpus <- tm_map(corpus, removeNumbers)

### lower case letters
corpus <- tm_map(corpus, content_transformer(tolower))

### Remove url
remURL <- function(x) gsub('http\\S+\\s*',"", x)
corpus <- tm_map(corpus, content_transformer(remURL))

# Eliminate extra white spaces
corpus <- tm_map(corpus, stripWhitespace)


### Save Plain text and corpus
save(corpus, file="./corpus.RData")



#************* Converting Final Corpus as data Frame  **********************
Dtext <- data.frame(text = sapply(corpus, as.character), stringsAsFactors = FALSE)
text <- unlist(Dtext)
remove(Dtext,corpus,sTotal)
gc()



#************* TOKENIZATION ***********************
## Tokenization = read text to break it into words and sentences and turn it into N-grams

## unigram 3min
Context <- ""
finword <- paste(Context, text, collapse = " ")
finword <- unlist(strsplit(finword, split=" "))
finword <- finword[-1]
save(finword, file="./finwords.RData")
remove(text)
gc()

unigram <- table(as.character(finword))       ## Getting unique bigrams count
unigram <- sort(unigram, decreasing = TRUE)
unigram <- data.frame(unigram, stringsAsFactors = FALSE)
pti <- which(unigram[,1] %in% stopwords("english") | unigram[,1]== "" )
unigram <- unigram[-pti,]
save(unigram, file="./unigram_tm.RData")
remove(unigram)
gc()

## Bigram 3min
bigram <- lapply(ngrams(finword, 2), paste, collapse= " " )
bigram <- unlist(bigram)

save(bigram, file= "./big_tm.RData")
remove(bigram)
gc()

## Trigram
trigram <- lapply(ngrams(finword, 3), paste, collapse= " " )
trigram <- unlist(trigram)

save(trigram, file= "./tri_tm.RData")
remove(trigram)
gc()

##QuadGram
quadgram <- lapply(ngrams(finword, 4), paste, collapse= " " )
quadgram <- unlist(quadgram)

save(quadgram, file= "./quad_tm.RData")
remove(quadgram)
gc()

remove(finword)
gc()

## Converting into Data frames: word1, word2, word3, freq
### Bigram
load(file= "./big_tm.RData")
bigram <- table(as.character(bigram))       ## Getting unique bigrams count
bigram <- sort(bigram, decreasing = TRUE)   ## Sorting unique bigrams 
save(bigram, file = "./big_sort.RData")


bigram <- data.frame(bigram, stringsAsFactors = FALSE)   ##Creating data frame
bigram <- data.frame(word = as.character(bigram[,1]), Freq = bigram[,2], stringsAsFactors = FALSE)
bigram <- separate(bigram, col="word", into = paste("word", 1:2, sep=" "))   ##Separating each bigram in 2 cols
colnames(bigram) <- c("word1", "word2", "Freq")

bigram <- bigram_clean(bigram)
print(object.size(bigram), units="Mb")
start.time <- Sys.time()
bigram <- bigram_sel(bigram) ## 6min
end.time <- Sys.time()
time.taken1 <- end.time - start.time
time.taken1
save(bigram, file = "./big_sel.RData")
remove(bigram)
gc()

### trigram
load( file= "./tri_tm.RData")
trigram <- table(as.character(trigram))       ## Getting unique bigrams count
trigram <- sort(trigram, decreasing = TRUE)   ## Sorting unique bigrams 
save(trigram, file = "./tri_sort.RData")

trigram <- data.frame(trigram, stringsAsFactors = FALSE)   ##Creating data frame
trigram <- data.frame(word = as.character(trigram[,1]), Freq = trigram[,2], stringsAsFactors = FALSE)
trigram <- separate(trigram, col="word", into = paste("word", 1:3, sep=" "))   ##Separating each bigram in 2 cols
colnames(trigram) <- c("word1", "word2","word3", "Freq")

trigram <- trigram_clean(trigram)
save(trigram, file="./tri_prim.RData")
print(object.size(trigram), units="Mb")
start.time <- Sys.time()
trigram <- trigram_sel(trigram)  ## 32min(600words + 100 words2)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken
save(trigram, file = "./tri_sel.RData")
remove(trigram)
gc()

### quadgram
load(file= "./quad_tm.RData")
quadgram <- table(as.character(quadgram))       ## Getting unique bigrams count
quadgram <- sort(quadgram, decreasing = TRUE)   ## Sorting unique bigrams 
save(quadgram, file = "./quad_sort.RData")

quadgram <- data.frame(quadgram, stringsAsFactors = FALSE)   ##Creating data frame
quadgram <- data.frame(word = as.character(quadgram[,1]), Freq = quadgram[,2], stringsAsFactors = FALSE)
quadgram <- separate(quadgram, col="word", into = paste("word", 1:4, sep=" "))   ##Separating each bigram in 2 cols
colnames(quadgram) <- c("word1", "word2","word3", "word4", "Freq")

quadgram <- quadgram_clean(quadgram)
save(quadgram, file="./quad_prim.RData")
print(object.size(quadgram), units="Mb")
start.time <- Sys.time()
quadgram <- quadgram_sel(quadgram)  ## 3.65h(400words + 80words2+ 25words3)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken
save(quadgram, file = "./quad_sel.RData")
remove(quadgram)
gc()




## **************  Preparing prediction model  ***********************

load(file = "./big_sel.RData")
load(file = "./tri_sel.RData")
load(file="./unigram_tm.RData")

unigram <- unigram[1:5,]

source("Katz_Back-off_2.2.R")

## Defining discount values
parc <- discount(bigram, trigram)

bigram <- parc[[1]]
trigram <- parc[[2]]

save(bigram, file="./bigram_fin.RData")
save(trigram, file="./trigram_fin.RData")
save(quadgram, file="./quadgram_fin.RData")
save(unigram, file="./unigram_fin.RData")
source("./Katz_Back-off_2.2.R")




