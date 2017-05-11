

## -------------------------------------------------------
##            Katz's Back-off implementation
## -------------------------------------------------------




## *******************  Notes  ***************************

## This implementation considers only 2-grams and 3-grams

## -----------------------------------------------------
##                  Discount coefficients 
## -----------------------------------------------------

discount <- function(TwoGram, ThreeGram){
        ## input : the text input with 2 words : input[1:2] : 1col= 1st words; 2col=2nd word; 
        ## d= (r+1)/r * (N_r+1)/N_r
        ## r: frequency of the gram ; Nr = number of times frequency "r" appears in the list
        
        
        
        ## --------------------
        ##  Treating 3 gram list 
        ## --------------------
        
        ## Tgram : list of 3grams containing the 2 words input (3rd col: freq)
        un <- unique(ThreeGram[,4])
        if ((TRUE %in% is.na(un)) == TRUE){
                print("There are frequencies = NA, please check your input 3-GRAM")
                stop()
        }
        
        ## Defining the discount values for each different frequency
        if (length(un) >0){
                disc <- vector(mode="numeric", length = length(un))  
                nm <- min(length(un),6)  ## If frequency is higher than 6-1=5 --> d=1
                for (i in 1:length(un)){
                        if (un[i] < nm){
                                freq <- un[i]
                                freq2 <- freq+1
                                Nfreq <- nrow(ThreeGram[ThreeGram[,4]==freq,])
                                Nfreq2 <- nrow(ThreeGram[ThreeGram[,4]==freq2,])
                                dis <- freq2 / freq * Nfreq2 / Nfreq
                                if (dis ==0){dis<-1}
                                disc[i] <- dis
                        } else {
                                dis <- 1
                                disc[i] <- dis
                        }
                }
        }
        
        ## disc --> discount values for each unique frequency
        ma <- match(ThreeGram[,4], un)
        val <- NULL
        for (j in 1: nrow(ThreeGram)){
                f <- disc[ma[j]]
                val <- c(val,f)
        }
        ThreeGram$disc <- round(val,3)  # Add discount values to 3-gram data frame
        mm <- which(ThreeGram[,5] >1)
        if (length(mm)>0) {ThreeGram[mm,5] <- 1} # Maximizing to 1
        
        ## --------------------
        ##  Treating 2-gram list 
        ## --------------------
        
        un <- unique(TwoGram[,3])
        if ((TRUE %in% is.na(un)) == TRUE){
                print("There are frequencies = NA, please check your input 2-GRAM")
                stop()
        }
        
        ## Defining the discount values for each different frequency
        if (length(un) >0){
                disc <- vector(mode="numeric", length = length(un))  
                nm <- min(length(un),7)  ## If frequency is higher than 7-1=6 --> d=1
                for (i in 1:length(un)){
                        if (un[i] < (nm-1)){
                                freq <- un[i]
                                freq2 <- freq+1
                                Nfreq <- nrow(TwoGram[TwoGram[,3]==freq,])
                                Nfreq2 <- nrow(TwoGram[TwoGram[,3]==freq2,])
                                dis <- freq2 / freq * Nfreq2 / Nfreq
                                if (dis ==0){dis<-1}
                                disc[i] <- dis
                        } else {
                                dis <- 1
                                disc[i] <- dis
                        }
                }
        }
        
        ## disc --> discount values for each unique frequency
        ma <- match(TwoGram[,3], un)
        val <- NULL
        for (j in 1: nrow(TwoGram)){
                f <- disc[ma[j]]
                val <- c(val,f)
        }
        TwoGram$disc <- round(val,3)  # Add discount values to 3-gram data frame
        mm <- which(TwoGram[,4] >1)
        if (length(mm)>0) {TwoGram[mm,4] <- 1} # Maximizing to 1
        
        return(list(TwoGram, ThreeGram))
        
        
}





## -----------------------------------------------------
##                  Prediction algorithm
## -----------------------------------------------------



## gets an input and return the 5 most probable words as  output as per Katz Back-off method
Katz_Backoff <- function(input){

        
        load( file="bigram_fin.RData") ##bigram
        load( file="trigram_fin.RData") ##trigram
        load( file="quadgram_fin.RData") ##quadgram
        
        Nwords <- length(input)
        
        # Defining the input texts for each Ngram
        if (Nwords >2){ 
                qtext <- input[(length(input)-2):length(input)] ##input for quadgram
                ttext <- input[(length(input)-1):length(input)] ##input for trigram
                btext <- input[length(input)]                   ##input for bigram
                
        } else if (Nwords==2){
                ttext <- input[1:2]
                btext <- input[2]
        } else if (Nwords==1) {
                btext <- input[1]
        }
        
        output <- NULL
        ## Predicting
        if (Nwords >2){
                qlist <- which(quadgram[,1] == qtext[1] & quadgram[,2] == qtext[2] & quadgram[,3] == qtext[3])
                tlist <- which(trigram[,1] == ttext[1] & trigram[,2] == ttext[2])
                blist <- which(bigram[,1] == btext[1])
                
                ## 4-gram
                if (length(qlist) >0){
                        output4 <-quadgram[qlist,4:5]
                        colnames(output4) <- c("Predic", "Freq")
                        output <- rbind(output, output4)
                        output$prob <- 1  ## We give prob=1 to all elements
                }
                
                ## 3-gram and 2-gram as per KATZ
                if (length(qlist)<5){
                        sel1 <- which(trigram[,1] %in% ttext[1] & trigram[,2] %in% ttext[2])
                        if (length(sel1) >0){
                                sel1 <- trigram[sel1,]
                                sel2 <- which(sel1[,2] %in% ttext[2])
                                sel2 <- sel1[sel2,]
                                
                                ## Defining pbeta
                                pbeta <- 1-(sum(sel2[,4]*sel2[,5]) / sum(sel2[,4]))
                                
                                ## Selecting bigram rows where the 1st word is equal to the last word
                                usel <- which(bigram[,1] %in% ttext[2])
                                usel <- bigram[usel,]
                                
                                ## Removing those having word 2 = word 3 of the trigram
                                rsel <- which(usel[,2] %in% sel2[,3])
                                usel <- usel[-rsel,]
                                
                                ## for each word left, we calculate its probability 2-gram
                                usel$prob <- usel$Freq * usel$disc * pbeta /(sum(usel$Freq*usel$disc))
                                
                                ## Calculate the probability for each 3-gram end word
                                sel2$prob <- sel2$Freq * sel2$disc / sum(sel2$Freq * sel2$disc)
                                
                                ##Subseting each end word in bigram and trigram with its probability and sorting
                                bisel <- data.frame(endword=usel$word2, prob=usel$prob, stringsAsFactors = FALSE)
                                trisel <- data.frame(endword=sel2$word3, prob=sel2$prob, stringsAsFactors = FALSE)
                                final <- rbind(bisel, trisel)
                                yy <- order(final$prob, decreasing = TRUE)
                                final <- final[yy,]
                                output2 <- final[1:5,]
                                output <- rbind(output,output2)
                                
                        } else { 
                                ## Applying bigram
                                find <- which(bigram[,1] %in% btext[1])
                                if (length(find)>0){
                                        bisel <- bigram[find,]
                                        output2 <- bisel[1:5,2:3]
                                        output <- rbind(output,output2)
                                }
                        }
                }
                
                
                
                
        }else if (Nwords ==2) {
                ## 3-gram and 2-gram as per KATZ
                sel1 <- which(trigram[,1] %in% ttext[1] & trigram[,2] %in% ttext[2])
                if (length(sel1) >0){
                        sel1 <- trigram[sel1,]
                        sel2 <- which(sel1[,2] %in% ttext[2])
                        sel2 <- sel1[sel2,]
                        
                        ## Defining pbeta
                        pbeta <- 1-(sum(sel2[,4]*sel2[,5]) / sum(sel2[,4]))
                        
                        ## Selecting bigram rows where the 1st word is equal to the last word
                        usel <- which(bigram[,1] %in% ttext[2])
                        usel <- bigram[usel,]
                        
                        ## Removing those having word 2 = word 3 of the trigram
                        rsel <- which(usel[,2] %in% sel2[,3])
                        usel <- usel[-rsel,]
                        
                        ## for each word left, we calculate its probability 2-gram
                        usel$prob <- usel$Freq * usel$disc * pbeta /(sum(usel$Freq*usel$disc))
                        
                        ## Calculate the probability for each 3-gram end word
                        sel2$prob <- sel2$Freq * sel2$disc / sum(sel2$Freq * sel2$disc)
                        
                        ##Subseting each end word in bigram and trigram with its probability and sorting
                        bisel <- data.frame(endword=usel$word2, prob=usel$prob, stringsAsFactors = FALSE)
                        trisel <- data.frame(endword=sel2$word3, prob=sel2$prob, stringsAsFactors = FALSE)
                        final <- rbind(bisel, trisel)
                        yy <- order(final$prob, decreasing = TRUE)
                        final <- final[yy,]
                        output2 <- final[1:5,]
                        output <- rbind(output,output2)
                } else { 
                        ## Applying bigram
                        find <- which(bigram[,1] %in% btext[1])
                        if (length(find)>0){
                                bisel <- bigram[find,]
                                output2 <- bisel[1:5,2:3]
                                output <- rbind(output,output2)
                        }
                }
                
                
        } else if (Nwords==1){
                blist <- which(bigram[,1] == btext[1])
                if (length(blist) >0){
                        output <- rbind(output, bigram[blist,])
                        colnames(output) <- c("Predic", "Freq")
                        num <- min(5, nrow(output))
                        output <- output[1:num,2:3]
                }
                
        }
        
        output <- output[1:5,]        
        return(output)
        
}

source("./Clean_2.0.R")
##---------   Main prediction algorithm   ----------------------------
predic_text <- function(input){
        load(file="./unigram_fin.RData")
        input <- textClean(input) ## Cleaning the input data
        input <- unlist(strsplit(input, split= " ")) ## Splitting in different words
        
        condition <- 0
        for (i in 1:(as.integer(length(input)/2)-1)){
                condition <- 1
                result <- Katz_Backoff(input) ## results from Katz prediction function
                
                if (is.null(result)==TRUE ){ ## if no results obtained from Katz, remove last word
                        condition <- 0
                        input <- input[-c(length(input)-1,length(input)) ] ## Remove 2 last words if no results
                }
                
                if (condition == 1){
                        break()
                }
                
        }
        
        if(is.null(result)==FALSE & nrow(result)>1) {result <- data.frame(words=as.character(result[,1]))}
        if(is.null(result)==FALSE & nrow(result)==1) {result <- data.frame(words=as.character(result))}
        
        
        if(is.null(result)==TRUE) {result <- data.frame(words=as.character(unigram[,1]))}
        
        return(result)
}
        






