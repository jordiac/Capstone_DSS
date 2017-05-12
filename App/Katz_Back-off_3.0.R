

## -------------------------------------------------------
##            Katz's Back-off implementation
## -------------------------------------------------------



## ********************   References:  *********************

## https://thachtranerc.wordpress.com/2016/04/12/katzs-backoff-model-implementation-in-r/
## https://github.com/ThachNgocTran/KatzBackOffModelImplementationInR

## ********************************************************

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

        






