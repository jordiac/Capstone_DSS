
## ------------------  MAIN FILE  -------------------------------
### This file contains the main function for Word prediction 
##--------------------------------------------------------------
source("Katz_Back-off_3.0.R")
source("Clean_2.0.R")

##---------   Main prediction algorithm   ----------------------------
predic_text <- function(input){
        load(file="unigram_fin.RData")
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
        
        if(is.null(result)==FALSE) {if (nrow(result)>1) {result <- data.frame(words=as.character(result[,1]))}}
        if(is.null(result)==FALSE) {if (nrow(result)==1) {result <- data.frame(words=as.character(result))}}
        
        
        if(is.null(result)==TRUE) {result <- data.frame(words=as.character(unigram[,1]))}
        
        return(result)
}
