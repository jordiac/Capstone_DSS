#
# server.R


library(shiny)
library(tm)
source("Prediction.R")

nextw <- function(phrase, lang) {
        
        if (lang== "en_us"){
                resu <- predic_text(phrase)
                resu <- data.frame(resu)
                return(as.character(resu[1,1]))
        } else if (lang == "foot"){
                return("MoneY, mOnEy, MoNey")
        }
                
                

}

shinyServer(function(input, output) {
        
        phraseGo <- eventReactive(input$goButton, {
                input$phrase
        })
        output$stats <- renderText({
                numword <- length(strsplit(input$phrase," ")[[1]])
                numchar <- nchar(input$phrase)
                paste("You've written ", numword, " words and ", numchar, "characters")
        })
        output$nextword <- renderText({
                result <- nextw(phraseGo(),input$lang)
                # result <- nextw(input$phrase, input$lang)
                paste0(result)
        })
        output$stats2 <- renderText({
                numword <- length(strsplit(input$phrase2," ")[[1]])
                numchar <- nchar(input$phrase2)
                paste("You've written ", numword, " words and ", numchar, "characters")
        })
        output$nextword2 <- renderText({
                result <- nextw(input$phrase2,input$lang)
                paste0(result)
        })
        
})