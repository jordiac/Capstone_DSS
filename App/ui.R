#
# ui.R
# Reference: https://github.com/melissatan/ds-jhu-wordprediction/tree/master/my_app
# (Thanks Melissa ;))

library(shiny)

shinyUI(fluidPage(
        theme = "bootstrap.css",
        
        titlePanel(h1("Next Word Prediction", align="center"),
                   windowTitle = "Capstone project"),
        h4("Just like a Lobotomized Penguin ;)", align="center"),
        h5("By jordiac, may 2017", align="center"),
        
        hr(),
        
        fluidRow(
                
                column(6, offset=3,
                       
                       tabsetPanel(type = "tabs",
                                   tabPanel("Push it!",
                                            "Just write something, come on!",
                                            textInput("phrase", label = "", value = ""),
                                            tags$head(tags$style(type="text/css", "#phrase {width: 600px;}")),
                                            
                                            fluidRow(
                                                    column(6,
                                                           actionButton("goButton", "Predict Now!"),
                                                           br(), br(),
                                                           p("Mmm, the next word Should be ...")
                                                    ),
                                                    column(6,
                                                           p(textOutput("stats")),
                                                           h2(textOutput("nextword"))
                                                    )
                                            )
                                            
                                   ),
                                   tabPanel("Hands on!",
                                            "Start writing a phrase and see what happens.....",
                                            textInput("phrase2", label = "", value = ""),
                                            tags$head(tags$style(type="text/css", "#phrase2 {width: 600px;}")),
                                            
                                            fluidRow(
                                                    column(6,
                                                           br(),br(),br(),
                                                           "Mmm, the next word Should be ..."
                                                    ),
                                                    column(6,
                                                           p(textOutput("stats2")),
                                                           h2(textOutput("nextword2"))
                                                    )
                                            )
                                   )
                       )
                )
        ),
        
        hr(),
        
        fluidRow(
                column(5, offset=1,
                       
                       wellPanel(
                               h4("How to use this app?"),
                               
                               p("-Get started by filling the blank cell with any word/sentence."),
                               p("-If you're in", em('Push it!'), "mode, click the Green button.
                                 In", em('Hands on!'), "mode, a word will appear automatically."),
                               
                               helpText("Select language ",em("English (US)"), ",
                                        if writing in english or ", em("Footballish (FB)"), 
                                        ", if willing to predict using Footballers' language.")
                               )
                       
                               ),
                column(5,
                       selectInput("lang",
                                   label = "Which language should we use?",
                                   choices = list("English (US)" = "en_us",
                                                  "Footballish (FB)" = "foot"),
                                   selected = "en_us"),
                       br(),
                       p("Source code: ",
                         a("Github", href="https://github.com/jordiac/Capstone_DSS"),
                         align="right")
                )
                               )
                       ))