#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
    # Application title
    titlePanel("Words prediction App"),
  
    fluidRow(column(width=5,align ="left", offset=0, p(HTML("<strong>  Author: jordiac</strong>") )) ),
    fluidRow(column(width=5,align ="left", offset=0, p(HTML("<strong>  Date: May 2017</strong>") )) ),
    
  

  
    fluidRow(
          br(),
          column(width=9, align="left",offset=0,p("This Shiny application predicts the next word using N-gram Katz Back-off model. The main information about the algorithm can be found here:"),
                 
                 lien <- tags$html(
                         tags$body(a("Rpubs presentation" , href="http://rpubs.com/jordiac/Capstone_DDS", target="_blank"))
                         ),
                 br()
          ),
          br()
          
    ),
  
  
  
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
            fluidRow(
                    column(width=10, align="left", offset=0,
                           textAreaInput("box1", "Write your text (in english) here:", value ="", width="100%", height = "100px"),
                           br(),
                           submitButton("Predict!"),
                           br()
                           )

            ),
            br()

    ),
    
    # Show a plot of the generated distribution
    mainPanel(
       h3("Predicted Next Words:"),
       fluidRow(
               column(width=5, offset = 0,
                      verbatimTextOutput("prediction")
                       
               )
       )
       
    )
  )
))
