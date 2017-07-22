# Next Word prediction algorithm

# Data science specialization from Johns Hopkins-Coursera

## Capstone project with Swiftkey collaboration

This project is about creating a Shiny app predicting next word given a sentence.

### -Description of the main folders:

### App:
contains the files leading **(ui.R, server.R and Prediction.R that includes the main function for predicting)** to the shiny app.

Please note that **Prediction.R** calls **Katz_Back-off_3.0.R** and **Clean_2.0.R** that uses the data file **.RData** containing the N-gram featured data. Those N-gram files are obtained using the files in **Cleaning_Featuring** folder.


### Cleaning_Featuring:
The cleaning and featuring actions can be carried out by running the **Clean_Feature.R** file that will call the **Katz_Back-off_2.2.R**. 

Finally we obtain the N-gram files:

- unigram_fin.RData

- bigram_fin.RData

- trigram_fin.RData

- quadgram_fin.RData


### Presentation:
Contains the main file **deck.Rpres** leading to the R presentation.


### Main links:

-[Shiny App](https://jordiac.shinyapps.io/nlp_nwp/)

-[RPubs Presentation](http://rpubs.com/jordiac/NPW)

-[Github](https://github.com/jordiac/Capstone_DSS)

