#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(DT)
library(shinycssloaders)
library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("Have some fun with WORDS"),
    # Sidebar with a slider input for number of bins
 
    fluidRow(
      
      column(6,
            textInput("not_in_word",
                        "Not in the word (just type the letters):",
                        value = ""),
            textInput("in_word",
                 "In the word (not sure where):",
                 value = ""),
           p(strong("In order")),
           fluidRow(
             column( 2, textInput("letter1",
                       NULL,
                       value = "")),
             column( 2, textInput("letter2",
                                  NULL,
                                  value = "")),
             column( 2, textInput("letter3",
                                  NULL,
                                  value = "")),
             column( 2, textInput("letter4",
                                  NULL,
                                  value = "")),
             column( 2, textInput("letter5",
                                  NULL,
                                  value = ""))
           )
        ),
       column(6,
   
#        fillCol(flex = 1, height = '100%',
                #  withSpinner(
                DT::DTOutput("filteredTable", height = '100%')
#) 
        
 #         )
       )
    )
)
)

