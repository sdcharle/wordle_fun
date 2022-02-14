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
    tabsetPanel(type = "tabs",
                tabPanel("Main",
                         fluidRow(
                           column(6,
                                  textInput("not_in_word",
                                            p("Not in the word (just type the letters):", style = "color:gray"),
                                            value = ""),
                                  p(strong("In the word (but not here)"), style = "color:#CCCC00"),
                                  
                                  fluidRow(
                                    column( 2, textInput("not_1",
                                                         NULL,
                                                         value = "")),
                                    column( 2, textInput("not_2",
                                                         NULL,
                                                         value = "")),
                                    column( 2, textInput("not_3",
                                                         NULL,
                                                         value = "")),
                                    column( 2, textInput("not_4",
                                                         NULL,
                                                         value = "")),
                                    column( 2, textInput("not_5",
                                                         NULL,
                                                         value = ""))),
                                  
                                  p(strong("In order"), style = "color:green"),
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
                                  ),
                                  actionButton("clearChoices", "Clear Filters")
                           ),
                           column(6,
                                  
                                  fillCol(flex = 1, height = '100%',
                                          withSpinner(
                                            DT::DTOutput("filteredTable", height = '100%')
                                            ) 
                                         )
                                  )
                     )
                ),
                tabPanel("What does it mean?",
                         br(),
                         p(strong("What's all this then?")),
                         br(),
                         p("This is an app to help you play Wordle (or Absurdle). Here's what the columns in the table mean:"),
                         p(strong("exclude_count:"), " this is mostly for Absurdle. Means if all the letters in this word are eliminated, how many words are left?"),
                         p(strong("mult_score:"), " this is based on how often the word's letters appear at the same position in the dictionary over all."),
                         p(strong("previous_answer:"), " has this word previously been a Wordle solution?"),
                         p(strong("log_freq:"), " measure of how frequently the word appears in usage. Used logs because the numbers are generally small."),
                         p(strong("rank:"), " rank of the mult_score over all words in our dictionary.")
                )
                
     )
  )
)
    

