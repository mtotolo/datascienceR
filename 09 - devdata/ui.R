shinyUI(
      fluidPage(  
      headerPanel(
           textOutput('myTitle')
#            "Ploynomial fit of mtcars dataset: mpg ~ wt"
            ),  
      sidebarPanel(    
            sliderInput('deg', 'Polynomial degree',value = 1, min = 0, max = 9, step = 1),
 #           h4('formula:'),
            radioButtons("toPred", "Choose the variable to predict:",
              list("mpg" = "mpg",
                "disp" = "disp",
                "hp" = "hp")),
            radioButtons("predictor", "Choose the predictor:",
              list("wt" = "wt",
                   "hp" = "hp")),
            actionButton("calButton", "Recalculate train/test")
            ),
      h4(a("link",ref="http:///~/doc.html")),
#      tabPanel("About",
#               mainPanel(includeMarkdown("doc.md")
#                         )
#      ),
      mainPanel(    
            plotOutput('myPlot'),
            h4('Formula:'),
            verbatimTextOutput('myString'),
            h4('calculated RMSE:'),
            verbatimTextOutput("inputValue")
      )
      
            
))