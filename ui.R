library(shiny)

# Define UI for random distribution application 
shinyUI(fluidPage(
    # Coursera - Data Science Capstone Projet
    # Application title
    titlePanel("Word Prediction application"),
    
    # Sidebar
    sidebarLayout(
        sidebarPanel(
            textInput(
                "text", 
                label = h5("Type one or more words and watch the application predicting the next most probable word to follow:"), 
                value = "I love"
                )
        ),
        
        # Tabset
        mainPanel(
            tabsetPanel(
                type = "tabs", 
                tabPanel("Result of the prediction", textOutput("value")),
                tabPanel("Plot", plotOutput("plot")),
                tabPanel("Table", tableOutput("table")),
                tabPanel("About",  includeMarkdown("About.Rmd"))
            )
        )
    )
)
)