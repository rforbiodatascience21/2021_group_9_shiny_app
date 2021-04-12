# Load R packages
library(shiny)
library(shinythemes)

# Define UI
ui <- fluidPage(theme = shinytheme("flatly"),
                navbarPage(
                  "Group 9 ShinyApp",
                  tabPanel("Random RNA",
                           
                           # INPUT
                           sidebarPanel(
                             tags$h3("Input:"),
                             sliderInput(
                               inputId = "rna_length",
                               label = "Length of RNA strand:",
                               min = 1, value = 30, max = 200
                               ) # sliderInput end
                             ), # sidebarPanel end
                           
                           # DESCRIPTION
                           mainPanel(
                             h1("Description"),
                             "Select a number with the slider, and a random RNA sequence of that lenght is generated.",
                             
                             # OUTPUT
                             h3("Output"),
                             verbatimTextOutput(outputId = "txtout")
                           ) # mainpanel end
                  ) # tabpanel end
                ) # navbarPage end
) # fluidPage end

# SERVER
server <- function(input, output) {
  
  output$txtout <-  renderText({
    
    nucleotides <- sample(c("A", "U", "G", "C"), size = input$rna_length, replace = TRUE)
    rna = paste(nucleotides, collapse = "")
    return(rna)
    })
} #server end


# Create Shiny object
shinyApp(ui = ui, server = server)

library(rsconnect)

rsconnect::deployApp('/cloud/project/R/app.R')
