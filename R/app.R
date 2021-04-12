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
                  ),
                  tabPanel("Complementary DNA",
                           #input
                           sidebarPanel(
                             tags$h3("Input:"),
                             textAreaInput("dna", "Introduce your DNA chain"),
                           ),#sidebar panel end
                           #description
                           mainPanel(
                             h1("Description"),
                             "Introduce a DNA chain to get its complementary",
                             
                             #OUTPUT
                             h3("Output:"),
                             textOutput("compl")
                           ) #mainPanel ends
                           
                           )# tabpanel end
                ) # navbarPage end
) # fluidPage end

# SERVER
server <- function(input, output) {
  
  output$txtout <-  renderText({
    
    nucleotides <- sample(c("A", "U", "G", "C"), size = input$rna_length, replace = TRUE)
    rna = paste(nucleotides, collapse = "")
    return(rna)
    })
  
  output$compl <- renderText({
    lookup <- c("A" = "T", "T" = "A", "G" = "C", "C" = "G", "a" = "T", "t" = "A", "g" = "C", "c" = "G")
    dna_split <- strsplit(input$dna, "")[[1]]
    dna_complement <- paste0(lookup[dna_split], collapse = "")
  })
} #server end


# Create Shiny object
shinyApp(ui = ui, server = server)

library(rsconnect)

rsconnect::deployApp('/cloud/project/R/app.R')
