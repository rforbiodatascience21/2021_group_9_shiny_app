library(shiny)
library(shinythemes)

library(tidyverse)
library(lab08aSimpleRpackage)



ui <- fluidPage(theme = shinytheme("flatly"),
  titlePanel("DNA modifier"),
  
  ### Input DNA or random gen
  h4("Generate random sequeunce or input your own"),
  
  div(style="display:inline-block",
      numericInput("gen_dna_len", "Lenght of generated DNA sequence",  value=100, min = 3)
  ),
  div(style="display:inline-block",
      actionButton("genDNA", "Generate Random DNA sequence")
  ),
  
  textAreaInput("dna_input", "DNA sequence", 
                value="",
                width = "600",
                height = "120"),
  
  
  ### Create Codons
  h2("Codons"),
  numericInput("codons_start", "Position of initial codon", 
               value=1, min=1),
  wellPanel(textOutput("codons")),
  
  ### Reverse compliment
  h2("Translate codons to AA"),
  wellPanel(textOutput("AA")),
  
)

server <- function(input, output, session){
  
  ### Generate random DNA
  observeEvent(input$genDNA,{
    updateTextAreaInput(inputId="dna_input",
                        value=random_dna(input$gen_dna_len))
  })
  
  
  ### Split into codons
  codons <- reactiveVal()
  
  observe({
    if (str_length(input$dna_input) >= 3){
      print(input$codons_start)
      c_start = input$codons_start
      if (!is.numeric(c_start) | c_start <= 0){c_start <- 1}
      
      
      c = mk_codons(dna = input$dna_input, s = c_start)
      codons(c)
    }
    else {
      codons <- reactiveVal()
    }
    
  })
  
  
  output$codons <- renderText(paste(codons(), sep=", "))
  
  ### To AA
  output$AA <- renderText(dna_codons_to_aa(codons()))
  
}

shinyApp(ui, server)