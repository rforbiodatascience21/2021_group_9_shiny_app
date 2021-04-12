# Load R packages
library(shiny)
library(shinythemes)
library(tidyverse)


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
                             fileInput("upload_dna", NULL, accept = "text/plain")
                           ),#sidebar panel end
                           #description
                           mainPanel(
                             h1("Description"),
                             "Introduce a DNA chain to get its complementary",
                             
                             #OUTPUT
                             h3("Output:"),
                             textOutput("compl")
                           )#main panel end
                      ),
                     tabPanel("DNA modifier",
                              ### Input DNA or random gen
                              h4("Generate random sequence or input your own"),
                              
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
                              wellPanel(textOutput("AA"))
                              
                              ),#end of the tabpanel

                  tabPanel("Make Codons",
                           sidebarPanel(
                             tags$h3("Input:"),
                             textInput("mk_codons_dna", "DNA Sequence:", "AUG..."),
                             textInput("mk_codons_start", "Start Position:", 1)
                           ), # sidebarPanel
                           mainPanel(
                             h1("Results"),
                             
                             h4("Codons: "),
                             verbatimTextOutput("mk_codons")
                             
                           ) # mainPanel
                           
                  ) # Navbar 3, tabPanel
                )
              ) # navbarPage end


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
  output$AA <- renderText({
    std_code_table <- c("TTT" = "F", "TCT" = "S", "TAT" = "Y", "TGT" = "C",
                        "TTC" = "F", "TCC" = "S", "TAC" = "Y", "TGC" = "C",
                        "TTA" = "L", "TCA" = "S", "TAA" = "*", "TGA" = "*",
                        "TTG" = "L", "TCG" = "S", "TAG" = "*", "TGG" = "W",
                        "CTT" = "L", "CCT" = "P", "CAT" = "H", "CGT" = "R",
                        "CTC" = "L", "CCC" = "P", "CAC" = "H", "CGC" = "R",
                        "CTA" = "L", "CCA" = "P", "CAA" = "Q", "CGA" = "R",
                        "CTG" = "L", "CCG" = "P", "CAG" = "Q", "CGG" = "R",
                        "ATT" = "I", "ACT" = "T", "AAT" = "N", "AGT" = "S",
                        "ATC" = "I", "ACC" = "T", "AAC" = "N", "AGC" = "S",
                        "ATA" = "I", "ACA" = "T", "AAA" = "K", "AGA" = "R",
                        "ATG" = "M", "ACG" = "T", "AAG" = "K", "AGG" = "R",
                        "GTT" = "V", "GCT" = "A", "GAT" = "D", "GGT" = "G",
                        "GTC" = "V", "GCC" = "A", "GAC" = "D", "GGC" = "G",
                        "GTA" = "V", "GCA" = "A", "GAA" = "E", "GGA" = "G",
                        "GTG" = "V", "GCG" = "A", "GAG" = "E", "GGG" = "G")
    aa <- paste0(std_code_table[codons()], collapse = "")
  })
  


  output$mk_codons <- renderText({
    s = as.integer(input$mk_codons_start)
    l = nchar(input$mk_codons_dna)
    codons <- substring(input$mk_codons_dna,
                        first = seq(from = s, to = l-3+1, by=3),
                        last = seq(from = 3+s-1, to = l, by=3))
    return(codons)
  })
  

} #server end


# Create Shiny object
shinyApp(ui = ui, server = server)

