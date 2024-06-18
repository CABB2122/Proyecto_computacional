#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)

server <- function(input, output) {
  
  datos <- reactive({
    req(input$file)
    read.csv(input$file$datapath)
  })
  
  datos_seleccionados <- eventReactive(input$process, {
    datos() %>%
      select(SEQN, SMQ020, RIAGENDR, RIDAGEYR, DMDEDUC2, BMXWT, BMXHT, BMXBMI)
  })
  
  datos_renombrados <- eventReactive(input$process, {
    datos_seleccionados() %>%
      rename(
        seqn = SEQN,
        smoking = SMQ020,
        gender = RIAGENDR,
        age = RIDAGEYR,
        education = DMDEDUC2,
        weight = BMXWT,
        height = BMXHT,
        bmi = BMXBMI
      )
  })
  
  datos_limpios <- eventReactive(input$process, {
    datos_renombrados() %>%
      filter(!duplicated(.)) %>%
      na.omit()
  })
