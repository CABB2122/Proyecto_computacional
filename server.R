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
  
   datos_sin_extremos <- reactive({
    datos_limpios() %>% 
      filter(between(weight, quantile(weight, 0.25) - 1.5 * IQR(weight), quantile(weight, 0.75) + 1.5 * IQR(weight)) &
               between(height, quantile(height, 0.25) - 1.5 * IQR(height), quantile(height, 0.75) + 1.5 * IQR(height)) &
               between(bmi, quantile(bmi, 0.25) - 1.5 * IQR(bmi), quantile(bmi, 0.75) + 1.5 * IQR(bmi)))
  })
  
  datos_recodificados <- reactive({
    datos_limpios() %>%
      mutate(
        smoking = case_when(
          smoking == 1 ~ "yes",
          smoking == 2 ~ "no",
          smoking %in% c(7, 9) ~ NA_character_
        ),
        gender = case_when(
          gender == 1 ~ "male",
          gender == 2 ~ "female"
        )
      )
  })
  
  datos_dummy <- reactive({
    dummy_cols(datos_recodificados(), select_columns = c("smoking", "gender"))
  })
  output$data_structure <- renderPrint({
    req(datos())
    str(datos())
  })
  
  output$selected_variables <- renderDataTable({
    datos_seleccionados()
  })
  
  output$renamed_variables <- renderDataTable({
    datos_renombrados()
  })
  
  output$duplicates_count <- renderPrint({
    sum(duplicated(datos_renombrados()))
  })
  
  output$na_count <- renderPrint({
    sapply(datos_renombrados(), function(x) sum(is.na(x)))
  })
  
  output$cleaned_data <- renderDataTable({
    datos_limpios()
  })
  
