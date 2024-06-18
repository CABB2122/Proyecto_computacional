library(shiny)

ui <- fluidPage(
  titlePanel("Análisis de Datos NHANES"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Seleccione el archivo CSV", accept = ".csv"),
      actionButton("process", "Procesar Datos")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("1. Leer y mostrar estructura",
                 verbatimTextOutput("data_structure")
        ),
        tabPanel("2. Seleccionar variables",
                 dataTableOutput("selected_variables")
        ),
        tabPanel("3. Renombrar variables",
                 dataTableOutput("renamed_variables")
        ),
        tabPanel("4. Limpiar datos",
                 verbatimTextOutput("duplicates_count"),
                 verbatimTextOutput("na_count"),
                 dataTableOutput("cleaned_data")
        )
      )
    )