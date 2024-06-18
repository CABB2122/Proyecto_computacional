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
        ),
        tabPanel("5. Histogramas",
                 plotOutput("histograms")
        ),
        tabPanel("6. Diagramas de caja",
                 plotOutput("boxplots")
        ),
        tabPanel("7. Analizar valores extremos",
                 verbatimTextOutput("outliers_count")
      ),
        tabPanel("8. Diagramas de caja sin valores extremos",
                 plotOutput("boxplots_no_outliers")
        ),
        tabPanel("9. Recodificar 'smoking' y 'gender'",
                 dataTableOutput("recoded_variables")
        ),
        tabPanel("10. Crear variables dummy",
                 dataTableOutput("dummy_variables")
        )
      )
    )
  )
)
    
