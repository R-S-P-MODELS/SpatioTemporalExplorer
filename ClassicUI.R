ui <- fluidPage(theme = shinytheme("cyborg"),
   tags$style(type="text/css",
         ".shiny-output-error { visibility: hidden; }",
         ".shiny-output-error:before { visibility: hidden; }"
),
   # Application title
   titlePanel("Spatio Temporal Explorer"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        
         fileInput('Arquivo','Insert here your NetCdf file'),
         uiOutput('GenerateVariables'),
         uiOutput('GenerateDimensionX'),
         uiOutput('GenerateDimensionY')
        # actionButton('Botao','BotaoDeTeste')
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        tabsetPanel(
           tabPanel('VariableReport',verbatimTextOutput('Report')),
          tabPanel('FilterConditions',uiOutput('GenerateFilters')),
           tabPanel('VisualizeVariable',plotOutput("VisualizeArray") ),
          tabPanel('Tabular Visualization',dataTableOutput('TableArray'))
        )
      )
   )
)
