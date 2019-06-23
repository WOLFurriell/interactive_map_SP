## app.R ##
library(shinydashboard)

ui <- dashboardPage(
  
  ## Sidebar content

  dashboardHeader(title = "Exemplo mapas"),
  dashboardSidebar(
    sidebarMenu(
      
      menuItem("Mapa 1", tabName = "mapa1", icon = icon("dashboard")),
      menuItem("Mapa 2", tabName = "mapa2", icon = icon("th"))
    )
  ),
  
  ## Body content
  
  dashboardBody(
    tabItems(
      
      # First tab content
      
      tabItem(tabName = "mapa1",

              leafletOutput("mymap1", width = "100%", height = "600px"),
              
              # Painel flutuante
              
              absolutePanel(
                id = "controls", class = "panel panel-default", fixed = TRUE,
                draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                width = 330, height = "auto",
                HTML('<button data-toggle="collapse" data-target="#demo"> Clique para ver o gráfico </button>'),
                tags$div(id = 'demo',  class="collapse",
                         plotOutput("plot"))
                
              )
              
      ),
      
      # Second tab content
      tabItem(tabName = "mapa2",
              
              column(width = 12,  
              box(width = NULL,
                leafletOutput("mymap2"))
              ),
              
              column(width = 10,
              box(width = NULL, height = NULL,
                title = "Controls",
                plotOutput("plot1")
               )
              
            )
          )
        )
      )
)

server <- function(input, output) {
  
  set.seed(122)
  histdata <- rnorm(500)
  
  output$mymap1 <- renderLeaflet({
    mapa
  })
  
  output$mymap2 <- renderLeaflet({
    mapa
  })
  
  output$plot <- renderPlot({
    hist(histdata)
  })
  
}

shinyApp(ui, server)