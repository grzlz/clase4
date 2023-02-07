# Demostración gráfica LGN y TLC
library(shiny)
library(shinydashboard)
library(ggplot2)


mf <- function(x, y) {
  nsim <- x
  p <- y
  x <- rbinom(nsim, 1, p)
  
  xbar <- cumsum(x)/(1:nsim)
  
  plot(1:nsim, xbar, type="l", ylim = c(0,1))
}



tlc <- function(x, y) {
  nsim <- x
  n <- y
  x <- matrix(runif(n*nsim), nrow=nsim, ncol=n)
  xbar <- rowMeans(x)
  hist(xbar)
}




# ShinyDashboard ----

header <- dashboardHeader(title = "LGN + TLC")

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem(
      "Overview", tabName = "over", icon = icon("dashboard")
    )
  )
)

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "over",
            fluidRow(
              column(width = 6,
                     box(width = NULL,
                         title = h4(strong("Ley de los grandes números")),
                         numericInput("lgn_n", "Cantidad de simulaciones", value = 1000, min = 100, max = 10000, step = 100),
                         numericInput("lgn_p", "Probabilidad asociada al experimento", 1/2, min = 0, max = 1, step = .1),
                         plotOutput("lgn"))
              ),
              column(width = 6,
                     box(width = NULL,
                         title = h4(strong("Teorema del límite central")),
                         numericInput("tlc_n", "Cantidad de muestras", 10000, min = 5, max = 10000, step = 10),
                         numericInput("tlc_m", "Tamaño de cada muestra", 12, min = 5, max = 10000, step = 10),
                         plotOutput("tlc")
                     )
              )
            )
    ),
    tabItem(tabName = "casos")
    
  )
)

ui <- dashboardPage(skin = "blue", header, sidebar, body)

server <- function(input, output){
  

  output$lgn <- renderPlot({
    mf(input$lgn_n, input$lgn_p)
  })
  
  output$tlc <- renderPlot({
    tlc(input$tlc_n, input$tlc_m)
  })
  

  
}

shinyApp(ui = ui, server = server)
