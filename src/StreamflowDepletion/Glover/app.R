## Glover
# Trying to make a simple shiny app which allows the user to input parameters
# for the Glover & Balmer (1954) analytical streamflow depletion solution.

library(shiny)
library(ggplot2)
library(pracma)

GloverBalmer1954 <- function(d, S, Tr, t){
  ## Glover and Balmer (1954) analytical model for streamflow depletion with fully-penetrating stream.
  #'
  #' Reference:
  #' Glover, RE, and GG Balmer (1954).River Depletion Resulting from Pumping a Well near a River. Eos, Transactions American Geophysical Union 35(3): 468-70. doi:10.1029/TR035i003p00468.
  #'   
  #' Inputs:
  #'  d  = distance from well to stream [L]
  #'  S  = aquifer storage coefficient [-] (specific yield for unconfined storativity for confined)
  #'  Tr = aquifer transmissivity [L2/T]
  #'  t  = time since pumping started [T]
  #'  
  #' Output:
  #'   Qf = streamflow depletion as fraction of pumping rate [-]
  
  Qf <- erfc(sqrt(S*d*d/(4*Tr*t)))
  return(Qf)
}


# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Glover & Balmer (1954)"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Slider for distance from well to stream ----
      sliderInput(inputId = "distToStream",
                  label = "Distance to Stream [m]:",
                  min = 5,
                  max = 500,
                  value = 100),
      
      # Input: Slider for transmissivity ----
      sliderInput(inputId = "Tr",
                  label = "Transmissivity [m/d]:",
                  min = 5,
                  max = 500,
                  value = 100),
      
      # Input: Slider for storage coefficienc ----
      sliderInput(inputId = "S",
                  label = "Storage Coefficient",
                  min = 0.01,
                  max = 0.3,
                  value = 0.1),
      
      # Input: Numeric box for start of plot
      numericInput("tStart", 
                   h3("Start Time"), 
                   value = 0),
      
      # Input: Numeric box for end of plot
      numericInput("tEnd", 
                   h3("End Time"), 
                   value = 100)
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Plot of Qf vs time ----
      plotOutput(outputId = "depletion")
      
    )
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  # Reactive expression to create data frame and calculate Qf ----
  df <- reactive({
    df <- data.frame(times=seq(input$tStart,input$tEnd), 
                     Qf = GloverBalmer1954(d=input$distToStream, S=input$S, input$Tr, seq(input$tStart,input$tEnd)))
  })
  
  # Make a plot ----
  
  output$depletion <- renderPlot({
    ggplot(df(), aes(times, Qf)) + 
      geom_line() + 
      geom_point() + 
      scale_x_continuous(name="Time",
                         expand=c(0,0)) +
      scale_y_continuous(name="Capture Fraction", 
                         limits=c(0,1), 
                         breaks=seq(0,1,0.25),
                         expand=c(0,0),
                         labels = scales::percent) + 
      theme_bw()
  })
  
}

shinyApp(ui = ui, server = server)