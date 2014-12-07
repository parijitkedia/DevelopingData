library(shiny)

#server.R

shinyServer(function(input, output) {
  
  # Render graph  
  output$radar <- renderPlot({
    
    # Load function
    source("radar.R")
    
    # Get values
    values <- data.frame("Energy"       = input$num1, 
                         "Soil"         = input$num2, 
                         "Emmissions"   = input$num3, 
                         "Biodiversity" = input$num4, 
                         "Water"        = input$num5, 
                         "Land"         = input$num6, 
                         "Resource"     = input$num7, 
                         "Ecotoxity"    = input$num8
    )
    
    # Plot
    webplot(values, col="darkgreen")
  })
})