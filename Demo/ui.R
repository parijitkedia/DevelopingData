library(shiny)

shinyUI(pageWithSidebar(
  headerPanel("Fieldprint Graph"),
  
  sidebarPanel(
    helpText("Agriculture is essential for human living, providing food and clothing, for example. Resources are becoming scarce, e.g fertilizers, and it is important to use them rationaly. This tool helps to create interactive graphs based on a farm indicators of performance and compare them with an average production system to know where farmers could start improving their system. A farmer could start improving the indicators that are below the red line. It was based on an existing online resource (view link). It is so far a quite simple app that will be improved and adapted to Brazil, but it already took a lot of time to adapt and build a function to create a customized radar/spider graph. Even though it does not seem primarily that calculations are executed in the server, they certainly are done within the graph function."),
    
    br(),    
    
    h2("Sustainable Indicators"),
    numericInput("num1", label=h4("Energy Consumption"), value=0.5, min=0, max=1, step=0.1),
    numericInput("num2", label=h4("Soil Conservation"), value=0.5, min=0, max=1, step=0.1),
    numericInput("num3", label=h4("Emmissions"), value=0.5, min=0, max=1, step=0.1),
    numericInput("num4", label=h4("Biodiversity"), value=0.5, min=0, max=1, step=0.1),
    numericInput("num5", label=h4("Water Use"), value=0.5, min=0, max=1, step=0.1),
    numericInput("num6", label=h4("Land Use"), value=0.5, min=0, max=1, step=0.1),
    numericInput("num7", label=h4("Resource Consumption"), value=0.5, max=1, min=0, step=0.1),
    numericInput("num8", label=h4("Ecotoxity"), value=0.5, min=0, max=1, step=0.1)
  ),
  mainPanel(
    #     h3("Sustainable Indicators", align="middle"),
    plotOutput(outputId = "radar", height = "1100px", width = "1100px")
  )
))