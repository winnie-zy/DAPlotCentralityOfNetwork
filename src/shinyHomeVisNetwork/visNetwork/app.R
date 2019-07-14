#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
require(shiny)
require(visNetwork)
library(igraph)
library(magrittr)
library(igraphdata)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Karate club  Centrality"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            # Input: Selector for choosing verticrs or edges centrality ----
            textInput(inputId = "centralityType",
                      label = "Choose a centrality type:",
                      value = "Vertices"),
            
            # Input: Selector for choosing type of centrality measures ----
            selectInput(inputId = "measuresType",
                        label = "Choose a centrality measure:",
                        choices = c("EigenCentrality","Degree", "Closeness", "Betweenness"))
                         #list("EigenCentrality"='a', "Degree"='b', "Closeness"='c', "Betweenness"='d'))  

    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
        # Output: Formatted text for caption ----
        h3(textOutput("centralityMeasure", container = span)),
        
        # Output: Verbatim text for data summary ----
        visNetworkOutput("network")
        
    )
  )
)





# Define server logic required to draw a histogram
require(shiny)
require(visNetwork)
library(igraph)
library(magrittr)
library(igraphdata)
server <- function(input, output) {

    # Return the requested centrality type ----
    
    # centralityTypetInput <- reactive({
    #     switch(input$centralityType,
    #            "vertices" = vertices,
    #            "edges" = edges)
    # }) 
    
    
    # Return the requested centrality measures ----
    # measuresTypeInput <- reactive({
    # })
    
    # Create caption ----
    
    output$centralityMeasure <- renderText({
        paste(input$centralityType,input$measuresType,sep = '&')
    }) 
    

    # Generate a network plot ----  
    output$network <- renderVisNetwork({
      data(karate)
      if(input$measuresType=='EigenCentrality'){ #centrality--EigenCentrality

           V(karate)$eigencentrality <- centr_eigen(karate, directed = FALSE)$vector
           nodes <- get.data.frame(karate, what="vertices")
           edges = data$edges
           nodes <- data.frame(id = nodes$name, title = nodes$name, group = nodes$eigencentrality, eigencentrality = nodes$eigencentrality)
           visNetwork(nodes, edges, width = "100%",main="Eigencentrality") %>%
           visOptions(selectedBy = "eigencentrality", highlightNearest = TRUE, nodesIdSelection = TRUE)%>%
           visPhysics(stabilization = FALSE)

      }else if(input$measuresType=='Degree'){ #centrality--Degree
        
        V(karate)$degree <- centr_degree(karate,mode = "all")$res
        nodes <- get.data.frame(karate, what="vertices")
        edges = data$edges
        nodes <- data.frame(id = nodes$name, title = nodes$name, group = nodes$degree, degree = nodes$degree)
        visNetwork(nodes, edges, width = "100%",main="Degree") %>%
          visOptions(selectedBy = "degree", highlightNearest = TRUE, nodesIdSelection = TRUE)%>%
          visPhysics(stabilization = FALSE)
      }else if(input$measuresType=='Closeness'){#centrality------Closeness
        
               V(karate)$closeness <- centr_clo(karate, mode = "all")$res
               nodes <- get.data.frame(karate, what="vertices")
               edges = data$edges
               nodes <- data.frame(id = nodes$name, title = nodes$name, group = nodes$closeness, closeness = nodes$closeness)
               visNetwork(nodes, edges, width = "100%",main="Closeness") %>%
               visOptions(selectedBy = "closeness", highlightNearest = TRUE, nodesIdSelection = TRUE)%>%
               visPhysics(stabilization = FALSE)
        
      }else{
        #centrality-------betweenness
        
               V(karate)$betweenness <- centr_betw(karate, directed = FALSE)$res
               nodes <- get.data.frame(karate, what="vertices")
               edges = data$edges
               nodes <- data.frame(id = nodes$name, title = nodes$name, group = nodes$betweenness, betweenness = nodes$betweenness)
               visNetwork(nodes, edges, width = "100%",main="Betweenness") %>%
               visOptions(selectedBy = "betweenness", highlightNearest = TRUE, nodesIdSelection = TRUE)%>%
               visPhysics(stabilization = FALSE)
      }
    })

}

# Run the application 
shinyApp(ui = ui, server = server)
