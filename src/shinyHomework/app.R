#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(igraph)
library(igraphdata)
library(sand) # Statistical Analysis of Network Data with R
library(sna) # Tools for Social Network Analysis
library(network)

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("Karate club Centrality"),
    
    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            
            # Input: Selector for choosing verticrs or edges centrality ----
            selectInput(inputId = "centralityType",
                        label = "Choose a centrality type:",
                        choices = c("Vertices", "edges")),
            
            # Input: Selector for choosing type of centrality measures ----
            selectInput(inputId = "measuresType",
                        label = "Choose a centrality measure:",
                        choices = c("Degree", "Closeness", "Betweenness","Rank"))
        ),
        
        # Main panel for displaying outputs ----
        mainPanel(
            # Output: Formatted text for caption ----
            h3(textOutput("centralityMeasure", container = span)),
            
            # Output: Verbatim text for data summary ----
            plotOutput(outputId = "networkPlot")
        )
    )
)
library(igraph)
library(igraphdata)
library(sand) # Statistical Analysis of Network Data with R
library(sna) # Tools for Social Network Analysis
library(network)
# Define server logic required to draw a histogram
server <- function(input, output) {
    
    # Return the requested centrality type ----
    
    centralityTypetInput <- reactive({
        switch(input$centralityType,
               "vertices" = vertices,
               "edges" = edges)
    }) 
    
    
    # Return the requested centrality measures ----
    # measuresTypeInput <- reactive({
    #     switch(input$measuresType,
    #            "Degree" = Degree,
    #            "Closeness" = Closeness,
    #            "Betweenness" = Betweenness,
    #            "Rank" = Rank,
    #            "Coreness" = Coreness,
    #     )
    # })
    
    # Create caption ----
    
    output$centralityMeasure <- renderText({
        paste(input$centralityType,input$measuresType,sep = '&')
    }) 
    
    
    # Generate a network plot ----
    output$networkPlot <- renderPlot({

        data(karate)
        A = get.adjacency(karate, sparse=FALSE)#adjacency matrix
        g = network::as.network.matrix(A) # make a matrix,变成 network martrix

      
      #   referring input measuresType in ui.r as input$measuresType
     if(input$centralityType=='edges'){#edges
       eb = edge.betweenness(karate)
       G <- graph.adjacency(A)
       kk.layout <- layout.kamada.kawai(G)
       com <- edge.betweenness.community(G)
       V(G)$color <- com$membership+1
       plot(G, vertex.label.dist=.75,layout=kk.layout,main="Edge-Betweenness Communities", edge.arrow.size=0.25)
     }
      else{#nodes
        g = network::as.network.matrix(A) # make a matrix,变成 network martrix
        if(input$measuresType=='Degree'){
          sna::gplot.target(g, degree(g), circ.lab = FALSE, circ.col="skyblue",usearrows = FALSE,
                            main="Degree",vertex.col=c("blue", rep("red", 32), "yellow"),
                            edge.col="darkgray",displaylabels=TRUE)

        }else if(input$measuresType=='Closeness'){
          sna::gplot.target(g, closeness(g), circ.lab = FALSE, circ.col="skyblue",usearrows = FALSE,
                            main="Closeness",vertex.col=c("blue", rep("red", 32), "yellow"),
                            edge.col="darkgray",displaylabels=TRUE)
        
        } else if(input$measuresType=='Betweenness'){
          sna::gplot.target(g, betweenness(g), circ.lab = FALSE, circ.col="skyblue",usearrows = FALSE,
                            main="Betweenness",vertex.col=c("blue", rep("red", 32), "yellow"),
                            edge.col="darkgray",displaylabels=TRUE)

        }

       else{
         sna::gplot.target(g, evcent(g), circ.lab = FALSE, circ.col="skyblue",usearrows = FALSE,
                           main="Rank",vertex.col=c("blue", rep("red", 32), "yellow"),
                           edge.col="darkgray",displaylabels=TRUE)

        }

       }

    })
}

# Run the application 
shinyApp(ui = ui, server = server)
