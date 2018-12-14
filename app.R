#The Jobverse: A World of Opportunities
jobs<-read.csv("data/BLSjobdata.csv",as.is=c(1,21),strip.white=F,stringsAsFactors=F)

#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny); require(collapsibleTree)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("The Jobverse: Explore the Possibilities"),
   
   # Sidebar with a slider input for number of bins 
   fluidRow(
    
     column(4,
       wellPanel(
       sliderInput("branchL","Branch Length:", ticks=F,
                     min = 100,max = 800,value = 350,)
    
     
   )
   )),
  br(),
      # Show a plot of the generated distribution
     
      collapsibleTreeOutput("jobtree")
      
      
)
   


# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$jobtree <- renderCollapsibleTree({
     collapsibleTree(jobs[-1,],hierarchy=c("Level1","Level2","Level3","Level4"),zoomable=T,collapsed=T,nodeSize = "leafCount",linkLength = input$branchL,tooltip=T,root="The Jobverse",fill = jobs$jobcol[-1],fillByLevel = F)})
   
}

# Run the application 
shinyApp(ui = ui, server = server)

