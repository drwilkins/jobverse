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
   
   tableOutput("tab"),
  
  br(),
      # Show a plot of the generated distribution
     fluidRow(
       column(1),
       column(style = "border: 5px solid black;",10,
      collapsibleTreeOutput("jobtree",height="600px") ),
       column(1) ),
  br(),
  hr(),
   # Sidebar with a slider input for number of bins 
   fluidRow(
    
     column(4,
       wellPanel(
       sliderInput("branchL","Branch Length:", ticks=F,
                     min = 100,max = 800,value = 350)
    
     
   )
   ))
)
   


# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$jobtree <- renderCollapsibleTree({
     collapsibleTree(jobs[-1,],hierarchy=c("Level1","Level2","Level3","Level4"),zoomable=T,collapsed=T,nodeSize = "leafCount",linkLength = input$branchL,tooltip=T,root="The Jobverse",fill = jobs$jobcol[-1],fillByLevel = F, inputId = "node")})
   
   output$tab<-renderTable({
     if(length(input$node[[1]])!=0)
     {
     selectedjob<-input$node[[1]]
     desiredcols<-c("ttl","Employment.2016","Employment.2026","ChgEmploy.2016to26.Num","ChgEmploy.2016to26.Perc","Occupational.openings..2016.26.annual.average","Median.annual.wage..2017.1.","Typical.education.needed.for.entry","Work.experience.in.a.related.occupation","Typical.on.the.job.training.needed.to.attain.competency.in.the.occupation")
     selectedrow<-jobs[which(jobs$ttl==selectedjob),desiredcols]
     
     names(selectedrow)<-c("Job Title","# Employed 2016","Exp # Employed 2026","Exp # Change in Eployment 2016-2026","Exp % Change in Employment 2016-2026","Exp Job Openings 2016-2026","Median Yearly Wages 2017", "Typ Ed Needed","Work Experience Needed", "Typ On-the-Job Training Needed")
     selectedrow
     }else{"Nothing Selected"}
   })
   
     observe(output$str <- renderPrint(str(input$node)))
   
}

# Run the application 
shinyApp(ui = ui, server = server)

