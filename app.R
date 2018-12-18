#The Jobverse: A World of Opportunities
jobs<-read.csv("data/BLSjobdata.csv",as.is=c(1,20,22),strip.white=F,stringsAsFactors=F)

#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny); require(collapsibleTree); require(shinythemes);require(DT);require(htmltools)

# Define UI for application that draws a histogram
ui <- fluidPage(theme=shinytheme("journal"),
   
   # Application title
   column( 8, offset=4,titlePanel("The Jobverse: Explore the Possibilities")),
   
  conditionalPanel("is.data.frame(output.tab)",DT::dataTableOutput("tab")),
  
  br(),
      # Show a plot of the generated distribution
     fluidRow(
       column(1),
       column(style = "border: 5px solid black;",10,
       uiOutput("jobnet"),
         tags$div(HTML(paste0("Source: ",tags$a(href="https://www.bls.gov/emp/tables/occupational-projections-and-characteristics.htm","US Bureau of Labor & Statistics"))))),
       column(1)),
  br(),
  hr(),
   # Sidebar with a slider input for number of bins 
  wellPanel( 
  fluidRow(
      
     column(2,
       sliderInput("branchL","Branch Length:", ticks=F,
                     min = 100,max = 800,value = 350)),
     column(2,
       sliderInput("plotH","Plot Height:",ticks=F,
                    min=300,max=1000,value=400)
       ),
    column(2,
      radioButtons("colorby","Color nodes by:",choiceValues=c("cat","chgemp"),selected="cat",choiceNames = c("Job Type","Expected %Change in Employment"))))
    
     
   )
   
)
   

#---------------------------------------------------
# SERVER SIDE
server <- function(input, output) {

#Tree Output  
   output$jobtree <- renderCollapsibleTree({
     
     #determines which color column to use from user input
     colorscheme=switch(input$colorby,cat=jobs$jobcol,chgemp=jobs$percentchg.col)
     
     collapsibleTree(jobs,hierarchy=c("Level1","Level2","Level3","Level4"),zoomable=T,collapsed=T,nodeSize = "leafCount",linkLength = input$branchL,tooltip=T,root="TheJobverse",fill = colorscheme[-1],fillByLevel = F, inputId = "node")})
   
   
#Table Output   
   output$tab<-DT::renderDataTable({
     if(length(input$node[[1]])!=0)
     {
     selectedjob<-input$node[[1]]
     desiredcols<-c("title","Employment.2016","Employment.2026","ChgEmploy.2016to26.Num","ChgEmploy.2016to26.Perc","Occupational.openings..2016.26.annual.average","Median.annual.wage..2017.1.","Typical.education.needed.for.entry","Work.experience.in.a.related.occupation","Typical.on.the.job.training.needed.to.attain.competency.in.the.occupation")
     
     selectedrow<-jobs[which(jobs$title==selectedjob),desiredcols]
     str(input$node)
     mybreaks<-c(-110,-50, -25, -12.5,-6.25,-3.125,-1.0625,0,1.0625,3.125,6.25,12.5,25,50,110)
     names(selectedrow)<-c("Job Title","Num. Employed 2016","Expected Num. Employed 2026","Expected Change in Eployment 2016-2026","Expected % Change in Employment 2016-2026","Expected Num. Job Openings 2016-2026","Median Yearly Wages 2017", "Typical Education Needed","Work Experience Needed", "Typical On-the-Job Training Needed")
     jobtab<-datatable(as.matrix(selectedrow),caption="*Job numbers are in thousands" , options=list(dom="t",scrollX=T,select=F))
     jobtab%>% 
        # formatStyle(columns=1:length(selectedrow),
        #   fontWeight = "bold"
       # )%>%
        formatStyle(
        columns = 5,
        color = styleInterval(mybreaks[-1],blue2red(length(mybreaks))))
     }else{x<-as.matrix("Click a node for job stats")
           datatable(x, options=list(dom="t"),colnames=NULL)
           }
   })
   
     observe(output$str <- renderPrint(str(input$node)))
     
#Output main job tree, w/ adjustable plot height
     output$jobnet<-renderUI({
      collapsibleTreeOutput("jobtree",height=input$plotH)
     })
   
}

# Run the application 
shinyApp(ui = ui, server = server)

