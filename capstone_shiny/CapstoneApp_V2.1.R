#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shiny)
library(shinydashboard)

# Define UI for application that draws a histogram
# ui <- fluidPage(
# 
#     # Application title
#     titlePanel("Old Faithful Geyser Data"),
# 
#     # Sidebar with a slider input for number of bins 
#     sidebarLayout(
#         sidebarPanel(
#             sliderInput("bins",
#                         "Number of bins:",
#                         min = 1,
#                         max = 50,
#                         value = 30)
#         ),
# 
#         # Show a plot of the generated distribution
#         mainPanel(
#            plotOutput("distPlot")
#         )
#     )
# )

ui <- dashboardPage(
    dashboardHeader(),
    dashboardSidebar( fileInput("file1", "Choose CSV File",
                                accept = c(
                                    "text/csv",
                                    "text/comma-separated-values,text/plain",
                                    ".csv"))),
    dashboardBody(box(dataTableOutput("contents"),
                   plotOutput("hist1"),
                   plotOutput("hist2")
                  
                  
                  , width = 12, title = "Version 2.0"))
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    
    winedata <- eventReactive(input$file1,{
        
        inFile <- input$file1
        
        if (is.null(inFile))
            return(NULL)
        
        mydata <- fread(inFile$datapath)
        
        mydata
    })
    

    
    output$contents <- renderDataTable({
        # input$file1 will be NULL initially. After the user selects
        # and uploads a file, it will be a data frame with 'name',
        # 'size', 'type', and 'datapath' columns. The 'datapath'
        # column will contain the local filenames where the data can
        # be found.
        #DT::datatable
        winedata()
    })
     output$hist1 <- renderPlot({
         
         tempDf <- winedata()
         
         tempDf$row_num <- seq.int(nrow(tempDf))
         
         tempDf <- tempDf[1:60,]
         
         INDV <- ggplot(tempDf, aes(x = row_num, y=fixed_acidity)) +
             geom_point() + geom_line() +
             ggtitle("XmR", "geom_point() and geom_line() required") + 
             ylim(-4,16) + #make scales same
             scale_x_continuous(expand =  expand_scale(mult = .15)) + #pad x-axis
             stat_QC(method="XmR", auto.label = T) +
             ggtitle("XmR", subtitle = "Auto Label QC lines")
         
         plot(INDV)
     })  
     output$hist2 <- renderPlot({
         
         
         tempDf <- winedata()
         
         tempDf$row_num <- seq.int(nrow(tempDf))
         
         tempDf <- tempDf[1:60,]
         
        
         
         UCL= mean(tempDf$fixed_acidity) + 3*sd(tempDf$fixed_acidity)
         LCL= mean(tempDf$fixed_acidity) - 3*sd(tempDf$fixed_acidity)
         
         CapabilityAnaylsis <- 
             ggplot(tempDf, aes(x = fixed_acidity)) + #init ggplot
             geom_histogram(binwidth = .75, color="purple") + #make the histogram
             stat_QC_Capability(
                 LSL=4.50, USL=17.00,                    #Specify LSL and USL
                 show.cap.summary = c("Cp", "Cpk"), #selected summary
                 digits = 2,                        #report two digits
                 method="XmR") +                    #Use the XmR method
             scale_x_continuous(expand =  expand_scale(mult = c(0.15,.65)))+ #pad the X-axis
             geom_vline(xintercept=mean(tempDf$fixed_acidity), colour="Blue") +
             geom_text(aes(x=mean(tempDf$fixed_acidity)), y = 25.5, label="Mean", colour="Blue", angle=0, vjust = 1.2, text=element_text(size=11))+
             geom_vline(xintercept=UCL, colour="Blue") +
             geom_text(aes(x=UCL), y = 25.5, label="3sd", colour="blue", angle=0, vjust = 1.2, text=element_text(size=11))+
             geom_vline(xintercept=LCL, colour="Blue") +
             geom_text(aes(x=LCL), y = 25.5, label="-3sd", colour="blue", angle=0, vjust = 1.2, text=element_text(size=11))
         
         
         CapabilityAnaylsis  #plot the graph
         
         
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
