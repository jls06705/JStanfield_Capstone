#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


#


library(shiny)
library(shiny)
library(shinydashboard)
library(data.table)
library(rlang)
library(ggplot2)
library(tidyverse)
library(ggQC)

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

ui <- fluidPage(
    dashboardHeader(),
    
    titlePanel("Tabsets"),
    
    dashboardSidebar( fileInput("file1", "Choose CSV File",
                                accept = c(
                                    "text/csv",
                                    "text/comma-separated-values,text/plain",
                                    ".csv")), 
                      # Select variables to display ----
                      uiOutput("dropdown"),
                      
                      numericInput(
                          "uslId",
                          "Upper Spec Limit",
                          value = NULL,
                          min = NA,
                          max = NA,
                          step = NA,
                          width = NULL
                      ),
                      
                      numericInput(
                          "lslId",
                          "Lower Spec Limit",
                          value = NULL,
                          min = NA,
                          max = NA,
                          step = NA,
                          width = NULL
                      )
    ),
    mainPanel(tabsetPanel(
        tabPanel("Plot", dataTableOutput("contents")),
        tabPanel("Individual", plotOutput("hist1")),
        tabPanel("Histogram" ,plotOutput("hist2"))
        
        
        
        
    ))
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
    
    
    # Dynamically generate UI input when data is uploaded ----
    output$dropdown <- renderUI({
        selectInput(inputId = "measure", 
                    label = "Select Measure", 
                    choices = names(winedata()))
    })
    
    # Select columns to print ----
    # df_sel <- reactive({
    #     req(input$measure)
    #     df_sel <- winedata() %>% select(input$measure)
    # })
    
    
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
        
        which(names(tempDf)==input$measure)
        
        print((noquote(input$measure)))
        
        
        
        tempVar <- sym(input$measure)
        
        INDV <- ggplot(tempDf, aes(x = row_num, y= !!tempVar)) +
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
        
        which(names(tempDf)==input$measure)
        
        print((noquote(input$measure)))
        
        
        tempVar <- sym(input$measure)
        
        
        #UCL= mean(tempDf[,input$measure]) + 3*sd(tempDf[,input$measure])
        #LCL= mean(tempDf$fixed_acidity) - 3*sd(tempDf$fixed_acidity)
        # print(UCL)
        # print(tempDf[,which(names(tempDf)==input$measure)])
        # print(tempDf %>% dplyr::select(input$measure))
        # mean(tempDf %>% dplyr::select(input$measure) %>% .[,1])
        
        ucl_var <- tempDf %>% dplyr::select(input$measure) %>% 
            pull(input$measure)
        
        print( mean(ucl_var) )
        
        
        
        UCL= mean(ucl_var) + 3*sd(ucl_var)
        LCL= mean(ucl_var) - 3*sd(ucl_var)
        # print(UCL)
        
        CapabilityAnaylsis <- 
            ggplot(tempDf, aes(x = !!tempVar)) + #init ggplot
            geom_histogram(binwidth = .75, color="purple") + #make the histogram
            stat_QC_Capability(
                LSL=input$lslId, USL=input$uslId,                    #Specify LSL and USL
                show.cap.summary = c("Cp", "Cpk"), #selected summary
                digits = 2,                        #report two digits
                method="XmR") +                    #Use the XmR method
            scale_x_continuous(expand =  expand_scale(mult = c(0.15,.65))) #pad the X-axis
        
        
        CapabilityAnaylsis + #plot the graph
            geom_vline(xintercept=mean(ucl_var), colour="Blue") +
            geom_text(aes(x=mean(ucl_var)), y = 25.5, label="Mean", colour="Blue", angle=0, vjust = 1.2, text=element_text(size=11))+
            geom_vline(xintercept=UCL, colour="Blue") +
            geom_text(aes(x=UCL), y = max(CapabilityAnaylsis$y), label="3sd", colour="blue", angle=0, vjust = 1.2, text=element_text(size=11))+
            geom_vline(xintercept=LCL, colour="Blue") +
            geom_text(aes(x=LCL), y = 25.5, label="-3sd", colour="blue", angle=0, vjust = 1.2, text=element_text(size=11))
        
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)