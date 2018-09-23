#
# Name: Ashok Raja
# Project # 1
# Andrew ID: araja1
# Dataset used was the FDA Orange book which contains the list of drug applications 
# link to data : https://www.fda.gov/downloads/Drugs/InformationOnDrugs/UCM163762.zip

# Loading libraries

library(shiny)
library(dplyr)
library(plyr)
library(plotly)
library(tibble)



# Fetching the dataset fda Orange Book  into fda
fda=read.csv("FDA_Orange_Book.csv")
fda$Approval_Date <- as.Date(fda$Approval_Date, format = "%m/%d/%Y")
fda$Approval
fda.load = fda

# Define UI for application 
ui <- navbarPage("FDA Drugs",
                 # Main tab panel to switch between graphs and data 
                 tabPanel("Plot",
                          sidebarPanel(
                            # Selecting the Drug in a selection box
                            selectInput("prod_select",
                                        "Drug:",
                                        choices = fda$Trade_Name,
                                        multiple = TRUE,
                                        selectize = TRUE
                            ),
                            #selecting the Approval Date using a slider
                            sliderInput("date_select",
                                        "Approval Date:",
                                        min = min(fda$Approval_Date, na.rm = T),
                                        max = max(fda$Approval_Date, na.rm = T),
                                        value = c(min(fda$Approval_Date, na.rm = T), max(fda$Approval_Date, na.rm = T)),
                                        step = 1
                            ),
                            #selecting the Application Type
                            checkboxGroupInput("app_select",
                                               "Application Type:",
                                               choices = levels(fda$Appl_Type),
                                               selected=1
                            )
                            
                          ),
                          # Main panel with the 3 graphs
#                          mainPanel(plotlyOutput("plot"),plotlyOutput("plot2"),plotlyOutput("plot3") )
                          mainPanel(plotlyOutput("plot") )
                 ),
                 # Data Panel
                 tabPanel("Data",
                          inputPanel(
                            # Download Button
                            downloadButton("downloadData","Download Orange Book")
                          ),
                          fluidPage(dataTableOutput("table")) 
                 )
)

# Define server logic 
server <- function(input, output, session=session) 
{
  # Caputing the inputs for reactive functions
#  swInput <- reactive({
#    fda=fda.load %>%
      # Filtering the slider for Approval Dates
#      filter(Approval_Date >=input$date_select[1] & Approval_Date <= input$date_select[2])
    # Filtering the products selected
#    if(length(input$prod_select)>0){
#      fda <- subset(fda, Trade_Name %in% input$prod_select)
#    }
    #Filtering the utilization type
#    if(length(input$app_select)>0){
#      fda <- subset(fda, Appl_Type %in% input$app_select)  
#    }
#    return(fda)
#  })
  
  
  # Plot the amount reimbursed by quarter
  output$plot <- renderPlotly({
#    fda=swInput()
    ggplotly(ggplot(data=fda,aes(x=Type))+
               geom_bar()+
               labs(title="Drug Approval Timeline",x="Approval Date",y="# of Approvals",colour="Application Type")
    )
  })
  # Plot the Units and AMount
#  output$plot2 <- renderPlotly({
#    fda=swInput()
#    ggplotly(ggplot(data=fda,aes(x=Units,y=Prescriptions,colour=Quarter,text=paste("<b>", Product.Name, ":</b> ")))+
#               geom_point()+
#               labs(title="Total Amount Reimbursed by Year",x="Units",y="Amount",colour="Quarter")
#             ,tooltip="text"
#    )
#  })
  # Plot the Product and amount
#  output$plot3 <- renderPlotly({
#    mdrp=swInput()
#    ggplotly(ggplot(data=mdrp,aes(x=Product.Name,y=sum(Amount),colour=Quarter))+
#               geom_col()+
#               theme(axis.text.x = element_text(angle = 60, hjust = 1,size=5)) +
#               labs(title="Total Amount Reimbursed by Year",x="Product",y="Amount",colour="Quarter")
#    )
#  })
  # Data table 
  output$table <- renderDataTable(fda)
  # Download function
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("download", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(fda, file)
    }
  )   

}


# Run the application 
shinyApp(ui = ui, server = server, enableBookmarking = "url")

