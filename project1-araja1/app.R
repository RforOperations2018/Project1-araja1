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
fda$Approval_Year <- as.numeric(format(as.Date(fda$Approval_Date, format="%d/%m/%Y"),"%Y"))
fda$Patent_Expire_Date <- as.Date(fda$Patent_Expire_Date_Text, format = "%m/%d/%Y")
fda$Patent_Expire_Year <- as.numeric(format(as.Date(fda$Patent_Expire_Date, format="%d/%m/%Y"),"%Y"))
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
                            # Selecting the Drug Applicant in a selection box
                            selectInput("applicant_select",
                                        "Applicant:",
                                        choices = fda$Applicant,
                                        multiple = FALSE,
                                        selected="MYLAN PHARMS INC"
                            ),
                            #selecting the Approval Date using a slider
                            sliderInput("year_select",
                                        "Approval Year:",
                                        min = min(fda$Approval_Year, na.rm = T),
                                        max = max(fda$Approval_Year, na.rm = T),
                                        value = c(min(fda$Approval_Year, na.rm = T), max(fda$Approval_Year, na.rm = T)),
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
                          mainPanel(plotlyOutput("plot"),plotlyOutput("plot2"),plotlyOutput("plot3") )
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
  swInput <- reactive({
    fda=fda.load %>%
      # Filtering the slider for Approval Dates
      filter(Approval_Year >=input$year_select[1] & Approval_Year <= input$year_select[2])
    # Filtering the products selected
    if(length(input$prod_select)>0){
      fda <- subset(fda, Trade_Name %in% input$prod_select)
    }
    # Filtering the Applicants selected
    if(length(input$applicant_select)>0){
      fda <- subset(fda, Applicant %in% input$applicant_select)
    }
    #Filtering the Applicantion type
    if(length(input$app_select)>0){
      fda <- subset(fda, Appl_Type %in% input$app_select)  
    }
    return(fda)
  })
  
  
  # Plot the amount reimbursed by quarter
  output$plot <- renderPlotly({
    fda=swInput()
    ggplotly(ggplot(data=fda,aes(x=Approval_Year,colour=Type))+
               geom_bar()+
               labs(title="Drug Approval Timeline",x="Approval Date",y="# of Approvals",colour="Type")
    )
  })
  # Plot the Units and AMount
  output$plot2 <- renderPlotly({
    fda=swInput()
    ggplotly(ggplot(data=fda,aes(x=Patent_Expire_Year))+
               geom_freqpoly(aes(colour=Type))+
               labs(title="Upcoming Patent expiries",x="Year",y="# of Patents",colour="Type")
             
    )
  })
  # Plot the Product and amount
  output$plot3 <- renderPlotly({
    mdrp=swInput()
    ggplotly(ggplot(data=mdrp,aes(x=Route,colour=TE_Code))+
               geom_bar()+ coord_flip()+
               labs(title="# Drugs by Administration route by Therapeutic Equivalent",x="Administration Route",y="# of Products",colour="Thearpeutic Equivalent")
    )
  })
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

