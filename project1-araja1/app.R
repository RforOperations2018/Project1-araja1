#
# Name: Ashok Raja
# Project # 1
# Andrew ID: araja1
# Dataset used was the FDA Orange book which contains the list of drug applications - filtered for Mylan due to size limitations
# link to data : https://www.fda.gov/downloads/Drugs/InformationOnDrugs/UCM163762.zip
# Shiny app online link : https://ashokkumarraja.shinyapps.io/project1-araja1/

# Loading libraries

library(shiny)
library(dplyr)
library(plyr)
library(plotly)
library(tibble)
library(shinydashboard)
library(reshape2)
library(shinythemes)



# Fetching the dataset fda Orange Book  into fda
fda=read.csv("FDA_Orange_Book-Mylan.csv")
fda$Approval_Date <- as.Date(fda$Approval_Date, format = "%m/%d/%Y")
fda$Approval_Year <- as.numeric(format(as.Date(fda$Approval_Date, format="%d/%m/%Y"),"%Y"))
fda$Patent_Expire_Date <- as.Date(fda$Patent_Expire_Date_Text, format = "%m/%d/%Y")
fda$Patent_Expire_Year <- as.numeric(format(as.Date(fda$Patent_Expire_Date, format="%d/%m/%Y"),"%Y"))
fda.load = fda
# Some day I'm gonna get you to use dplyr ;-)

header <- dashboardHeader(title = "FDA Orange Book")

sidebar <- dashboardSidebar(
  sidebarMenu(
    id = "tabs",
    menuItem("Plot", icon = icon("bar-chart"), tabName = "plot"),
    menuItem("Table", icon = icon("table"), tabName = "table"),
    # Where is my third page?!
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
                selected="MYLAN PHARMS INC" # This default does not appear on shinyapps.io My guess is leading or trailing white space. When cleaning your data on load check out the trimws() function
    ),
    #selecting the Approval Date using a slider
    sliderInput("year_select",
                "Approval Year:",
                min = min(fda$Approval_Year, na.rm = T),
                max = max(fda$Approval_Year, na.rm = T),
                value = c(min(fda$Approval_Year, na.rm = T), max(fda$Approval_Year, na.rm = T)),
                step = 1
                # Bah, there are commas in years!
    ),
    #selecting the Application Type
    checkboxGroupInput("app_select",
                       "Application Type:",
                       choices = levels(fda$Appl_Type),
                       selected=1 # You need this to be the value, so I'm guessing "A"? If you want the first level put llevels(fda$Appl_Type)[1]
    )
  )
)



body <- dashboardBody(tabItems(
  tabItem("plot",
          fluidRow(
            # info anf value boxes - note- Applicants os fo the entire data set and is intentionally hnot filtered by selection
            infoBoxOutput("applicants"),
            valueBoxOutput("products"),
            valueBoxOutput("patents")
          ),
          fluidRow(
            # Rendering the 3 differnt plots in tabs
            tabBox(title = "Plot",
                   width = 12,
                   tabPanel("Approvals", plotlyOutput("plot")),
                   tabPanel("Patents", plotlyOutput("plot2")),
                   tabPanel("Route and TE", plotlyOutput("plot3")))
          )
  ),
  tabItem("table",
          # Rendering the table
          fluidPage(
            box(title = "Orange Book Data", DT::dataTableOutput("table"), width=300))
  )
)
)

# passing the ui to the dashboard
ui <- dashboardPage(header, sidebar, body)

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
  
  
  # Plot for approved applicatnion by year
  output$plot <- renderPlotly({
    fda=swInput()
    ggplotly(ggplot(data=fda,aes(x=Approval_Year,colour=Type))+
               geom_bar()+
               labs(title="Drug Approval Timeline",x="Approval Date",y="# of Approvals",colour="Type")
    )
  })
  # Plot the upcomgin aptent expiry
  output$plot2 <- renderPlotly({
    fda=swInput()
    ggplotly(ggplot(data=fda,aes(x=Patent_Expire_Year))+
               geom_freqpoly(aes(colour=Type))+
               labs(title="Upcoming Patent expiries",x="Year",y="# of Patents",colour="Type")
             
    )
  })
  # Plot the administration route and therapeutic equivalent
  output$plot3 <- renderPlotly({
    fda=swInput()
    ggplotly(ggplot(data=fda,aes(x=Route,colour=TE_Code))+
               geom_bar()+ coord_flip()+
               labs(title="# Drugs by Administration route by Therapeutic Equivalent",x="Administration Route",y="# of Products",colour="Thearpeutic Equivalent")
    )
  })
  # Data table 
  output$table <- DT::renderDataTable(fda <-swInput(), options = list(scrollX = TRUE))
  # Download function
  


  
    # Toal Applicants info box- not filtered intentionally
  output$applicants <- renderInfoBox({
    num <- length(unique(fda.load$Applicant))
    
    infoBox("Total # Applicants", value = num, icon = icon("balance-scale"), color = "purple")
   
  })

  # Total products per selection
  output$products <- renderValueBox({
    fda=swInput()
    num <- length(unique(fda$Trade_Name))
    
    valueBox(subtitle = "Products for selection", value = num, icon = icon("sort-numeric-asc"), color = "green")
    
    
  }) 
  
  # Total patents per selection
  output$patents <- renderValueBox({
    fda=swInput()
    num <- length(unique(fda$Patent_No))
    
    valueBox(subtitle = "Patents per selection", value = num, icon = icon("sort-numeric-asc"), color = "red")
    
    
  }) 
  
}


# Run the application 
shinyApp(ui = ui, server = server, enableBookmarking = "url")

