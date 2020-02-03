library(shiny); library(shinythemes)
library(DT); library(dplyr); library(ggplot2)
library(knitr)
source("percentileCalculator.R")
source("BPappCalculations.R")

ui <- navbarPage("BP Percentiles", theme = shinytheme("flatly"), collapsible = T,
  header = 
    tags$head(
        includeHTML("google-analytics.js"),
        tags$style(HTML("
                        #test {
                          padding: 100px;
                        }
                        .navbar {
                          margin: 0px;
                        }
                        .footer {
                            position: relative;
                            left: 0;
                            bottom: 0;
                            width: 100%;
                            background-color: #d7dfea;
                            # color: white;
                            text-align: center;
                        }
                        "))
      ),
    
  tabPanel("Calculator", id="test", 
      div(h2("Blood pressure percentiles using the 2017 AAP Guidelines for Screening and Management of High Blood Pressure"),br()),
      sidebarLayout(
        sidebarPanel(width = 3,
          div(fluidRow(
            column(6, numericInput("age", label = h3("Age (years)"), value = 10)),
            column(6, radioButtons("sex", label = h3("Sex"), choices = list("Male" = "M", "Female" = "F"), selected = "M"))
          )),
          div(fluidRow(
            column(6, numericInput("height", label = h3("Height"), value = 141.3)),
            column(6, radioButtons("heightUnit", label = h3("Unit"), choices = list("Centimeters" = "cm", "Inches" = "in"), selected = "cm"))
          )),
          div(fluidRow(
            column(6, numericInput("SBP", label = h3("Systolic BP"), value = 100)),
            column(6, numericInput("DBP", label = h3("Diastolic BP"), value = 62))
          ))
        ),
        mainPanel(
          div(DT::dataTableOutput("componentTable"), style = "overflow-x: auto;"),
          br(),
          fluidRow(
            column(6, plotOutput("plotSBP")),
            column(6, plotOutput("plotDBP"))
          ),
          div(br()),
          div(br())
        )
      )
    ),
  tabPanel("Methods",
    # h3("Coming soon")
     div(
       includeHTML("HTNmethod.html"
       )
     )
  )
  # footer = div(class="footer",
  #     column(2, offset=10, HTML("<p>Created by Ben Solomon &copy; 2018</p>"))
  # )
)

server <- function(input, output) {
  #Input validation and conversions
  age <- reactive({
    validate(need(input$age > 0 & input$age < 19, "Please input an age between 0 and 19 years old"))
    input$age
  })
  SBP <- reactive({
    validate(need(input$SBP > 50 & input$SBP < 300, "Please input an appropriate systolic blood pressure"))
    input$SBP
  })
  DBP <- reactive({
    validate(need(input$DBP > 25 & input$DBP < 200, "Please input an appropriate diastolic blood pressure"))
    input$DBP
  })
  height <- reactive({
    if (input$heightUnit == "cm"){
      height <- input$height
    } else if (input$heightUnit == "in"){
      height <- input$height * 2.54
    }
    validate(need(height > 0 & height < 245, "Please input an appropriate height"))
    height
  })
  #Calculations
  S <- reactive({
    BPappCalculations(age = age(), height = height(), sex = input$sex, BP = SBP(), SBP.DBP = "SBP")
  })
  
  D <- reactive({
    BPappCalculations(age = age(), height = height(), sex = input$sex, BP = DBP(), SBP.DBP = "DBP")
  })
  #Data table generation
  output$componentTable = DT::renderDataTable({
    S <- c(SBP(), floor(unlist(S()[2:6])))
    D <- c(DBP(), floor(unlist(D()[2:6])))
      datatable(
        data.frame(rbind(S, D)),
        colnames = c("Patient BP", "BP percentile", "50th percentile", "90th percentile", "95th percentile", "95th percentile + 12mmHg"),
        rownames = c("Systolic", "Diastolic"),
        caption = htmltools::tags$caption(
          style = 'caption-side: bottom; text-align: right;',
          "Note: patient percentiles may not precisely match cut off percentiles due to the effects of rounding"),
        class = "cell-border",
        options = list(dom = "t", 
                       ordering = F,
                       columnDefs = list(list(className = 'dt-center', targets = 0:6)))
      )
    })
  #Plot generation
  output$plotSBP <- renderPlot({
    S()$plot + ggtitle("Systolic BP percentiles")
  })
  
  output$plotDBP <- renderPlot({
    D()$plot +ggtitle("Diastolic BP percentiles")
  })
}

shinyApp(ui, server)
