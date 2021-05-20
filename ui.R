library(shiny)
library(shinyjs)
library(stats)
library(graphics)
library(RColorBrewer)

shinyUI(fluidPage(
  useShinyjs(),
   h1("Thermal Shift Assay Analysis"),
  
  sidebarLayout(
    sidebarPanel(
      width=3,
      tags$div(id="General",  
               h3("File Upload"),
               fileInput('file1', 'Choose CSV File', accept=c('text/csv','text/comma-separated-values,text/plain','.csv')),   
               checkboxInput("checkbox", label = "Display CSV Header", value = FALSE)
      ),
      tags$div(id="TSA",
               radioButtons("tsa_radio", h4("Method"), c("Fraction Unfolded" = "unfold", "1st Derivative" ="dt")),
               radioButtons("tsa_style", h4("TSA Styles"), c("Points" ="dot","Line" = "line", "Both" = "both")),
               textInput("well_select", label="Well Select"), value="A1",
               sliderInput("linewidth", label = "Width of Line", min = 0, max = 5, value =1, step=1.0),
               sliderInput("TSA_datarange", label = "Select the range of temperatures to use in calculation:", min = 1, max = 100, value = c(25,75)),
               sliderInput("TSA_visrange", label = "Select x-axis range:", min = 1, max = 100, value = c(25,95))
      ),
    ),
    mainPanel(
      tableOutput("table1"),
      tags$span(id="plotarea", 
                tags$div(id="plot1", plotOutput("plot1", click="plot1_click")),
      )
    )#end 2nd main panel
  )#end sidebarLayout
))#end fluidPage and shinyUI