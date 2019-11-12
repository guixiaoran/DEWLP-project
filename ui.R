library(shiny)


library(ggplot2)
ui <- fluidPage(
  navbarPage("User Interface:",tabPanel("Upload",
                                        titlePanel("Uploading Files"),
                                        sidebarLayout(
                                          sidebarPanel(
                                            fileInput("file1", "Choose CSV File",
                                                      multiple = TRUE,
                                                      accept = c("text/csv",
                                                                 "text/comma-separated-values,text/plain",
                                                                 ".csv")),
                                            tags$hr(),
                                            checkboxInput("header", "Header", TRUE),
                                            radioButtons("sep", "Separator",
                                                         choices = c(Comma = ",",
                                                                     Semicolon = ";",
                                                                     Tab = "\t"),
                                                         selected = ","),
                                            tags$hr(),
                                            radioButtons("disp", "Display",
                                                         choices = c(Head = "head",
                                                                     All = "all"),
                                                         selected = "head"),
                                            radioButtons("quote", "Quote",
                                                         choices = c(None = "",
                                                                     "Double Quote" = '"',
                                                                     "Single Quote" = "'"),
                                                         selected = '"'),
                                            actionButton("predict_button","Predict distribution"),
                                            downloadButton("downloadData","Download data")),
                                          mainPanel(
                                            h3("Uploaded data"),
                                            tableOutput("contents"),
                                            h3("Prediction outcome"),
                                            tableOutput("contents2"),
                                            
                                            plotOutput("graph")
                                          ))), 
             tabPanel("Graphing",
                      titlePanel("Plotting Graphs"),
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("variable", "Variable:",
                                      list("Location")),
                          actionButton("Plot_button","Plot graph")),
                        mainPanel(
                          h3(textOutput("caption")),
                          tableOutput("contents3"),
                          h3("Distribution graph"),
                          plotOutput("ggplot")
                        )
                      ))
  ))
