library(shiny)
library(spgwr)

shinyUI(fluidPage(titlePanel("Linear Regression"),
                  helpText("The data were collected from "),
                  
                  sidebarLayout(
                    sidebarPanel(
                      fileInput("file1", label = h3("File input"),accept = c(".xls")),
                      selectInput("select", label = h3("Select box"),
                                   choices = list( "Multivariate Regression"=3, "GWR" =2), 
                                   selected = 1),
                      
                      
                      # selectInput("region", label = h2("X-Vars"),
                      #              choices = list("kerja" = 1, "pertanian" = 2,"hotel"=3,
                      #                             "ipm"=4,"pmdn"=5,"pma"=6 ,"wisatawan"=7,"penduduk"=8)),
                      
                    #  conditionalPanel(condition = "input.select <= 2 ",
                                          checkboxGroupInput("region", label = h2("X-Vars"),
                                                       choices = list("kerja" = 1, "pertanian" = 2,"hotel"=3,
                                                                      "ipm"=4,"pmdn"=5,"pma"=6 ,"wisatawan"=7,"penduduk"=8))
                      
                      
                      
                      
                      )
                    
                  ,
                    mainPanel( tabsetPanel(
                      tabPanel("Contents",h1("Dataset"),tableOutput('contents')),
                     # tabPanel("Regression",h1("Result Calculation"),verbatimTextOutput("text0")),
                      
                      tabPanel("Regression Summary",h1("Result Calculation"),verbatimTextOutput("text1")),
                      #tabPanel("GWR Summary",h1("Result Calculation"),verbatimTextOutput("text2")),
                     tabPanel("Plot",plotOutput("plot"))#,
                     #tabPanel("PlotGWR",plotOutput("plot GWR"))
                      
                      
                      ))
                  )))