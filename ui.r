library(shiny)

shinyUI(pageWithSidebar(
  headerPanel(title="CA One: Apllied Stats and Machine Learning"),
  sidebarPanel(
    
    selectInput("ds","Data Source", choices = c("File" = "file",
                                                "URL" = "url",
                                                "In-Build" = "ib",
                                                "Yahoo Finance" = "yf")),
    # Input: Select a file --
    
    conditionalPanel(
      condition = "input.ds == 'file'",
      fileInput("datafile", "Choose CSV File",
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separted-vlues, text/plain",
                           ".csv"))
      
    ),
    
    conditionalPanel(
      condition = "input.ds == 'url'",
      textInput("link", "Input URL", value = "http://users.stat.ufl.edu/~winner/data/nfl2008_fga.csv")
    ),
    
    conditionalPanel(
      condition = "input.ds == 'ib'",
      selectInput(inputId = "ib", "Select a Dataset", choices = ls("package:datasets"))
    ),
    conditionalPanel(
      condition = "input.ds == 'yf'",
      textInput("yf", "Enter Ticker Name", value = "CTSH")
    ),
    selectInput(inputId = "analytics", "Choose Type of Analysis", choices = c("Descriptive Analysis"="da",
                                                                              "Probabilistic Modelling"="pm",
                                                                              "Machine Learning"="glm")),
    conditionalPanel(
      condition = "input.analytics == 'glm'",
      uiOutput("ModelSelection"),
      selectInput(inputId = "tarvar", "Select a Target Variable", choices = ""),
      selectInput(inputId = "indvar", "Select Independent Variables", multiple = TRUE, choices = ""),
      sliderInput("ratio", "Ratio for trainset",min=1,max= 100,value=70),
      uiOutput("Input_Ind"),
      actionButton("do_pred", "Predict")
    ),
    conditionalPanel(
      condition = "input.analytics == 'da'",
      uiOutput("ExploreWay"),
      actionButton("Go", "Process")
      
    ),
    conditionalPanel(
      condition = "input.analytics == 'pm'",
      selectInput(inputId="attr", "Select A Column for Probabilistic Modelling", choices = ""),
      selectInput("dismodel", "Select Model",
                  choices = c("Binomial" = "binomial",
                              "Poisson" = "poisson",
                              "Geometric" = "geometric",
                              "Bernouilli" = "bernoulli",
                              "Multinomial"= "multinomial",
                              "Hypergeometric" = "hypergeometric"),
                  selected = "binomial"
      ),
      conditionalPanel(
        condition = "input.dismodel == 'bernoulli'",
        
        
      ),
      conditionalPanel(
        condition = "input.dismodel == 'multinomial'",
        numericInput("n", "parameter n in Multinomial" , value = 10),
        #numericInput("samVar2", "upper limit for x2" , value = 6),
        #numericInput("samVar3", "upper limit for x3" , value = 7)
      ),
      conditionalPanel(
        condition = "input.dismodel == 'hypergeometric'",
        numericInput("n", "parameter n in HyperGeometric" , value = 10),
        numericInput("N", "parameter N in HyperGeometric" , value = 15),
        numericInput("k", "parameter k in HyperGeometric" , value = 7),
        
      ),
      conditionalPanel(
        condition = "input.dismodel == 'binomial'",
        numericInput("n", "parameter n in Binomial" , value = 10),
        
      ),
      
      conditionalPanel(    
        condition = "input.dismodel == 'poisson'",
        
      ),
      
      conditionalPanel(    
        condition = "input.dismodel == 'geometric'",
        
      ),
      
      numericInput("max", "upper limit for x" , value = 5), 
      sliderInput("s", "number of simulated data" ,min=1, max=1000, value = 10), 
      
      #conditionalPanel(
      #condition = "input.dismodel == 'binomial'",
      #numericInput("j1", "j for Bin" , value = 1)
      #),
      
      #conditionalPanel(
      #condition = "input.dismodel == 'poisson'",
      #numericInput("j2", "j for Poisson" , value = 1)
      #),
      
      #conditionalPanel(
      #condition = "input.dismodel == 'geometric'",
      #numericInput("j3", "j for geometric" , value = 1)
      #)
      
      
    )
  ),
  mainPanel(
    tabsetPanel(type = "tabs",
                tabPanel("Data Set", DT:: dataTableOutput("extdata")),
                tabPanel("Descriptive Analysis",
                         tabsetPanel(
                           tabPanel("Descriptions",verbatimTextOutput("Summary")),
                           tabPanel("Plots",uiOutput("PlotTypes"),
                                    uiOutput("Plots"))
                         )
                ),
                
                tabPanel("Discrete Probability Models",
                         tabsetPanel(
                           
                           tabPanel("Plots",plotOutput("ProbPlot"))
                         )
                ),
                
                tabPanel("Machine Learning",
                         tabsetPanel(
                           tabPanel("Selected", DT:: dataTableOutput("selData")),
                           tabPanel("Test/Predicted", plotOutput("glmperf")),
                           tabPanel("RMSE/Accuracy", DT:: dataTableOutput("RMSE")),
                           tabPanel("Prediction for Poisson", DT:: dataTableOutput("Prediction")),
                           tabPanel("Prediction for Logistic", DT:: dataTableOutput("PredictionLogistic"))
                         )
                )
                
                
                
                
    )
  )
)
)