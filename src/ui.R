library(shiny)
library(ggplot2)

shinyUI(fluidPage(
  
  # application title
  titlePanel("Bayesian A/B Testing Calculator"),
  h3("The Beta-Bernoulli Model"),
  
  sidebarLayout(
    
    sidebarPanel(
      numericInput("nsims", label = h3("Number of Simulations"), value = 5000, step = 1),
      
      hr(),
      h3("Prior Parameters"),
      numericInput("alpha", label = "Alpha", value = 10, step = 0.1),
      numericInput("beta", label = "Beta", value = 10, step = 0.1),
      
      hr(),
      h3("Control Results"),
      numericInput("cs", label = "Successes", value = 0, step = 1),
      numericInput("cf", label = "Failures", value = 0, step = 1),
      
      hr(),
      h3("Test Results"),
      numericInput("ts", label = "Successes", value = 0, step = 1),
      numericInput("tf", label = "Failures", value = 0, step = 1),
      
      hr(),
      checkboxInput("gauss", label = "Show Density Estimate", value = FALSE),
      conditionalPanel(condition = "input.gauss == true",
                       checkboxInput("remove_hist", label = "Remove Histogram", value = FALSE),
                       sliderInput("bw_adjust", label = "Bandwidth Adjustment",
                                   min = 0.2, max = 2, value = 1, step = 0.2), hr()),
      checkboxInput("hpdi", label = "Show Highest Posterior Density Interval", value = FALSE),
      conditionalPanel(condition = "input.hpdi == true",
                       numericInput("hpdilevel", label = "Confidence Level", value = 95, step = 0.5), hr()),
      checkboxInput("qbpi", label = "Show Quantile-Based Probability Interval", value = FALSE),
      conditionalPanel(condition = "input.qbpi == true",
                       numericInput("qbpilevel", label = "Confidence Level", value = 95, step = 0.5)),
      
      hr(),
      actionButton("updateButton", "Update Model")
    ),
    
    mainPanel(
      h3("The Success Probability Distributions in Test and Control Groups"),
      plotOutput("plot"),
      hr(),
      h3("Test-Control Histogram"),
      h4("Distribution of Differences in Successs Probability between Test and Control Groups"),
      plotOutput("diff"),
      hr(),
      strong("The Average Difference between Test and Control"),
      verbatimTextOutput("rev"),
      strong("The Probability that Test performs better than Control"),
      verbatimTextOutput("bet"),
      fluidRow(
        column(6,
               conditionalPanel(condition = "input.hpdi == true", hr(), 
                                h3("HPD Intervals"), br(),
                                strong("Control Distribution"),
                                verbatimTextOutput("summary1"),
                                strong("Test Distribution"),
                                verbatimTextOutput("summary2"),
                                strong("Difference Distribution"),
                                verbatimTextOutput("summary5"))),
        column(6,
               conditionalPanel(condition = "input.qbpi == true", hr(),
                                h3("Quantile-Based Probability Intervals"), br(),
                                strong("Control Distribution"),
                                verbatimTextOutput("summary3"),
                                strong("Test Distribution"),
                                verbatimTextOutput("summary4"),
                                strong("Difference Distribution"),
                                verbatimTextOutput("summary6")))
      ))
  )))
