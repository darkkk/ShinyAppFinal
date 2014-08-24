
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
# 
# http://www.rstudio.com/shiny/
#

library(shiny)

shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("Job opportunity evaluation app"),
  
  
  sidebarPanel(
    h3("Description:"),
    p("This application calculates the optimal duration of a job search in years with some assumptions about behaviour and parameters.
      Person looking for a job is every year offered a well paid job with certain probability (parameter p), and less good job 
    (with probability 1-p). The good salary is known parameter, not so good salary is also known parameter. Every year the person can either accept the good job, accept the not so good job or keep waiting.
      However, once the job is chosen, person will keep it until the end. It is expected that in this (simplified) scenario it is not optimal to take bad job very early, even if the
      probability of getting the good job is not very high.

      "),
    p("This application use dynamic programming algorithm to solve this task deterministically."),
    h3("Parameters:"),
    tags$ul(
      tags$li("Duration period - the whole period in which person is looking to be employed (integer between 2-40, due to computational complexity)"), 
      tags$li("Good job annual salary - expected pay of good job (whole number >= 1)"), 
      tags$li("Not so good job annual salary - expected pay of not so good job (whole number >= 1)"),
      tags$li("Probability of getting the good job - the probability of being offered a good job every year (slider betweeen 0-1)")
    ),

    h3("Inputs:"),
    numericInput("numID1", "Duration period", value = 10, min = 2, max = 40,  step = 1),
    numericInput("numID2", "Good job annual salary", value = 100000, min = 1),
    numericInput("numID3", "Not so good job annual salary", value = 44000, min = 1),
    sliderInput("sliderID1", 
                "Probability of getting the good job", 
                min = 0, 
                max = 1, 
                value = 0.5,
                step = 0.01),
    actionButton("goButton", "Go!")
    
    
  ),

    mainPanel(
    h3("Results:"),
    h4("Quick result:"),
    tableOutput("quick"),
    
    h4("Graphical results:"),
    plotOutput("decision"),
    plotOutput("incomes"),
    

    h3("The parameters of previous calculation:"),
#     code("this is code"),
    tableOutput("varCheck")
#     verbatimTextOutput("oid1"),
#     verbatimTextOutput("oid2")
    
  )
))
