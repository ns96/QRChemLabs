# Experiment 9 UI/Server module code
# Module used for development and testing
#
# https://shiny.rstudio.com/articles/modules.html
# 

# The UI code
exp09UI <- function(id, lab.number = 9) {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  # wrap ui components in tablist
  tagList(
    h3(paste("Experiment", lab.number, "-- Analysis of Vinegar")),
    
    fluidRow(
      box(title = "Initial Data", status = "primary", width = 12,
          textInput(ns("q1"), "1. Volume of Vinegar Used in Each Trial (mL):", value = "5.00"),
          span(textOutput(ns("v1")), style="color:blue"),
          
          textInput(ns("q2"), "2. Molarity of NaOH Used in Each Trial:", value = "0.200"),
          span(textOutput(ns("v2")), style="color:blue"),
          
          textInput(ns("q3"), "3. Volume of NaOH Used in Trail#1 (mL):"),
          span(textOutput(ns("v3")), style="color:blue"),
          
          textInput(ns("q4"), "4. Volume of NaOH Used in Trail#2 (mL):"),
          span(textOutput(ns("v4")), style="color:blue"),
          
          textInput(ns("q5"), "5.	Average Volume of NaOH Used (mL):"),
          span(textOutput(ns("v5")), style="color:blue")
      )
    ),

    fluidRow(
      box(title = 'Calculations', status = "primary", width = 12,
          textInput(ns("q6"), "6.	Molarity of Acetic Acid in Vinegar (M):"),
          span(textOutput(ns("v6")), style="color:blue"),
          
          textInput(ns("q7"), "7.	Molecular Weight of Acetic Acid (g/mol):", value="60.052"),
          span(textOutput(ns("v7")), style="color:blue"),
          
          textInput(ns("q8"), "8.	Grams of Acetic Acid in one Liter of Vinegar (g):"),
          span(textOutput(ns("v8")), style="color:blue"),
          
          textInput(ns("q9"), "9.	Grams of Acetic Acid in 100 mL of Vinegar (g):"),
          span(textOutput(ns("v9")), style="color:blue"),
          
          textInput(ns("q10"), "10.	Calculate % of Acetic Acid in Vinegar:"),
          span(textOutput(ns("v10")), style="color:blue")
      )  
    ),
    
    fluidRow(
      # add button to check data
      column(3, actionButton(ns("load"), "Load Sample Data")),
      column(3, actionButton(ns("check"), "Check Input Data")),
      column(3, span(textOutput(ns("dbm")), style="color:green")),
      column(3, actionButton(ns("view"), "View Saved Data"))
    )
  )
}

# Server code
exp09 <- function(input, output, session, pin) {
  # load data for students
  observeEvent(input$load, {
    q3 = round(runif(1, min=16, max=21), digits = 1)
    q3v = format(q3, nsmall = 1)
    updateTextInput(session, "q3", value = q3v)
    
    q4 = round(runif(1, min=21, max=26), digits = 1)
    q4v = format(q4, nsmall = 1)
    updateTextInput(session, "q4", value = q4v)
  })
  
  observeEvent(input$check, {
    qlist = list()
    
    # load initial data
    q1 = as.numeric(input$q1)
    qlist["q1"] = q1
    
    q2 = as.numeric(input$q2)
    qlist["q2"] = q2
    
    q3 = as.numeric(input$q3)
    qlist["q3"] = q3
    
    q4 = as.numeric(input$q4)
    qlist["q4"] = q4
    
    q5 = as.numeric(input$q5)
    ans5 = (q3 + q4)/2
    error5 = abs(q5 - ans5)
    valid5 = error5 < 0.5
    output$v5 <- renderText({ showValid(valid5, ans5, pin, 1) })
    qlist["q5"] = q5
    
    q6 = as.numeric(input$q6)
    ans6 = (q2*ans5)/q1
    #print(cat("Q6 Debug", q1L, q5L, ans6))
    error6 = abs(q6 - ans6)
    valid6 = error6 < 0.0005
    output$v6 <- renderText({ showValid(valid6, ans6, pin, 3) })
    qlist["q6"] = q6
    
    q7 = as.numeric(input$q7)
    ans7 = 60.052
    error7 = abs(q7 - ans7)
    valid7 = error7 < 0.005
    output$v7 <- renderText({ showValid(valid7, ans7, pin, 3) })
    qlist["q7"] = q7
    
    q8 = as.numeric(input$q8)
    ans8 = ans6*ans7
    error8 = abs(q8 - ans8)
    valid8 = error8 < 0.05
    output$v8 <- renderText({ showValid(valid8, ans8, pin, 1) })
    qlist["q8"] = q8
    
    q9 = as.numeric(input$q9)
    ans9 = ans8/10.0
    error9 = abs(q9 - ans9)
    valid9 = error9 < 0.005
    output$v9 <- renderText({ showValid(valid9, ans9, pin) })
    qlist["q9"] = q9
    
    q10 = as.numeric(input$q10)
    ans10 = ans9
    error10 = abs(q10 - ans10)
    valid10 = error10 < 0.5
    output$v10 <- renderText({ showValid(valid10, ans10, pin) })
    qlist["q10"] = q10
    
    # save to the database now
    dbm = saveToDB(pin, "EXP09", qlist)
    output$dbm <- renderText({dbm})
    
    cat("Experiment 9", "PIN", pin, "\n")
  })
  
  # handle viewing of old data
  observeEvent(input$view, {
    showModal(modalDialog(
      title = "Experiment 9 Saved Data",
      HTML(getSavedData(pin, "EXP09"))
    ))
    
    cat("View Data -- EXP09", "\n")
  })
}