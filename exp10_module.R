# Experiment 10 UI/Server module code
# Module used for development and testing
#
# https://shiny.rstudio.com/articles/modules.html
# 

# The UI code
exp10UI <- function(id, lab.number = 10) {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  # wrap ui components in tablist
  tagList(
    h3(paste("Experiment", lab.number, "-- Analysis of an Antacid")),
    
    fluidRow(
      box(title = "Initial Data", status = "primary", width = 12,
          textInput(ns("q1"), "1. Volume of Milk of Magnesia Used in Each Trial (mL):", value = '5.00'),
          span(textOutput(ns("v1")), style="color:blue"),
          
          textInput(ns("q2"), "2. Molarity of NaOH Used in Each Trial:", value = '0.200'),
          span(textOutput(ns("v2")), style="color:blue"),
          
          textInput(ns("q3"), "3. Volume of NaOH Used in Trail#1 (mL):", value = ''),
          span(textOutput(ns("v3")), style="color:blue"),
          
          textInput(ns("q4"), "4. Volume of NaOH Used in Trail#2 (mL):", value = ''),
          span(textOutput(ns("v4")), style="color:blue"),
          
          textInput(ns("q5"), "5.	Average Volume of NaOH Used (L):", value = ''),
          span(textOutput(ns("v5")), style="color:blue")
      )
    ),

    fluidRow(
      box(title = 'Calculations', status = "primary", width = 12,
          textInput(ns("q6"), "6. Moles of NaOH Used:", value = ''),
          span(textOutput(ns("v6")), style="color:blue"),
          
          textInput(ns("q7"), "7.	Volume of standard HCl Used in Each Trial (L):", value = '0.0160'),
          span(textOutput(ns("v7")), style="color:blue"),
          
          textInput(ns("q8"), "8. Exact Molarity of Standard HCl Used in Each Trial (M):", value = '1.00'),
          span(textOutput(ns("v8")), style="color:blue"),
          
          textInput(ns("q9"), "9. Moles of Standard HCl Used:", value = ''),
          span(textOutput(ns("v9")), style="color:blue"),
          
          textInput(ns("q10"), "10. Moles of HCl Neutralized by Milk of Magnesia:", value = ''),
          span(textOutput(ns("v10")), style="color:blue"),
          
          textInput(ns("q11"), "11. Moles of  Mg(OH)2 per 5.00 mL of Milk of Magnesia:", value = ''),
          span(textOutput(ns("v11")), style="color:blue"),
          
          textInput(ns("q12"), "12.	Grams of Mg(OH)2 per 5.00 mL of Milk of Magnesia (g):", value = ''),
          span(textOutput(ns("v12")), style="color:blue"),
          
          textInput(ns("q13"), "13.	Milligrams of Mg(OH)2 per 5.00 mL of Milk of Magnesia (mg):", value = ''),
          span(textOutput(ns("v13")), style="color:blue"),
          
          textInput(ns("q14"), "14.	Actual milligrams of Mg(OH)2 per 5.00 mL of Milk of Magnesia (mg):", value = '400'),
          span(textOutput(ns("v14")), style="color:blue"),
          
          textInput(ns("q15"), "Percent Error:", value = ''),
          span(textOutput(ns("v15")), style="color:blue")
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
exp10 <- function(input, output, session, pin) {
  # load data for students
  observeEvent(input$load, {
    q3 = round(runif(1, min=10, max=16), digits = 1)
    q3v = format(q3, nsmall = 1)
    updateTextInput(session, "q3", value = q3v)
    
    q4 = round(runif(1, min=12, max=18), digits = 1)
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
    ans5 = ((q3 + q4)/2)/1000
    error5 = abs(q5 - ans5)
    valid5 = error5 < 0.005
    output$v5 <- renderText({ showValid(valid5, ans5, pin, 4) })
    qlist["q5"] = q5
    
    q6 = as.numeric(input$q6)
    ans6 = q2*ans5
    error6 = abs(q6 - ans6)
    valid6 = error6 < 0.0005
    output$v6 <- renderText({ showValid(valid6, ans6, pin, 5) })
    qlist["q6"] = q6
    
    q7 = as.numeric(input$q7)
    qlist["q7"] = q7
    
    q8 = as.numeric(input$q8)
    qlist["q8"] = q8
    
    q9 = as.numeric(input$q9)
    ans9 = q7*q8
    error9 = abs(q9 - ans9)
    valid9 = error9 < 0.0005
    output$v9 <- renderText({ showValid(valid9, ans9, pin, 4) })
    qlist["q9"] = q9
    
    q10 = as.numeric(input$q10)
    ans10 = ans9 - ans6
    error10 = abs(q10 - ans10)
    valid10 = error10 < 0.0005
    output$v10 <- renderText({ showValid(valid10, ans10, pin, 4) })
    qlist["q10"] = q10
    
    q11 = as.numeric(input$q11)
    ans11 = ans10*0.5
    error11 = abs(q11 - ans11)
    valid11 = error11 < 0.0005
    output$v11 <- renderText({ showValid(valid11, ans11, pin, 4) })
    qlist["q11"] = q11
    
    q12 = as.numeric(input$q12)
    ans12 = ans11*58.32
    error12 = abs(q12 - ans12)
    valid12 = error12 < 0.005
    output$v12 <- renderText({ showValid(valid12, ans12, pin, 3) })
    qlist["q12"] = q12
    
    q13 = as.numeric(input$q13)
    ans13 = ans12*1000
    error13 = abs(q13 - ans13)
    valid13 = error13 < 5
    output$v13 <- renderText({ showValid(valid13, ans13, pin, 0) })
    qlist["q13"] = q13
    
    q14 = as.numeric(input$q14)
    qlist["q14"] = q14
    
    q15 = as.numeric(input$q15)
    ans15 = (abs(q14 - ans13)/q14)*100
    error15 = abs(q15 - ans15)
    valid15 = error15 < 0.5
    
    if(ans15 < 10) {
      output$v15 <- renderText({ showValid(valid15, ans15, pin, 2) })
    } else {
      output$v15 <- renderText({ showValid(valid15, ans15, pin, 1) })
    }
    
    qlist["q15"] = q15
    
    # save to the database now
    dbm = saveToDB(pin, "EXP10", qlist)
    output$dbm <- renderText({dbm})
    
    cat("Experiment 10", "PIN", pin, "\n")
  })
  
  # handle viewing of old data
  observeEvent(input$view, {
    showModal(modalDialog(
      title = "Experiment 10 Saved Data",
      HTML(getSavedData(pin, "EXP10"))
    ))
    
    cat("View Data -- EXP10", "\n")
  })
}