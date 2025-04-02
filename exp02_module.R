# Experiment 2 UI/Server module code
# @ version 0.1 (09/18/2018)
# https://shiny.rstudio.com/articles/modules.html
# 

# The UI code
exp02UI <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  # wrap ui components in tablist
  tagList(
    h3("Experiment 2 -- Density"),
    
    fluidRow(
      box(title = "Part-A density of water", status = "primary",
          textInput(ns("q6"), "6. Mass of 10.0 mL water (g):"),
          span(textOutput(ns("v6")), style="color:blue"),
          
          textInput(ns("q7"), "7. Mass of 20.0 mL water (g):"),
          span(textOutput(ns("v7")), style="color:blue"),
          
          textInput(ns("q8"), "8.	Mass of 30.0 mL water (g):"),
          span(textOutput(ns("v8")), style="color:blue"),
          
          textInput(ns("q9"), "9.	Mass of 40.0 mL water (g):"),
          span(textOutput(ns("v9")), style="color:blue")
      ),
      
      # density plot
      box(title = "Density Plot", status = "primary",
          plotlyOutput(ns("plot1"))
      )
    ),
    
    fluidRow(
      box(title = "Part-B Unknown Liquid (UL)", status = "primary",
          textInput(ns("u1"), "Unknown Liquid Number:"),
          span(textOutput(ns("u1v")), style="color:blue"),
          
          textInput(ns("q10"), "10.	Mass Graduated cylinder (g):"),
          span(textOutput(ns("v10")), style="color:blue"),
          
          textInput(ns("q11"), "11.	Mass Graduated cylinder + 10.0 mL UL (g):"),
          span(textOutput(ns("v11")), style="color:blue"),
          
          textInput(ns("q12"), "12.	Mass of 10.0 mL UL (g):"),
          span(textOutput(ns("v12")), style="color:blue"),
          
          textInput(ns("q13"), "13.	Density of Unknown Liquid (g/mL):"),
          span(textOutput(ns("v13")), style="color:blue")
      ),
      
      # part C
      box(title = "Part-C Density of Black Rubber Stoppers", status = "primary",
          textInput(ns("q14"), "14.	Mass of Rubber stoppers (g):"),
          span(textOutput(ns("v14")), style="color:blue"),
          
          textInput(ns("q15"), "15. Volume of Rubber stoppers (cc):"),
          span(textOutput(ns("v15")), style="color:blue"),
          
          textInput(ns("q16"), "16. Density of Rubber stoppers (g/cc):"),
          span(textOutput(ns("v16")), style="color:blue")
      )
    ),
    
    fluidRow(
      box(title = "Part-D Density of Unknown Metal ", status = "primary",
          textInput(ns("u2"), "Unknown Metal #:"),
          span(textOutput(ns("u2v")), style="color:blue"),
          
          textInput(ns("q17"), "17.	Mass of Unknown Metal (g):"),
          span(textOutput(ns("v17")), style="color:blue"),
          
          textInput(ns("q18"), "18. Volume of Unknown Metal (cm3):"),
          span(textOutput(ns("v18")), style="color:blue"),
          
          textInput(ns("q19"), "19. Density of Unknown Metal (g/cm3):"),
          span(textOutput(ns("v19")), style="color:blue")
      ),
      
      box(title = "Part-E Thickness of Aluminum Foil", status = "primary",
          textInput(ns("q20"), "20. Length of Aluminum foil (cm):"),
          span(textOutput(ns("v20")), style="color:blue"),
          
          textInput(ns("q21"), "21.	Width of Aluminum foil (cm):"),
          span(textOutput(ns("v21")), style="color:blue"),
          
          textInput(ns("q22"), "22.	Mass of Aluminum foil (g):"),
          span(textOutput(ns("v22")), style="color:blue"),
          
          textInput(ns("q23"), "23. Density of Aluminum foil (g/cm3):", value = "2.70"),
          span(textOutput(ns("v23")), style="color:blue"),
          
          textInput(ns("q24"), "24.	Volume of Aluminum foil (cm3):"),
          span(textOutput(ns("v24")), style="color:blue"),
          
          textInput(ns("q25"), "25. Thickness of Aluminum foil (cm):"),
          span(textOutput(ns("v25")), style="color:blue")
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
exp02 <- function(input, output, session, pin) {
  # load sample data for students
  observeEvent(input$load, {
    q6 = round(runif(1, min=9, max=11), digits = 4)
    q6v = format(q6, nsmall = 4)
    updateTextInput(session, "q6", value = q6v)
    
    q7 = round(runif(1, min=19, max=21), digits = 4)
    q7v = format(q7, nsmall = 4)
    updateTextInput(session, "q7", value = q7v)
    
    q8 = round(runif(1, min=29, max=31), digits = 4)
    q8v = format(q8, nsmall = 4)
    updateTextInput(session, "q8", value = q8v)
    
    q9 = round(runif(1, min=39, max=41), digits = 4)
    q9v = format(q9, nsmall = 4)
    updateTextInput(session, "q9", value = q9v)
    
    # plot data with regression line
    output$plot1 <- renderPlotly({
      x = c(0.0, 10.0, 20.0, 30.0, 40.0)
      y = c(0.0, q6, q7, q8, q9)
      data = data.frame(x, y)
      
      getRegressionPlot(data, "Water Density Plot", "Volume (mL)", "Mass (g)")
    })
    
    u1v = sample(1:9, 1)
    updateTextInput(session, "u1", value = u1v)
    
    q10 = round(runif(1, min=101, max=112), digits = 4)
    q10v = format(q10, nsmall = 4)
    updateTextInput(session, "q10", value = q10v)
    
    q11 = round(getQ11DataExp02(u1v, q10), digits = 4)
    q11v = format(q11, nsmall = 4)
    updateTextInput(session, "q11", value = q11v)
    
    q14 = round(runif(1, min=28, max=32), digits = 4)
    q14v = format(q14, nsmall = 4)
    updateTextInput(session, "q14", value = q14v)
    
    # density of rubber = 1.52 g/mL so get numbers based on min and max
    # d = m/v => v = m/d
    q15 = round(runif(1, min=18, max=22), digits = 1)
    q15v = format(q15, nsmall = 1)
    updateTextInput(session, "q15", value = q15v)
    
    u2v = sample(1:4, 1)
    updateTextInput(session, "u2", value = u2v)
    
    q17 = round(runif(1, min=145, max=245), digits = 4)
    q17v = format(q17, nsmall = 4)
    updateTextInput(session, "q17", value = q17v)
    
    q18 = round(getQ18DataExp02(u2v, q17), digits = 1)
    q18v = format(q18, nsmall = 1)
    updateTextInput(session, "q18", value = q18v)
    
    q20 = round(runif(1, min=18, max=22), digits = 1)
    q20v = format(q20, nsmall = 1)
    updateTextInput(session, "q20", value = q20v)
    
    q21 = round(runif(1, min=13, max=17), digits = 1)
    q21v = format(q21, nsmall = 1)
    updateTextInput(session, "q21", value = q21v)
    
    q22 = round(runif(1, min=1.7, max=1.9), digits = 4)
    q22v = format(q22, nsmall = 4)
    updateTextInput(session, "q22", value = q22v)
  })
  
  observeEvent(input$check, {
    qlist = list() # stores inputed answers for saving to DB
    
    #
    # Handle part A data
    #
    q6 = as.numeric(input$q6)
    ans6 = 10.0
    error6 = abs(q6-ans6)
    valid6 = error6 < 2.0
    output$v6 <- renderText({ showValid(valid6, ans6, pin) })
    qlist["q6"] = q6
    
    q7 = as.numeric(input$q7)
    ans7 = 20.0
    error7 = abs(q7-ans7)
    valid7 = error7 < 2.0
    output$v7 <- renderText({ showValid(valid7, ans7, pin) })
    qlist["q7"] = q7
    
    q8 = as.numeric(input$q8)
    ans8 = 30.0
    error8 = abs(q8-ans8)
    valid8 = error8 < 2.0
    output$v8 <- renderText({ showValid(valid8, ans8, pin) })
    qlist["q8"] = q8
    
    q9 = as.numeric(input$q9)
    ans9 = 40.0
    error9 = abs(q9-ans9)
    valid9 = error9 < 2.0
    output$v9 <- renderText({ showValid(valid9, ans9, pin) })
    qlist["q9"] = q9
    
    # plot data with regression line
    output$plot1 <- renderPlotly({
      x = c(0.0, 10.0, 20.0, 30.0, 40.0)
      y = c(0.0, q6, q7, q8, q9)
      data = data.frame(x, y)
      
      getRegressionPlot(data, "Water Density Plot", "Volume (mL)", "Mass (g)")
    })
    
    #
    # Handle part B data
    #
    q10 = as.numeric(input$q10)
    qlist["q10"] = q10
    
    q11 = as.numeric(input$q11)
    qlist["q11"] = q11
    
    q12 = as.numeric(input$q12)
    ans12 = q11 - q10
    error12 = abs(q12-ans12)
    valid12 = error12 < 0.1
    output$v12 <- renderText({ showValid(valid12, ans12, pin) })
    qlist["q12"] = q12
    
    q13 = as.numeric(input$q13)
    ans13 = q12/10.0
    error13 = abs(q13-ans13)
    valid13 = error13 < 0.05
    output$v13 <- renderText({ showValid(valid13, ans13, pin) })
    qlist["q13"] = q13
    
    # Compare to the actual percent error and print % error
    if(input$u1 != "") {
      u1 = as.numeric(input$u1)
      qlist["u1"] = u1
      
      perror1 = "Invalid Unknown #"
      
      if(u1 == 1 || u1 == 5) {perror1 = percentError(1.027, q13)}
      if(u1 == 2 || u1 == 6) {perror1 = percentError(1.055, q13)}
      if(u1 == 3 || u1 == 7) {perror1 = percentError(1.086, q13)}
      if(u1 == 4 || u1 == 8) {perror1 = percentError(1.116, q13)}
      
      output$u1v <- renderText({ perror1 })
    }
    
    #
    # Handle part C data
    #
    q14 = as.numeric(input$q14)
    qlist["q14"] = q14
    
    q15 = as.numeric(input$q15)
    qlist["q15"] = q15
    
    q16 = as.numeric(input$q16)
    ans16 = q14/q15
    error16 = abs(q16-ans16)
    valid16 = error16 < 0.1
    output$v16 <- renderText({ showValid(valid16, ans16, pin) })
    qlist["q16"] = q16
    
    #
    # Handle part D data
    #
    q17 = as.numeric(input$q17)
    qlist["q17"] = q17
    
    q18 = as.numeric(input$q18)
    qlist["q18"] = q18
    
    q19 = as.numeric(input$q19)
    ans19 = q17/q18
    error19 = abs(q19-ans19)
    valid19 = error19 < 0.1
    output$v19 <- renderText({ showValid(valid19, ans19, pin) })
    qlist["q19"] = q19
    
    # Display perent error based on known value
    if(input$u2 != "") {
      u2 = as.numeric(input$u2)
      qlist["u2"] = u2
      
      perror2 = "Invalid Unknown"
      
      if(u2 == 31) {
        perror2 = percentError(8.470, q19)
      } else if(u2 == 0) { # steel?
        perror2 = percentError(7.86, q19)
      } else {
        perror2 = percentError(2.710, q19)
      }
      
      output$u2v <- renderText({ perror2 })
    }
    
    #
    # Handle part E data
    #
    q20 = as.numeric(input$q20)
    qlist["q20"] = q20
    
    q21 = as.numeric(input$q21)
    qlist["q21"] = q21
    
    q22 = as.numeric(input$q22)
    qlist["q22"] = q22
    
    q23 = as.numeric(input$q23)
    qlist["q23"] = q23
    
    q24 = as.numeric(input$q24)
    ans24 = q22/q23
    error24 = abs(q24-ans24)
    valid24 = error24 < 0.1
    output$v24 <- renderText({ showValid(valid24, ans24, pin) })
    qlist["q24"] = q24
    
    q25 = as.numeric(input$q25)
    ans25 = ans24/(q20*q21)
    error25 = abs(q25 - ans25)
    valid25 = error25 < 0.001
    output$v25 <- renderText({ showValid(valid25, ans25, pin) })
    qlist["q25"] = q25
    
    # save to the database
    dbm = saveToDB(pin, "EXP02", qlist)
    output$dbm <- renderText({dbm})
    
    cat("Experiment 2", "PIN", pin, "\n")
  })
  
  # handle viewing of old data
  observeEvent(input$view, {
    showModal(modalDialog(
      title = "Experiment 2 Saved Data",
      HTML(getSavedData(pin, "EXP02"))
    ))
    
    cat("View Data -- EXP02", "\n")
  })
}

# get the data for question 11
getQ11DataExp02 = function(u1, cylinder) {
  sample.mass = 0
  
  if(u1 == 1 || u1 == 5) {
    sample.mass = 1.027*10 + cylinder
  } else if(u1 == 2 || u1 == 6) {
    sample.mass = 1.055*10 + cylinder
  } else if(u1 == 3 || u1 == 7) {
    sample.mass = 1.086*10 + cylinder
  } else if(u1 == 4 || u1 == 8) {
    sample.mass = 1.116*10 + cylinder
  } else {
    sample.mass = 1.060*10 + cylinder
  }
  
  return(sample.mass)
}

# get the data for question 17
getQ18DataExp02 = function(u2, metal) {
  sample.volume = 0
  
  if(u2 == 1) {
    sample.volume = metal/8.470
  } else if(u2 == 2) {
    sample.volume = metal/7.86
  } else {
    sample.volume = metal/2.710
  }
  
  return(sample.volume)
}