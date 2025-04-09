# Experiment 1 UI/Server module code
# https://shiny.rstudio.com/articles/modules.html
# 

# The UI code
exp01UI <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  # wrap ui components in tablist
  tagList(
    h3("Experiment 1 -- Scientific Notation"),
    
    fluidRow(
      box(title = "Part A -- Mass Measurements", status = "primary",
          textInput(ns("q1"), "1. Mass of a 100-mL Beaker (g):", value = ""),
          span(textOutput(ns("v1")), style="color:blue"),
          
          textInput(ns("q2"), "2. Mass of a 10-mL Graduated Cylinder (g):", value = ""),
          span(textOutput(ns("v2")), style="color:blue"),
          
          textInput(ns("q3"), "3. Mass of a 125-mL Erlenmeyer flask (g):", value = ""),
          span(textOutput(ns("v3")), style="color:blue")
      ),
      
      box(title = "Part B -- Length Measurements", status = "primary",
          textInput(ns("q4"), "4. Diameter of a Watchglass (cm):", value = ""),
          span(textOutput(ns("v4")), style="color:blue"),
          
          textInput(ns("q5a"), "5a. Length a Test-Tube (cm):", value = ""),
          span(textOutput(ns("v5a")), style="color:blue"),
          
          textInput(ns("q5b"), "5b. Inside-mouth Diameter of the Test-Tube (cm):", value = ""),
          span(textOutput(ns("v5b")), style="color:blue")
      )
    ),
    
    fluidRow(
      box(title = "Part C -- Volume Measurements", status = "primary",
          textInput(ns("q6"), "6. Calculated Volume of the Test-Tube (cc):", value = ""),
          span(textOutput(ns("v6")), style="color:blue"),
          
          textInput(ns("q7"), "7. Experimental Volume of the Test-Tube (mL):", value = ""),
          span(textOutput(ns("v7")), style="color:blue")
      ),
      
      box(title = "Part D -- Temperature Measurement", status = "primary",
          textInput(ns("q8"), '8. "Hot-bath" Temperature (degC):', value = ""),
          span(textOutput(ns("v8")), style="color:blue"),
          
          textInput(ns("q9"), '9. "Cold-bath" Temperature (degC):', value = ""),
          span(textOutput(ns("v9")), style="color:blue"),
          
          textInput(ns("q10"), "10. Room Temperature (degC):", value = ""),
          span(textOutput(ns("v10")), style="color:blue")
      )
    ),
    
    fluidRow(
      box(title = "Part D -- Unit Conversion", status = "primary",
          textInput(ns("q11"), '11. Convert#1 into kg":', value = ""),
          span(textOutput(ns("v11")), style="color:blue"),
          
          textInput(ns("q12"), '12. "Convert #4 into mm:', value = ""),
          span(textOutput(ns("v12")), style="color:blue"),
          
          textInput(ns("q13"), "13. Convert #7 into L:", value = ""),
          span(textOutput(ns("v13")), style="color:blue"),
          
          textInput(ns("q14"), '14. Convert #8 into K":', value = ""),
          span(textOutput(ns("v14")), style="color:blue"),
          
          textInput(ns("q15"), '15. Convert #9 into degF":', value = ""),
          span(textOutput(ns("v15")), style="color:blue"),
          
          textInput(ns("q16"), '16. Convert #7 into gallons":', value = ""),
          span(textOutput(ns("v16")), style="color:blue")
      ),
      
      box(title = "Part D -- Temperature Plot", status = "primary",
          plotlyOutput(ns("plot1"))
      )
    ),
    
    fluidRow(
      # add button to add and check data
      column(3, actionButton(ns("load"), "Load Sample Data")),
      column(3, actionButton(ns("check"), "Check Input Data")),
      column(3, span(textOutput(ns("dbm")), style="color:green")),
      column(3, actionButton(ns("view"), "View Saved Data"))
    )
  )
}

# Server code
exp01 <- function(input, output, session, pin) {
  # load sample data for students
  observeEvent(input$load, {
    q1 = round(runif(1, min=124, max=138), digits = 4)
    q1v = format(q1, nsmall = 4)
    updateTextInput(session, "q1", value = q1v)
    
    q2 = round(runif(1, min=75, max=86), digits = 4)
    q2v = format(q2, nsmall = 4)
    updateTextInput(session, "q2", value = q2v)
    
    q3 = round(runif(1, min=165, max=180), digits = 4)
    q3v = format(q3, nsmall = 4)
    updateTextInput(session, "q3", value = q3v)
    
    q4 = round(runif(1, min=80, max=90), digits = 1)
    q4v = format(q4, nsmall = 1)
    updateTextInput(session, "q4", value = q4v)
    
    q5a = runif(1, min=18.8, max=20.2)
    q5av = round(q5a, digits = 1)
    updateTextInput(session, "q5a", value = q5av)
    
    q5b = runif(1, min=1.8, max=2.3)
    q5bv = round(q5b, digits = 1)
    updateTextInput(session, "q5b", value = q5bv)
    
    ans6 = 3.14*((q5b/2)^2)*as.numeric(q5a)
    q7v = round(ans6 - 2.5, digits = 1)
    updateTextInput(session, "q7", value = q7v)
    
    q8 = round(runif(1, min=70, max=90), digits = 1)
    q8v = format(q8, nsmall = 1)
    updateTextInput(session, "q8", value = q8v)
    
    q9 = round(runif(1, min=1, max=5), digits = 1)
    q9v = format(q9, nsmall = 1)
    updateTextInput(session, "q9", value = q9v)
    
    q10 = round(runif(1, min=20, max=26), digits = 1)
    q10v = format(q10, nsmall = 1)
    updateTextInput(session, "q10", value = q10v)
    
    x = c( "Hot-Bath", "Ice-Bath", "Room-Temp")
    y = c(q8, q9, q10)
    output$plot1 <- renderPlotly({getBarPlot(x, y, "Temperature (degC)")})
  })
  
  observeEvent(input$check, {
    qlist = list()
    
    # part A    
    q1 = as.numeric(input$q1)
    qlist["q1"] = q1
    
    q2 = as.numeric(input$q2)
    qlist["q2"] = q2
    
    q3 = as.numeric(input$q3)
    qlist["q3"] = q3
    
    # part B
    q4 = as.numeric(input$q4)
    qlist["q4"] = q4
    
    q5a = as.numeric(input$q5a)
    qlist["q5a"] = q5a
    
    q5b = as.numeric(input$q5b)
    qlist["q5b"] = q5b
    
    # part C
    q6 = as.numeric(input$q6)
    ans6 = 3.14*((q5b/2)^2)*q5a
    error6 = abs(q6-ans6)
    valid6 = error6 < 0.5
    output$v6 <- renderText({ showValid(valid6, ans6, pin, 0) })
    qlist["q6"] = q6
    
    q7 = as.numeric(input$q7)
    output$v7 <- renderText({ percentError(q7, q6) })
    qlist["q7"] = q7
    
    # part D - Temperature
    q8 = as.numeric(input$q8)
    qlist["q8"] = q8
    
    q9 = as.numeric(input$q9)
    qlist["q9"] = q9
    
    q10 = as.numeric(input$q10)
    qlist["q10"] = q10
    
    # show the temperature plot
    x = c( "Hot-Bath", "Ice-Bath", "Room-Temp")
    y = c(q8, q9, q10)
    output$plot1 <- renderPlotly({getBarPlot(x, y, "Temperature (degC)")})
    
    # part D conversions
    q11 = as.numeric(input$q11)
    ans11 = q1/1000.0
    error11 = abs(q11-ans11)
    valid11 = error11 < 0.5
    output$v11 <- renderText({ showValid(valid11, ans11, pin, 7) })
    qlist["q11"] = q11
    
    q12 = as.numeric(input$q12)
    ans12 = q4*10.0
    error12 = abs(q12-ans12)
    valid12 = error12 < 0.5
    cat(ans12, error12, valid12)
    output$v12 <- renderText({ showValid(valid12, ans12, pin, 0) })
    qlist["q12"] = q12
    
    q13 = as.numeric(input$q13)
    ans13 = q7/1000.0
    error13 = abs(q13-ans13)
    valid13 = error13 < 0.5
    output$v13 <- renderText({ showValid(valid13, ans13, pin, 4) })
    qlist["q13"] = q13
    
    q14 = as.numeric(input$q14)
    ans14 = q8 + 273.15
    error14 = abs(q14-ans14)
    valid14 = error14 < 0.5
    output$v14 <- renderText({ showValid(valid14, ans14, pin, 0) })
    qlist["q14"] = q14
    
    q15 = as.numeric(input$q15)
    ans15 = (q9*1.8) + 32
    error15 = abs(q15-ans15)
    valid15 = error15 < 0.5
    output$v15 <- renderText({ showValid(valid15, ans15, pin, 0) })
    qlist["q15"] = q15
    
    q16 = as.numeric(input$q16)
    ans16 = ans13/3.7854
    error16 = abs(q16-ans16)
    valid16 = error16 < 0.5
    output$v16 <- renderText({ showValid(valid16, ans16, pin, 4) })
    qlist["q16"] = q16
    
    # save to the database now
    dbm = saveToDB(pin, "EXP01", qlist)
    output$dbm <- renderText({dbm})
    
    cat("Experiment 1", "PIN", pin, "\n")
  })
  
  # handle viewing of old data
  observeEvent(input$view, {
    showModal(modalDialog(
      title = "Experiment 1 Saved Data",
      HTML(getSavedData(pin, "EXP01"))
    ))
    
    cat("View Data -- EXP01", "\n")
  })
}