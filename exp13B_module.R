# Experiment 13B UI/Server module code
#
# https://shiny.rstudio.com/articles/modules.html
# 

# The UI code
exp13BUI <- function(id, lab.number = 13) {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  # wrap ui components in tablist
  tagList(
    h3(paste("Experiment", lab.number, "-- Vitamin C")),
    
    fluidRow(
      box(title = "Vitamin C Tablet Data and Calculations", status = "primary", width = 12,
          textInput(ns("q1"), "1. Mass of one glossy square weighing paper (g):", value = ''),
          span(textOutput(ns("v1")), style="color:blue"),
          
          textInput(ns("q2"), "2. Mass of weighing paper with Vitamin C tablet (g):", value = ''),
          span(textOutput(ns("v2")), style="color:blue"),
          
          textInput(ns("q3"), "3. Mass of one Vitamin C tablet (g):"),
          span(textOutput(ns("v3")), style="color:blue"),
          
          textInput(ns("q4"), "4. Weight percent of Vitamin C in this tablet (%):"),
          span(textOutput(ns("v4")), style="color:blue"),
          
          textInput(ns("q5"), "5.	Volume of potassium iodate (KIO3) used (mL):"),
          span(textOutput(ns("v5")), style="color:blue"),
          
          textInput(ns("q6"), "6. Calculate the Tire Value (mL/g):"),
          span(textOutput(ns("v6")), style="color:blue")
      )
    ),

    fluidRow(
      box(title = 'Tang Data and Calculations', status = "primary", width = 12,
          textInput(ns("q7"), "7. Volume of Tang used (mL):", value=''),
          span(textOutput(ns("v7")), style="color:blue"),
          
          textInput(ns("q8"), "8. Volume of potassium iodate (KIO3) (mL):"),
          span(textOutput(ns("v8")), style="color:blue"),
          
          textInput(ns("q9"), "9. Calculated mg of Vitamin C in 100 mL Tang sample (mg):"),
          span(textOutput(ns("v9")), style="color:blue")
      )  
    ),
    
    fluidRow(
      box(title = "Vitamic C Tablet", status = "primary",
        img(src='images/vitamin_c.jpg', width = "300px", height = "300px")
      ),
      
      box(title = "Tang", status = "primary",
        img(src='images/tang.jpg', width = "300px", height = "300px")      
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
exp13B <- function(input, output, session, pin) {
  # load data for students
  observeEvent(input$load, {
    q1 = round(runif(1, min=0.2, max=0.4), digits = 4)
    q1v = format(q1, nsmall = 4)
    updateTextInput(session, "q1", value = q1v)
    
    mass.tablet = runif(1, min=0.7, max=0.9)
    q2 = round(mass.tablet + q1, digits = 4)
    q2v = format(q2, nsmall = 4)
    updateTextInput(session, "q2", value = q2v)
    
    q5 = round(runif(1, min=30, max=40), digits = 1)
    q5v = format(q5, nsmall = 1)
    updateTextInput(session, "q5", value = q5v)
    
    updateTextInput(session, "q7", value = '100.0')
    
    q8 = round(runif(1, min=2, max=4), digits = 2)
    q8v = format(q8, nsmall = 2)
    updateTextInput(session, "q8", value = q8v)
  })
  
  observeEvent(input$check, {
    qlist = list()
    
    # load intitial data
    q1 = as.numeric(input$q1)
    qlist["q1"] = q1
    
    q2 = as.numeric(input$q2)
    qlist["q2"] = q2
    
    q3 = as.numeric(input$q3)
    ans3 = q2 - q1
    error3 = abs(q3 - ans3)
    valid3 = error3 < 0.5
    output$v3 <- renderText({ showValid(valid3, ans3, pin, 4) })
    qlist["q3"] = q3
    
    q4 = as.numeric(input$q4)
    ans4 = (0.500/ans3)*100.0
    error4 = abs(q4 - ans4)
    valid4 = error4 < 0.5
    output$v4 <- renderText({ showValid(valid4, ans4, pin, 1) })
    qlist["q4"] = q4
    
    q5 = as.numeric(input$q5)
    qlist["q5"] = q5
    
    q6 = as.numeric(input$q6)
    ans6 = q5/0.500
    error6 = abs(q6 - ans6)
    valid6 = error6 < 0.5
    output$v6 <- renderText({ showValid(valid6, ans6, pin, 1) })
    qlist["q6"] = q6
    
    q7 = as.numeric(input$q7)
    qlist["q7"] = q7
    
    q8 = as.numeric(input$q8)
    qlist["q8"] = q8
    
    q9 = as.numeric(input$q9)
    ans9 = (500.0/q5)*q8
    error9 = abs(q9 - ans9)
    valid9 = error9 < 0.5
    output$v9 <- renderText({ showValid(valid9, ans9, pin, 1) })
    qlist["q9"] = q9
    
    # save to the database now
    dbm = saveToDB(pin, "EXP13B", qlist)
    output$dbm <- renderText({dbm})
    
    cat("Experiment 13B", "PIN", pin, "\n")
  })
  
  # handle viewing of old data
  observeEvent(input$view, {
    showModal(modalDialog(
      title = "Experiment 13B Saved Data",
      HTML(getSavedData(pin, "EXP13B"))
    ))
    
    cat("View Data -- EXP13B", "\n")
  })
}