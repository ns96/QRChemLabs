# Experiment 7 UI/Server module code
# Module used for development and testing
#
# https://shiny.rstudio.com/articles/modules.html
# 

# The UI code
exp07UI <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  # wrap ui components in tablist
  tagList(
    h3("Experiment 7 -- Analysis by Precipitation"),
    
    fluidRow(
      box(title = "Unknown and Data", status = "primary", width = 12,
          selectInput(ns("u1"), "Unknown #:", c(Choose = '','1', '2', '3')),
          span(textOutput(ns("u1v")), style="color:blue"),
          
          textInput(ns("q1"), "1. Mass of Beaker#1 (g):"),
          span(textOutput(ns("v1")), style="color:blue"),
          
          textInput(ns("q2"), "2. Mass of Beaker#1 and Unknown Mixture (g):"),
          span(textOutput(ns("v2")), style="color:blue"),
          
          textInput(ns("q3"), "3. Mass of Unknown Mixture Used (g):"),
          span(textOutput(ns("v3")), style="color:blue"),
          
          textInput(ns("q4"), "4. Mass of Beaker#2	(g):"),
          span(textOutput(ns("v4")), style="color:blue"),
          
          textInput(ns("q5"), "5. Mass of Beaker#2 With Sodium Carbonate (g):"),
          span(textOutput(ns("v5")), style="color:blue"),
          
          textInput(ns("q6"), "6. Mass of Sodium Carbonate Used (g):"),
          span(textOutput(ns("v6")), style="color:blue"),
          
          textInput(ns("q7"), "7. Mass of Filter Paper (g):"),
          span(textOutput(ns("v7")), style="color:blue"),
          
          tags$h4(style = "color: red;", "After Precipitation, Filtration, Drying and Cooling"),
          
          textInput(ns("q8"), "8. Mass of the Filter Paper and Calcium Carbonate (g):"),
          span(textOutput(ns("v8")), style="color:blue")
      )
    ),

    
    fluidRow(
      box(title = 'Calculations', status = "primary", width = 12,
          textInput(ns("q9"), "9. Mass of Calcium Carbonate Formed  (g):"),
          span(textOutput(ns("v9")), style="color:blue"),
          
          textInput(ns("q10"), "10. Moles of Calcium Carbonate Formed:"),
          span(textOutput(ns("v10")), style="color:blue"),
          
          textInput(ns("q11"), "11. Moles of Calcium Chloride Present in Beaker#1:"),
          span(textOutput(ns("v11")), style="color:blue"),
          
          textInput(ns("q12"), "12. Grams of Calcium Chloride Present in Beaker#1	(g):"),
          span(textOutput(ns("v12")), style="color:blue"),
          
          textInput(ns("q13"), "13. Percent of Calcium Chloride:"),
          span(textOutput(ns("v13")), style="color:blue")
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
exp07 <- function(input, output, session, pin) {
  # load data for students
  observeEvent(input$load, {
    # set the unknown
    u1v = sample(c('1', '2', '3'),  1)
    updateSelectInput(session, "u1", selected = u1v)
    
    q1 = round(runif(1, min=110, max=120), digits = 4)
    q1v = format(q1, nsmall = 4)
    updateTextInput(session, "q1", value = q1v)
    
    mass.unknown= runif(1, min=1.4, max=1.5)
    q2 = round(mass.unknown + q1, digits = 4)
    q2v = format(q2, nsmall = 4)
    updateTextInput(session, "q2", value = q2v)
    
    q4 = round(runif(1, min=80, max=85), digits = 4)
    q4v = format(q4, nsmall = 4)
    updateTextInput(session, "q4", value = q4v)
    
    mass.carbonate= runif(1, min=1.8, max=2.2)
    q5 = round(mass.carbonate + q4, digits = 4)
    q5v = format(q5, nsmall = 4)
    updateTextInput(session, "q5", value = q5v)
    
    mass.paper= runif(1, min=0.7, max=0.9)
    q7 = round(mass.paper, digits = 4)
    q7v = format(q7, nsmall = 4)
    updateTextInput(session, "q7", value = q7v)
    
    q8 = q7 + getQ8DataExp07(u1v, mass.carbonate)
    q8 = round(q8, digits = 4)
    q8v = format(q8, nsmall = 4)
    updateTextInput(session, "q8", value = q8v)
  })
  
  observeEvent(input$check, {
    qlist = list()
    
    # load the initial data
    qlist["u1"] = input$u1
    
    # load the initial data
    q1 = as.numeric(input$q1)
    qlist["q1"] = q1
    
    q2 = as.numeric(input$q2)
    qlist["q2"] = q2
    
    q3 = as.numeric(input$q3)
    ans3 = q2 - q1
    error3 = abs(q3 - ans3)
    valid3 = error3 < 0.5
    output$v3 <- renderText({ showValid(valid3, ans3, pin) })
    qlist["q3"] = q3
    
    q4 = as.numeric(input$q4)
    qlist["q4"] = q4
    
    q5 = as.numeric(input$q5)
    qlist["q5"] = q5
    
    q6 = as.numeric(input$q6)
    ans6 = q5 - q4
    error6 = abs(q6 - ans6)
    valid6 = error6 < 0.5
    output$v6 <- renderText({ showValid(valid6, ans6, pin) })
    qlist["q6"] = q6
    
    q7 = as.numeric(input$q7)
    qlist["q7"] = q7
    
    q8 = as.numeric(input$q8)
    qlist["q8"] = q8
    
    q9 = as.numeric(input$q9)
    ans9 = q8 - q7
    error9 = abs(q9 - ans9)
    valid9 = error9 < 0.5
    output$v9 <- renderText({ showValid(valid9, ans9, pin) })
    qlist["q9"] = q9
    
    # Carry out final calcuations
    q10 = as.numeric(input$q10)
    ans10 = ans9/100.1
    error10 = abs(q10 - ans10)
    valid10 = error10 < 0.0005
    output$v10 <- renderText({ showValid(valid10, ans10, pin) })
    qlist["q10"] = q10
    
    q11 = as.numeric(input$q11)
    ans11 = ans10
    error11 = abs(q11 - ans11)
    valid11 = error11 == 0
    output$v11 <- renderText({ showValid(valid11, ans11, pin) })
    qlist["q11"] = q11
    
    q12 = as.numeric(input$q12)
    ans12 = ans11*111.0
    error12 = abs(q12 - ans12)
    valid12 = error12 < 0.5
    output$v12 <- renderText({ showValid(valid12, ans12, pin) })
    qlist["q12"] = q12
    
    q13 = as.numeric(input$q13)
    ans13 = (ans12/ans3)*100.0
    error13 = abs(q13 - ans13)
    valid13 = error13 < 0.5
    output$v13 <- renderText({ showValid(valid13, ans13, pin) })
    qlist["q13"] = q13
    
    # indicate the percent error
    output$u1v <- renderText({ getUnknownPercentErrorExp07(input$u1, q13)})
    
    # save to the database now
    dbm = saveToDB(pin, "EXP07", qlist)
    output$dbm <- renderText({dbm})
    
    cat("Experiment 07", "PIN", pin, "\n")
  })
  
  # handle viewing of old data
  observeEvent(input$view, {
    showModal(modalDialog(
      title = "Experiment 7 Saved Data",
      HTML(getSavedData(pin, "EXP07"))
    ))
    
    cat("View Data -- EXP07", "\n")
  })
}

# function to get the unknown percent error
getUnknownPercentErrorExp07 = function(unknown, p.cacl2) {
  if(unknown == '') {
    return("Please select unknown ...")
  } else if(unknown == "1") {
    actual = 25
  } else if(unknown == "2") {
    actual = 40
  } else { # must be 3
    actual = 50
  }
  
  return(percentError(actual, p.cacl2))
}

# get the unknown mass
getQ8DataExp07 = function(unknown, unknown.mass) {
  if(unknown == "1") {
    percent.caco3 = runif(1, min=24, max=26)/100.0
    caco3.mass = unknown.mass*percent.caco3
  } else if(unknown == "2") {
    percent.caco3 = runif(1, min=36, max=42)/100.0
    caco3.mass = unknown.mass*percent.caco3
  } else if(unknown == "3") {
    percent.caco3 = runif(1, min=45, max=52)/100.0
    caco3.mass = unknown.mass*percent.caco3
  } else {
    caco3.mass = 0
  }
  
  return(caco3.mass)
}