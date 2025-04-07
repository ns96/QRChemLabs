# Experiment 9B UI/Server module code
# Module used for development and testing
#
# https://www.vanderbilt.edu/cso/Saltwater_Density.pdf
#
# https://shiny.rstudio.com/articles/modules.html
# 

# The UI code
exp09BUI <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  # wrap ui components in tablist
  tagList(
    h3("Experiment 9 -- Salt Water Analysis"),
    
    fluidRow(
      box(title = "Data and Calculations ", status = "primary", width = 12,
          selectInput(ns("u1"), "Unknown #:", c(Choose = '','1', '2', '3')),
          span(textOutput(ns("u1v")), style="color:blue"),
          
          textInput(ns("q2"), "2. Mass of empty and dry Evapoating Dish (g):"),
          span(textOutput(ns("v2")), style="color:blue"),
          
          textInput(ns("q3"), "3. Mass of Evapoating Dish + 30.0mL of unknown salt solution (g):"),
          span(textOutput(ns("v3")), style="color:blue"),
          
          textInput(ns("q4"), "4. Mass unknown salt solution used (g):"),
          span(textOutput(ns("v4")), style="color:blue"),
          
          textInput(ns("q5"), "5. Volume of unknown salt solution used = 30.0mL / 1000  (L):"),
          span(textOutput(ns("v5")), style="color:blue")
      )
    ),

    fluidRow(
      box(title = 'Calculations After Evaporation, Drying, and Cooling', status = "primary", width = 12,
          textInput(ns("q6"), "6. Mass of Evapoating Dish containing dry salt (g):"),
          span(textOutput(ns("v6")), style="color:blue"),
          
          textInput(ns("q7"), "7. Mass salt recovered (g):"),
          span(textOutput(ns("v7")), style="color:blue"),
          
          textInput(ns("q8"), "8. Moles of salt recovered:"),
          span(textOutput(ns("v8")), style="color:blue"),
          
          textInput(ns("q9"), "9. Molarity of unknown solution (M):"),
          span(textOutput(ns("v9")), style="color:blue"),
          
          textInput(ns("q10"), "10. Percent by mass of unknown solution (%):"),
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
exp09B <- function(input, output, session, pin) {
  # load data for students
  observeEvent(input$load, {
    # set the unknown
    u1v = sample(c('1', '2', '3'),  1)
    updateSelectInput(session, "u1", selected = u1v)
    
    q2 = round(runif(1, min=110, max=120), digits = 4)
    q2v = format(q2, nsmall = 4)
    updateTextInput(session, "q2", value = q2v)
    
    mass.salt = getQ3DataExp09B(u1v)
    mass.unknown = 50.0 + mass.salt
    q3 = round(mass.unknown + q2, digits = 4)
    q3v = format(q3, nsmall = 4)
    updateTextInput(session, "q3", value = q3v)
    
    q6 = q2 + mass.salt
    q6 = round(q6, digits = 4)
    q6v = format(q6, nsmall = 4)
    updateTextInput(session, "q6", value = q6v)
  })
  
  observeEvent(input$check, {
    qlist = list()
    
    # load the initial data
    qlist["u1"] = input$u1
    
    # load the initial data
    q2 = as.numeric(input$q2)
    qlist["q2"] = q2
    
    q3 = as.numeric(input$q3)
    qlist["q3"] = q3
    
    q4 = as.numeric(input$q4)
    ans4 = q3 - q2
    error4 = abs(q4 - ans4)
    valid4 = error4 < 0.5
    output$v4 <- renderText({ showValid(valid4, ans4, pin) })
    qlist["q4"] = q4
    
    q5 = as.numeric(input$q5)
    qlist["q5"] = q5
    
    q5 = as.numeric(input$q5)
    qlist["q5"] = q5
    
    q6 = as.numeric(input$q6)
    qlist["q6"] = q6
    
    q7 = as.numeric(input$q7)
    ans7 = q6 - q2
    error7 = abs(q7 - ans7)
    valid7 = error7 < 0.5
    output$v7 <- renderText({ showValid(valid7, ans7, pin) })
    qlist["q7"] = q7
    
    q8 = as.numeric(input$q8)
    ans8 = ans7/58.44
    error8 = abs(q8 - ans8)
    valid8 = error8 < 0.5
    output$v8 <- renderText({ showValid(valid8, ans8, pin) })
    qlist["q8"] = q8
    
    q9 = as.numeric(input$q9)
    ans9 = ans8/0.050
    error9 = abs(q9 - ans9)
    valid9 = error9 < 0.005
    output$v9 <- renderText({ showValid(valid9, ans9, pin) })
    qlist["q9"] = q9
    
    # Carry out final calcuations
    q10 = as.numeric(input$q10)
    ans10 = (ans7/ans4)*100
    error10 = abs(q10 - ans10)
    valid10 = error10 < 0.5
    output$v10 <- renderText({ showValid(valid10, ans10, pin) })
    qlist["q10"] = q10
    
    # indicate the percent error
    percentErrorText = paste("Molarity", getMolarityPercentErrorExp09B(input$u1, q9),
                             "||",
                             "%Mass/Mass", getMassPercentErrorExp09B(input$u1, q10))
    output$u1v <- renderText({ percentErrorText })
    
    # save to the database now
    dbm = saveToDB(pin, "EXP09B", qlist)
    output$dbm <- renderText({dbm})
    
    cat("Experiment 9B", "PIN", pin, "\n")
  })
  
  # handle viewing of old data
  observeEvent(input$view, {
    showModal(modalDialog(
      title = "Experiment 9B Saved Data",
      HTML(getSavedData(pin, "EXP09B"))
    ))
    
    cat("View Data -- EXP09B", "\n")
  })
}

# function to get the unknown percent error
getMolarityPercentErrorExp09B = function(unknown, molarity) {
  if(unknown == '') {
    return("Please select unknown ...")
  } else if(unknown == "1") {
    actual = 1.57
  } else if(unknown == "2") {
    actual = 4.72
  } else { # must be 3
    actual = 7.87
  }
  
  return(percentError(actual, molarity))
}

# function to get the unknown percent error
getMassPercentErrorExp09B = function(unknown, mass.percent) {
  if(unknown == '') {
    return("Please select unknown ...")
  } else if(unknown == "1") {
    actual = 8.42
  } else if(unknown == "2") {
    actual = 21.6
  } else { # must be 3
    actual = 31.5
  }
  
  return(percentError(actual, mass.percent))
}

# get the unknown mass
getQ3DataExp09B = function(unknown) {
  if(unknown == "1") {
    mass.salt = runif(1, min=4.5, max=4.7)
  } else if(unknown == "2") {
    mass.salt = runif(1, min=13.7, max=13.9)
  } else if(unknown == "3") {
    mass.salt = runif(1, min=22.9, max=23.1)
  } else {
    mass.salt = 0
  }
  
  return(mass.salt)
}