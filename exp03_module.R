# Experiment 3 UI/Server module code
# https://shiny.rstudio.com/articles/modules.html
# 

# The UI code
exp03UI <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  # wrap ui components in tablist
  tagList(
    h3("Experiment 3 -- Separation of a Salt and Sand Mixture"),
    
    fluidRow(
      box(title = "Data and Calculations", status = "primary",
          selectInput(ns("u1"), "1. Unknown Sample:", c(Choose = '', "A", "B", "C")),
          span(htmlOutput(ns("u1v")), style="color:blue"),
          
          textInput(ns("q2"), "2. Mass of Beaker#1 (g):"),
          span(textOutput(ns("v2")), style="color:blue"),
          
          textInput(ns("q3"), "3. Mass of Beaker#1 + Unknown (g):"),
          span(textOutput(ns("v3")), style="color:blue"),
          
          textInput(ns("q4"), "4. Mass Unknown (g):"),
          span(textOutput(ns("v4")), style="color:blue"),
          
          textInput(ns("q5"), "5. Mass of Dry Evaporating Dish (g):"),
          span(textOutput(ns("v5")), style="color:blue")
      ),
      
      box(title = "Percent Salt/Sand Plot", status = "primary",
          plotlyOutput(ns("plot1"))
      )
    ),
    
    fluidRow(
      box(title = "Percent Sand", status = "primary",
          textInput(ns("q6"), "6. Mass of Beaker#1 + Sand (g):"),
          span(textOutput(ns("v6")), style="color:blue"),
          
          textInput(ns("q7"), "7. Mass of Sand Recovered (g):"),
          span(textOutput(ns("v7")), style="color:blue"),
          
          textInput(ns("q8"), "8. Percent Sand (%):"),
          span(textOutput(ns("v8")), style="color:blue")
      ),
      
      box(title = "Percent Salt", status = "primary",
          textInput(ns("q9"), "9. Mass of Evaporating Dish + Salt (g):"),
          span(textOutput(ns("v9")), style="color:blue"),
          
          textInput(ns("q10"), "10. Mass of Salt Recovered (g):"),
          span(textOutput(ns("v10")), style="color:blue"),
          
          textInput(ns("q11"), "11. Percent Salt (%):"),
          span(textOutput(ns("v11")), style="color:blue")
      )
    ),
    
    fluidRow(
      box(title = "Total Sample Recovered", status = "primary",
          textInput(ns("q12"), "12. % Unknown Sample Recovered:"),
          span(textOutput(ns("v12")), style="color:blue")
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
exp03 <- function(input, output, session, pin) {
  # load sample data for students
  observeEvent(input$load, {
    u1v = sample(c('A', 'B', 'C'), 1)
    updateSelectInput(session, "u1", selected = u1v)
    
    q2 = round(runif(1, min=80, max=85), digits = 4)
    q2v = format(q2, nsmall = 4)
    updateTextInput(session, "q2", value = q2v)
    
    unknown.mass = runif(1, min=1.8, max=2.2)
    q3 = round(unknown.mass + q2, digits = 4)
    q3v = format(q3, nsmall = 4)
    updateTextInput(session, "q3", value = q3v)
    
    q5 = round(runif(1, min=195, max=205), digits = 4)
    q5v = format(q5, nsmall = 4)
    updateTextInput(session, "q5", value = q5v)
    
    q6 = q2 + getQ6DataExp03(u1v, unknown.mass)
    q6 = round(q6, digits = 4)
    q6v = format(q6, nsmall = 4)
    updateTextInput(session, "q6", value = q6v)
    
    q9 = q5 + getQ9DataExp03(u1v, unknown.mass)
    q9 = round(q9, digits = 4)
    q9v = format(q9, nsmall = 4)
    updateTextInput(session, "q9", value = q9v)
  })
  
  observeEvent(input$check, {
    qlist = list()
    
    u1 = input$u1
    qlist["u1"] = u1
    
    q2 = as.numeric(input$q2)
    qlist["q2"] = q2
    
    q3 = as.numeric(input$q3)
    qlist["q3"] = q3
    
    q4 = as.numeric(input$q4)
    ans4 = q3 - q2
    error4 = abs(q4-ans4)
    valid4 = error4 < 0.5
    output$v4 <- renderText({ showValid(valid4, ans4, pin, 4) })
    qlist["q4"] = q4
    
    q5 = as.numeric(input$q5)
    qlist["q5"] = q5
    
    # Percent sand recovered
    q6 = as.numeric(input$q6)
    qlist["q6"] = q6
    
    q7 = as.numeric(input$q7)
    ans7 = q6 - q2
    error7 = abs(q7-ans7)
    valid7 = error7 < 0.5
    output$v7 <- renderText({ showValid(valid7, ans7, pin, 4) })
    qlist["q7"] = q7
    
    q8 = as.numeric(input$q8)
    ans8 = (ans7/ans4)*100
    error8 = abs(q8-ans8)
    valid8 = error8 < 0.5
    output$v8 <- renderText({ showValid(valid8, ans8, pin, 1) })
    qlist["q8"] = q8
    
    # Percent salt recovered
    q9 = as.numeric(input$q9)
    qlist["q9"] = q9
    
    q10 = as.numeric(input$q10)
    ans10 = q9 - q5
    error10 = abs(q10-ans10)
    valid10 = error10 < 0.5
    output$v10 <- renderText({ showValid(valid10, ans10, pin, 4) })
    qlist["q10"] = q10
    
    q11 = as.numeric(input$q11)
    ans11 = (ans10/ans4)*100
    error11 = abs(q11-ans11)
    valid11 = error11 < 0.5
    output$v11 <- renderText({ showValid(valid11, ans11, pin, 1) })
    qlist["q11"] = q11
    
    # total percent recovered
    q12 = as.numeric(input$q12)
    ans12 = ans8 + ans11
    error12 = abs(q12-ans12)
    valid12 = error12 < 0.5
    output$v12 <- renderText({ showValid(valid12, ans12, pin, 1) })
    qlist["q12"] = q12
    
    # now compare percentage to actual samples to calculate percent errors
    output$u1v <- renderText({ getPercentErrorExp03(u1, q8, q11) })
    
    # display a pie plot
    output$plot1 <- renderPlotly({getPercentagePlotExp03(q8, q11)})
    
    # save to the database now
    dbm = saveToDB(pin, "EXP03", qlist)
    output$dbm <- renderText({dbm})
    
    cat("Experiment 3", "PIN", pin, "\n")
  })
  
  # handle viewing of old data
  observeEvent(input$view, {
    showModal(modalDialog(
      title = "Experiment 3 Saved Data",
      HTML(getSavedData(pin, "EXP03"))
    ))
    
    cat("View Data -- EXP03", "\n")
  })
}

# function to get the percent error for sand and salt
getPercentErrorExp03 = function(unknown, sand, salt) {
  if(unknown == '') {
    return("Please select unknown ...")
  }
  else if(unknown == "A") {
    p.sand = 50
    p.salt = 50
  } else if(unknown == "B") {
    p.sand = 40
    p.salt = 60
  } else { # must be C
    p.sand = 60
    p.salt = 40
  }
  
  # calculate the percent unknowns
  pe.sand = (abs(p.sand - sand)/p.sand)*100
  pe.salt = (abs(p.salt - salt)/p.salt)*100
  
  sandText = paste("Percent Error Sand: ", sprintf(pe.sand, fmt = '%#.1f'), "%") 
  saltText = paste("Percent Error Salt: ", sprintf(pe.salt, fmt = '%#.1f'), "%") 
  
  return(paste(sandText, "<br>", saltText))
}

# function to get the percentage plot
getPercentagePlotExp03 = function(sand, salt) {
  missing.unknown = 100 - (sand + salt)
  
  print(paste("Values: ", sand, salt, missing.unknown))
  
  df = data.frame("Compound" = c("Sand", "Salt", "Missing"), "Amount" = c(sand, salt, missing.unknown))
  p = getPiePlot(df, "Percent Sand/Salt")
  return(p)
}

getQ6DataExp03 = function(u1, unknown.mass) {
  if(u1 == 'A') {
    sand.mass = addNoise(unknown.mass*0.50, 0.05)
  } else if(u1 == 'B') {
    sand.mass = addNoise(unknown.mass*0.40, 0.05)
  } else if(u1 == 'C') {
    sand.mass = addNoise(unknown.mass*0.60, 0.05)
  } else {
    sand.mass = 0
  }
  
  return(sand.mass)
}

getQ9DataExp03 = function(u1, unknown.mass) {
  if(u1 == 'A') {
    salt.mass = addNoise(unknown.mass*0.50, 0.10)
  } else if(u1 == 'B') {
    salt.mass = addNoise(unknown.mass*0.60, 0.10)
  } else if(u1 == 'C') {
    salt.mass = addNoise(unknown.mass*0.40, 0.05)
  } else {
    salt.mass = 0
  }
  
  return(salt.mass)
}