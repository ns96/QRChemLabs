# Experiment 8 UI/Server module code
# Module used for developement and testing
#
# https://shiny.rstudio.com/articles/modules.html
# 

# The UI code
exp08UI <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  # wrap ui components in tablist
  tagList(
    h3("Experiment 8 -- Thermal Decomposition of Baking Soda"),
    
    fluidRow(
      box(title = "Part A -- NaHCO3 Data and Calculations", status = "primary", width = 12,
          textInput(ns("q1"), "1. Mass of EVAPORATING DISH (g):"),
          span(textOutput(ns("v1")), style="color:blue"),
          
          textInput(ns("q2"), "2. Mass of EVAPORATING DISH and NaHCO3 (g):"),
          span(textOutput(ns("v2")), style="color:blue"),
          
          textInput(ns("q3"), "3. Mass of NaHCO3 (g):"),
          span(textOutput(ns("v3")), style="color:blue"),
          
          tags$h4(style = "color: red;", "After Heating and Cooling"),
          
          textInput(ns("q4"), "4. Mass of  EVAPORATING DISH and Sodium Carbonate	(g):"),
          span(textOutput(ns("v4")), style="color:blue"),
          
          textInput(ns("q5"), "5. Mass of Sodium Carbonate Produced (g):"),
          span(textOutput(ns("v5")), style="color:blue"),
          
          textInput(ns("q6"), "6. Mol of NaHCO3 Used:"),
          span(textOutput(ns("v6")), style="color:blue"),
          
          textInput(ns("q7"), "7. Mol of Na2CO3 Expected:"),
          span(textOutput(ns("v7")), style="color:blue"),
          
          textInput(ns("q8"), "8. Mass of Sodium Carbonate Theoretically Expected (g):"),
          span(textOutput(ns("v8")), style="color:blue"),
          
          textInput(ns("q9"), "9. Percent Yield of Sodium Carbonate:"),
          span(textOutput(ns("v9")), style="color:blue")
      )
    ),

    fluidRow(
      box(title = 'Part -- B Unknown Data and Calculations', status = "primary", width = 12,
          selectInput(ns("u1"), "Unknown #:", c(Choose = '','1', '2', '3', '4', '5')),
          span(textOutput(ns("u1v")), style="color:blue"),
          
          textInput(ns("q10"), "10. Mass of EVAPORATING DISH (g):"),
          span(textOutput(ns("v10")), style="color:blue"),
          
          textInput(ns("q11"), "11. Mass of EVAPORATING DISH and Unknown (g):"),
          span(textOutput(ns("v11")), style="color:blue"),
          
          textInput(ns("q12"), "12. Mass of Unknown Mixture Used (g):"),
          span(textOutput(ns("v12")), style="color:blue"),
          
          tags$h4(style = "color: red;", "After Heating and Cooling"),
          
          textInput(ns("q13"), "13. Mass of EVAPORATING DISH and Residue (g):"),
          span(textOutput(ns("v13")), style="color:blue"),
          
          textInput(ns("q14"), "14. Mass of H2CO3 Produced (g):"),
          span(textOutput(ns("v14")), style="color:blue"),
          
          textInput(ns("q15"), "15. Mol of H2CO3:"),
          span(textOutput(ns("v15")), style="color:blue"),
          
          textInput(ns("q16"), "16. Mol of NaHCO3 Present in the Unknown:"),
          span(textOutput(ns("v16")), style="color:blue"),
          
          textInput(ns("q17"), "17. Grams of NaHCO3 Present in the Unknown:"),
          span(textOutput(ns("v17")), style="color:blue"),
          
          textInput(ns("q18"), "18. Percent of NaHCO3 in the Unknown :"),
          span(textOutput(ns("v18")), style="color:blue")
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
exp08 <- function(input, output, session, pin) {
  # load data for students
  observeEvent(input$load, {
    q1 = round(runif(1, min=110, max=120), digits = 4)
    q1v = format(q1, nsmall = 4)
    updateTextInput(session, "q1", value = q1v)
    
    mass.nahco3 = runif(1, min=0.9, max=1.5)
    q2 = round(mass.nahco3 + q1, digits = 4)
    q2v = format(q2, nsmall = 4)
    updateTextInput(session, "q2", value = q2v)
    
    moles.naco3 = (mass.nahco3/84.01)*0.5
    mass.naco3 = (moles.naco3*105.99)*runif(1, min=0.95, max=1.0)
    #mass.naco3 = (moles.naco3*105.99) # DEBUG
    q4 = round((mass.nahco3 - mass.naco3) + q1, digits = 4)
    q4v = format(q4, nsmall = 4)
    updateTextInput(session, "q4", value = q4v)
    
    
    # set the unknown
    u1v = sample(c('1', '2', '3', '4', '5'),  1)
    updateSelectInput(session, "u1", selected = u1v)
    
    q10 = round(runif(1, min=110, max=120), digits = 4)
    q10v = format(q10, nsmall = 4)
    updateTextInput(session, "q10", value = q10v)
    
    mass.unknown = runif(1, min=1.1, max=1.5)
    q11 = round(mass.unknown + q10, digits = 4)
    q11v = format(q11, nsmall = 4)
    updateTextInput(session, "q11", value = q11v)
    
    mass.residue = getQ13DataExp08(u1v, mass.unknown)
    q13 = round(mass.residue + q10, digits = 4)
    q13v = format(q13, nsmall = 4)
    updateTextInput(session, "q13", value = q13v)
  })
  
  observeEvent(input$check, {
    qlist = list()
    
    # load the part A data
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
    ans5 = q4 - q1
    error5 = abs(q5 - ans5)
    valid5 = error5 < 0.5
    output$v5 <- renderText({ showValid(valid5, ans5, pin) })
    qlist["q5"] = q5
    
    q6 = as.numeric(input$q6)
    ans6 = q3/84.01
    error6 = abs(q6 - ans6)
    valid6 = error6 < 0.0005
    output$v6 <- renderText({ showValid(valid6, ans6, pin) })
    qlist["q6"] = q6
    
    q7 = as.numeric(input$q7)
    ans7 = ans6/2
    error7 = abs(q7 - ans7)
    valid7 = error7 < 0.0005
    output$v7 <- renderText({ showValid(valid7, ans7, pin) })
    qlist["q7"] = q7
    
    q8 = as.numeric(input$q8)
    ans8 = ans7*105.99
    error8 = abs(q8 - ans8)
    valid8 = error8 < 0.05
    output$v8 <- renderText({ showValid(valid8, ans8, pin) })
    qlist["q8"] = q8
    
    q9 = as.numeric(input$q9)
    ans9 = (ans5/ans8)*100.0
    error9 = abs(q9 - ans9)
    valid9 = error9 < 0.5
    output$v9 <- renderText({ showValid(valid9, ans9, pin) })
    qlist["q9"] = q9
    
    # Part B Data and calculations
    q10 = as.numeric(input$q10)
    qlist["q10"] = q10
    
    q11 = as.numeric(input$q11)
    qlist["q11"] = q11
    
    q12 = as.numeric(input$q12)
    ans12 = q11 - q10
    error12 = abs(q12 - ans12)
    valid12 = error12 < 0.5
    output$v12 <- renderText({ showValid(valid12, ans12, pin) })
    qlist["q12"] = q12
    
    q13 = as.numeric(input$q13)
    qlist["q13"] = q13
    
    q14 = as.numeric(input$q14)
    ans14 = q11 - q13
    error14 = abs(q14 - ans14)
    valid14 = error14 < 0.5
    output$v14 <- renderText({ showValid(valid14, ans14, pin) })
    qlist["q14"] = q14
    
    q15 = as.numeric(input$q15)
    ans15 = q14/62.03
    error15 = abs(q15 - ans15)
    valid15 = error15 < 0.0005
    output$v15 <- renderText({ showValid(valid15, ans15, pin) })
    qlist["q15"] = q15
    
    q16 = as.numeric(input$q16)
    ans16 = ans15*2
    error16 = abs(q16 - ans16)
    valid16 = error16 < 0.0005
    output$v16 <- renderText({ showValid(valid16, ans16, pin) })
    qlist["q16"] = q16
    
    q17 = as.numeric(input$q17)
    ans17 = ans16*84.01
    error17 = abs(q17 - ans17)
    valid17 = error17 < 0.5
    output$v17 <- renderText({ showValid(valid17, ans17, pin) })
    qlist["q17"] = q17
    
    q18 = as.numeric(input$q18)
    ans18 = (ans17/ans12)*100.0
    error18 = abs(q18 - ans18)
    valid18 = error18 < 0.5
    output$v18 <- renderText({ showValid(valid18, ans18, pin) })
    qlist["q18"] = q18
    
    # indicate the percent error
    output$u1v <- renderText({ getUnknownPercentErrorExp08(input$u1, q18)})
    
    # save to the database now
    dbm = saveToDB(pin, "EXP08", qlist)
    output$dbm <- renderText({dbm})
    
    cat("Experiment 8", "PIN", pin, "\n")
  })
  
  # handle viewing of old data
  observeEvent(input$view, {
    showModal(modalDialog(
      title = "Experiment 8 Saved Data",
      HTML(getSavedData(pin, "EXP08"))
    ))
    
    cat("View Data -- EXP08", "\n")
  })
}

# function to get the unknown percent error
getUnknownPercentErrorExp08 = function(unknown, p.found) {
  if(unknown == '') {
    return("Please select unknown ...")
  } else if(unknown == "1") {
    actual = 50
  } else if(unknown == "2") {
    actual = 60
  } else if(unknown == "3") {
    actual = 70
  } else if(unknown == "4") {
    actual = 80
  } else { # must be 5
    actual = 90
  }
  
  return(percentError(actual, p.found))
}

# get the unknown mass
getQ13DataExp08 = function(unknown, mass.unknown) {
  if(unknown == "1") {
    mass.percent = runif(1, min=48, max=52)/100.0
  } else if(unknown == "2") {
    mass.percent = runif(1, min=58, max=62)/100.0
  } else if(unknown == "3") {
    mass.percent = runif(1, min=68, max=72)/100.0
  } else if(unknown == "4") {
    mass.percent = runif(1, min=78, max=82)/100.0
  } else if(unknown == "5") {
    mass.percent = runif(1, min=88, max=92)/100.0
  } else {
    mass.percent = 0
  }
  
  mass.nahco3 = mass.percent*mass.unknown
  moles.nahco3 = mass.nahco3/84.01
  moles.h2co3 = moles.nahco3/2
  mass.h2co3 = moles.h2co3*62.03
  mass.residue = mass.unknown - mass.h2co3
  
  return(mass.residue)
}