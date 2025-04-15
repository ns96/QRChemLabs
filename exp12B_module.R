# Experiment 12B UI/Server module code
#
# https://shiny.rstudio.com/articles/modules.html
# 

# The UI code
exp12BUI <- function(id, lab.number = 12) {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  # wrap ui components in tablist
  tagList(
    h3(paste("Experiment", lab.number, "-- Amount of Fat in Potato Chips")),
    
    fluidRow(
      box(title = "Initial Data", status = "primary", width = 12,
          textInput(ns("q1"), "1. Mass of a dry and clean 125-mL Erlenmeyer flask (g):", value = ''),
          span(textOutput(ns("v1")), style="color:blue"),
          
          textInput(ns("q2"), "2. Mass of the 125-mL Erlenmeyer flask plus potato chips (g):", value = ''),
          span(textOutput(ns("v2")), style="color:blue"),
          
          textInput(ns("q3"), "3. Mass of potato chips (g):"),
          span(textOutput(ns("v3")), style="color:blue"),
          
          textInput(ns("q4"), "4. Mass of dry and clean 150-mL beaker (g):"),
          span(textOutput(ns("v4")), style="color:blue"),
          
          textInput(ns("q5"), "5.	Mass of the 150-mL beaker plus lipid extracted (g):"),
          span(textOutput(ns("v5")), style="color:blue"),
          
          textInput(ns("q6"), "6.	Mass of lipid extracted (g):"),
          span(textOutput(ns("v6")), style="color:blue")
      )
    ),

    fluidRow(
      box(title = 'Percent Calculations', status = "primary", width = 12,
          textInput(ns("q7"), "7.	Percent of lipid in potato chips (%):", value=''),
          span(textOutput(ns("v7")), style="color:blue"),
          
          textInput(ns("q8"), "8.	Calculated % of lipid in potato chips from Nutritional label:"),
          span(textOutput(ns("v8")), style="color:blue"),
          
          textInput(ns("q9"), "9.	Calculated the % error:"),
          span(textOutput(ns("v9")), style="color:blue")
      )  
    ),
    
    fluidRow(
      box(title = "Potato Chips Front", status = "primary",
        img(src='images/LaysFront.jpeg', width = "300px", height = "300px")
      ),
      
      box(title = "Potato Chips Back", status = "primary",
        img(src='images/LaysBack.jpeg', width = "300px", height = "300px")      
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
exp12B <- function(input, output, session, pin) {
  # load data for students
  observeEvent(input$load, {
    q1 = round(runif(1, min=125, max=135), digits = 4)
    q1v = format(q1, nsmall = 4)
    updateTextInput(session, "q1", value = q1v)
    
    mass.chips = runif(1, min=10, max=15)
    q2 = round(mass.chips + q1, digits = 4)
    q2v = format(q2, nsmall = 4)
    updateTextInput(session, "q2", value = q2v)
    
    q4 = round(runif(1, min=140, max=150), digits = 4)
    q4v = format(q4, nsmall = 4)
    updateTextInput(session, "q4", value = q4v)
    
    mass.lipids = getQ5DataExp12B(mass.chips)
    q5 = round(mass.lipids + q4, digits = 4)
    q5v = format(q5, nsmall = 4)
    updateTextInput(session, "q5", value = q5v)
  })
  
  observeEvent(input$check, {
    qlist = list()
    
    # load initial data
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
    qlist["q4"] = q4
    
    q5 = as.numeric(input$q5)
    qlist["q5"] = q5
    
    q6 = as.numeric(input$q6)
    ans6 = q5 - q4
    error6 = abs(q6 - ans6)
    valid6 = error6 < 0.0005
    output$v6 <- renderText({ showValid(valid6, ans6, pin, 4) })
    qlist["q6"] = q6
    
    q7 = as.numeric(input$q7)
    ans7 = (ans6/ans3)*100.0
    error7 = abs(q7 - ans7)
    valid7 = error7 < 0.5
    output$v7 <- renderText({ showValid(valid7, ans7, pin, 1) })
    qlist["q7"] = q7
    
    q8 = as.numeric(input$q8)
    ans8 = (12.0/31.8)*100.0
    error8 = abs(q8 - ans8)
    valid8 = error8 < 0.5
    output$v8 <- renderText({ showValid(valid8, ans8, pin, 1) })
    qlist["q8"] = q8
    
    q9 = as.numeric(input$q9)
    ans9 = (abs(ans8 - ans7)/ans8)*100.0
    error9 = abs(q9 - ans9)
    valid9 = error9 < 0.5
    output$v9 <- renderText({ showValid(valid9, ans9, pin, 1) })
    qlist["q9"] = q9
    
    # save to the database now
    dbm = saveToDB(pin, "EXP12B", qlist)
    output$dbm <- renderText({dbm})
    
    cat("Experiment 12B", "PIN", pin, "\n")
  })
  
  # handle viewing of old data
  observeEvent(input$view, {
    showModal(modalDialog(
      title = "Experiment 12B Saved Data",
      HTML(getSavedData(pin, "EXP12B"))
    ))
    
    cat("View Data -- EXP12B", "\n")
  })
}

# get the mass of lipids
getQ5DataExp12B = function(mass.chips) {
  mass.lipids = (runif(1, min=30, max=40)/100.0)*mass.chips
  return(mass.lipids)
}