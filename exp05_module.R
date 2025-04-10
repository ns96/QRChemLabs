# Experiment 5 UI/Server module code
# Module used for development and testing
#
# https://shiny.rstudio.com/articles/modules.html
# 

# The UI code
exp05UI <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  # wrap ui components in tablist
  tagList(
    h3("Experiment 5 -- Specific Heat of a Metal"),
    
    fluidRow(
      box(title = "Initial Data", status = "primary",
          textInput(ns("q1"), "1. Mass of empty Styrofoam cup (g):", value = ''),
          span(textOutput(ns("v1")), style="color:blue"),
          
          textInput(ns("q2"), "2. Mass of Cup + Water (g):", value = ''),
          span(textOutput(ns("v2")), style="color:blue"),
          
          textInput(ns("q3"), "3. Mass of Water (g):", value = ''),
          span(textOutput(ns("v3")), style="color:blue"),
          
          textInput(ns("q4"), "4. Temp. of Tap Water (degC):"),
          span(textOutput(ns("v4")), style="color:blue"),
          
          textInput(ns("q5"), "5. Temp. of Hot Metal Sample (degC):"),
          span(textOutput(ns("v5")), style="color:blue"),
          
          textInput(ns("q6"), "6. Equilibrium temperature (degC):"),
          span(textOutput(ns("v6")), style="color:blue")
      ),
      
      box(title = "Temperature Plots", status = "primary",
          plotlyOutput(ns("plot1"))
      )
    ),
    
    fluidRow(
      box(title = "Mass Of Metal", status = "primary",
          textInput(ns("q7"), "7. Mass of Cup + Water + Metal (g):"),
          span(textOutput(ns("v7")), style="color:blue"),
          
          textInput(ns("q8"), "8. Mass of Metal Sample (g):"),
          span(textOutput(ns("v8")), style="color:blue")
      ),
      
      box(title = "Heat Gained/Lost", status = "primary",
          textInput(ns("q9"), "9. Heat Gained by Water (cal):"),
          span(textOutput(ns("v9")), style="color:blue"),
          
          textInput(ns("q10"), "10. Heat Lost by the Metal  (cal):"),
          span(textOutput(ns("v10")), style="color:blue")
      )
    ),
    
    fluidRow(
      box(title = "Heat Capacity", status = "primary", width = 12,
          textInput(ns("q11"), "11. Specific Heat of Metal (cal/g*degC):"),
          span(textOutput(ns("v11")), style="color:blue"),
          a(href = "http://www2.ucdsb.on.ca/tiss/stretton/database/Specific_Heat_Capacity_Table.html", 
            "Link to Standard Metal Heat Capacities", target="_blank")
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
exp05 <- function(input, output, session, pin) {
  # load sample data for students
  observeEvent(input$load, {
    q1 = round(runif(1, min=2.0, max=3.0), digits = 4)
    q1v = format(q1, nsmall = 4)
    updateTextInput(session, "q1", value = q1v)
    
    mass.water = runif(1, min=48, max=52)
    q2 = round(mass.water + q1, digits = 4)
    q2v = format(q2, nsmall = 4)
    updateTextInput(session, "q2", value = q2v)
    
    q4 = round(runif(1, min=20.0, max=25.0), digits = 1)
    q4v = format(q4, nsmall = 1)
    updateTextInput(session, "q4", value = q4v)
    
    q5 = round(runif(1, min=98.0, max=102.0), digits = 1)
    q5v = format(q5, nsmall = 1)
    updateTextInput(session, "q5", value = q5v)
    
    q6 = round(runif(1, min=39.0, max=40.0), digits = 1)
    q6v = format(q6, nsmall = 1)
    updateTextInput(session, "q6", value = q6v)
    
    # show the temperature plot
    x = c( "Cold Water", "Hot Metal", "Equilibrium")
    y = c(q4, q5, q6)
    output$plot1 <- renderPlotly({getBarPlot(x, y, "Temperature (degC)")})
    
    mass.metal = runif(1, min=12.5, max=13.5)
    q7 = round(mass.metal + q2, digits = 4)
    q7v = format(q7, nsmall = 4)
    updateTextInput(session, "q7", value = q7v)
  })
  
  observeEvent(input$check, {
    qlist = list()
    
    # load the initial data
    q1 = as.numeric(input$q1)
    qlist["q1"] = q1
    
    q2 = as.numeric(input$q2)
    qlist["q2"] = q2
    
    # calculate the mass of water
    q3 = as.numeric(input$q3)
    ans3 = q2 - q1
    error3 = abs(q3-ans3)
    valid3 = error3 < 0.5
    output$v3 <- renderText({ showValid(valid3, ans3, pin, 4) })
    qlist["q3"] = q3
    
    q4 = as.numeric(input$q4)
    qlist["q4"] = q4
    
    q5 = as.numeric(input$q5)
    qlist["q5"] = q5
    
    q6 = as.numeric(input$q6)
    qlist["q6"] = q6
    
    # show the temperature plot
    x = c( "Cold Water", "Hot Metal", "Equilibrium")
    y = c(q4, q5, q6)
    output$plot1 <- renderPlotly({getBarPlot(x, y, "Temperature (degC)")})
    
    # calculate the mass of the metal
    q7 = as.numeric(input$q7)
    qlist["q7"] = q7
    
    q8 = as.numeric(input$q8)
    ans8 = q7 - q2
    error8 = abs(q8-ans8)
    valid8 = error8 < 0.5
    output$v8 <- renderText({ showValid(valid8, ans8, pin, 4) })
    qlist["q8"] = q8
    
    # calculate heat gained by water and lost by metal sample
    q9 = as.numeric(input$q9)
    ans9 = ans3*1.00*(q6 - q4)
    error9 = abs(q9 - ans9)
    valid9 = error9 < 0.5
    output$v9 <- renderText({ showValid(valid9, ans9, pin, 0) })
    qlist["q9"] = q9
    
    q10 = as.numeric(input$q10)
    ans10 = ans9
    error10 = abs(q10 - ans10)
    valid10 = error10 < 0.5
    output$v10 <- renderText({ showValid(valid10, ans10, pin, 0) })
    qlist["q10"] = q10
    
    # calculate the heat capacity now
    q11 = as.numeric(input$q11)
    ans11 = ans9/(ans8*(q5 - q6))
    error11 = abs(q11 - ans11)
    valid11 = error11 < 0.01
    
    if(ans11 < 1) {
      validText = showValid(valid11, ans11, pin, 3)
    } else {
      validText = showValid(valid11, ans11, pin, 2)
    }
    
    percentErrorText = percentError(0.900, q11)
    output$v11 <- renderText({ paste(validText, " / ", percentErrorText) })
    qlist["q11"] = q11
    
    # save to the database now
    dbm = saveToDB(pin, "EXP05", qlist)
    output$dbm <- renderText({dbm})
    
    cat("Experiment 05", "PIN", pin, "\n")
  })
  
  # handle viewing of old data
  observeEvent(input$view, {
    showModal(modalDialog(
      title = "Experiment 5 Saved Data",
      HTML(getSavedData(pin, "EXP05"))
    ))
    
    cat("View Data -- EXP05", "\n")
  })
}