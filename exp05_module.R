# Experiment 5 UI/Server module code
# Module used for development and testing
#
# https://shiny.rstudio.com/articles/modules.html
# 

# Define the list of metals and their specific heats
metalsSpecificHeat <- list(
  Aluminum = 0.215,
  Copper = 0.092,
  Iron = 0.107,
  Gold = 0.031,
  Lead = 0.031,
  Nickel = 0.106,
  Silver = 0.056,
  Zinc = 0.093,
  Chromium = 0.107,
  Titanium = 0.125,
  Tin = 0.054,
  Platinum = 0.032,
  Magnesium = 0.244,
  Brass = 0.091,
  Bronze = 0.085
)

# Prepare the choices for the selectInput
# We'll create a named vector where:
# - the names are the display labels (e.g., "Aluminum (0.215 cal/g.C)")
# - the values are the actual metal names (e.g., "Aluminum") which we'll use for lookup
unknownChoicesExp05 <- setNames(
  object = names(metalsSpecificHeat), # The actual value passed to the server? Doesn't work!
  nm = paste0(names(metalsSpecificHeat), " (", metalsSpecificHeat, " cal/g.\u00B0C)")
)

# The UI code
exp05UI <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  # wrap ui components in tablist
  tagList(
    h3("Experiment 5 -- Specific Heat of a Metal"),
    
    fluidRow(
      box(title = "Initial Data", status = "primary",
          textInput(ns("q1"), "1. Mass of water 50.0 mL in Styrofoam cup (g):", value = '50.0000'),
          span(textOutput(ns("v1")), style="color:blue"),
          
          textInput(ns("q2"), "2. Mass of Metal Sample (g):", value = ''),
          span(textOutput(ns("v2")), style="color:blue"),
          
          textInput(ns("q3"), "3. Temp. of Tap Water (degC):"),
          span(textOutput(ns("v3")), style="color:blue"),
          
          textInput(ns("q4"), "4. Temp. of Hot Metal/Bioling Water (degC):"),
          span(textOutput(ns("v4")), style="color:blue"),
          
          textInput(ns("q5"), "5. Equilibrium temperature (degC):"),
          span(textOutput(ns("v5")), style="color:blue")
      ),
      
      box(title = "Temperature Plots", status = "primary",
          plotlyOutput(ns("plot1"))
      )
    ),
    
    fluidRow(
      box(title = "Heat Gained By Water/Lost By Metal", status = "primary",
          textInput(ns("q6"), "6. Heat Gained by Water (cal):"),
          span(textOutput(ns("v6")), style="color:blue"),
          
          textInput(ns("q7"), "7. Heat Lost by the Metal (cal):"),
          span(textOutput(ns("v7")), style="color:blue")
      )
    ),
    
    fluidRow(
      box(title = "Heat Capacity", status = "primary", width = 12,
          textInput(ns("q8"), "8. Specific Heat of Metal (cal/g*degC):"),
          span(textOutput(ns("v8")), style="color:blue"),
          
          a(href = "https://us.misumi-ec.com/blog/specific-heat-capacity-of-metals/", 
            "Link to Metal Heat Capacities", target="_blank"),
          
          # add an html break here
          br(),
          
          # add a drop down menu of the most common metals specific heat in cal/g/degC
          selectInput(
            inputId = ns("u1"),
            label = "Choose a Metal:",
            choices = names(unknownChoicesExp05) # Use the names of the list as choices
          ),
          span(textOutput(ns("u1v")), style="color:blue")
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
    q2 = round(runif(1, min=20.0, max=25.0), digits = 4)
    q2v = format(q2, nsmall = 4)
    updateTextInput(session, "q2", value = q2v)
    mass.metal = q2
    
    q3 = round(runif(1, min=18.0, max=22.0), digits = 1)
    q3v = format(q3, nsmall = 1)
    updateTextInput(session, "q3", value = q3v)
    
    q4 = round(runif(1, min=98.0, max=101.0), digits = 1)
    q4v = format(q4, nsmall = 1)
    updateTextInput(session, "q4", value = q4v)
    
    q5 = round(runif(1, min=27.0, max=31.0), digits = 1)
    q5v = format(q5, nsmall = 1)
    updateTextInput(session, "q5", value = q5v)
    
    # show the temperature plot
    x = c( "Cold Water", "Hot Metal", "Equilibrium")
    y = c(q3, q4, q5)
    output$plot1 <- renderPlotly({getBarPlot(x, y, "Temperature (degC)")})
  })
  
  observeEvent(input$check, {
    qlist = list()
    
    # mass of water
    q1 = as.numeric(input$q1)
    qlist["q1"] = q1
    
    # mass of metal sample
    q2 = as.numeric(input$q2)
    qlist["q2"] = q2
    
    q3 = as.numeric(input$q3)
    qlist["q3"] = q3
    
    q4 = as.numeric(input$q4)
    qlist["q4"] = q4
    
    q5 = as.numeric(input$q5)
    qlist["q5"] = q5
    
    # show the temperature plot
    x = c( "Cold Water", "Hot Metal", "Equilibrium")
    y = c(q3, q4, q5)
    output$plot1 <- renderPlotly({getBarPlot(x, y, "Temperature (degC)")})
    
    # calculate heat gained by water and lost by metal sample
    q6 = as.numeric(input$q6)
    ans6 = q1*1.00*(q5 - q3)
    error6 = abs(q6 - ans6)
    valid6 = error6 < 0.5
    output$v6 <- renderText({ showValid(valid6, ans6, pin, 0) })
    qlist["q6"] = q6
    
    q7 = as.numeric(input$q7)
    ans7 = ans6
    error7 = abs(q7 - ans7)
    valid7 = (error7 <= 0.000001)
    output$v7 <- renderText({ showValid(valid7, ans7, pin, 0) })
    qlist["q7"] = q7
    
    # calculate the heat capacity now
    q8 = as.numeric(input$q8)
    ans8 = ans7/(q2*(q4 - q5))
    error8 = abs(q8 - ans8)
    valid8 = error8 < 0.01
    
    if(ans8 < 1) {
      validText = showValid(valid8, ans8, pin, 3)
    } else {
      validText = showValid(valid8, ans8, pin, 2)
    }
    
    output$v8 <- renderText({ paste(validText) })
    qlist["q8"] = q8
    
    # show the percent error based on the heat capacity of aluminum (0.215)
    # https://us.misumi-ec.com/blog/specific-heat-capacity-of-metals/
    percentErrorText = percentError(0.215, ans8)
    
    # get the specific heat of the metal from the selectInput
    metalSelected = input$u1
    
    # indicate if the metal selected was correctly identified as aluminum and let user know
    output$u1v <- renderText({paste("Selected Metal:", metalSelected, 
                                    "|| Actual Metal: Aluminum (0.215 cal/g.\u00B0C), ",
                                    percentErrorText)})
    qlist["u1"] = metalSelected
    
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