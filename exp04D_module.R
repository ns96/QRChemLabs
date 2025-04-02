# Experiment 4D UI/Server module code
# Module used for development and testing
#
# https://shiny.rstudio.com/articles/modules.html
# 

exp04DKnowns = c(Choose = '', "Vanillin (80-81)", "Stearic Acid (69-70)", 
                "Biphenyl (69-71)", "Benzophenone (49-51)", "Naphthalene (80-82)", 
                "1-Naphthol (95-96)", "Paradichlorobenzene (54-56)", 
                "Palmitic Acid (61-64)")

# The UI code
exp04DUI <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  # wrap ui components in tablist
  tagList(
    h3("Experiment 4 -- Freezing Point of an Unknown Sample"),
    
    fluidRow(
      box(title = "Data and Calculations", status = "primary", width = 12,
          rHandsontableOutput(ns("hot1")),
          span(htmlOutput(ns("vhot1")), style="color:blue"),
          
          br(),
          
          downloadButton(ns("downloadData"), "Download Data")
      )
    ),
    
    # add plot to show linear relationship pressure and volume
    fluidRow(
      box(width = 12, title = "Data Plot", status = "primary",
          plotlyOutput(ns("plot1")),
          span(textOutput(ns("vplot1")), style="color:blue; font-weight:bold"),
      )
    ),
    
    # add row to input average temperature
    fluidRow(
      column(6,
        # only accept numeric values for the freezing point
        numericInput(ns("q1"), "Freezing Point from Graph (deg C):", min = 40, max = 100, value = 0),
        span(textOutput(ns("v1")), style="color:blue")
      ),
      column(6, 
        selectInput(ns("u1"), "Unknown Compound Identity:", exp04DKnowns),
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
exp04D <- function(input, output, session, pin) {
  # load data for students
  observeEvent(input$load, {
    DF = getTableDataExp04D(pin)
    
    # render the table for the gas law data
    output$hot1 <- renderRHandsontable({
      rhandsontable(DF, stretchH = "all", rowHeaders = TRUE)
    })
    
    # plot the data
    x = DF[[1]]
    y = DF[[2]]
    
    # get the average for the level section,i.e. freezing point section
    # between the 5 and 13 min mark
    freezing.point = mean(y[5:13])
    y2 = rep(freezing.point)
    
    output$plot1 <- renderPlotly({
      plot.df = data.frame(x = x, y1 = y, y2 = y2)
      
      fig = getFittedLinePlot(plot.df, "", "Time (min)", "Temperature (deg C)")
    })
    
    # display the freezing point if admin
    if(isAdminUser(pin)) {
      output$vplot1 <- renderText({ 
      HTML(paste("Freezing Point:"), round(freezing.point, 1)) 
      })
    }
  })
  
  #render the table with blank data
  output$hot1 <- renderRHandsontable({
    DF = getTableDataExp04D(pin, empty = TRUE)
    rhandsontable(DF, stretchH = "all", rowHeaders = TRUE)
  })
  
  # Download the data to users computer as csv
  output$downloadData <- downloadHandler(
    filename = function() {
      paste0("Experiment04D_", pin, ".csv")
    },
    content = function(file) {
      DF = hot_to_r(input$hot1)
      write.csv(DF, file, row.names = FALSE)
    }
  )
  
  observeEvent(input$check, {
    qlist = list()
    
    # get the dataframe containing data of the melting points
    DF1 = hot_to_r(input$hot1)
    x = DF1[[1]]
    y = DF1[[2]]
    y2 = DF1[[3]]
    freezing.point = mean(y[5:13])
    
    # store this data in the qlist
    qlist["d.X"] = paste(x, collapse = ",")
    qlist["d.Y"] = paste(y, collapse = ",")
    qlist["d.Y2"] = paste(y2, collapse = ",")
    qlist["d.FP"] = freezing.point
    
    output$vhot1 <- renderText({"Checking Data"})
    
    q1 = as.numeric(input$q1)
    error1 = abs(q1 - freezing.point)
    valid1 = error1 < 2.0
    output$v1 <- renderText({ showValid(valid1, freezing.point, pin) })
    qlist["q1"] = q1
    
    q2 = input$u1
    
    # based on the selected input get the freezing point
    compound.fp = 0
    compound.name = ""
    
    if(q2 == "Vanillin (80-81)" || q2 == "Naphthalene (80-82)") {
      compound.fp = 81
      compound.name = "Vanillin or Naphthalene"
    } else if(q2 == "Stearic Acid (69-70)" || q2 == "Biphenyl (69-71)") {
      compound.fp = 70
      compound.name = "Stearic Acid or Biphenyl"
    } else if(q2 == "Benzophenone (49-51)") {
      compound.fp = 50
      compound.name = "Benzophenone"
    } else if(q2 == "1-Naphthol (95-96)") {
      compound.fp = 95.5
      compound.name = "1-Naphthol"
    } else if(q2 == "Paradichlorobenzene (54-56)") {
      compound.fp = 55
      compound.name = "Paradichlorobenzene"
    } else if(q2 == "Palmitic Acid (61-64)") {
      compound.fp = 62
      compound.name = "Palmitic Acid"
    } 
    
    error2 = abs(compound.fp - q1)
    valid2 = error2 < 2.0
    output$u1v <- renderText({ showValid(valid2, compound.name, pin) })
    qlist["q2"] = q2

    # save to the database now
    dbm = saveToDB(pin, "EXP04D", qlist)
    output$dbm <- renderText({dbm})
    
    cat("Experiment 04D", "PIN", pin, "\n")
  })
  
  # handle viewing of old data
  ns <- session$ns
  
  observeEvent(input$view, {
    showModal(modalDialog(
      size = "l",
      title = "Experiment 04D Saved Data",
      HTML(getSavedData(pin, "EXP04D")),
      
      numericInput(ns("saveDataId"), "Enter Data to Load:", value = 1, min = 1, max = 25),
      footer = tagList(
        actionButton(ns("loadsaved"), "Load"),
        modalButton("Cancel")
      ),
    ))
    
    cat("View Data -- EXP04D", "\n")
  })
  
  # load the saved data into the UI
  observeEvent(input$loadsaved, ignoreInit = TRUE, {
    dataId = input$saveDataId
    cat("Loading Saved Data to UI -- EXP04D", dataId, "\n")
  })
}

# function to get the initial data for table 1
getTableDataExp04D = function(pin, empty = FALSE) {
  if(!empty) {
    v1 = seq(0, 10, 0.5)
    v2 = getDataForCompoundExp04D()
    v3 = c(rep('Liquid', 6), rep('Liquid+Solid', 4), rep('Solid', 11))
  } else {
    v1 = seq(0, 10, 0.5)
    v2 = rep(' ', 21)
    v3 = rep(' ', 21)
  }
  
  df = data.frame(v1, v2, v3, stringsAsFactors = FALSE)
  colnames(df) <- c("Time (min)",
                    "Temperature (deg C)",
                    "Physical State (solid, liquid, both)")
  
  return(df)
}

# function to return new set of data with the a freezing point matching
# one of the compounds in the list
getDataForCompoundExp04D = function() {
  #Vanillin (80-81), Stearic Acid (69-70), 
  #Biphenyl (69-71), Benzophenone (49-51), 
  #Naphthalene (80-82), 1-Naphthol (95-96), 
  #Paradichlorobenzene (54-56), Palmitic Acid (61-64)
  
  # heating curve data for vanillin
  v2 = c(88.38, 85.81, 83.34, 81.1, 80.08, 79.99, 79.99, 79.95, 79.94, 79.94,
         79.95, 79.91, 79.34, 78.42, 77.73, 75.92, 74.1, 71.85, 69.64, 67.8, 65.1)
  
  # store the offset for all the compounds then randomly select one to add to
  # vanillin data and return that
  offsets = c(0, -10.5, -10.0, -30.0, 1.25, 15.5, -25.0, -17.5)
  v2 = v2 + sample(offsets, 1)
  
  # add some random noise
  v2 = addNoise(v2, 0.005)
  
  return(v2)
} 