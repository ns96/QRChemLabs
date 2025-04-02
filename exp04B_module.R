# Experiment 4B UI/Server module code
# Module used for development and testing
#
# https://shiny.rstudio.com/articles/modules.html
# 

# The UI code
exp04BUI <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  # wrap ui components in tablist
  tagList(
    h3("Experiment 4 -- Boyle's Law: Pressure-Volume Relationship in Gases"),
    
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
exp04B <- function(input, output, session, pin) {
  # load data for students
  observeEvent(input$load, {
    DF = getTableDataExp04B(pin)
    
    # render the table for the gas law data
    output$hot1 <- renderRHandsontable({
      rhandsontable(DF, stretchH = "all", rowHeaders = TRUE)
    })
    
    # fit and plot the data
    x = DF[[1]]
    y = DF[[2]]
    fit.df = data.frame(x, y)
    fit.numbers = doPowerFit(fit.df)
    
    output$plot1 <- renderPlotly({
      plot.df = data.frame(x = x, y1 = y, y2 = fit.numbers$predicted)
      
      fig = getFittedLinePlot(plot.df, "", "Volume (mL)", "Pressure (KPa)")
    })
    
    output$vplot1 <- renderText({ 
      HTML(fit.numbers$equaton) 
    })
  })
  
  #render the table with blank data
  output$hot1 <- renderRHandsontable({
    DF = getTableDataExp04B(pin, empty = TRUE)
    rhandsontable(DF, stretchH = "all", rowHeaders = TRUE)
  })
  
  # Download the data to users computer as csv
  output$downloadData <- downloadHandler(
    filename = function() {
      paste0("Experiment04B_", pin, ".csv")
    },
    content = function(file) {
      DF = hot_to_r(input$hot1)
      write.csv(DF, file, row.names = FALSE)
    }
  )
  
  observeEvent(input$check, {
    qlist = list()
    
    # get the dataframe containing data for the salts
    DF1 = hot_to_r(input$hot1)
    
    output$vhot1 <- renderText({ checkTableDataExp04B(DF1, pin) })
    
    # store the salt data
    qlist["q1"] = paste(DF1[1, c(1,2,3)], collapse = ",")
    qlist["q2"] = paste(DF1[2, c(1,2,3)], collapse = ",")
    qlist["q3"] = paste(DF1[3, c(1,2,3)], collapse = ",")
    qlist["q4"] = paste(DF1[4, c(1,2,3)], collapse = ",")
    qlist["q5"] = paste(DF1[5, c(1,2,3)], collapse = ",")

    # save to the database now
    dbm = saveToDB(pin, "EXP04B", qlist)
    output$dbm <- renderText({dbm})
    
    cat("Experiment 04B", "PIN", pin, "\n")
  })
  
  # handle viewing of old data
  observeEvent(input$view, {
    showModal(modalDialog(
      title = "Experiment 04B Saved Data",
      HTML(getSavedData(pin, "EXP04B"))
    ))
    
    cat("View Data -- EXP04B", "\n")
  })
}

# function to get the initial data for table 1
getTableDataExp04B = function(pin, empty = FALSE) {
  if(!empty) {
    v1 = c(5.8, 7.8, 10.8, 12.8, 15.8)
    v2 = c(186.4, 142.4, 101.7, 86.8, 74.1)
  
    # randomize the data
    v2 = addNoise(v2, 0.1)
  
    v3 = c(' ', ' ', ' ', ' ', ' ')
    if(isAdminUser(pin)) {
      v3 = v1*v2
    }
  } else {
    v1 = rep(' ', 5)
    v2 = rep(' ', 5)
    v3 = rep(' ', 5)
  }
  
  df = data.frame(v1,v2,v3, stringsAsFactors = FALSE)
  colnames(df) <- c("Volume (mL)",
                    "Pressure (KPa)",
                    "Constant, k (V x P)")
  
  return(df)
}

# function to check the table data
checkTableDataExp04B = function(DF, pin) {
  validText = paste('Constant, k ||', checkDataExp04B(DF, pin))
  
  return(HTML(validText))                  
}

checkDataExp04B = function(DF, pin) {
  q1 = as.numeric(DF[1,3])
  ans1 = DF[1, 1] * DF[1, 2]
  error1 = abs(q1 - ans1)
  valid1 = error1 < 0.5
  vt1 = showValid(valid1, ans1, pin)
  validText = paste('Q1 > ', vt1, '/')
  
  q2 = as.numeric(DF[2, 3])
  ans2 = DF[2, 1] * DF[2, 2]
  error2 = abs(q2 - ans2)
  valid2 = error2 < 0.5
  vt2 = showValid(valid2, ans2, pin)
  validText = paste(validText, 'Q2 > ', vt2, '/')
  
  q3 = as.numeric(DF[3, 3])
  ans3 = DF[3, 1] * DF[3, 2]
  error3 = abs(q3 - ans3)
  valid3 = error3 < 0.5
  vt3 = showValid(valid3, ans3, pin)
  validText = paste(validText, 'Q3 > ', vt3, '/')
  
  q4 = as.numeric(DF[4, 3])
  ans4 = DF[4, 1] * DF[4, 2]
  error4 = abs(q4 - ans4)
  valid4 = error4 < 0.5
  vt4 = showValid(valid4, ans4, pin)
  validText = paste(validText, 'Q4 > ', vt4, '/')
  
  q5 = as.numeric(DF[5, 3])
  ans5 = DF[5, 1] * DF[5, 2]
  error5 = abs(q5 - ans5)
  valid5 = error5 < 0.5
  vt5 = showValid(valid5, ans5, pin)
  validText = paste(validText, 'Q5 > ', vt5)
  
  return(validText)
}