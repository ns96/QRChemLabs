# Experiment 4 UI/Server module code
# Module used for developement and testing
#
# https://shiny.rstudio.com/articles/modules.html
# 

# The UI code
exp04UI <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  # wrap ui components in tablist
  tagList(
    h3("Experiment 4 -- Salt Water Analysis & Solubility of KNO3"),
    
    fluidRow(
      box(title = "Part A: Initial Data", status = "primary",
          selectInput(ns("u1"), "1. Unknown Solution #:", c(Choose = '', "1", "2", "3", "4")),
          span(textOutput(ns("u1v")), style="color:blue"),
          
          textInput(ns("q2"), "2. Mass of Beaker (g):"),
          span(textOutput(ns("v2")), style="color:blue"),
          
          textInput(ns("q3"), "3. Mass of Beaker + 50.0 mL of Unknown (g):"),
          span(textOutput(ns("v3")), style="color:blue"),
          
          textInput(ns("q4"), "4. Mass of Unknown (g):"),
          span(textOutput(ns("v4")), style="color:blue"),
          
          textInput(ns("q5"), "5. Volume of Unknown (L):"),
          span(textOutput(ns("v5")), style="color:blue")
      ),
      
      box(title = "Part A: Results and Calculations", status = "primary",
          textInput(ns("q6"), "6. Mass of Beaker + Dry Salt (g):"),
          span(textOutput(ns("v6")), style="color:blue"),
          
          textInput(ns("q7"), "7. Mass of Salt (g):"),
          span(textOutput(ns("v7")), style="color:blue"),
          
          textInput(ns("q8"), "8. Moles of Salt (mol):"),
          span(textOutput(ns("v8")), style="color:blue"),
          
          textInput(ns("q9"), "9. Molarity of Unknown (M):"),
          span(textOutput(ns("v9")), style="color:blue"),
          
          textInput(ns("q10"), "10. Percent by Mass of Unknown (%):"),
          span(textOutput(ns("v10")), style="color:blue")
      )
    ),
    
    # input table
    fluidRow(
      box(title = "Part B: Solubility of KNO3", status = "primary", width = 12,
          rHandsontableOutput(ns("hot")),
          
          span(textOutput(ns("vhot")), style="color:blue"),
          
          br(),
          
          downloadButton(ns("downloadData"), "Download Data")
      )
    ),
    
    fluidRow(
      box(title = "Solubility Curve", status = "primary",
          img(src='images/kno3_solubility3.jpg', width = "325px", height = "422px")
      ),
      
      box(title = "Your Curve", status = "primary",
          plotlyOutput(ns("plot1"))
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
exp04 <- function(input, output, session, pin) {
  # load data for students
  observeEvent(input$load, {
    u1v = sample(c("1", "2", "3", "4"), 1)
    updateSelectInput(session, "u1", selected = u1v)
    
    q2 = round(runif(1, min=80, max=85), digits = 4)
    q2v = format(q2, nsmall = 4)
    updateTextInput(session, "q2", value = q2v)
    
    unknown.mass = getQ3DataExp04(u1v)
    q3 = round(unknown.mass + q2, digits = 4)
    q3v = format(q3, nsmall = 4)
    updateTextInput(session, "q3", value = q3v)
    
    salt.mass = unknown.mass - 50.0
    q6 = round(salt.mass + q2, digits = 4)
    q6v = format(q6, nsmall = 4)
    updateTextInput(session, "q6", value = q6v)
    
    DF = getTableDataExp04(pin)
    
    # render the table for kno3 data
    output$hot <- renderRHandsontable({
      rhandsontable(DF, stretchH = "all", rowHeaders = FALSE)
    })
  })
  
  
  # render the datatable
  output$hot <- renderRHandsontable({
    if (!is.null(input$hot)) {
      DF = hot_to_r(input$hot)
    } else {
      DF = getTableDataExp04(pin, TRUE)
    }
    
    rhandsontable(DF, stretchH = "all", rowHeaders = FALSE)
  })
  
  # Download the data to users computer as csv
  output$downloadData <- downloadHandler(
    filename = function() {
      paste0("Experiment05_", pin, ".csv")
    },
    content = function(file) {
      DF = hot_to_r(input$hot)
      write.csv(DF, file, row.names = FALSE)
    }
  )
  
  observeEvent(input$check, {
    qlist = list()
    
    # read in initial data
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
    ans5 = 0.0500
    error5 = abs(q5-ans5)
    valid5 = error5 == 0.0
    output$v5 <- renderText({ showValid(valid5, ans5, pin, 4) })
    qlist["q5"] = q5
    
    # calculate the Molarity and percent by mass now
    q6 = as.numeric(input$q6)
    qlist["q6"] = q6
    
    q7 = as.numeric(input$q7)
    ans7 = q6 - q2
    error7 = abs(q7-ans7)
    valid7 = error7 < 0.1
    output$v7 <- renderText({ showValid(valid7, ans7, pin, 4) })
    qlist["q7"] = q7
    
    q8 = as.numeric(input$q8)
    ans8 = ans7/58.44
    error8 = abs(q8-ans8)
    valid8 = error8 < 0.001
    output$v8 <- renderText({ showValid(valid8, ans8, pin, 4) })
    qlist["q8"] = q8
    
    q9 = as.numeric(input$q9)
    ans9 = ans8/ans5
    error9 = abs(q9-ans9)
    valid9 = error9 < 0.01
    output$v9 <- renderText({ showValid(valid9, ans9, pin) })
    qlist["q9"] = q9
    
    q10 = as.numeric(input$q10)
    ans10 = (ans7/ans4)*100
    error10 = abs(q10-ans10)
    valid10 = error10 < 0.5
    output$v10 <- renderText({ showValid(valid10, ans10, pin, 1) })
    qlist["q10"] = q10
    
    # now compare percentage to actual samples to calculate 
    # the percent errors
    output$u1v <- renderText({ getPercentErrorExp04(u1, q10) })
    
    # plot the solubility data
    DF = hot_to_r(input$hot)
    
    output$plot1 <- renderPlotly({
      x_exp = c(0, DF[, 5], 100)
      y_exp = c(13, DF[, 4], 247)
      plot.data = getSolubilityDataExp04(x_exp, y_exp)
      getSmoothPlot(plot.data, "Temperature", "Solubility") 
    })
    
    output$vhot <- renderText({ checkTableDataExp04(DF, pin) })
    
    # store the test tube data
    qlist["t1"] = paste(DF[1, ], collapse = ",")
    qlist["t2"] = paste(DF[2, ], collapse = ",")
    qlist["t3"] = paste(DF[3, ], collapse = ",")
    qlist["t4"] = paste(DF[4, ], collapse = ",")
    
    # save to the database now
    dbm = saveToDB(pin, "EXP04", qlist)
    output$dbm <- renderText({dbm})
    
    cat("Experiment 4", "PIN", pin, "\n")
  })
  
  # handle viewing of old data
  observeEvent(input$view, {
    showModal(modalDialog(
      title = "Experiment 04 Saved Data",
      HTML(getSavedData(pin, "EXP04"))
    ))
    
    cat("View Data -- EXP04", "\n")
  })
}

# function to get the percent error for salt
getPercentErrorExp04 = function(unknown, salt) {
  if(unknown == '') {
    return("Please select unknown ...")
  } else if(unknown == "1") {
    p.salt = 4
  } else if(unknown == "2") {
    p.salt = 8
  } else if(unknown == "3") {
    p.salt = 12
  } else { # must be 4
    p.salt = 16
  }
  
  return(percentError(p.salt, salt))
}

# function to get the initial data
getTableDataExp04 = function(pin, empty = FALSE) {
  if(!empty) {
    v1 = c("1", "2", "3", "4")
    
    v2 = c(1.3, 2.4, 3.2, 4.3)
    v2 = addNoise(v2, 0.10)
      
    v3 = rep(5, 4)
    
    v4 = rep(-1, 4)
    if(isAdminUser(pin)){
      v4 = v2*20
    }
    
    v5 = c(10.0, 30.0, 40.0, 50.0)
    v5 = addNoise(v5, 0.10)
  } else {
    v1 = c("1", "2", "3", "4")
    v2 = rep(-1, 4)
    v3 = rep(5, 4)
    v4 = rep(-1, 4)
    v5 = rep(-1, 4)
  }
  
  DF = data.frame(v1, v2, v3, v4, v5, stringsAsFactors = FALSE)
  colnames(DF) <- c("Test Tube",
                    "Mass Of KNO3 (g)",
                    "Volume of H20",
                    "Grams Per 100 mL",
                    "Saturation Temp")
  
  return(DF)
}

# function to check the table data
checkTableDataExp04 = function(DF, pin) {
  validText = ''
  
  for(i in 1:4) {
    t.mass = DF[i,4]
    ans = DF[i, 2]*20.0
    error = abs(t.mass-ans)
    valid = error < 0.1
    vt = showValid(valid, ans, pin)
    if(i < 4) {
      validText = paste(validText, "Test Tube", i, ":", vt, "/")
    } else {
      validText = paste(validText, "Test Tube", i, ":", vt)
    }
  }
  cat(validText)
  return(validText)
}

# function to return dataframe containing actual vs experiment solubility data
getSolubilityDataExp04 = function(x_exp, y_exp) {
  s.data = list(
    x1 = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100),
    y1 = c(13, 24, 34, 48, 68, 90, 114, 140, 170, 205, 247),
    x2 = x_exp,
    y2 = y_exp
  )
  
  return(s.data)
}

getQ3DataExp04 = function(u1) {
  solution.mass = 0
  
  if(u1 == '1') {
    solution.mass = 52.30 # 4 percent salt
  } else if(u1 == '2') {
    solution.mass = 54.60 # 8 percent salt
  } else if(u1 == '3') {
    solution.mass = 56.90 # 12 percent salt
  } else {
    solution.mass = 59.20 # 16 percent salt
  }
  
  return(solution.mass)
}