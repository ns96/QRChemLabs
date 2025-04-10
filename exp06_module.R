# Experiment 6 UI/Server module code
# Module used for developement and testing
#
# https://shiny.rstudio.com/articles/modules.html
# 

unknownsExp06 = c("Calcium Sulfate", "Cupric Sulfate", 
             "Magnesium Sulfate", "Sodium Carbonate")

avg.waterEXP06 = 0.0

# The UI code
exp06UI <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  # wrap ui components in tablist
  tagList(
    h3("Experiment 6 -- Analysis of a Hydrate"),
    
    fluidRow(
      box(title = "Part-A: Percent Water in Alum Hydrate", status = "primary", width = 12,
          rHandsontableOutput(ns("hot1")),
          downloadButton(ns("downloadData1"), "Download Data"),
          span(textOutput(ns("vhot1")), style="color:blue"),
          
          br(),
          
          textInput(ns("q7"), "7. Average Experimental % of water in Alum:"),
          span(textOutput(ns("v7")), style="color:blue"),
          
          textInput(ns("q8"), "8. Theoretical % of water in Alum:", value = 45.576),
          span(textOutput(ns("v8")), style="color:blue"),
          
          textInput(ns("q9"), "9. % Error:"),
          span(textOutput(ns("v9")), style="color:blue")
      )
    ),

    
    fluidRow(
      box(title = 'Part-B: Percent Water in "Unknown" Hydrate', status = "primary", width = 12,
          selectInput(ns("u1"), "Unknown :", c(Choose = '', unknownsExp06)),
          span(textOutput(ns("u1v")), style="color:blue"),
          
          rHandsontableOutput(ns("hot2")),
          downloadButton(ns("downloadData2"), "Download Data"),
          span(textOutput(ns("vhot2")), style="color:blue")
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
exp06 <- function(input, output, session, pin) {
  # load data for students
  observeEvent(input$load, {
    # render the table for the alum data
    DF1 = getTable1DataExp06(pin)
    
    output$hot1 <- renderRHandsontable({
      rhandsontable(DF1, stretchH = "all", rowHeaders = FALSE)
    })
  
    # set the unknown
    u1v = sample(unknownsExp06, 1)
    updateSelectInput(session, "u1", selected = u1v)
    
    # render the table for the unknown hydrate
    DF2 = getTable2DataExp06(pin, u1v)
    
    output$hot2 <- renderRHandsontable({
      rhandsontable(DF2, stretchH = "all", rowHeaders = FALSE)
    })
  })
  
  # render the datatable for alum hydate
  output$hot1 <- renderRHandsontable({
    if (!is.null(input$hot1)) {
      DF = hot_to_r(input$hot1)
    } else {
      DF = getTable1DataExp06(pin, TRUE)
    }
    
    rhandsontable(DF, stretchH = "all", rowHeaders = FALSE)
  })
  
  # render the datatable for unknown hydrate
  output$hot2 <- renderRHandsontable({
    if (!is.null(input$hot2)) {
      DF = hot_to_r(input$hot2)
    } else {
      DF = getTable2DataExp06(pin, NULL, TRUE)
    }
    
    rhandsontable(DF, stretchH = "all", rowHeaders = FALSE)
  })
  
  # Download the data to users computer as csv
  output$downloadData1 <- downloadHandler(
    filename = function() {
      paste0("Experiment07-1_", pin, ".csv")
    },
    content = function(file) {
      DF = hot_to_r(input$hot1)
      write.csv(DF, file, row.names = FALSE)
    }
  )
  
  output$downloadData2 <- downloadHandler(
    filename = function() {
      paste0("Experiment07-2_", pin, ".csv")
    },
    content = function(file) {
      DF = hot_to_r(input$hot2)
      write.csv(DF, file, row.names = FALSE)
    }
  )
  
  observeEvent(input$check, {
    qlist = list()
    
    # get the dataframe containing data for alum
    DF1 = hot_to_r(input$hot1)
    checklist = checkTableDataExp06(DF1, pin, 1)
    output$vhot1 <- renderText({ checklist$text })
    print(paste("Percent Water: ", avg.waterEXP06))
    
    # store the alum data
    qlist["q1"] = paste(DF1[1, c(2,3)], collapse = ",")
    qlist["q2"] = paste(DF1[2, c(2,3)], collapse = ",")
    qlist["q3"] = paste(DF1[3, c(2,3)], collapse = ",")
    qlist["q4"] = paste(DF1[4, c(2,3)], collapse = ",")
    qlist["q5"] = paste(DF1[5, c(2,3)], collapse = ",")
    qlist["q6"] = paste(DF1[6, c(2,3)], collapse = ",")
    
    # calculate average of the percent water
    q7 = as.numeric(input$q7)
    ans7 = checklist$data
    error7 = abs(q7-ans7)
    valid7 = error7 < 0.5
    output$v7 <- renderText({ showValid(valid7, ans7, pin) })
    qlist["q7"] = q7
    
    # get the theoretical percent water
    q8 = as.numeric(input$q8)
    qlist["q8"] = q8
    
    # calculate the percent error
    q9 = as.numeric(input$q9)
    ans9 = (abs(ans7-q8)/q8)*100
    error9 = abs(q9 - ans9)
    valid9 = error9 < 0.5
    
    # show the percent error
    if(ans9 < 1) {
      output$v9 <- renderText({ showValid(valid9, ans9, pin, 3) })
    } else {
      output$v9 <- renderText({ showValid(valid9, ans9, pin) })
    }
    
    qlist["q9"] = q9
    
    # get the dataframe containing data for alum
    DF2 = hot_to_r(input$hot2)
    checklist = checkTableDataExp06(DF2, pin, 13)
    output$vhot2 <- renderText({ checklist$text })
    
    # store the alum data
    qlist["q10"] = paste(DF2[1, c(2,3)], collapse = ",")
    qlist["q11"] = paste(DF2[2, c(2,3)], collapse = ",")
    qlist["q12"] = paste(DF2[3, c(2,3)], collapse = ",")
    qlist["q13"] = paste(DF2[4, c(2,3)], collapse = ",")
    qlist["q14"] = paste(DF2[5, c(2,3)], collapse = ",")
    qlist["q15"] = paste(DF2[6, c(2,3)], collapse = ",")
    
    # indicate the percent error
    avg.water = checklist$data
    avg.water.inputed = (as.numeric(DF2[6,2]) + as.numeric(DF2[6,3]))/2
    output$u1v <- renderText({ getHydratePercentErrorExp06(input$u1, avg.water, avg.water.inputed) })
    qlist["u1"] = input$u1
    
    # save to the database now
    dbm = saveToDB(pin, "EXP06", qlist)
    output$dbm <- renderText({dbm})
    
    
    cat("Experiment 06", "PIN", pin, "\n")
  })
  
  # handle viewing of old data
  observeEvent(input$view, {
    showModal(modalDialog(
      title = "Experiment 6 Saved Data",
      HTML(getSavedData(pin, "EXP06"))
    ))
    
    cat("View Data -- EXP06", "\n")
  })
}

# function to get the initial data for table 1
getTable1DataExp06 = function(pin, empty = FALSE) {
  v1 = c('1. Mass of evaporating dish',
          '2. Mass of evaporating dish with Alum',
          '3. Mass of evaporating dish with Anhydrous Alum',
          '4. Mass of Alum Hydrate used',
          '5. Mass of Water lost',
          '6. % of Water in Alum')
  
  if(!empty) {
    v2 = getSampleDataTable1Exp06()
    v3 = getSampleDataTable1Exp06()
  } else {
    v2 = rep(' ', 6)
    v3 = rep(' ', 6)
  }
  
  DF = data.frame(v1, v2, v3, stringsAsFactors = FALSE)
  colnames(DF) <- c("Observation", "Trial 1", "Trial 2")
  
  return(DF)
}

# get sameple data for table 1
getSampleDataTable1Exp06 = function() {
  q1 = round(runif(1, min=50.0, max=55.0), digits = 4)
  q1v = format(q1, nsmall = 4)
  
  alum.mass = runif(1, min=1.8, max=2.3)
  q2 = round(q1 + alum.mass, digits = 4)
  q2v = format(q2, nsmall = 4)
  
  percent.water = runif(1, min=45, max=46)/100.0
  alum.anhydrous = alum.mass * (1 - percent.water)
  q3 = round(q1 + alum.anhydrous, digits = 4)
  q3v = format(q3, nsmall = 4)
  
  return (c(q1v, q2v, q3v, ' ', ' ', ' '))
}

# function to get the initial data for table 1
getTable2DataExp06 = function(pin, u1, empty = FALSE) {
  v1 = c('10. Mass of evaporating dish',
          '11. Mass of evaporating dish with Unknown Hydrate',
          '12. Mass of evaporating dish with Anhydrous solid',
          '13. Mass of Unknown Hydrate used',
          '14. Mass of Water lost',
          '15. % of Water in Unknown Hydrate')
  
  if(!empty) {
    v2 = getSampleDataTable2Exp06(u1)
    v3 = getSampleDataTable2Exp06(u1)
  } else {
    v2 = rep(' ', 6)
    v3 = rep(' ', 6)
  }
  
  DF = data.frame(v1, v2, v3, stringsAsFactors = FALSE)
  colnames(DF) <- c("Observation", "Trial 1", "Trial 2")
  
  return(DF)
}

# get sameple data for table 2
getSampleDataTable2Exp06 = function(unknown) {
  q10 = round(runif(1, min=50.0, max=55.0), digits = 4)
  q10v = format(q10, nsmall = 4)
  
  unknown.mass = runif(1, min=1.8, max=2.3)
  q11 = round(q10 + unknown.mass, digits = 4)
  q11v = format(q11, nsmall = 4)
  
  if(unknown == 'Calcium Sulfate') {
    percent.water = runif(1, min=18, max=22)/100.0 # actual 19.702
  } else if(unknown == 'Cupric Sulfate') {
    percent.water = runif(1, min=34, max=38)/100.0 # actual 36.0771
  } else if(unknown == 'Magnesium Sulfate') {
    percent.water = runif(1, min=49, max=53)/100.0 # actual 51.16
  } else if(unknown == 'Sodium Carbonate') {
    percent.water = runif(1, min=60, max=65)/100.0 # actual 62.94
  } else {
    percent.water = 0
  }
  
  unknown.anhydrous = unknown.mass * (1 - percent.water)
  q12 = round(q10 + unknown.anhydrous, digits = 4)
  q12v = format(q12, nsmall = 4)
  
  return (c(q10v, q11v, q12v, ' ', ' ', ' '))
}

# function to check the table data
checkTableDataExp06 = function(DF, pin, q.start) {
  validText = ''
  
  # hydrate mass
  ans1a = as.numeric(DF[2,2]) - as.numeric(DF[1,2])
  ans1b = as.numeric(DF[2,3]) - as.numeric(DF[1,3])
  validText = checkRowDataExp06(DF[4,2], DF[4,3], ans1a, ans1b, q.start, pin)
  
  # mass of water lost
  ans2a = as.numeric(DF[2,2]) - as.numeric(DF[3,2])
  ans2b = as.numeric(DF[2,3]) - as.numeric(DF[3,3])
  validText = paste(validText, '/', checkRowDataExp06(DF[5,2], DF[5,3], ans2a, ans2b, q.start+1, pin))
  
  # percent of water in hydrate
  ans3a = (ans2a/ans1a)*100
  ans3b = (ans2b/ans1b)*100
  validText = paste(validText, '/', checkRowDataExp06(DF[6,2], DF[6,3], ans3a, ans3b, q.start+2, pin, 1))
  
  # calculate the average of answer 3 
  avg.water = (ans3a + ans3b)/2
  print(paste("Check Table Percent Water: ", avg.water))
  
  checklist = list("text" = validText, "data" = avg.water)
  return(checklist)                  
}

checkRowDataExp06 = function(qXa, qXb, ansXa, ansXb, q.number, pin, rdigits = 4) {
  qXa = as.numeric(qXa)
  qXb = as.numeric(qXb)
  
  errorXa = abs(qXa-ansXa)
  errorXb = abs(qXb-ansXb)
  valid = (errorXa < 0.5) && (errorXb < 0.5)
  
  ansXa = format(round(ansXa, rdigits), nsmall = rdigits)
  ansXb = format(round(ansXb, rdigits), nsmall = rdigits)
  
  ans = paste(ansXa, ',' , ansXb)
  vt = showValid(valid, ans, pin)
  validText = paste("Q", q.number, ":", vt)
  return(validText)
}

getHydratePercentErrorExp06 = function(unknown, avg.water, avg.water.inputed) {
  actual.water = 0.0
  
  if(unknown == 'Calcium Sulfate') {
    actual.water = 19.702
  } else if(unknown == 'Cupric Sulfate') {
    actual.water = 36.0771
  } else if(unknown == 'Magnesium Sulfate') {
    actual.water = 51.16
  } else if(unknown == 'Sodium Carbonate') {
    actual.water = 62.94
  }
  
  avg.water = format(round(avg.water, 1), nsmall = 1)
  percentError = paste("Answer:", avg.water, "Inputed % Water:", avg.water.inputed,
                       percentError(actual.water, avg.water.inputed))
  
  return(percentError)
}