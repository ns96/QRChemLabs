# Experiment 11 UI/Server module code
# Module used for development and testing
#
# https://shiny.rstudio.com/articles/modules.html
# 

# The UI code
exp11UI <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  # wrap ui components in tablist
  tagList(
    h3("Experiment 11 -- Energy Changes in Chemical Reactions"),
    
    fluidRow(
      box(title = "Data and Calculations", status = "primary", width = 12,
          rHandsontableOutput(ns("hot1")),
          span(htmlOutput(ns("vhot1")), style="color:blue"),
          
          br(),
          
          downloadButton(ns("downloadData"), "Download Data")
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
exp11 <- function(input, output, session, pin) {
  # load data for students
  observeEvent(input$load, {
    DF = getTableDataExp11(pin)
    
    # render the table for the gas law data
    output$hot1 <- renderRHandsontable({
      rhandsontable(DF, stretchH = "all", rowHeaders = TRUE)
    })
  })
  
  # render the data table
  output$hot1 <- renderRHandsontable({
    if (!is.null(input$hot1)) {
      DF = hot_to_r(input$hot1)
    } else {
      DF = getTableDataExp11(pin, empty = TRUE)
    }
    
    rhandsontable(DF, stretchH = "all", rowHeaders = FALSE)
  })
  
  # Download the data to users computer as csv
  output$downloadData <- downloadHandler(
    filename = function() {
      paste0("Experiment12_", pin, ".csv")
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
    
    output$vhot1 <- renderText({ checkTableDataExp11(DF1, pin) })
    
    # store the salt data
    qlist["q1"] = paste(DF1[1, c(2,3,4)], collapse = ",")
    qlist["q2"] = paste(DF1[2, c(2,3,4)], collapse = ",")
    qlist["q3"] = paste(DF1[3, c(2,3,4)], collapse = ",")
    qlist["q4"] = paste(DF1[4, c(2,3,4)], collapse = ",")
    qlist["q5"] = paste(DF1[5, c(2,3,4)], collapse = ",")
    qlist["q6"] = paste(DF1[6, c(2,3,4)], collapse = ",")
    qlist["q7"] = paste(DF1[7, c(2,3,4)], collapse = ",")
    qlist["q8"] = paste(DF1[8, c(2,3,4)], collapse = ",")
    qlist["q9"] = paste(DF1[9, c(2,3,4)], collapse = ",")
    qlist["q10"] = paste(DF1[10, c(2,3,4)], collapse = ",")
    qlist["q11"] = paste(DF1[11, c(2,3,4)], collapse = ",")
    qlist["q12"] = paste(DF1[12, c(2,3,4)], collapse = ",")
    qlist["q13"] = paste(DF1[13, c(2,3,4)], collapse = ",")
    qlist["q14"] = paste(DF1[14, c(2,3,4)], collapse = ",")
    
    # save to the database now
    dbm = saveToDB(pin, "EXP11", qlist)
    output$dbm <- renderText({dbm})
    
    cat("Experiment 11", "PIN", pin, "\n")
  })
  
  # handle viewing of old data
  observeEvent(input$view, {
    showModal(modalDialog(
      title = "Experiment 11 Saved Data",
      HTML(getSavedData(pin, "EXP11"))
    ))
    
    cat("View Data -- EXP11", "\n")
  })
}

# function to get the initial data for table 1
getTableDataExp11 = function(pin, empty = FALSE) {
  # rep(-1, 4)
  questions = c('1. Volume of water used, mL',
               '2. Temperature (t_initial), degC',
               '3. Temperature (t_final), degC',
               '4. Mass of  water used, g',
               '5. Mass of solution, g',
               '6. Delta T, degC',
               '7. Calories absorbed or release, cal',
               '8. Grams of solid used, g',
               '9. Molar Mass of solid, g/mol',
               '10. Mols of solid used, mol',
               '11. Molar Heat of solution, cal/mol',
               '12. Molar Heat of solution, kcal/mol',
               '13. Given,Theoretical molar heat of solution, kcal/mol',
               '14. Percent Error')
  
  #salt1 = c(40.0, 24.5, 7.5, 40.0, 51.5, 17.0, 744.18, 11.5, 80.043, 0.144, 5168, 5.168, 6.2, 16.6)
  #salt2 = c(40.0, 24.5, 61.4, 40.0, 52.5, -36.9, -1646.66, 12.5, 110.98, 0.113, -14572, -14.572, -17.4, 16.3)
  #salt3 = c(40.0, 24.5, 22.0, 40.0, 53.2, 2.5, 113.05, 13.2, 58.44, 0.226, 500, 0.5, 1.2, 58.3)
  
  if(!empty) {
    # NH4NO3 Data
    # salt1 = c(40.0, 24.5, 7.5, 40.0, 51.5, 17.0, 744.18, 11.5, 80.043, 0.144, 5168, 5.168, 6.2, 16.6)
    ft1 = addNoise(7.5, 0.1)
    mass1 = addNoise(11.5, 0.1)
    sol.mass1 = 40.0 + mass1
    diff1 = 24.5 - ft1
    cal1 = sol.mass1*0.85*diff1
    moles1 = mass1/80.043
    mheat1 = cal1/moles1
    mheat1.kcal = mheat1/1000
    pe1 = (abs(mheat1.kcal - 6.2)/6.2)*100
    
    if(isAdminUser(pin)) {
      salt1 = c(40.0, 24.5, ft1, 40.0, sol.mass1, diff1, cal1, mass1, 80.043, moles1, mheat1, mheat1.kcal, 6.2, pe1)
    } else {
      salt1 = c(40.0, 24.5, ft1, 40.0, -1, -1, -1, mass1, 80.043, -1, -1, -1, 6.2, -1)
    }
    
    # CaCl2 Data
    # salt2 = c(40.0, 24.5, 61.4, 40.0, 52.5, -36.9, -1646.66, 12.5, 110.98, 0.113, -14572, -14.572, -17.4, 16.3)
    ft2 = addNoise(61.4, 0.1)
    mass2 = addNoise(12.5, 0.1)
    sol.mass2 = 40.0 + mass2
    diff2 = 24.5 - ft2
    cal2 = sol.mass2*0.85*diff2
    moles2 = mass2/110.98
    mheat2 = cal2/moles2
    mheat2.kcal = mheat2/1000
    pe2 = abs((mheat2.kcal - -17.4)/-17.4)*100
    
    if(isAdminUser(pin)) {
      salt2 = c(40.0, 24.5, ft2, 40.0, sol.mass2, diff2, cal2, mass2, 110.98, moles2, mheat2, mheat2.kcal, -17.4, pe2)
    } else {
      salt2 = c(40.0, 24.5, ft2, 40.0, -1, -1, -1, mass2, 110.98, -1, -1, -1, -17.4, -1)
    }
    
    # NaCl2 Data
    # salt3 = c(40.0, 24.5, 22.0, 40.0, 53.2, 2.5, 113.05, 13.2, 58.44, 0.226, 500, 0.5, 1.2, 58.3)
    ft3 = addNoise(22.4, 0.05)
    mass3 = addNoise(13.2, 0.10)
    sol.mass3 = 40.0 + mass3
    diff3 = 24.5 - ft3
    cal3 = sol.mass3*0.85*diff3
    moles3 = mass3/58.44
    mheat3 = cal3/moles3
    mheat3.kcal = mheat3/1000
    pe3 = abs((mheat3.kcal - 1.2)/1.2)*100
    
    if(isAdminUser(pin)) {
      salt3 = c(40.0, 24.5, ft3, 40.0, sol.mass3, diff3, cal3, mass3, 58.44, moles3, mheat3, mheat3.kcal, 1.2, pe3)
    } else {
      salt3 = c(40.0, 24.5, ft3, 40.0, -1, -1, -1, mass3, 58.44, -1, -1, -1, 1.2, -1)
    }
  } else {
    salt1 = c(40.0, -1, -1, 40.0, -1, -1, -1, -1, 80.043, -1, -1, -1, 6.2, -1)
    salt2 = c(40.0, -1, -1, 40.0, -1, -1, -1, -1, 110.98, -1, -1, -1, -17.4, -1)
    salt3 = c(40.0, -1, -1, 40.0, -1, -1, -1, -1, 58.44, -1, -1, -1, 1.2, -1)
  }
  
  DF = data.frame(Heat.Of.Solution = questions,
                  NH4NO3 = salt1,
                  CaCl2 = salt2,
                  NaCl = salt3)
  
  # round numerical values in the dataframe to 2 digits
  DF[, -1] = round(DF[, -1], digits = 2)
  return(DF)
}

# function to check the table data
checkTableDataExp11 = function(DF, pin) {
  validText = ''
  
  # salt 1
  validText = paste('NH4NO3 ||', checkDataExp11(DF, 2, pin))
  
  # salt 2
  validText = paste(validText, '<br><br>CaCl2 ||', checkDataExp11(DF, 3, pin))
  
  # salt 3
  validText = paste(validText, '<br><br>NaCl ||', checkDataExp11(DF, 4, pin))
  
  return(HTML(validText))                  
}

checkDataExp11 = function(DF, column, pin) {
  q5 = DF[5, column]
  ans5 = DF[4, column] + DF[8, column]
  error5 = abs(q5 - ans5)
  valid5 = error5 < 0.5
  vt5 = showValid(valid5, ans5, pin)
  validText = paste('Q5 > ', vt5, '/')
  
  q6 = DF[6, column]
  ans6 = DF[2, column] - DF[3, column]
  error6 = abs(q6 - ans6)
  valid6 = error6 < 0.5
  vt6 = showValid(valid6, ans6, pin)
  validText = paste(validText, 'Q6 > ', vt6, '/')
  
  q7 = DF[7, column]
  ans7 = ans5*0.85*ans6
  error7 = abs(q7 - ans7)
  valid7 = error7 < 0.5
  vt7 = showValid(valid7, ans7, pin)
  validText = paste(validText, 'Q7 > ', vt7, '/')
  
  q10 = DF[10, column]
  ans10 = round(DF[8, column]/DF[9, column], 3)
  error10 = abs(q10 - ans10)
  valid10 = error10 < 0.005
  vt10 = showValid(valid10, ans10, pin)
  validText = paste(validText, 'Q10 > ', vt10, '/')
  
  q11 = DF[11, column]
  ans11 = ans7/ans10
  error11 = abs(q11 - ans11)
  valid11 = error11 < abs(0.01*ans11)
  vt11 = showValid(valid11, ans11, pin)
  validText = paste(validText, 'Q11 > ', vt11, '/')
  
  q12 = DF[12, column]
  ans12 = ans11/1000.0
  error12 = abs(q12 - ans12)
  valid12 = error12 < 0.05
  vt12 = showValid(valid12, ans12, pin)
  validText = paste(validText, 'Q12 > ', vt12, '/')
  
  q14 = DF[14, column]
  ans14 = abs((ans12 - DF[13, column])/DF[13, column])*100
  error14 = abs(q14 - ans14)
  valid14 = error14 < 0.5
  vt14 = showValid(valid14, ans14, pin)
  validText = paste(validText, 'Q14 > ', vt14)
  
  return(validText)
}