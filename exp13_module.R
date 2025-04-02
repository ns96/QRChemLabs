# Experiment 13 UI/Server module code
# Module used for development and testing
#
# https://shiny.rstudio.com/articles/modules.html
# 

# The compounds we are using
compounds = c('Cl2', 'HBr', 'NH3', 'SO4[2-]', 'N2', 'SbI5', 'CCl4',
              'SO3', 'XeF4', 'OF2', 'H3O+', 'H2O', 'CO2', 'CH2Cl2', 'SF4')

# shape of molecules
shapes = c('Linear', 'Trigonal Planar', 'Bent', 'Tetrahedral', 
           'Trigonal Pyramid', 'Trigonal Bipyramid', 'Seesaw', 
           'T-Shape', 'Octahedral', 'Square Pyramid', 'Square Planar')

# polarity
polarity = c('Polar', 'Non-Polar')

# keep tract of count
exp13.correct = 0

# The UI code
exp13UI <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  # wrap ui components in tablist
  tagList(
    h3("Experiment 13 -- VSEPR Theory and Modeling of Molecular Compounds"),
    
    fluidRow(
      box(title = "Molecular Compounds", status = "primary", width = 12,
          rHandsontableOutput(ns("hot1")),
          htmlOutput(ns("vhot1"))
      )
    ),

    
    fluidRow(
      # add button to check data
      column(4, actionButton(ns("check"), "Check Input Data")),
      column(4, span(textOutput(ns("dbm")), style="color:green")),
      column(4, actionButton(ns("view"), "View Saved Data"))
    )
  )
}


# Server code
exp13 <- function(input, output, session, pin) {
  # render the datatable for alum hydate
  output$hot1 <- renderRHandsontable({
    if (!is.null(input$hot1)) {
      DF = hot_to_r(input$hot1)
    } else {
      DF = getTableDataExp13()
    }
    
    rhandsontable(DF, stretchH = "all", rowHeaders = FALSE) %>%
      hot_col("Bonding.Pairs", format = "0") %>%
      hot_col("Lone.Pairs", format = "0")
  })
  
  
  observeEvent(input$check, {
    # reset the correct count
    exp13.correct <<- 0
    
    qlist = list()
    
    # get the dataframe containing data for the salts
    DF1 = hot_to_r(input$hot1)
    
    output$vhot1 <- renderText({ checkTableDataExp13(DF1, pin) })
    
    # store the molecular data by first using apply to get a list then
    # reading text from the list. If the dataframe is read directly it 
    # does not work!
    expList = apply(DF1, 1, paste, collapse=",")
    
    qlist["q1"] = paste(expList[1], collapse = ",")
    qlist["q2"] = paste(expList[2], collapse = ",")
    qlist["q3"] = paste(expList[3], collapse = ",")
    qlist["q4"] = paste(expList[4], collapse = ",")
    qlist["q5"] = paste(expList[5], collapse = ",")
    qlist["q6"] = paste(expList[6], collapse = ",")
    qlist["q7"] = paste(expList[7], collapse = ",")
    qlist["q8"] = paste(expList[8], collapse = ",")
    qlist["q9"] = paste(expList[9], collapse = ",")
    qlist["q10"] = paste(expList[10], collapse = ",")
    qlist["q11"] = paste(expList[11], collapse = ",")
    qlist["q12"] = paste(expList[12], collapse = ",")
    qlist["q13"] = paste(expList[13], collapse = ",")
    qlist["q14"] = paste(expList[14], collapse = ",")
    qlist["q15"] = paste(expList[15], collapse = ",")
    
    # save to the database now
    dbm = saveToDB(pin, "EXP13", qlist)
    output$dbm <- renderText({dbm})
    
    cat("Experiment 13", "PIN", pin, "\n")
  })
  
  # handle viewing of old data
  observeEvent(input$view, {
    showModal(modalDialog(
      title = "Experiment 13 Saved Data",
      HTML(getSavedData(pin, "EXP13"))
    ))
    
    cat("View Data -- EXP13", "\n")
  })
}

# function to get the initial data for table 1
getTableDataExp13 = function() {
  geometry = factor(rep('?', 15), levels = c('?', shapes))
  polarity = factor(rep('?', 15), levels = c('?', polarity))
  
  DF = data.frame(Formula = compounds,
                  Bonding.Pairs = rep(-1, 15),
                  Lone.Pairs = rep(-1, 15),
                  Molecular.Geometry = geometry,
                  Polar.Or.Non_Polar = polarity)
  return(DF)
}

# funct   ion to check the table data
checkTableDataExp13 = function(DF, pin) {
  validText = ''
  
  # Cl2
  validText = checkDataExp13(DF, 1, c(1, 3, 'Linear', 'Non-Polar'), pin)
  
  # HBr
  validText = paste(validText, checkDataExp13(DF, 2, c(1, 3, 'Linear', 'Polar'), pin))
  
  # NH3
  validText = paste(validText, checkDataExp13(DF, 3, c(3, 1, 'Trigonal Pyramid', 'Polar'), pin))
  
  # SO4
  validText = paste(validText, checkDataExp13(DF, 4, c(6, 0, 'Tetrahedral', 'Non-Polar'), pin))
  
  # N2
  validText = paste(validText, checkDataExp13(DF, 5, c(3, 1, 'Linear', 'Non-Polar'), pin))
  
  # SbI5
  validText = paste(validText, checkDataExp13(DF, 6, c(5, 0, 'Trigonal Bipyramid', 'Non-Polar'), pin))
  
  # CCl4
  validText = paste(validText, checkDataExp13(DF, 7, c(4, 0, 'Tetrahedral', 'Non-Polar'), pin))
  
  # SO3
  validText = paste(validText, checkDataExp13(DF, 8, c(6, 0, 'Trigonal Planar', 'Non-Polar'), pin))
  
  # XeF4
  validText = paste(validText, checkDataExp13(DF, 9, c(4, 2, 'Square Planar', 'Non-Polar'), pin))
  
  # OF2
  validText = paste(validText, checkDataExp13(DF, 10, c(2, 2, 'Bent', 'Polar'), pin))
  
  # H3O+
  validText = paste(validText, checkDataExp13(DF, 11, c(3, 1, 'Trigonal Pyramid', 'Polar'), pin))
  
  # H2O
  validText = paste(validText, checkDataExp13(DF, 12, c(2, 2, 'Bent', 'Polar'), pin))
  
  # CO2
  validText = paste(validText, checkDataExp13(DF, 13, c(4, 0, 'Linear', 'Non-Polar'), pin))
  
  # CH2Cl2
  validText = paste(validText, checkDataExp13(DF, 14, c(4, 0, 'Tetrahedral', 'Polar'), pin))
  
  # SF4
  validText = paste(validText, checkDataExp13(DF, 15, c(4, 1, 'Seesaw', 'Polar'), pin))
  
  # percent correct
  percent.correct = (exp13.correct/60.0)*100
  #cat('Exp 14 Data >>', percent.correct, exp13.correct)
  validText = paste(validText, '<br>Percent Correct:', round(percent.correct), '%')
  
  return(HTML(validText))                  
}

# check the molecular information entered against the actuals answers
checkDataExp13 = function(DF, rn, answers, pin) {
  text.color = '<p style="color:red;">'
  
  valid.BP = 'INVALID'
  if(DF[rn, 2] == answers[1]) {
    valid.BP = 'VALID'
    exp13.correct <<- exp13.correct + 1
    text.color = '<p style="color:blue;">'
  }
  
  valid.LP = 'INVALID'
  if(DF[rn, 3] == answers[2]) {
    valid.LP = 'VALID'
    exp13.correct <<- exp13.correct + 1
    text.color = '<p style="color:blue;">'
  }
  
  valid.MG = 'INVALID'
  if(DF[rn, 4] == answers[3]) {
    valid.MG = 'VALID'
    exp13.correct <<- exp13.correct + 1
    text.color = '<p style="color:blue;">'
  }
  
  valid.MP = 'INVALID'
  if(DF[rn, 5] == answers[4]) {
    valid.MP = 'VALID'
    exp13.correct <<- exp13.correct + 1
    text.color = '<p style="color:blue;">'
  }
  
  validText = paste(DF[rn, 1], '|| Bonding Pairs:', valid.BP, 
                    '/ Lone Pairs:', valid.LP, '/ Geometry:', valid.MG, 
                    '/ Polarity:', valid.MP)
  
  validText = paste(text.color, validText, '</p>')
  
  return(validText)
}