# Experiment 12 UI/Server module code
#
# https://shiny.rstudio.com/articles/modules.html
# 

# list of known samples
exp12Knowns = c(Choose = '', "Vanillin", "Stearic Acid", "Biphenyl", 
                "Benzophenone", "Naphthalene", "1-Naphthol",
                "Paradichlorobenzene", "Palmitic Acid")

# The UI code
exp12UI <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  # wrap ui components in tablist
  tagList(
    h3("Experiment 12 -- Melting Point"),
    
    fluidRow(
      box(title = "Unknown Sample", status = "primary", width = 12,
          selectInput(ns("u1"), "Unknown Sample Number:", c(Choose = '', c(1:20))),
          span(textOutput(ns("u1v")), style="color:blue"),
          
          textInput(ns("q1a"), "Melting Point of Unknown Sample Trial #1:", value = ""),
          span(textOutput(ns("v1a")), style="color:blue"),
          
          textInput(ns("q1b"), "Melting Point of Unknown Sample Trial #2:", value = ""),
          span(textOutput(ns("v1b")), style="color:blue")
      ),
      
      box(title = "Pure Sample", status = "primary", width = 12,
          rHandsontableOutput(ns("hot1")),
          
          selectInput(ns("p1"), "Pure Sample :", exp12Knowns),
          span(textOutput(ns("p1v")), style="color:blue"),
          
          textInput(ns("q2a"), "Melting Point of Pure Sample Trial #1:", value = ""),
          span(textOutput(ns("v2a")), style="color:blue"),
          
          textInput(ns("q2b"), "Melting Point of Pure Sample Trial #2:", value = ""),
          span(textOutput(ns("v2b")), style="color:blue")
      ),
      
      box(title = "Mixed Sample and Identity of Unknown", status = "primary", width = 12,
          textInput(ns("q3a"), "Melting Point of Mixed Sample Trial #1:", value = ""),
          span(textOutput(ns("v3a")), style="color:blue"),
          
          textInput(ns("q3b"), "Melting Point of Mixed Sample Trial #2:", value = ""),
          span(textOutput(ns("v3b")), style="color:blue"),
          
          selectInput(ns("q4"), "What is the name of your unknown sample?", exp12Knowns),
          span(textOutput(ns("q4v")), style="color:blue")
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
exp12 <- function(input, output, session, pin) {
  # load data for students
  observeEvent(input$load, {
    u1v = sample(c(1:20), 1)
    updateSelectInput(session, "u1", selected = u1v)
    
    expData = getSampleDataExp12(u1v) # get the sample data
    
    updateTextInput(session, "q1a", value = expData[2])
    updateTextInput(session, "q1b", value = expData[3])
    
    updateSelectInput(session, "p1", selected = expData[1])
    updateTextInput(session, "q2a", value = expData[4])
    updateTextInput(session, "q2b", value = expData[5])
    
    updateTextInput(session, "q3a", value = expData[6])
    updateTextInput(session, "q3b", value = expData[7])
    
    print(expData)
  })
  
  # render the datatable containing the known values
  output$hot1 <- renderRHandsontable({
    if (!is.null(input$hot1)) {
      DF = hot_to_r(input$hot1)
    } else {
      DF = getTableDataExp12()
    }
    
    rhandsontable(DF, stretchH = "all", rowHeaders = FALSE)
  })
  
  observeEvent(input$check, {
    qlist = list()
    
    u1 = as.numeric(input$u1)
    qlist["u1"] = u1
    
    q1a = as.numeric(input$q1a)
    qlist["q1a"] = q1a
    
    q1b = as.numeric(input$q1b)
    qlist["q1b"] = q1b
    
    p1 = input$u1
    qlist["p1"] = p1
    
    q2a = as.numeric(input$q2a)
    qlist["q2a"] = q2a
    
    q2b = as.numeric(input$q2b)
    qlist["q2b"] = q2b
    
    # make sure the known and unkown melting points are within 5 deg C
    unknown.temp = (q1a + q1b)/2.0
    known.temp = (q2a + q2b)/2.0
    temp.diff = abs(unknown.temp - known.temp)
    
    if(temp.diff < 5) {
      output$p1v <- renderText({ 'VALID' })  
    } else {
      output$p1v <- renderText({ 'INVALID' }) 
    }
    
    q3a = as.numeric(input$q3a)
    qlist["q3a"] = q3a
    
    q3b = as.numeric(input$q3b)
    qlist["q3b"] = q3b
    
    q4 = input$q4
    qlist["q4"] = q4
    
    # now check that values matches
    output$q4v <- renderText({ checkUnknownExp12(u1, q4) })
    
    # save to the database now
    dbm = saveToDB(pin, "EXP12", qlist)
    output$dbm <- renderText({dbm})
    
    cat("Experiment 12", "PIN", pin, "\n")
  })
  
  # handle viewing of old data
  observeEvent(input$view, {
    showModal(modalDialog(
      title = "Experiment 12 Saved Data",
      HTML(getSavedData(pin, "EXP12"))
    ))
    
    cat("View Data -- EXP12", "\n")
  })
}

# function to return melting point data
getTableDataExp12 = function() {
  v1 = c('Benzophenone', 'Steric Acid', 'Vanillin', 'Naphthalene',
         '1-Naphthol', 'Paradichlorobenzene', 'Biphenyl', 'Palmitic Acid')
  
  v2 = c('49-51', '69-70', '80-81', '80-82', '95-96', '54-56', '69-71', '61-64')
  
  df = data.frame(v1,v2, stringsAsFactors = FALSE)
  colnames(df) <- c("Compound",
                    "Melting Point (degC)")
  
  return(df)
}

# function to get melting point value base on sample number
getSampleDataExp12 = function(unknown) {
  realIdentity = ''
  
  sameSample = sample(c(TRUE,FALSE),1)
  
  if(unknown %in% c(1,6,11,16)) {
    realIdentity = 'Vanillin'
    mp = c(82, 85, 81, 83)
    
    if(sameSample) {
      mix.mp = c(81,84)
    } else {
      mix.mp = c(61,64)
    }
  } else if(unknown %in% c(2,7,12,17)) {
    realIdentity = 'Stearic Acid'
    mp = c(70, 74, 68, 72)
    
    if(sameSample) {
      mix.mp = c(69,74)
    } else {
      mix.mp = c(51,53)
    }
  } else if(unknown %in% c(3,8,13,18)) {
    realIdentity = 'Biphenyl'
    mp = c(68, 75, 70, 73)
    
    if(sameSample) {
      mix.mp = c(70,74)
    } else {
      mix.mp = c(50,54)
    }
  } else if(unknown %in% c(4,9,14,19)) {
    realIdentity = 'Naphthalene'
    mp = c(82, 86, 81, 84)
    
    if(sameSample) {
      mix.mp = c(82,85)
    } else {
      mix.mp = c(64,66)
    }
  } else if(unknown %in% c(5,10,15,20)) {
    realIdentity = 'Benzophenone'
    mp = c(50, 54, 49, 51)
    mix.mp = c(51,53)
  }
  
  sample = c(realIdentity, mp, mix.mp)
}

# function to check the unknown identity and compare to actual
checkUnknownExp12 = function(unknown, unknownIdentity) {
  realIdentity = ''
  
  if(unknown %in% c(1,6,11,16)) {
    realIdentity = 'Vanillin'
  } else if(unknown %in% c(2,7,12,17)) {
    realIdentity = 'Stearic Acid'
  } else if(unknown %in% c(3,8,13,18)) {
    realIdentity = 'Biphenyl'
  } else if(unknown %in% c(5,10,15,20)) {
    realIdentity = 'Benzophenone'
  } else {
    realIdentity = 'Error, Invalid Unknown'
  }
  
  # now see if the real identity matches the selected identity
  if(realIdentity == unknownIdentity) {
    realIdentity = 'VALID'
  } else {
    realIdentity = 'INVALID'
  }
  
  return(realIdentity)
}