# Experiment 0 UI/Server module code
# Module used for developement and testing
#
# https://shiny.rstudio.com/articles/modules.html
# 

# The UI code
exp00UI <- function(id) {
  return("Invalid PIN ...")
  
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  # wrap ui components in tablist
  tagList(
    h3("Experiment 00 -- Development Module"),
    
    fluidRow(
      box(title = "Part A", status = "primary",
          textInput(ns("q1"), "Text input 1:", value = sample(10:25, 1)),
          span(textOutput(ns("v1")), style="color:blue"),
          
          textInput(ns("q2"), "Text input 2:", value = sample(25:41, 1)),
          span(textOutput(ns("v2")), style="color:blue"),
          
          textInput(ns("q3"), "Text input 3:", value = sample(50:65, 1)),
          span(textOutput(ns("v3")), style="color:blue")
      ),
      
      box(title = "Part B", status = "primary",
          textInput(ns("u1"), "Unkown input 1:", value = sample(1:5, 1)),
          textInput(ns("q4"), "Text input 4:", value = round(runif(1, 5.0, 7.5), 3)),
          textInput(ns("q5"), "Text input 5:", value = round(runif(1, 0.0, 1.5), 3))
      )
    ),
    
    # input table
    rHandsontableOutput(ns("hot")),
    span(textOutput(ns("vhot")), style="color:blue"),
    
    br(),
    
    # test qr code
    h4("Test QR Code"),
    
    plotOutput(ns("qrcode")),
    
    fluidRow(
      # add button to check data
      column(3, actionButton(ns("check"), "Check Input Data")),
      column(3, span(textOutput(ns("dbm")), style="color:green")),
      column(3, actionButton(ns("view"), "View Saved Data")),
      column(3, actionButton(ns("ylink"), "Youtube Video"))
    )
  )
}

# Server code
exp00 <- function(input, output, session, pin) {
  # render the datatable
  output$hot <- renderRHandsontable({
    if (!is.null(input$hot)) {
      DF = hot_to_r(input$hot)
    } else {
      DF = getTableDataExp00()
    }
    
    rhandsontable(DF, stretchH = "all")
  })
  
  # test output of QR code
  # https://github.com/Broccolito/QR_Code_Generator
  output$qrcode <- renderPlot({
    cat("QRCode Code Called ...\n")
    qrcode_gen("Test Test Test")
  })
  
  observeEvent(input$check, {
    qlist = list()
    
    q1 = input$q1
    output$v1 <- renderText({ paste("Q1 Value", q1) })
    qlist["q1"] = q1
    
    q2 = input$q2
    output$v2 <- renderText({ paste("Q2 Value", q2) })
    qlist["q2"] = q2
    
    q3 = input$q3
    output$v3 <- renderText({ paste("Q3 Value", q3) })
    qlist["q3"] = q3
    
    qlist["u1"] = input$u1
    qlist["q4"] = as.numeric(input$q4) 
    qlist["q5"] = as.numeric(input$q5)
    
    DF = hot_to_r(input$hot)
    output$vhot <- renderText({ paste("Table Data:", DF$Water.mL[1]) })
    
    # save to the database now
    dbm = saveToDB(pin, "EXP00", qlist)
    output$dbm <- renderText({dbm})
    
    cat("Experiment 00", "PIN", pin, "\n")
  })
  
  
  # returns the data for a particular experiment
  savedDataTable <- reactive({
    sdDF <<- loadFromDB(pin, "EXP00")
  })
  
  # handle viewing of old data
  observeEvent(input$view, {
    showModal(modalDialog(
      title = "Experiment 00 Saved Data",
      #HTML(getSavedData(pin, "EXP00"))
      renderDataTable(options = list(searching = FALSE, paging = FALSE,
        scrollX = "10px"), {
        getSavedData(pin, "EXP00")
      })
    ))
    
    cat("View Data -- EXP00", "\n")
  })
  
  # handle view youtuve video in modal window
  observeEvent(input$ylink, {
    showModal(modalDialog(
      title = "YouTube Video",
      HTML('<iframe width="560" height="315" src="https://www.youtube.com/embed/vn3Rx3g1VPk" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>'),
      easyClose = TRUE 
    ))  
  })
}

# function to get the initial data
getTableDataExp00 = function() {
  DF = data.frame(Test.Tube = c("1", "2", "3", "4"),
                  Mass.Of.KNO3 = rep(-1, 4),
                  Water.mL = rep(5, 4),
                  Grams.Per.100.mL = rep(-1, 4),
                  Saturation.Temp = rep(-1, 4))
  
  return(DF)
}