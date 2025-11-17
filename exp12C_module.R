# Experiment 11 UI/Server module code
# Module used for developement and testing
#
# https://shiny.rstudio.com/articles/modules.html
# 

exp12CL = list()

# The UI code
exp12CUI <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  # wrap ui components in tablist
  tagList(
    h3("Experiment 12 -- THE BUFFER IN LEMONADE"),
    
    fluidRow(
      box(width = 6, title = "Lemonade Trial", status = "primary",
        plotlyOutput(ns("plot1")),
        downloadButton(ns("downloadPlot1Data"), "Download Plot Data"),
        
        fileInput(ns("plot1Data"), "Upload CSV File",
                  multiple = FALSE,
                  accept = c("text/csv",
                             "text/comma-separated-values,text/plain",
                             ".csv")),
        
        br(),
        
        numericInput(ns("i1"), "NaOH Volume Before Eq Point (mL):", value = ""),
        numericInput(ns("i2"), "NaOH Volume After Eq Point (mL):", value = "")
      ),
      
      box(width = 6, title = "Citric Acid Trial", status = "primary",
        plotlyOutput(ns("plot2")),
        downloadButton(ns("downloadPlot2Data"), "Download Plot Data"),
        
        fileInput(ns("plot2Data"), "Upload CSV File",
                  multiple = FALSE,
                  accept = c("text/csv",
                             "text/comma-separated-values,text/plain",
                             ".csv")),
        
        br(),
        
        numericInput(ns("i3"), "NaOH Volume Before Eq Point (mL):", value = ""),
        numericInput(ns("i4"), "NaOH Volume After Eq Point (mL):", value = "")
      )
    ),
    
    fluidRow(
      # the data table
      rHandsontableOutput(ns("hot1")),
      
      br(),
      
      downloadButton(ns("downloadData"), "Download Data"),
      
      hr(),

      a("YouTube --  Household Products' Buffering Activity", target="_blank", href="https://www.youtube.com/watch?v=6dhOJ9mrWHw"),
      br(),
      HTML('<iframe width="560" height="315" src="https://www.youtube.com/embed/6dhOJ9mrWHw" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>')
    ),
    
    # add UI elements to send prompts to LLM API for Abstract generation
    getLLMPromptUIRow(ns)
  )
}

# Server code
exp12C <- function(input, output, session, pin) {
  # render the data table
  output$hot1 <- renderRHandsontable({
    if (!is.null(input$hot1)) {
      DF = hot_to_r(input$hot1)

      DF[[2]][4] = input$i1
      DF[[2]][5] = input$i2
      DF[[3]][4] = input$i3
      DF[[3]][5] = input$i4
      
      if(isAdminUser(pin)) {
        DF = getResultsExp12C(DF)
      }
    } else {
      DF = getTableDataExp12C()
    }
    
    rhandsontable(DF, stretchH = "all", readOnly = FALSE)
  })
  
  # Download plot 1 data
  output$downloadPlot1Data <- downloadHandler(
    filename = function() {
      paste0("Experiment12C_Plot1-", pin, ".csv")
    },
    content = function(file) {
      x_data = getPlotDataExp12C('L_NaOH_X')
      y_data = getPlotDataExp12C('L_pH_Y')

      df = data.frame(Volume_mL = x_data, pH = y_data)
      write.csv(df, file, row.names = FALSE)
    }
  )
  
  # download plot 2 data
  output$downloadPlot2Data <- downloadHandler(
    filename = function() {
      paste0("Experiment12C_Plot2-", pin, ".csv")
    },
    content = function(file) {
      x_data = getPlotDataExp12C('CA_NaOH_X')
      y_data = getPlotDataExp12C('CA_pH_Y')

      df = data.frame(Volume_mL = x_data, pH = y_data)
      write.csv(df, file, row.names = FALSE)
    }
  )
  
  # Download the data to users computer as csv
  output$downloadData <- downloadHandler(
    filename = function() {
      paste0("Experiment12C_", pin, ".csv")
    },
    content = function(file) {
      DF = hot_to_r(input$hot1)
      write.csv(DF, file, row.names = FALSE)
    }
  )
  
  # Display pH plot # 1 data for lemonade
  output$plot1 <- renderPlotly({
    x_data = getPlotDataExp12C('L_NaOH_X')
    y_data = getPlotDataExp12C('L_pH_Y')

    df = data.frame(x_data, y_data)

    fig = getLinePlot(df, " ", "Volume 0.1M NaOH (mL)", "pH", smooth = FALSE) %>%
      layout(showlegend = FALSE)
  })

  # Display the plot # 2 for citric acid
  output$plot2 <- renderPlotly({
    x_data = getPlotDataExp12C('CA_NaOH_X')
    y_data = getPlotDataExp12C('CA_pH_Y')

    df = data.frame(x_data, y_data)

    fig = getLinePlot(df, " ", "Volume 0.1M NaOH (mL)", "pH", smooth = FALSE) %>%
      layout(showlegend = FALSE)
  })
  
  # handle uploading of data for plot 1
  observeEvent(input$plot1Data, {
    req(input$plot1Data)
    
    df = read.csv(input$plot1Data$datapath)
    
    exp12CL[['L_NaOH_X']] <<- df[[1]]
    exp12CL[['L_pH_Y']] <<- df[[2]]
    
    # update plot 1 now
    output$plot1 <- renderPlotly({
      x_data = getPlotDataExp12C('L_NaOH_X')
      y_data = getPlotDataExp12C('L_pH_Y')
      
      df = data.frame(x_data, y_data)
      
      fig = getLinePlot(df, " ", "Volume 0.1M NaOH (mL)", "pH", smooth = FALSE) %>%
        layout(showlegend = FALSE)
    })
  })
  
  # handle uploading of data for plot 2
  observeEvent(input$plot2Data, {
    req(input$plot2Data)
    
    df = read.csv(input$plot2Data$datapath)
    
    exp12CL[['CA_NaOH_X']] <<- df[[1]]
    exp12CL[['CA_pH_Y']] <<- df[[2]]
    
    # update plot 2 now
    output$plot2 <- renderPlotly({
      x_data = getPlotDataExp12C('CA_NaOH_X')
      y_data = getPlotDataExp12C('CA_pH_Y')
      
      df = data.frame(x_data, y_data)
      
      fig = getLinePlot(df, " ", "Volume 0.1M NaOH (mL)", "pH", smooth = FALSE) %>%
        layout(showlegend = FALSE)
    })
  })
  
  # handle llm generate button selection
  observeEvent(input$llmGenerate, {
    # get the selected model and temperature
    model = input$llmModel
    temp = input$llmTemp
    
    # get the data from the table and covert to csv string
    DF = hot_to_r(input$hot1)
    csvString = paste(capture.output(write.csv(DF, row.names = FALSE)), collapse = "\n")
    
    # create the prompt now
    abstractPrompt = paste("Generate a 200-300 word scientific abstract about the BUFFERING CAPACITY OF LEMONADE for data below.",
                           "Only return the Abstract text.",
                           "Here is the csv data:\n", csvString)
    
    print(abstractPrompt)
    
    # display the abstract after call the LLM API
    displayAbstract(abstractPrompt, model, temp, pin)
  })
}

# function to return the column data
getPlotDataExp12C = function(columnName) {
  return(exp12CL[[columnName]])
}

# function to return random data set
getDataExp12C = function() {
  if (!exists("exp12CDF")) {
    df.name = load("data/Exp12.Rda")
    exp12CDF <<- get(df.name)
  }
}

# function to get the initial data
getTableDataExp12C = function() {
  getDataExp12C()
    
  columnNames = c('L_NaOH_X', 'L_pH_Y', 'CA_NaOH_X', 'CA_pH_Y')
    
  for(columnName in columnNames) {
    n_data = exp12CDF[[columnName]]
    n_data = n_data[!is.na(n_data)]
      
    # convert time to seconds
    if(grepl("X", columnName, fixed = TRUE)) {
      n_data = n_data + n_data*(runif(1, 0, 0.6)) # add randomness
    } else {
      n_data = n_data + runif(1, -0.25, 0.25) # add randomness
    }
  
    # store the data list in case we need to process or export
    # https://stackoverflow.com/questions/20133344/find-closest-value-in-a-vector-with-binary-search
    exp12CL[[columnName]] <<- n_data  
  }
  
  v1 = c("Concentration of NaOH (M)", "Initial volume of NaOH, mL", 
         "Final volume of NaOH, mL", 
         "NaOH volume added before the largest pH increase (mL)",
         "NaOH volume added after the largest pH increase (mL)",
         "Results", 
         "Volume of NaOH added at the equivalence point (mL)",
         "Moles NaOH (mol)")
  
  diffL = 100 - max(exp12CL$L_NaOH_X)
  diffL = formatC(diffL, format = "f", digits = 1)
  
  diffC = 100 - max(exp12CL$CA_NaOH_X)
  diffC = formatC(diffC, format = "f", digits = 1)
  
  v2 = c("0.10", "100", diffL, " ", " ", " ", " ", " ")
  v3 = c("0.10", "100", diffC, " ", " ", " ", " ", " ")
  
  df = data.frame(v1, v2, v3, stringsAsFactors = FALSE)
  colnames(df) <- c("Data",
                    "Lemonade",
                    "Citric Acid")
  
  return(df)
}

# calculate the results
getResultsExp12C = function(DF) {
  eq1A = DF[[2]][4]
  eq1B = DF[[2]][5]
  if(!is.na(eq1A) && !is.na(eq1B)) {
    eq1Avg = (as.numeric(eq1A) + as.numeric(eq1B))/2
    molesNaOH1 = 0.1*(eq1Avg/1000)
    
    DF[[2]][7] = eq1Avg
    DF[[2]][8] = molesNaOH1
  }
  
  eq2A = DF[[3]][4]
  eq2B = DF[[3]][5]
  if(!is.na(eq2A) && !is.na(eq2B)) {
    eq2Avg = (as.numeric(eq2A) + as.numeric(eq2B))/2
    molesNaOH2 = 0.1*(eq2Avg/1000)
    
    DF[[3]][7] = eq2Avg
    DF[[3]][8] = molesNaOH2
  }
  
  return(DF)
}