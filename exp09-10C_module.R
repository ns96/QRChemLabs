# Experiment 9 & 10C UI/Server module code
# Module used for developement and testing
#
# https://shiny.rstudio.com/articles/modules.html
# 

exp0910C.MaxTime = 250
exp0910CL = list()

# The UI code
exp0910CUI <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  # wrap ui components in tablist
  tagList(
    h3("Experiment 9 & 10 -- TITRATION CURVES OF STRONG AND WEAK ACIDS AND BASES"),
    
    fluidRow(
      box(width = 6, title = "Titration With Strong Base (NaOH)", status = "primary",
        plotlyOutput(ns("plot1")),
        downloadButton(ns("downloadPlot1Data"), "Download Plot Data"),
        
        hr(),
        
        plotlyOutput(ns("plot2")),
        downloadButton(ns("downloadPlot2Data"), "Download Plot Data")
      ),
      
      box(width = 6, title = "Titration With Weak Base (NH4OH)", status = "primary",
        plotlyOutput(ns("plot3")),
        downloadButton(ns("downloadPlot3Data"), "Download Plot Data"),
        
        hr(),
        
        plotlyOutput(ns("plot4")),
        downloadButton(ns("downloadPlot4Data"), "Download Plot Data")
      )
    ),
    
    fluidRow(
      # the data table
      rHandsontableOutput(ns("hot1")),
      
      downloadButton(ns("downloadData"), "Download Data"),

      hr(),

      a("YouTube -- Acid-Base Titration Curves", target="_blank", href="https://www.youtube.com/watch?v=ApRYZQnuVoU"),
      br(),
      HTML('<iframe width="560" height="315" src="https://www.youtube.com/embed/ApRYZQnuVoU" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>'),
      br(),
      a("YouTube -- Comparing Strong and Weak Acid Titration Curves", target="_blank", href="https://www.youtube.com/watch?v=xRVTdDrrtLc"),
      br(),
      HTML('<iframe width="560" height="315" src="https://www.youtube.com/embed/xRVTdDrrtLc" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>')
    ),
    
    fluidRow(
      img(src='images/phenolphthalein.png')  
    ),
    
    # add row to send prompts to google gemini or other LLM API
    fluidRow(
      # add drop down for selecting the llm model
      column(4, selectInput(ns("llmModel"), "Select LLM Model", choices = c("Google Gemini", "ChatGPT", "DeepSeek"))),
      
      # add slider input for selecting temperature
      column(4, sliderInput(ns("llmTemp"), "Temperature", min = 0, max = 1, value = 0.7)),
      
      column(4, actionButton(ns("llmGenerate"), "Generate Abstract"))
    )
  )
}

# Server code
exp0910C <- function(input, output, session, pin) {
  values <- reactiveValues(data = getTableDataExp0910C(pin))
  
  observe({
    if(!is.null(input$hot1)) {
      values$data <- hot_to_r(input$hot1)
    }
  })
  
  # render the data table
  output$hot1 <- renderRHandsontable({
    rhandsontable(values$data, stretchH = "all", readOnly = FALSE)
  })
  
  # Display the pH data plot and add line at pH nine
  # https://stackoverflow.com/questions/34093169/horizontal-vertical-line-in-plotly
  output$plot1 <- renderPlotly({
    if(!exists("exp0910CPlot1DF")) {
      x_data = getPlotDataExp0910C('SA_SBX')
      y_data = getPlotDataExp0910C('SA_SBY')
      
      exp0910CPlot1DF <<- data.frame(x_data, y_data)
    } else {
      x_data = exp0910CPlot1DF[[1]]
      y_data = exp0910CPlot1DF[[2]]
    }
    
    x_min = min(x_data)
    y_min = min(y_data) - 0.5
    
    df = data.frame(x_data, y_data)
    
    # get time at equivalence point
    if(is.numeric(values$data[1,3])) {
      eq_time = values$data[1,3]
    } else {
      eq_time = 250
    }
    
    fig = getLinePlot(df, "HCl", "Time (seconds)", "pH", smooth = FALSE) %>%
      add_segments(x = x_min, xend = exp0910C.MaxTime, y = 9, yend = 9) %>%
      add_segments(x = eq_time, xend = eq_time, y = y_min, yend = 12) %>%
      layout(showlegend = FALSE)
  })
  
  # Display the plot # 2
  output$plot2 <- renderPlotly({
    if(!exists("exp0910CPlot2DF")) {
      x_data = getPlotDataExp0910C('WA_SBX')
      y_data = getPlotDataExp0910C('WA_SBY')
      
      exp0910CPlot2DF <<- data.frame(x_data, y_data)
    } else {
      x_data = exp0910CPlot2DF[[1]]
      y_data = exp0910CPlot2DF[[2]]
    }
    
    x_min = min(x_data)
    y_min = min(y_data) - 0.5
        
    df = data.frame(x_data, y_data)
    
    # get time at equivalence point
    if(is.numeric(values$data[2,3])) {
      eq_time = values$data[2,3]
    } else {
      eq_time = 250
    }
    
    fig = getLinePlot(df, "Acetic Acid", "Time (seconds)", "pH", smooth = FALSE) %>%
      add_segments(x = x_min, xend = exp0910C.MaxTime, y = 9, yend = 9) %>%
      add_segments(x = eq_time, xend = eq_time, y = y_min, yend = 12) %>%
      layout(showlegend = FALSE)
  })
  
  # Display the plot # 3
  output$plot3 <- renderPlotly({
    if(!exists("exp0910CPlot3DF")) {
      x_data = getPlotDataExp0910C('SA_WBX')
      y_data = getPlotDataExp0910C('SA_WBY')
      
      exp0910CPlot3DF <<- data.frame(x_data, y_data)
    } else {
      x_data = exp0910CPlot3DF[[1]]
      y_data = exp0910CPlot3DF[[2]]
    }
      
    x_min = min(x_data)
    y_min = min(y_data) - 0.5
    
    df = data.frame(x_data, y_data)
    
    # get time at equivalence point
    if(is.numeric(values$data[3,3])) {
      eq_time = values$data[3,3]
    } else {
      eq_time = 250
    }
    
    fig = getLinePlot(df, "HCl", "Time (seconds)", "pH", smooth = FALSE) %>%
      add_segments(x = x_min, xend = exp0910C.MaxTime, y = 9, yend = 9) %>%
      add_segments(x = eq_time, xend = eq_time, y = y_min, yend = 12) %>%
      layout(showlegend = FALSE)
  })
  
  # Display the plot # 4
  output$plot4 <- renderPlotly({
    if(!exists("exp0910CPlot4DF")) {
      x_data = getPlotDataExp0910C('WA_WBX')
      y_data = getPlotDataExp0910C('WA_WBY')
      
      exp0910CPlot4DF <<- data.frame(x_data, y_data)
    } else {
      x_data = exp0910CPlot4DF[[1]]
      y_data = exp0910CPlot4DF[[2]]
    }
      
    x_min = min(x_data)
    y_min = min(y_data) - 0.5
    
    df = data.frame(x_data, y_data)
    
    # get time at equivalence point
    if(is.numeric(values$data[4,3])) {
      eq_time = values$data[4,3]
    } else {
      eq_time = 250
    }
    
    fig = getLinePlot(df, "Acetic Acid", "Time (seconds)", "pH", smooth = FALSE) %>%
      add_segments(x = x_min, xend = exp0910C.MaxTime, y = 9, yend = 9) %>%
      add_segments(x = eq_time, xend = eq_time, y = y_min, yend = 12) %>%
      layout(showlegend = FALSE)
  })
  
  # Download the data for plot 1 to users computer as csv
  output$downloadPlot1Data <- downloadHandler(
    filename = function() {
      paste0("Experiment0910C_Plot1-", pin, ".csv")
    },
    
    content = function(file) {
      write.csv(exp0910CPlot1DF, file, row.names = FALSE)
    }
  )
  
  # Download the data for plot 2 to users computer as csv
  output$downloadPlot2Data <- downloadHandler(
    filename = function() {
      paste0("Experiment0910C_Plot2-", pin, ".csv")
    },
    
    content = function(file) {
      write.csv(exp0910CPlot2DF, file, row.names = FALSE)
    }
  )
  
  # Download the data for plot 3 to users computer as csv
  output$downloadPlot3Data <- downloadHandler(
    filename = function() {
      paste0("Experiment0910C_Plot3-", pin, ".csv")
    },
    
    content = function(file) {
      write.csv(exp0910CPlot3DF, file, row.names = FALSE)
    }
  )
  
  # Download the data for plot 4 to users computer as csv
  output$downloadPlot4Data <- downloadHandler(
    filename = function() {
      paste0("Experiment0910C_Plot4-", pin, ".csv")
    },
    
    content = function(file) {
      write.csv(exp0910CPlot4DF, file, row.names = FALSE)
    }
  )
  
  # Download the data to users computer as csv
  output$downloadData <- downloadHandler(
    filename = function() {
      paste0("Experiment0910C_", pin, ".csv")
    },

    content = function(file) {
      DF = hot_to_r(input$hot1)
      write.csv(DF, file, row.names = FALSE)
    }
  )
  
  # handle llm generate button selection
  observeEvent(input$llmGenerate, {
    # get the selected model and temperature
    model = input$llmModel
    temp = input$llmTemp
    
    # get the data from the table and covert to csv string
    DF = hot_to_r(input$hot1)
    csvString = paste(capture.output(write.csv(DF, row.names = FALSE)), collapse = "\n")
    
    # create the prompt now
    abstractPrompt = paste("Generate a 200-300 word scientific abstract about ",
                           "TITRATION CURVES OF STRONG AND WEAK ACIDS AND BASES",
                           "Only return the Abstract text.",
                           "Here is the results csv data:\n", csvString)
    
    print(abstractPrompt)
    
    # display the abstract after call the LLM API
    displayAbstract(abstractPrompt, model, temp)
  })
}

# function to return the column data
getPlotDataExp0910C = function(columnName) {
  if (!exists("exp0910CDF")) {
    getDataExp0910C()    
  }
  
  n_data = exp0910CDF[[columnName]]
  n_data = n_data[!is.na(n_data)]
  
  # convert time to seconds
  if(grepl("X", columnName, fixed = TRUE)) {
    n_data = n_data + sample(0:25, 1) # Time Data
    n_data = (n_data/max(n_data))*exp0910C.MaxTime
  } else {
    n_data = n_data + runif(1, 0, 0.5) # pH Data
  }
  
  # store the data list in case we need to export
  # https://stackoverflow.com/questions/20133344/find-closest-value-in-a-vector-with-binary-search
  exp0910CL[[columnName]] <<- n_data
  
  return(n_data)
}

# function to return random data set
getDataExp0910C = function() {
  df.name = load("data/Exp09_10.Rda")
  exp0910CDF <<- get(df.name)
}

# function to get the initial data
getTableDataExp0910C = function(pin) {
  if (!exists("exp0910CDF")) {
    getDataExp0910C()    
  }
  
  v1 = c("HCl + NaOH", "HC2H3O2 + NaOH", "HCl + NH4OH", "HC2H3O2 + NH4OH")
  v2 = c(" ", " ", " ", " ")
  v3 = c(" ", " ", " ", " ")
  v4 = c(" ", " ", " ", " ")
  v5 = c(" ", " ", " ", " ")
  v6 = c(" ", " ", " ", " ")
  
  # add some data to table if admin
  if(isAdminUser(pin)) {
    v2 = c(181, 184, 198, 197)
    v3 = c(180, 184, 186, 185)
    v4 = c(7.0, 8.0, 6.5, 7.4)
    v5 = c(2.4, 3.2, 2.5, 3.3)
    v6 = c(12.0, 12.0, 10.4, 10.2)
  }
  
  df = data.frame(v1, v2, v3, v4, v5, v6, stringsAsFactors = FALSE)
  
  colnames(df) <- c("Acid-Base Combination", 
                    "Time of Indicator Color Change (sec)",
                    "Time at Equivalence Point (sec)",
                    "pH at the Equivalence Point",
                    "Initial pH", "Final pH")
  
  return(df)
}