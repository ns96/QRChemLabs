# Experiment 6C UI/Server module code
# Module used for developement and testing
#
# https://shiny.rstudio.com/articles/modules.html
# 

# The UI code
exp06CUI <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  # wrap ui components in tablist
  tagList(
    h3("Experiment 6 -- Rate Law Determination of the Crystal Violet Reaction"),
    
    fluidRow(
      box(width = 4, title = "Colorimeter Data", status = "primary",
          # add input for setting the temperature
          textInput(ns("temp"), "Temperature (C):", value = 23.5),
          
          # the data version
          span(textOutput(ns("vhot1")), style="color:blue"),
          
          # the data table
          rHandsontableOutput(ns("hot")),
          
          #span(textOutput(ns("vhot")), style="color:blue"),    
          br(),
          
          downloadButton(ns("downloadData"), "Download Data"),
      ),
      
      box(width = 8, title = "Absorbance Plot", status = "primary",
          plotlyOutput(ns("plot1")),
          
          hr(),
          
          selectInput(ns("si1"), "Reaction Order", c("Zero", "First", "Second")),
          plotlyOutput(ns("plot2")),
          span(textOutput(ns("v1")), style="color:blue")
      )
    ),
    
    fluidRow(
      a("YouTube/CityTech -- Lab Walkthrough", target="_blank", href="https://www.youtube.com/playlist?list=PL6tbvx3WF3cAijQ67LSfrkLkV3H2qR17c"),
      br(),
      HTML('<iframe width="560" height="315" src="https://www.youtube.com/embed/videoseries?list=PL6tbvx3WF3cAijQ67LSfrkLkV3H2qR17c" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>'),
      br(),
      a("YouTube -- Colorimeter Instrument", target="_blank", href="https://www.youtube.com/watch?v=YoAlHKWwn_I"),
      br(),
      HTML('<iframe width="560" height="315" src="https://www.youtube.com/embed/YoAlHKWwn_I" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>'),
      br(),
      a("YouTube -- Colorimeter Instrument", target="_blank", href="https://www.youtube.com/watch?v=xs8T_dKsPjg"),
      br(),
      br(),
      HTML('<iframe width="560" height="315" src="https://www.youtube.com/embed/xs8T_dKsPjg" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>'),
      br(),
      a("YouTube -- Using Excel for Analysis", target="_blank", href="https://www.youtube.com/watch?v=da-G2BxGEVE"),
      br(),
      HTML('<iframe width="560" height="315" src="https://www.youtube.com/embed/da-G2BxGEVE" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>')
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
exp06C <- function(input, output, session, pin) {
  # render the datatable
  output$hot <- renderRHandsontable({
    if (!is.null(input$hot)) {
      exp06CDF <<- hot_to_r(input$hot)
    } else {
      getTableDataExp06C(pin)
    }
    
    output$vhot1 <- renderText({paste("Data Version:", dataNumberExp06C)})
      
    rhandsontable(exp06CDF, stretchH = "all")
  })
  
  # Display the simulated plot data
  output$plot1 <- renderPlotly({
    if (is.null(exp06CDF)) {
      getTableDataExp06C()    
    }

    x_data = exp06CDF[['Time.Minutes']]
    y_data = exp06CDF[['Absorbance.Value']]
    df = data.frame(x_data,y_data)
    
    fig = getLinePlot(df, "", "Time (minutes)", "Absorbance")
  })
  
  # Display the simulated plot data
  output$plot2 <- renderPlotly({
    if (is.null(exp06CDF)) {
      getTableDataExp06C()    
    }
    
    reactionOrder = input$si1
    cat('Reaction Order:', reactionOrder, "\n")
    
    x_data = exp06CDF[['Time.Minutes']]*60

    if(reactionOrder == 'Zero') {
      y_data = exp06CDF[['Absorbance.Value']]
      plotTitle = "Zero Order || [Cv+] vs time"
      yaxisTitle = "Asorbance"
    } else if(reactionOrder == 'First') {
      y_data = log(exp06CDF[['Absorbance.Value']])
      plotTitle = "First Order || ln([Cv+]) vs time"
      yaxisTitle = "ln(Asorbance)"
    } else {
      y_data = 1/exp06CDF[['Absorbance.Value']]
      plotTitle = "Second Order || 1/[Cv+] vs time"
      yaxisTitle = "1/Asorbance"
    }
    
    # display the results in admin or instructor
    if(isAdminUser(pin)) {
      fit.numbers = doLinearFit(x_data, y_data)
      rate.constant = -1*fit.numbers$slope
      
      if(reactionOrder == 'First') {
        half.life = 0.693/rate.constant
      } else {
        half.life = 0
      }
      
      # store the results for sending to llm for abstract generation
      resultsExp06CDF <<- data.frame(
        "Temperature (C)" = input$temp,
        "Reaction Order" = reactionOrder,
        "Slope" = fit.numbers$slope,
        "R^2" = fit.numbers$rsquare,
        "Rate Constant (M/s)" = rate.constant,
        "Half Life (s)" = half.life
      )
      
      output$v1 <- renderText({ 
        paste("Slope:", formatC(fit.numbers$slope, format="f", digits = 6),
              " || R^2:", formatC(fit.numbers$rsquare, format = "f", digits = 3),
              " || k(M/s):", formatC(rate.constant, format="f", digits = 6), 
              " || Half Life (s):", formatC(half.life, format = "d"))
      })
    } else {
      # clear the output, mainly for debugging
      output$v1 <- renderText({""})
    }
    
    df = data.frame(x_data, y_data)
    getLinePlot(df, plotTitle, "Time (seconds)", yaxisTitle)
  })
  
  # Download the data to users computer as csv
  output$downloadData <- downloadHandler(
    filename = function() {
      paste0("Experiment06_", pin, ".csv")
    },
    content = function(file) {
      DF = hot_to_r(input$hot)
      write.csv(DF, file, row.names = FALSE)
    }
  )
  
  # returns the data for a particular experiment
  savedDataTable <- reactive({
    sdDF <<- loadFromDB(pin, "EXP06C")
  })
  
  # handle viewing of old data
  observeEvent(input$view, {
    showModal(modalDialog(
      title = "Experiment 06C Saved Data",
      #HTML(getSavedData(pin, "EXP00"))
      renderDataTable(options = list(searching = FALSE, paging = FALSE,
        scrollX = "10px"), {
        getSavedData(pin, "EXP06C")
      })
    ))
    
    cat("View Data -- EXP06C", "\n")
  })
  
  # handle llm generate button selection
  observeEvent(input$llmGenerate, {
    # get the selected model and temperature
    model = input$llmModel
    temp = input$llmTemp
    
    # get the data from the table and covert to csv string
    DF = resultsExp06CDF
    csvString = paste(capture.output(write.csv(DF, row.names = FALSE)), collapse = "\n")
    
    # create the prompt now
    abstractPrompt = paste("Generate a 200-300 word scientific abstract about the ",
                           "Rate Law Determination of the Crystal Violet Reaction With NaOH using Colorimeter Data.",
                           "Only return the Abstract text.",
                           "Here is the csv data:\n", csvString)
    
    print(abstractPrompt)
    
    # display the abstract after call the LLM API
    displayAbstract(abstractPrompt, model, temp)
  })
}

# function to return random data set
getDataExp06C = function() {
  dataNumberExp06C <<- sample(1:3, 1)
  
  expData = list()
  
  if(dataNumberExp06C == 1) {
    # Data -- Nathan
    expData$actual.Time = c(0, 1.53, 2.67, 3.76, 5.07, 6.54, 8.02, 9.17, 10.28, 11.76, 12.47, 13.61, 14.77, 16.0, 17.41, 18.57, 19.61, 20.62, 21.74)
    expData$actual.Absorbance = c(0.334, 0.286, 0.258, 0.227, 0.208, 0.183, 0.157, 0.147, 0.133, 0.118, 0.113, 0.109, 0.094, 0.088, 0.078, 0.074, 0.070, 0.064, 0.062)
  } else if(dataNumberExp06C == 2) {
    # Data -- Lois
    expData$actual.Time = c(0.105, 0.450, 0.960, 1.390, 1.835, 2.252, 2.710, 3.163, 3.612, 4.062, 4.512, 4.988, 5.433, 5.957, 6.283, 6.785, 7.210, 7.660, 8.110, 8.542, 9.010, 9.458, 9.912, 10.365, 10.807, 11.265, 11.708, 12.158,	12.613,	13.055,	13.512,	13.962, 14.410, 14.972, 15.312, 15.753, 16.205, 16.653, 17.107, 17.560, 18.003, 18.448, 18.942, 19.448, 20.258, 20.578)
    expData$actual.Absorbance = c(0.2955, 0.2825, 0.2635, 0.2496, 0.2358, 0.2235, 0.2110, 0.1991, 0.1882, 0.1780, 0.1685, 0.1588, 0.1506, 0.1419, 0.1377, 0.1303, 0.1232, 0.1164, 0.1101, 0.1045, 0.0986, 0.0933, 0.0888, 0.0838, 0.0796, 0.0756, 0.0715, 0.0679, 0.0646, 0.0614, 0.0583, 0.0554, 0.0526, 0.0496, 0.0474, 0.0452, 0.0432, 0.0410, 0.0390, 0.0370, 0.0350, 0.0332, 0.0318, 0.0301, 0.0272, 0.0263)
  } else {
    # Data -- Abiola
    expData$actual.Time = c(0.451666667,1.671666667,2.426666667,3.073333333,3.786666667,4.778333333,5.791666667,6.805,7.815,8.828333333,9.88,10.88,11.89,12.88,13.85,14.86666667,15.86833333,16.90833333,17.87166667,18.89333333,19.805,19.82666667)
    expData$actual.Absorbance = c(0.232753434,0.208924202,0.192115937,0.177569522,0.171050139,0.146585407,0.140721193,0.129875708,0.107956287,0.098863487,0.09612629,0.084621514,0.07451365,0.070653528,0.063374154,0.057110026,0.054633751,0.046581871,0.04364105,0.041063256,0.037994097,0.038159444)
  }
    
  return(expData)
}

# function to get the initial data
getTableDataExp06C = function(pin) {
  #Time.Minutes = seq(0, 20, by=0.5)
  #Absorbance.Value = runif(length(Time.Minutes), 0.15, 0.34)
  
  expData = getDataExp06C()  
  Time.Minutes = addNoise(expData$actual.Time, 0.01)
  Absorbance.Value = addNoise(expData$actual.Absorbance, 0.02)
  
  if(!showSampleData(pin)) {
    Time.Minutes = seq(0, 20, by=0.5)
    Absorbance.Value = runif(length(Time.Minutes), 0.15, 0.34)  
  }
  
  exp06CDF <<- data.frame(Time.Minutes, Absorbance.Value)
  exp06CDF2 <<- data.frame(expData$actual.Time, expData$actual.Absorbance) # used for debugging
}