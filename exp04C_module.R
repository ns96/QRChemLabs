# Experiment 4C UI/Server module code
# Module used for development and testing
#
# https://shiny.rstudio.com/articles/modules.html
# 

# The UI code
exp04CUI <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  # wrap ui components in tablist
  tagList(
    h3("Experiment 4 -- HEAT OF FUSION OF ICE"),
    
    fluidRow(
      box(title = "Experiment Data", status = "primary", width = 12,
        rHandsontableOutput(ns("hot1")),
        span(htmlOutput(ns("vhot1")), style="color:blue"),
          
        br(),
          
        downloadButton(ns("downloadData1"), "Download Data")
      )
    ),
    
    fluidRow(
      box(title = "Results", status = "primary", width = 12,
          rHandsontableOutput(ns("hot2")),
          
          br(),
          
          downloadButton(ns("downloadData2"), "Download Data")
      )
    ),
    
    fluidRow(
      box(title = "Plot Data", status = "primary", width = 12,
        plotlyOutput(ns("plot1"))
      ),
      
      a("YouTube -- Enthalpy of fusion of ice experiment", target="_blank", href="https://youtu.be/5zyQfS5qfvM"),
      br(),
      HTML('<iframe width="560" height="315" src="https://www.youtube.com/embed/5zyQfS5qfvM" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>'),
      br(),
      a("YouTube -- Heat of Fusion of Water Lab", target="_blank", href="https://youtu.be/rTERi2Tc-Io"),
      br(),
      HTML('<iframe width="560" height="315" src="https://www.youtube.com/embed/rTERi2Tc-Io" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>'),
    ),
    
    # add UI elements to send prompts to LLM API for Abstract generation
    getLLMPromptUIRow(ns)
  )
}

# Server code
exp04C <- function(input, output, session, pin) {
  # single source of truth for the experiment table; seeded once per session
  values <- reactiveValues(data = getTableDataExp04C(pin))
  
  # keep the reactive data in sync with table edits so the derived values, the
  # results table and the plot always reflect what the student has entered
  observeEvent(input$hot1, {
    values$data <- hot_to_r(input$hot1)
  })
  
  # the four measured values (t1, t2, V1, V2) taken from the edited table
  measurements <- reactive({
    getMeasurementsExp04C(values$data)
  })
  
  # render the data table with the change in temperature and volume filled in
  output$hot1 <- renderRHandsontable({
    DF = values$data
    mv = measurements()
    
    # check to see if to do calculations
    if(isAdminUser(pin)) {
      deltaT = mv[2] - mv[1]
      deltaV = mv[4] - mv[3]
      
      DF[3,2] = ifelse(is.finite(deltaT), formatC(deltaT, format="f", digits = 1), '---')
      DF[6,2] = ifelse(is.finite(deltaV), formatC(deltaV, format="f", digits = 1), '---')
    } else {
      DF[3,2] = '---'
      DF[6,2] = '---'
    }
    
    rhandsontable(DF, stretchH = "all", rowHeaders = FALSE)
  })
  
  # render the second table, recalculated from the edited measurements
  output$hot2 <- renderRHandsontable({
    DF = getResultsExp04C(pin, measurements())
    
    rhandsontable(DF, stretchH = "all", rowHeaders = FALSE)
  })
  
  # plot the measured data, reacting to table edits
  output$plot1 <- renderPlotly({
    x = c('T1', 'T2', 'V1', 'V2')
    y = measurements()
    fig = getBarPlot(x, y, "Measured Data")
  })
  
  # Download the data to users computer as csv
  output$downloadData1 <- downloadHandler(
    filename = function() {
      paste0("Experiment04C_", pin, ".csv")
    },
    content = function(file) {
      DF = hot_to_r(input$hot1)
      write.csv(DF, file, row.names = FALSE)
    }
  )
  
  # Download the results data to the users computer as csv
  output$downloadData2 <- downloadHandler(
    filename = function() {
      paste0("ExperimentResults04C_", pin, ".csv")
    },
    content = function(file) {
      DF = hot_to_r(input$hot2)
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
    csvString1 = paste(capture.output(write.csv(DF, row.names = FALSE)), collapse = "\n")
    
    DF = hot_to_r(input$hot2)
    csvString2 = paste(capture.output(write.csv(DF, row.names = FALSE)), collapse = "\n")
    
    # create the prompt now
    abstractPrompt = paste("Generate a 200-300 word scientific abstract about ",
                           "calculating the HEAT OF FUSION OF ICE",
                           "Only return the Abstract text.",
                           "Here is the mesurements csv data:\n", csvString1, "\n", 
                           "Here is the results csv data:\n", csvString2)
    
    print(abstractPrompt)
    
    # display the abstract after call the LLM API
    displayAbstract(abstractPrompt, model, temp, pin)
  })
}

# function to get the initial data for table 1
getTableDataExp04C = function(pin) {
  v1 = c('Initial water temperature, t1 (degC)', 'Final water temperature, t2 (degC)',
         'Change in water temperature (t2 - t1)', 
         'Initial water volume, V1 (mL)', 'Final water volume, V2 (mL)',
         'Change in water volume (V2 - V1)', 'Density of Water (g/mL)')
  
  exp04CV = addNoise(c(53.4, 5.17, 100.0, 156.5), 0.08, r.values = TRUE, r.digits = 1)
  
  if(!showSampleData(pin)) {
    exp04CV = addNoise(c(0.0, 0.0, 0.0, 0.0), 0.08, r.values = TRUE, r.digits = 1)
  }
  
  v2 = c(exp04CV[1], exp04CV[2], '---', exp04CV[3], exp04CV[4], '---', '1.00')
  
  df = data.frame(v1, v2, stringsAsFactors = FALSE)
  
  # rename the column names here
  colnames(df) <- c("Measurements", "Values")
  return(df)
}

# function to pull the four measured values (t1, t2, V1, V2) out of table 1
getMeasurementsExp04C = function(df) {
  # the numeric measurements sit in the second column on rows 1, 2, 4 and 5
  v = suppressWarnings(as.numeric(df[c(1, 2, 4, 5), 2]))
  return(v)
}

# function to calculate the results from the measured values
getResultsExp04C = function(pin, measured) {
  v1 = c('Mass of ice melted (g)', 
         'Heat released by cooling water', 
         'Heat of fusion of ice (J/g)', 
         'Calculated Molar heat of fusion (kJ/mol)',
         'Actual Molar heat of fusion (kJ/mol)',
         'Percent error')
  
  v2 = rep('---', 6)
  v2[5] = '6.02'
  
  # display the results in admin or instructor, and only for a usable set of
  # measurements (all four entered, and a non-zero change in volume)
  if(isAdminUser(pin) && all(is.finite(measured)) && measured[4] != measured[3]) {
    deltaT = measured[2] - measured[1]
    deltaV = measured[4] - measured[3]
    
    v2[1] = formatC(deltaV, format="f", digits = 1)
    
    heat.water = measured[3]*4.18*deltaT
    v2[2] = formatC(heat.water, format="f", digits = 1)
    
    heat.ice = (-1*heat.water/deltaV)
    v2[3] = formatC(heat.ice, format="f", digits = 1)
    
    molar.heat = (heat.ice*18.02)/1000.0
    v2[4] = formatC(molar.heat, format="f", digits = 2)
    
    molar.heat.error = (abs(6.02 - molar.heat)/6.02)*100
    v2[6] = formatC(molar.heat.error, format="f", digits = 1)
  }
  
  df = data.frame(v1, v2, stringsAsFactors = FALSE)
  
  # rename the column names here
  colnames(df) <- c("Calculated Results", "Values")
  return(df)
}
