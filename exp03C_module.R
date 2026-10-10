# Experiment 3C UI/Server module code
# Module used for development and testing
#
# https://shiny.rstudio.com/articles/modules.html
# 

# The UI code
exp03CUI <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  # wrap ui components in taglist
  tagList(
    h3("Experiment 3 -- DETERMINING THE CONCENTRATION OF A SOLUTION USING BEER'S LAW"),
    
    fluidRow(
      box(title = "Experiment Data", status = "primary", width = 12,
        rHandsontableOutput(ns("hot1")),
        span(htmlOutput(ns("vhot1")), style="color:blue"),
          
        br(),
          
        downloadButton(ns("downloadData"), "Download Data")
      )
    ),
    
    fluidRow(
      box(title = "Plot Data", status = "primary", width = 12,
        plotlyOutput(ns("plot1"))
      ),
      
      a("YouTube -- Determining the Concentration of a Solution", target="_blank", href="https://youtu.be/s47xMoF9_xE"),
      br(),
      HTML('<iframe width="560" height="315" src="https://www.youtube.com/embed/s47xMoF9_xE" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>'),
      br(),
      a("YouTube -- Calibration Curve With Nickel (II) Sulfate", target="_blank", href="https://youtu.be/V6eDXnISx5s"),
      br(),
      HTML('<iframe width="560" height="315" src="https://www.youtube.com/embed/V6eDXnISx5s" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>'),
    ), 

    # add UI elements to send prompts to LLM API for Abstract generation
    getLLMPromptUIRow(ns)
  )
}

# Server code
exp03C <- function(input, output, session, pin) {
  # single source of truth for the experiment table; seeded once per session
  values <- reactiveValues(data = getTableDataExp03C(pin))
  
  # keep the reactive data in sync with table edits so the plot and the
  # calculated results always reflect what the student has entered
  observeEvent(input$hot1, {
    values$data <- hot_to_r(input$hot1)
  })
  
  # render the data table
  output$hot1 <- renderRHandsontable({
    rhandsontable(values$data, stretchH = "all", rowHeaders = FALSE)
  })
  
  # plot the standard calibration data, reacting to table edits
  output$plot1 <- renderPlotly({
    plot.df = getStandardDataExp03C(values$data)
    
    # not enough valid standard data yet (e.g. cells cleared) -- show an empty plot
    if(nrow(plot.df) < 2 || length(unique(plot.df$x)) < 2) {
      return(plotly_empty() %>% layout(title = "Standard Plot",
                                       xaxis = list(title = "NiSO4 Concentration (mol/L)"),
                                       yaxis = list(title = "Absorbance")))
    }
    
    getRegressionPlot(plot.df, "Standard Plot", "NiSO4 Concentration (mol/L)", "Absorbance")
  })
  
  # display the calculated fit results to admin users, reacting to table edits
  output$vhot1 <- renderText({
    if(!isAdminUser(pin)) {
      return(NULL)
    }
    
    getFitResultsExp03C(values$data)
  })
  
  # Download the data to users computer as csv
  output$downloadData <- downloadHandler(
    filename = function() {
      paste0("Experiment03C_", pin, ".csv")
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
                           "DETERMINING THE CONCENTRATION OF AN UNKNOWN Nickel (II) Sulfate SOLUTION USING BEER'S LAW",
                           "Make sure to do linear fit of data and use the resulting equation to calculate concentration of unknown.",
                           "Only return the Abstract text.",
                           "Here is the csv data:\n", csvString)
    
    print(abstractPrompt)
    
    # display the abstract after call the LLM API
    displayAbstract(abstractPrompt, model, temp, pin)
  })
}

# pull the standard (non-unknown) rows from the experiment table as numeric
# concentration/absorbance pairs, dropping rows with blank or invalid entries
getStandardDataExp03C = function(df) {
  x = suppressWarnings(as.numeric(df[[4]]))   # Concentration NiSO4 (mol/L)
  y = suppressWarnings(as.numeric(df[[5]]))   # ABSORBANCE
  
  # the unknown row holds no standard concentration, so leave it out of the fit
  standards = is.finite(x) & is.finite(y) & df[[1]] != "Unknown"
  
  return(data.frame(x = x[standards], y = y[standards]))
}

# calculate the fit results and unknown concentration from the table values
getFitResultsExp03C = function(df) {
  plot.df = getStandardDataExp03C(df)
  
  if(nrow(plot.df) < 2 || length(unique(plot.df$x)) < 2) {
    return("Fit Results: enter at least two standard concentrations and absorbances ...")
  }
  
  fit.numbers = doLinearFit(plot.df$x, plot.df$y)
  
  # absorbance of the unknown row (its concentration cell is blank)
  unknown.row = which(df[[1]] == "Unknown")
  if(length(unknown.row) < 1) {
    unknown.row = nrow(df)
  }
  
  unknown.Abs = suppressWarnings(as.numeric(df[[5]][unknown.row[1]]))
  
  results = paste0('Fit Results: Intercept -> ',
                   formatC(fit.numbers$intercept, format="f", digits = 6),
                   ', Slope -> ', formatC(fit.numbers$slope, format="f", digits = 6),
                   ', R-squared -> ', formatC(fit.numbers$rsquare, format = "f", digits = 3))
  
  # the unknown absorbance may not have been entered yet
  if(!is.finite(unknown.Abs)) {
    return(paste0(results, ', Unknown Absorbance -> not entered'))
  }
  
  unknown.Conc = (unknown.Abs + fit.numbers$intercept) / fit.numbers$slope
  
  return(paste0(results,
                ', Unknown Absorbance -> ', formatC(unknown.Abs, format = "f", digits = 3),
                ', Unknown Concentration -> ', formatC(unknown.Conc, format = "f", digits = 3)))
}

# function to get the initial data for table 1
getTableDataExp03C = function(pin) {
  v1 = c("Blank", "1", "2", "3", "4", "5", "Unknown")
  v2 = c("0.00", "2.00", "4.00", "6.00", "8.00", "10.00", "-----")
  v3 = c("10.00", "8.00", "6.00", "4.00", "2.00", "0.00", "-----")
  v4 = c("0", "0.08", "0.16", "0.24", "0.32", "0.40", "-----")
    
  unknownExp03C <<- addNoise(c(0.411), 0.25)
  knownExp03C <<- addNoise(c(0.00, 0.119, 0.241, 0.356, 0.474, 0.601), 0.05)
  
  if(!showSampleData(pin)) {
    unknownExp03C <<- addNoise(c(0.0), 0.25)
    knownExp03C <<- addNoise(c(0.00, 0.00, 0.00, 0.00, 0.00, 0.00), 0.05)    
  }
  
  v5 = c(knownExp03C, unknownExp03C)
  
  df = data.frame(v1, v2, v3, v4, v5, stringsAsFactors = FALSE)
  
  # rename the column names here
  colnames(df) <- c("Sample",
                    "Vol. of 0.40 M NiSO4 (mL)",
                    "Vol. of water (mL)",
                    "Concentration NiSO4 (mol/L)",
                    "ABSORBANCE")
  return(df)
}
