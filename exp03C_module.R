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
  # render the data table for electrolytes
  output$hot1 <- renderRHandsontable({
    if (!is.null(input$hot1)) {
      DF = hot_to_r(input$hot1)
    } else {
      DF = getTableDataExp03C(pin)
    }
    
    # plot the data
    output$plot1 <- renderPlotly({
      x = c(0, 0.08, 0.16, 0.24, 0.32, 0.40) 
      y = knownExp03C
      
      if(isAdminUser(pin)) {
        fit.numbers = doLinearFit(x, y)
        unknown.Conc = (unknownExp03C + fit.numbers$intercept) / fit.numbers$slope 
        
        resultsExp03C <<- paste0('Fit Results: Intercept ->', fit.numbers$intercept, 
                         ', Slope ->', fit.numbers$slope, ', R-squared ->', fit.numbers$rsquare,
                         ', Unknown Absorbance ->', unknownExp03C,
                         ', Unknown Concentration ->', unknown.Conc)
        
        output$vhot1 <- renderText({ resultsExp03C })
      }
      
      p.df = data.frame(x, y)
      getRegressionPlot(p.df, "Standard Plot", "NiSO4 Concentration (mol/L)", "Absorbance")
    })
    
    rhandsontable(DF, stretchH = "all", rowHeaders = FALSE)
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