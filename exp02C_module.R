# Experiment 2C UI/Server module code
# Module used for developement and testing
#
# https://shiny.rstudio.com/articles/modules.html
# 

# The UI code
exp02CUI <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  # wrap ui components in tablist
  tagList(
    h3("Experiment 2 -- THE EFFECT OF CONCENTRATION ON THE CONDUCTIVITY OF DILUTE SOLUTIONS"),
    
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
      
      a("YouTube -- Electrolyte and None Electrolyte Solution", target="_blank", href="https://m.youtube.com/watch?v=jg2uJFa8EVo"),
      br(),
      HTML('<iframe width="560" height="315" src="https://www.youtube.com/embed/jg2uJFa8EVo" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>'),
      br(),
      a("YouTube -- Strong, Weak and Non Electrolyte Demonstration", target="_blank", href="https://m.youtube.com/watch?v=IPKvdD8btgs"),
      br(),
      HTML('<iframe width="560" height="315" src="https://www.youtube.com/embed/IPKvdD8btgs" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>')
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
exp02C <- function(input, output, session, pin) {
  # render the datatable for electrolytes
  output$hot1 <- renderRHandsontable({
    if (!is.null(input$hot1)) {
      DF = hot_to_r(input$hot1)
    } else {
      DF = getTableDataExp02C(pin)
    }
    
    # plot the data
    output$plot1 <- renderPlotly({
      s.data = list(x1 = DF[[1]], y1 = DF[[2]], x2 = DF[[1]], y2 = DF[[3]])
      fig = getSmoothPlot(s.data, "No. of Drops", "Relative Conductivity (uS)", n1 = "NaCl", n2="HC2H3O2")
    })
    
    if(isAdminUser(pin)) {
      fit.numbers = doLinearFit(DF[[1]], DF[[2]])
      
      resultsNaCl <<- paste0('NaCl Fit Results: Intercept ->', fit.numbers$intercept, 
                               ', Slope ->', fit.numbers$slope, ', R-squared ->', fit.numbers$rsquare)
      
      fit.numbers = doLinearFit(DF[[1]], DF[[3]])
      resultsAceticAcid <<- paste0('HC2H3O2 Fit Results: Intercept ->', fit.numbers$intercept, 
                               ', Slope ->', fit.numbers$slope, ', R-squared ->', fit.numbers$rsquare)
      
      
      output$vhot1 <- renderText({ paste0(resultsNaCl, '<br>', resultsAceticAcid) })
    }
    
    rhandsontable(DF, stretchH = "all", rowHeaders = FALSE)
  })
  
  # Download the data to users computer as csv
  output$downloadData <- downloadHandler(
    filename = function() {
      paste0("Experiment02C_", pin, ".csv")
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
                           "THE EFFECT OF CONCENTRATION ON THE CONDUCTIVITY OF DILUTE SOLUTIONS OF NACl and HC2H3O2",
                           "Make sure to do linear fit of data and indicate which solution shows a linear response to ",
                           "concentration vs conductivity. Show the R-squared values in text",
                           "Only return the Abstract text.",
                           "Here is the csv data:\n", csvString)
    
    print(abstractPrompt)
    
    # display the abstract after call the LLM API
    displayAbstract(abstractPrompt, model, temp)
  })
}

# function to get the initial data for table 1
getTableDataExp02C = function(pin) {
  v1 = c(0:8)
  v2 = c(0, 91, 168, 241, 350, 421, 499, 561, 643)
  v3 = c(0, 52, 79, 90, 103, 112, 128, 132, 140)
  
  v2 = addNoise(v2, 0.05, r.values=TRUE)
  v3 = addNoise(v3, 0.05, r.values=TRUE)
  
  if(!showSampleData(pin)) {
    v2 = c(0, 1, 2, 3, 4, 5, 6, 7, 8)
    v3 = c(0, 1, 2, 3, 4, 5, 6, 7, 8)   
  }
  
  df = data.frame(v1, v2, v3)
  
  # rename the column names here
  colnames(df) <- c("No. of Drops",
                    "NaCl Conductivity (uS)",
                    "HC2H3O2 Conductivity (uS)")
  return(df)
}