# Experiment 1C UI/Server module code
# Module used for developement and testing
#
# https://shiny.rstudio.com/articles/modules.html
# 

# The UI code
exp01CUI <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  # wrap ui components in tablist
  tagList(
    h3("Experiment 1 -- ELECTROLYTES AND NON-ELECTROLYTES"),
    
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
      
      a("YouTube -- Conductivity Probe", target="_blank", href="https://www.youtube.com/watch?v=-K-CJoxLf7s"),
      br(),
      HTML('<iframe width="560" height="315" src="https://www.youtube.com/embed/-K-CJoxLf7s" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>'),
      br(),
      a("YouTube -- Conductivity Probe", target="_blank", href="https://youtu.be/H3gLnwbn1bg"),
      br(),
      HTML('<iframe width="560" height="315" src="https://www.youtube.com/embed/H3gLnwbn1bg" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>'),
      br(),
      a("YouTube -- Electrolyte and nonelectrolyte solutions", target="_blank", href="https://www.youtube.com/watch?v=jg2uJFa8EVo"),
      br(),
      HTML('<iframe width="560" height="315" src="https://www.youtube.com/embed/jg2uJFa8EVo" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>')
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
exp01C <- function(input, output, session, pin) {
  # render the data table for electrolytes
  output$hot1 <- renderRHandsontable({
    if (!is.null(input$hot1)) {
      DF = hot_to_r(input$hot1)
    } else {
      DF = getTableDataExp01C(pin)
    }
    
    # plot the data
    output$plot1 <- renderPlotly({
      x = paste(DF[[2]], "-" ,DF[[3]])
      y = DF[[4]]
      fig = getBarPlot(x, y, "Relative Conductivity (uS/cm)")
    })
    
    rhandsontable(DF, stretchH = "all", rowHeaders = FALSE)
  })
  
  # Download the data to users computer as csv
  output$downloadData <- downloadHandler(
    filename = function() {
      paste0("Experiment01C_", pin, ".csv")
    },
    content = function(file) {
      DF = hot_to_r(input$hot1)
      write.csv(DF, file, row.names = FALSE)
    }
  )
  
  # handle llm generate button selection
  observeEvent(input$llmGenerate, {
    # get the selected model and temperature. Model is not used for now
    model = input$llmModel
    temp = input$llmTemp
    
    # get the data from the table and covert to csv string
    DF = hot_to_r(input$hot1)
    csvString = paste(capture.output(write.csv(DF, row.names = FALSE)), collapse = "\n")
    
    # create the prompt now
    abstractPrompt = paste("Generate a 200-300 word scientific abstract about ELECTROLYTES AND NON-ELECTROLYTES for data below.",
    "Also calculate the conductivty of group A chemicals using the Kohlrausch Law assuming 1M concentration and include those values in the abstract as well.",
    "Only return the Abstract text.",
    "Here is the csv data:\n", csvString)
    
    print(abstractPrompt)
    
    # generate the abstract using the selected model and temperature
    if(model == "Google Gemini") {
      # use google gemini
      abstract = askGemini(prompt = abstractPrompt, temperature = temp)
    } else if(model == "ChatGPT") {
      # use chatgpt
      abstract = askChatGPT(prompt = abstractPrompt, temperature = temp)
    } else if(model == "DeepSeek") {
      # use deep seek
      abstract = askDeepSeek(prompt = abstractPrompt, temperature = temp)
    } else {
      abstract = "Model not supported ..."
    }
    
    # display the generated abstract
    showModal(modalDialog(
      title = paste("Generated Abstract --", model),
      abstract,
      easyClose = TRUE,
      footer = NULL
    ))
  })
}

# function to get the initial data for table 1
getTableDataExp01C = function(pin) {
  v1 = c(1:11)
  v2 = c('A', 'A', 'A', 'B', 'B', 'B', 'B', 'C', 'C', 'C', 'C')
  v3 = c('CaCl2', 'NaCl', 'AlCl3', 'HC2H3O2', 'HCl', 'H3PO4', 'H3BO3', 'distilled H2O', 'tap H2O', 'CH3OH', 'C2H6O2')
  v4 = c(17895, 9810, 22155, 732, 29680, 11320, 54, 54, 138, 54, 54)
  v4 = addNoise(v4, 0.2, r.values=TRUE)
  
  if(!showSampleData(pin)) {
    v4 = c(-1, 0, 1, 0, -1, 0, 1, 0, -1, 0, 1)  
  }
  
  df = data.frame(v1, v2, v3, v4)
  
  # rename the column names here
  colnames(df) <- c("Solution",
                    "Group",
                    "Compound",
                    "Relative Conductivity (uS/cm)")
  return(df)
}