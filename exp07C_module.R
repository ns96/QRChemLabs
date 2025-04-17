# Experiment 7C UI/Server module code
# Module used for developement and testing
#
# https://shiny.rstudio.com/articles/modules.html
# 

# The UI code
exp07CUI <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  # wrap ui components in tablist
  tagList(
    h3("Experiment 7 -- CHEMICAL EQUILIBRIUM: FINDING A CONSTANT, KC"),
    
    fluidRow(
      box(width = 4, title = "Colorimeter Data", status = "primary",
          # temperature input
          textInput(ns("q1"), "Solution Temperature:"),
          span(textOutput(ns("v1")), style="color:blue"),
          
          # the data table
          span(textOutput(ns("vhot1")), style="color:blue"),
          rHandsontableOutput(ns("hot1")),
          br(),
          downloadButton(ns("downloadData"), "Download Data"),
          
          hr(),
          
          a("Experimental Kc Value", target="_blank", href="https://www.odinity.com/determining-equilibrium-constant-for-thiocyanatoiron/"),
          br(),
          a("Temperature And Kc Value", target="_blank", href="https://www.chemguide.co.uk/physical/equilibria/change.html")
      ),
      
      box(width = 8, title = "Trial Absorbance Values", status = "primary",
          plotlyOutput(ns("plot1")),
          
          rHandsontableOutput(ns("hot2")),
          span(textOutput(ns("vhot2")), style="color:blue"),
      )
    ),
    
    fluidRow(
      a("YouTube -- Equilibrium Constant Analysis", target="_blank", href="https://www.youtube.com/watch?v=NWPj28JryPU"),
      br(),
      HTML('<iframe width="560" height="315" src="https://www.youtube.com/embed/NWPj28JryPU" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>'),
      br(),
      a("YouTube -- Equilibrium Constant Analysis", target="_blank", href="https://www.youtube.com/watch?v=0vvqyLajvoA"),
      br(),
      HTML('<iframe width="560" height="315" src="https://www.youtube.com/embed/0vvqyLajvoA" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>'),
      br(),
      a("YouTube -- Experiment", target="_blank", href="https://www.youtube.com/watch?v=haDVYf-tzRo"),
      br(),
      HTML('<iframe width="560" height="315" src="https://www.youtube.com/embed/haDVYf-tzRo" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>'),
      br(),
      a("YouTube -- Experiment", target="_blank", href="https://www.youtube.com/watch?v=s1klzuLzcfg"),
      br(),
      HTML('<iframe width="560" height="315" src="https://www.youtube.com/embed/s1klzuLzcfg" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>'),
      br(),
      a("YouTube -- Experiment", target="_blank", href="https://www.youtube.com/watch?v=iz3lbZ6ukQI"),
      br(),
      HTML('<iframe width="560" height="315" src="https://www.youtube.com/embed/iz3lbZ6ukQI" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>')
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
exp07C <- function(input, output, session, pin) {
  # render the experiment data table
  output$hot1 <- renderRHandsontable({
    if (!is.null(input$hot1)) {
      exp07CDF <<- hot_to_r(input$hot1)
    } else {
      getTableDataExp07C(pin)
    }
    
    output$vhot1 <- renderText({paste("Data Version:", dataNumberExp07C)})
    
    rhandsontable(exp07CDF, stretchH = "all")
  })
  
  # render the results data table
  output$hot2 <- renderRHandsontable({
    if(isAdminUser(pin)) {
      df = getResultsExp07C()    
      
      # get the average
      avgKc <<- round(rowMeans(df)[6])
      output$vhot2 <- renderText({paste("Average Kc:", avgKc)})
      
      rhandsontable(df, rowHeaderWidth = 100, stretchH = "all") %>%
        hot_col("Trial.1", format = "0.000000") %>%
        hot_col("Trial.2", format = "0.000000") %>%
        hot_col("Trial.3", format = "0.000000") %>%
        hot_col("Trial.4", format = "0.000000")
    }
  })
  
  # Display the simulated plot data
  output$plot1 <- renderPlotly({
    if (is.null(exp07CDF)) {
      getTableDataExp07C()    
    }
    
    # update the temperature
    updateTextInput(session, "q1", value = Exp07C.Temperature)
    
    x = exp07CDF[['Trial.Name']]
    y = exp07CDF[['Absorbance.Value']]
    
    fig = getBarPlot(x, y, "Absorbance for Trials")
  })
  
  # Download the data to users computer as csv
  output$downloadData <- downloadHandler(
    filename = function() {
      paste0("Experiment07_", pin, ".csv")
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
    csvString1 = paste(capture.output(write.csv(DF, row.names = FALSE)), collapse = "\n")
    
    DF = hot_to_r(input$hot2)
    csvString2 = paste(capture.output(write.csv(DF, row.names = FALSE)), collapse = "\n")
    
    # create the prompt now
    abstractPrompt = paste("Generate a 200-300 word scientific abstract about",
                           "DETERMINING THE Kc OF IRON THIOCYANATE using colorimetric data.",
                           "Only return the Abstract text.",
                           "The temperature of the solution is: ", Exp07C.Temperature, "\n",
                           "The Average Kc value is: ", avgKc, "\n",
                           "Here is the colorimetric data: ", csvString1, "\n",
                           "Here is the Kc calculations data:\n", csvString2)
    
    print(abstractPrompt)
    
    # display the abstract after call the LLM API
    displayAbstract(abstractPrompt, model, temp)
  })
}

# calculate Kc for a trial
calculateKc = function(trial) {
  Fe3i = (5/10)*0.002
  SCNi = ((trial + 1)/10)*0.002
  FeSCN2eq = (exp07CDF[[2]][trial]/exp07CDF[[2]][5])*0.0002
  Fe3eq = Fe3i - FeSCN2eq
  SCNeq = SCNi - FeSCN2eq
  Kc = FeSCN2eq/(Fe3eq * SCNeq)
  return(c(Fe3i, SCNi, FeSCN2eq, Fe3eq, SCNeq, round(Kc)))
}

# calculate the Kc Values
getResultsExp07C = function() {
  Variable = c("[Fe3+]i","[SCN-]i", "[FeSCN2+]eq", "[Fe3+]eq", "[SCN-]eq", "Kc")
  Trial.1 = calculateKc(1)
  Trial.2 = calculateKc(2)
  Trial.3 = calculateKc(3)
  Trial.4 = calculateKc(4)
  
  df = data.frame(Trial.1, Trial.2, Trial.3, Trial.4)
  row.names(df) <- Variable
  return(df)
}

# function to return data set
getDataExp07C = function(pin) {
  dataNumberExp07C <<- sample(1:2, 1)
  expData = list()
  
  expData$Trial.Name = c("Trial 1","Trial 2","Trial 3","Trail 4","Standard")
  
  if(dataNumberExp07C == 1) {
    # Data -- Nathan
    expData$Actual.Absorbance = c(0.153, 0.255, 0.344, 0.421, 0.547)
    expData$Temperature = 13.0
  } else {
    # Data -- Abiola
    expData$Actual.Absorbance = c(0.144, 0.262, 0.322, 0.457, 0.758)
    expData$Temperature = 20.0
  }
  
  # return dummy data
  if(!showSampleData(pin)) {
    expData$Actual.Absorbance = c(0.0, 1.0, 0.0, 1.0, 0.0)
    expData$Temperature = 300.0  
  }
  
  return(expData)
}

# function to get the initial data
getTableDataExp07C = function(pin) {
  expData = getDataExp07C(pin)
  
  Trial.Name =  expData$Trial.Name 
  Absorbance.Value = addNoise(expData$Actual.Absorbance, 0.10)
  
  Exp07C.Temperature <<- expData$Temperature
  exp07CDF <<- data.frame(Trial.Name, Absorbance.Value)
}