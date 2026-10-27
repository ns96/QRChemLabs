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
          a("Temperature And Kc Value", target="_blank", href="https://www.chemguide.co.uk/physical/equilibria/change.html"),
          br(),
          # add button to open dialog to display data table of temperature vs Kc ranges
          actionButton(ns("showKcRanges"), "Show Kc Ranges", icon = icon("table"), class = "btn-primary")
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
    
    # add UI elements to send prompts to LLM API for Abstract generation
    getLLMPromptUIRow(ns)
  )
}

# Server code
exp07C <- function(input, output, session, pin) {
  # single source of truth for the experiment table; seeded once per session
  values <- reactiveValues(data = getTableDataExp07C(pin))
  
  # keep the reactive data in sync with table edits so the Kc results and the
  # plot always reflect what the student has entered
  observeEvent(input$hot1, {
    values$data <- hot_to_r(input$hot1)
  })
  
  # render the experiment data table
  output$hot1 <- renderRHandsontable({
    output$vhot1 <- renderText({paste("Data Version:", dataNumberExp07C)})
    
    rhandsontable(values$data, stretchH = "all")
  })
  
  # render the results data table, recalculated from the edited data
  output$hot2 <- renderRHandsontable({
    if(isAdminUser(pin)) {
      df = getResultsExp07C(values$data)
      
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
  
  # Display the simulated plot data, reacting to table edits
  output$plot1 <- renderPlotly({
    # update the temperature
    updateTextInput(session, "q1", value = Exp07C.Temperature)
    
    x = values$data[['Trial.Name']]
    y = values$data[['Absorbance.Value']]
    
    fig = getBarPlot(x, y, "Absorbance for Trials")

    # plotly sorts categorical axes alphabetically, so pin the bars to the table order
    fig = layout(fig, xaxis = list(categoryorder = "array", categoryarray = x))
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
  
  # show the reference table of temperature vs expected Kc ranges
  observeEvent(input$showKcRanges, {
    showModal(modalDialog(
      title = "Temperature vs Kc Ranges",
      getKcRangesTableExp07C(Exp07C.Temperature),
      easyClose = TRUE,
      footer = modalButton("Close")
    ))

    cat("Show Kc Ranges -- EXP07C", "\n")
  })

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
    displayAbstract(abstractPrompt, model, temp, pin)
  })
}

# calculate Kc for a trial
calculateKc = function(DF, trial) {
  Fe3i = (5/10)*0.002
  SCNi = ((trial + 1)/10)*0.002
  FeSCN2eq = (as.numeric(DF[[2]][trial])/as.numeric(DF[[2]][5]))*0.0002
  Fe3eq = Fe3i - FeSCN2eq
  SCNeq = SCNi - FeSCN2eq
  Kc = FeSCN2eq/(Fe3eq * SCNeq)
  return(c(Fe3i, SCNi, FeSCN2eq, Fe3eq, SCNeq, round(Kc)))
}

# calculate the Kc Values
getResultsExp07C = function(DF) {
  Variable = c("[Fe3+]i","[SCN-]i", "[FeSCN2+]eq", "[Fe3+]eq", "[SCN-]eq", "Kc")
  Trial.1 = calculateKc(DF, 1)
  Trial.2 = calculateKc(DF, 2)
  Trial.3 = calculateKc(DF, 3)
  Trial.4 = calculateKc(DF, 4)
  
  df = data.frame(Trial.1, Trial.2, Trial.3, Trial.4)
  row.names(df) <- Variable
  return(df)
}

# build the temperature vs Kc reference table used by the "Show Kc Ranges" dialog
getKcRangesExp07C = function() {
  temp_C = 12:30
  temp_K = temp_C + 273.15

  # fine-tuned target Kc equation
  target_kc = exp(6051.46 / temp_K - 15.4111)

  # standard deviation for the expected student range
  sd_val = 22.5

  expected_range = paste0(round(target_kc - sd_val, 1), " \u2013 ",
                          round(target_kc + sd_val, 1))

  data.frame(
    "Temperature (\u00b0C)" = temp_C,
    "Target Kc" = round(target_kc, 1),
    "Expected Student Range" = expected_range,
    check.names = FALSE
  )
}

# render the Kc range table, highlighting the row for the assigned temperature
getKcRangesTableExp07C = function(currentTemp) {
  df = getKcRangesExp07C()
  currentTemp = as.numeric(currentTemp)

  highlightStyle = "background-color: #d9edf7; font-weight: bold;"

  caption = if (length(currentTemp) != 1 || is.na(currentTemp)) {
    "No assigned solution temperature was found."
  } else {
    paste0("Your assigned solution temperature: ", currentTemp, " \u00b0C (highlighted)")
  }

  header = tags$tr(lapply(names(df), function(label) tags$th(label)))

  rows = lapply(seq_len(nrow(df)), function(i) {
    style = if (isTRUE(all.equal(df[[1]][i], currentTemp))) highlightStyle else NULL

    tags$tr(lapply(df[i, ], function(value) tags$td(as.character(value), style = style)))
  })

  tags$div(
    tags$p(caption),
    tags$div(style = "max-height: 420px; overflow-y: auto;",
      tags$table(class = "table table-condensed table-bordered",
        tags$thead(header),
        tags$tbody(rows)
      )
    )
  )
}

# function to return data set
getDataExp07C = function(pin) {
  dataNumberExp07C <<- sample(1:2, 1)
  expData = list()
  
  expData$Trial.Name = c("Trial 1","Trial 2","Trial 3","Trail 4","Standard")
  
  if(dataNumberExp07C == 1) {
    # Data -- Nathan
    expData$Actual.Absorbance = c(0.255, 0.365, 0.457, 0.523, 0.901)
    expData$Temperature = 22.0
  } else {
    # Data -- Nathan
    expData$Actual.Absorbance = c(0.198, 0.282, 0.374, 0.432, 0.820)
    expData$Temperature = 24.0
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
