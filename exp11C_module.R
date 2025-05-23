# Experiment 11 UI/Server module code
# Module used for development and testing
#
# https://shiny.rstudio.com/articles/modules.html
# 

exp11CL = list()

# The UI code
exp11CUI <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  # wrap ui components in tablist
  tagList(
    h3("Experiment 11 -- DETERMINING THE KSP OF CALCIUM HYDROXIDE"),
    
    fluidRow(
      box(width = 6, title = paste("Trial One / 20 mL Ca(OH)2"), status = "primary",
        plotlyOutput(ns("plot1")),
        hr(),
        
        plotlyOutput(ns("plot2")),
        
        numericInput(ns("i1"), "Trial 1 Equivalence Point (mL):", value = ""),
        
        numericInput(ns("volume.CaOH2"), "Volume Ca(OH)2 (mL):", value = "20.0"),
        
        numericInput(ns("room.temp"), "Room Temperature (C):", value = "25"),
      ),
      
      box(width = 6, title = paste("Trial Two / 20 mL Ca(OH)2"), status = "primary",
        plotlyOutput(ns("plot3")),
        hr(),
        
        plotlyOutput(ns("plot4")),
        
        numericInput(ns("i2"), "Trial 2 Equivalence Point (mL):", value = ""),
        
        numericInput(ns("hcl.Conc"), "HCl Concentration (M):", value = "0.050"),
        
        selectInput(ns("literature.Ksp"), "Excepted Ksp Value:", c("5.02E-6", "7.9E-6", "4.42E-5"))
      )
    ),
    
    fluidRow(
      # the data table
      rHandsontableOutput(ns("hot1")),
      
      br(),
      
      downloadButton(ns("downloadData"), "Download Data"),
      
      hr(),

      a("YouTube -- Determining the Ksp of Calcium Hydroxide", target="_blank", href="https://www.youtube.com/watch?v=-FF-D3cfST0"),
      br(),
      #HTML('<iframe width="560" height="315" src="https://www.youtube.com/embed/-FF-D3cfST0" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>'),
      #br(),
      a("YouTube -- Analyzing Data from the Vernier Ksp of Calcium Hydroxide Lab", target="_blank", href="https://www.youtube.com/watch?v=KOIcjIGQU3U"),
      br(),
      HTML('<iframe width="560" height="315" src="https://www.youtube.com/embed/KOIcjIGQU3U" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>'),
      br(),
      a("YouTube -- Determining the Ksp of Calcium Hydroxide", target="_blank", href="https://www.youtube.com/watch?v=fPoo_o6TCGc"),
      br(),
      HTML('<iframe width="560" height="315" src="https://www.youtube.com/embed/fPoo_o6TCGc" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>'),
      br(),
      a("Solubility Product Constants near 25 degC", target="_blank", href="https://www.chm.uri.edu/weuler/chm112/refmater/KspTable.html")
    ),
    
    # add UI elements to send prompts to LLM API for Abstract generation
    getLLMPromptUIRow(ns)
  )
}

# Server code
exp11C <- function(input, output, session, pin) {
  # render the data table
  output$hot1 <- renderRHandsontable({
    trial1.mL = input$i1
    trial2.mL = input$i2
    volume.CaOH2 <<- as.numeric(input$volume.CaOH2)
    conc.HCl <<- as.numeric(input$hcl.Conc)
      
    if(!is.na(trial1.mL) && !is.na(trial1.mL)) {
      # get the selected literature value for ksp
      literature.Ksp = input$literature.Ksp
      
      df = getTableDataExp11C(trial1.mL, trial2.mL, literature.Ksp, pin)
      rhandsontable(df, stretchH = "all", readOnly = FALSE)
    }
  })
  
  # Download the data to users computer as csv
  output$downloadData <- downloadHandler(
    filename = function() {
      paste0("Experiment11_", pin, ".csv")
    },
    content = function(file) {
      DF = hot_to_r(input$hot1)
      write.csv(DF, file, row.names = FALSE)
    }
  )
  
  # Display the pH data plots
  output$plot1 <- renderPlotly({
    x_data = getPlotDataExp11C('Trial1_X')
    y_data = getPlotDataExp11C('Trial1_Y')

    df = data.frame(x_data, y_data)

    fig = getLinePlot(df, "Titration Data", "Volume HCl (mL)", "pH", smooth = FALSE) %>%
      layout(showlegend = FALSE)
  })

  # Display plot # 2
  output$plot2 <- renderPlotly({
    dL = derivative(data.frame(exp11CL$Trial1_X, exp11CL$Trial1_Y), plot=FALSE)
    x_data = dL$second_deriv[[1]]
    y_data = dL$second_deriv[[2]]

    df = data.frame(x_data, y_data)
    
    fig = getLinePlot(df, "Second Derivative", "Volume HCl (mL)", "pH", smooth = FALSE) %>%
      layout(showlegend = FALSE)
  })

  # Display the plot # 3
  output$plot3 <- renderPlotly({
    x_data = getPlotDataExp11C('Trial2_X')
    y_data = getPlotDataExp11C('Trial2_Y')

    df = data.frame(x_data, y_data)

    fig = getLinePlot(df, "Titration Data", "Volume HCl (mL)", "pH", smooth = FALSE) %>%
      layout(showlegend = FALSE)
  })

  # Display the plot # 4
  output$plot4 <- renderPlotly({
    dL = derivative(data.frame(exp11CL$Trial2_X, exp11CL$Trial2_Y), plot=FALSE)
    x_data = dL$second_deriv[[1]]
    y_data = dL$second_deriv[[2]]
    
    df = data.frame(x_data, y_data)
    
    fig = getLinePlot(df, "Second Derivative", "Volume HCl (mL)", "pH", smooth = FALSE) %>%
      layout(showlegend = FALSE)
  })
  
  # handle llm generate button selection
  observeEvent(input$llmGenerate, {
    # get the selected model and temperature
    model = input$llmModel
    temp = input$llmTemp
    room.temp = input$room.temp
    
    # get the data from the table and covert to csv string
    DF = hot_to_r(input$hot1)
    csvString = paste(capture.output(write.csv(DF, row.names = FALSE)), collapse = "\n")
    
    # create the prompt now
    abstractPrompt = paste("Generate a 200-300 word scientific abstract about DETERMINING THE KSP OF CALCIUM HYDROXIDE for data below.",
                           "Room Temperature:", room.temp, "degC",
                           "Only return the Abstract text.",
                           "Here is the csv data:\n", csvString)
    
    print(abstractPrompt)
    
    # display the abstract after call the LLM API
    displayAbstract(abstractPrompt, model, temp, pin)
  })
}

# function to return the column data
getPlotDataExp11C = function(columnName) {
  if (!exists("exp11CDF")) {
    getDataExp11C()    
  }
  
  n_data = exp11CDF[[columnName]]
  n_data = n_data[!is.na(n_data)]
  
  # convert time to seconds
  if(grepl("X", columnName, fixed = TRUE)) {
    n_data = n_data + sample(0:3, 1) # add some random number
  } else {
    n_data = n_data + runif(1, -0.25, 0.25) # add some random number
  }
  
  # store the data list incase we need to export
  # https://stackoverflow.com/questions/20133344/find-closest-value-in-a-vector-with-binary-search
  exp11CL[[columnName]] <<- n_data
  
  return(n_data)
}

# function to return random data set
getDataExp11C = function() {
  df.name = load("data/Exp11.Rda")
  exp11CDF <<- get(df.name)
}

# function to get the initial data
getTableDataExp11C = function(trial1.mL, trial2.mL, literature.Ksp, pin) {
  v1 = c("Trial 1", "Trial 2", "Average")
  v2 = c(volume.CaOH2, volume.CaOH2, volume.CaOH2)
  v3 = c(trial1.mL, trial2.mL, " ")
  v4 = c(" ", " ", " ")
  v5 = c(" ", " ", " ")
  v6 = c(" ", " ", " ")
  v7 = c(literature.Ksp, literature.Ksp, literature.Ksp)
  v8 = c(" ", " ", " ")
  
  if(isAdminUser(pin)) {
    v3 = c(trial1.mL, trial2.mL, (trial1.mL + trial2.mL)/2)
    
    molesOH1 = (trial1.mL/1000)*conc.HCl
    molarityOH1 = molesOH1/(volume.CaOH2/1000)
    molesOH2 = (trial2.mL/1000)*conc.HCl
    molarityOH2 = molesOH2/(volume.CaOH2/1000)
    molarityOHAvg = (molarityOH1 + molarityOH2)/2
    v4 = c(formatC(molarityOH1, format = "f", digits = 4), 
           formatC(molarityOH2, format = "f", digits = 4), 
           formatC(molarityOHAvg, format = "f", digits = 4))
  
    molarityCa1 = molarityOH1/2
    molarityCa2 = molarityOH2/2
    molarityCaAvg = molarityOHAvg/2
    v5 = c(formatC(molarityCa1, format = "f", digits = 4), 
           formatC(molarityCa2, format = "f", digits = 4), 
           formatC(molarityCaAvg, format = "f", digits = 4))
    
    ksp1 = molarityCa1*(molarityOH1^2)
    ksp2 = molarityCa2*(molarityOH2^2)
    kspAvg = molarityCaAvg*(molarityOHAvg^2)
    v6 = c(formatC(ksp1, format = "E", digits = 2), 
           formatC(ksp2, format = "E", digits = 2), 
           formatC(kspAvg, format = "E", digits = 2))
    
    actual = as.numeric(literature.Ksp)
    error1 = (abs(actual - ksp1)/actual)*100
    error2 = (abs(actual - ksp2)/actual)*100
    errorAvg = (abs(actual - kspAvg)/actual)*100
    v8 = c(formatC(error1, format = "f", digits = 1), 
           formatC(error2, format = "f", digits = 1), 
           formatC(errorAvg, format = "f", digits = 1))
  }
  
  df = data.frame(v1, v2, v3, v4, v5, v6, v7, v8, stringsAsFactors = FALSE)
  colnames(df) <- c("Trial",
                    "Volume Ca(OH)2 (mL)",
                    "Equivalence point (mL)",
                    "Molarity [OH-]",
                    "Molarity [Ca2+]",
                    "Ksp Ca(OH)2",
                    "Actual Ksp",
                    "Percent Error")
  
  return(df)
}