# Experiment 11 UI/Server module code
# Module used for development and testing
#
# https://shiny.rstudio.com/articles/modules.html
# 

expData13L = list()

# The UI code
exp13CUI <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  # wrap ui components in tablist
  tagList(
    h3("Experiment 13 -- THERMODYNAMICS: ENTHALPY OF FORMATION - MAGNESIUM OXIDE"),
    
    fluidRow(
      box(width = 6, title = "Reaction # 1 (MgO -> MgCl2)", status = "primary",
        plotlyOutput(ns("plot1"))
      ),
      
      box(width = 6, title = "Reaction # 2 (Mg -> MgCl2)", status = "primary",
        plotlyOutput(ns("plot2"))
      )
    ),
    
    fluidRow(
      span(textOutput(ns("vhot1")), style="color:blue"),
      
      # the data table
      rHandsontableOutput(ns("hot1")),
      
      br(),
      
      downloadButton(ns("downloadData"), "Download Data"),
      
      hr(),

      a("YouTube -- The Heat of Formation of Magnesium Oxide", target="_blank", href="https://www.youtube.com/watch?v=jehXd-sw9Eg"),
      
      br(),
      
      HTML('<iframe width="560" height="315" src="https://www.youtube.com/embed/jehXd-sw9Eg" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>'),
      
      br(),
      
      a("YouTube -- Hess's Law and Calorimetry", target="_blank", href="https://www.youtube.com/watch?v=sV-Fu8Kzxdg"),
      
      br(),
      
      HTML('<iframe width="560" height="315" src="https://www.youtube.com/embed/sV-Fu8Kzxdg" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>'),
      
      br(),
      
      a("YouTube -- Heat of Combustion of Magnesium Hess' Law Lab", target="_blank", href="https://www.youtube.com/watch?v=MwNPLcgb9Wk"),
      
      br(),
      
      HTML('<iframe width="560" height="315" src="https://www.youtube.com/embed/MwNPLcgb9Wk" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>')
    ),
    
    # add UI elements to send prompts to LLM API for Abstract generation
    getLLMPromptUIRow(ns)
  )
}

# Server code
exp13C <- function(input, output, session, pin) {
  # render the data table
  output$hot1 <- renderRHandsontable({
    if (!is.null(input$hot1)) {
      DF = hot_to_r(input$hot1)

      if(isAdminUser(pin)) {
        DF = getResultsExp13C(DF)
      }
    } else {
      DF = getTableDataExp13C(pin)
    }
    
    output$vhot1 <- renderText({paste("Data Version:", dataNumberExp13C)})
    
    rhandsontable(DF, stretchH = "all", readOnly = FALSE)
  })
  
  # Download the data to users computer as csv
  output$downloadData <- downloadHandler(
    filename = function() {
      paste0("Experiment13_", pin, ".csv")
    },
    content = function(file) {
      DF = hot_to_r(input$hot1)
      write.csv(DF, file, row.names = FALSE)
    }
  )
  
  # Display plot # 1 of temperature change
  output$plot1 <- renderPlotly({
    x = c("1. Initial Temperature", "2. Final Temperature")
    y = c(expData13L$Reaction1.T1, expData13L$Reaction1.T2)
    
    fig = getBarPlot(x, y, "Temperature Change")
  })

  # Display the plot # 2 of temperature change
  output$plot2 <- renderPlotly({
    x = c("1. Initial Temperature", "2. Final Temperature")
    y = c(expData13L$Reaction2.T1, expData13L$Reaction2.T2)
    
    fig = getBarPlot(x, y, "Temperature Change")
  })
  
  # handle llm generate button selection
  observeEvent(input$llmGenerate, {
    # get the selected model and temperature
    model = input$llmModel
    temp = input$llmTemp
    
    # get the data from the table and covert to csv string
    DF = hot_to_r(input$hot1)
    csvString = paste(capture.output(write.csv(DF, row.names = FALSE)), collapse = "\n")
    
    # create the prompt now
    abstractPrompt = paste("Generate a 200-300 word scientific abstract about the ENTHALPY OF FORMATION OF MAGNESIUM OXIDE for data below.",
                           "Only return the Abstract text.",
                           "Here is the csv data:\n", csvString)
    
    print(abstractPrompt)
    
    # display the abstract after call the LLM API
    displayAbstract(abstractPrompt, model, temp, pin)
  })
}

# function to return random data set
getDataExp13C = function(pin) {
  dataNumberExp13C <<- sample(1:6, 1)
  expData = list()
  
  if(dataNumberExp13C == 1) {
    expData$Reaction1.VOL = 100
    expData$Reaction1.T2 = 29.0
    expData$Reaction1.T1 = 20.0
    expData$Reaction1.MASS = 1.001
    
    expData$Reaction2.VOL = 100
    expData$Reaction2.T2 = 42.5
    expData$Reaction2.T1 = 21.5
    expData$Reaction2.MASS = 0.502
  } else if(dataNumberExp13C == 2) {
    expData$Reaction1.VOL = 100
    expData$Reaction1.T2 = 29.6
    expData$Reaction1.T1 = 20.7
    expData$Reaction1.MASS = 1.001
    
    expData$Reaction2.VOL = 100
    expData$Reaction2.T2 = 45.0
    expData$Reaction2.T1 = 20.3
    expData$Reaction2.MASS = 0.501
  } else if(dataNumberExp13C == 3) {
    expData$Reaction1.VOL = 100
    expData$Reaction1.T2 = 30.0
    expData$Reaction1.T1 = 22.2
    expData$Reaction1.MASS = 1.009
    
    expData$Reaction2.VOL = 100
    expData$Reaction2.T2 = 43.1
    expData$Reaction2.T1 = 21.4
    expData$Reaction2.MASS = 0.488
  } else if(dataNumberExp13C == 4) {
    expData$Reaction1.VOL = 100
    expData$Reaction1.T2 = 28.4
    expData$Reaction1.T1 = 19.7
    expData$Reaction1.MASS = 1.064
    
    expData$Reaction2.VOL = 100
    expData$Reaction2.T2 = 34.2
    expData$Reaction2.T1 = 17.4
    expData$Reaction2.MASS = 0.491
  } else if(dataNumberExp13C == 5) {
    expData$Reaction1.VOL = 100
    expData$Reaction1.T2 = 25.2
    expData$Reaction1.T1 = 17.2
    expData$Reaction1.MASS = 1.001
    
    expData$Reaction2.VOL = 100
    expData$Reaction2.T2 = 38.5
    expData$Reaction2.T1 = 16.0
    expData$Reaction2.MASS = 0.508
  } else { # data number 6
    expData$Reaction1.VOL = 100
    expData$Reaction1.T2 = 33.4
    expData$Reaction1.T1 = 27.7
    expData$Reaction1.MASS = 1.000
    
    expData$Reaction2.VOL = 100
    expData$Reaction2.T2 = 46.1
    expData$Reaction2.T1 = 25.6
    expData$Reaction2.MASS = 0.494
  }
  
  # see if to hide the sample data
  if(!showSampleData(pin)) {
    expData$Reaction1.VOL = 1
    expData$Reaction1.T2 = 0.0
    expData$Reaction1.T1 = 0.0
    expData$Reaction1.MASS = 0.0
    
    expData$Reaction2.VOL = 1
    expData$Reaction2.T2 = 0.0
    expData$Reaction2.T1 = 0.0
    expData$Reaction2.MASS = 0.0  
  }
  
  return(expData)
}

# function to get the initial data
getTableDataExp13C = function(pin) {
  expData = getDataExp13C(pin)
  
  v1 = c("Volume of 1.00 M HCl", 
         "Final temperature, T2 (degC)", 
         "Initial temperature, T1 (degC)", 
         "Change in temperature (degC)",
         "Mass of solid (g)",
         "Results", 
         "Heat (kJ)",
         "Delta H (kJ)",
         "Moles solid (mol)",
         "Delta H/mole (kJ/mol)",
         " ",
         "Mg(s) + 1/2 O2(g) -> MgO(s)",
         "Mg(s) + 1/2 O2(g) -> MgO(s)",
         " ")
  
  v2 = c(expData$Reaction1.VOL, expData$Reaction1.T2, expData$Reaction1.T1, " ", 
         expData$Reaction1.MASS, " ", " ", " ", " ", " ", " ",
         "Experimental Enthalpy (kJ/mol)", "Actual Enthalpy (kJ/mol)", "Percent Error")
  
  v3 = c(expData$Reaction2.VOL, expData$Reaction2.T2, expData$Reaction2.T1, " ", 
         expData$Reaction2.MASS, " ", " ", " ", " ", " "," ",
         " ", "-601.7", " ")
  
  df = data.frame(v1, v2, v3, stringsAsFactors = FALSE)
  colnames(df) <- c("Data",
                    "Reaction 1, MgO(s)",
                    "Reaction 2, Mg(s)")
  
  # make this global
  expData13L <<- expData
  
  return(df)
}

# calculate the results
getResultsExp13C = function(DF) {
  exp13CDF <<- DF
  
  r1TDiff = as.numeric(DF[[2]][2]) - as.numeric(DF[[2]][3])
  DF[[2]][4] = r1TDiff
  
  r2TDiff = as.numeric(DF[[3]][2]) - as.numeric(DF[[3]][3])
  DF[[3]][4] = r2TDiff
  
  r1mass = as.numeric(DF[[2]][1]) + as.numeric(DF[[2]][5])
  r1q = (r1mass*(4.18)*r1TDiff)/1000.0
  DF[[2]][7] = formatC(r1q, format = "f", digits = 2)
  
  r2mass = as.numeric(DF[[3]][1]) + as.numeric(DF[[3]][5])
  r2q = (r2mass*(4.18)*r2TDiff)/1000.0
  DF[[3]][7] = formatC(r2q, format = "f", digits = 2)
  
  r1DH = -1*r1q
  DF[[2]][8] = formatC(r1DH, format = "f", digits = 2)
  
  r2DH = -1*r2q
  DF[[3]][8] = formatC(r2DH, format = "f", digits = 2)
  
  r1moles = as.numeric(DF[[2]][5])/40.3044 # f.w. MgO
  DF[[2]][9] = formatC(r1moles, format = "f", digits = 4)
  
  r2moles = as.numeric(DF[[3]][5])/24.305  # f.w. Mg
  DF[[3]][9] = formatC(r2moles, format = "f", digits = 4)
  
  r1DHm = r1DH/r1moles
  DF[[2]][10] = formatC(r1DHm, format = "f", digits = 1)
  
  r2DHm = r2DH/r2moles
  DF[[3]][10] = formatC(r2DHm, format = "f", digits = 1)
  
  expEnthalpy = -(r1DHm) + r2DHm + (-285.8)
  DF[[3]][12] = formatC(expEnthalpy, format = "f", digits = 1)
  
  pe = ((expEnthalpy - (-601.7))/-601.7)*100
  pe = abs(pe)
  DF[[3]][14] = formatC(pe, format = "f", digits = 2)
  
  return(DF)
}