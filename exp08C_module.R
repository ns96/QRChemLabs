# Experiment 8C UI/Server module code
# Module used for development and testing
#
# https://shiny.rstudio.com/articles/modules.html
# 

# The UI code
exp08CUI <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  # wrap ui components in tablist
  tagList(
    h3("Experiment 8 -- THE ACID DISSOCIATION CONSTANT, KA"),
    
    fluidRow(
      box(width = 4, title = "Experiment Data", status = "primary",
          # temperature
          textInput(ns("q1"), "Assigned Concentration (M):"),
          textInput(ns("q2"), "Volume of 2.0 M Acetic Acid (mL):", value = 0),
          span(textOutput(ns("v2")), style="color:blue"),
          
          # the data table
          span(textOutput(ns("vhot1")), style="color:blue"),
          rHandsontableOutput(ns("hot1")),
          br(),
          downloadButton(ns("downloadData"), "Download Data"),
          
          hr(),
          
          a("Chegg -- Molarity Of Vinegar", target="_blank", href="https://www.chegg.com/homework-help/questions-and-answers/vinegar-5-acetic-acid-molarity-0833m-concentration-hydronium-ion-vinegar-greater-less-equa-q1334244")
      ),
      
      box(width = 8, title = "Group Constants", status = "primary",
          plotlyOutput(ns("plot1")),
          
          rHandsontableOutput(ns("hot2")),
          span(textOutput(ns("vhot2")), style="color:blue"),
      )
    ),
    
    fluidRow(
      a("YouTube -- Data Analysis", target="_blank", href="https://www.youtube.com/watch?v=Qq-1m2lp7hI"),
      br(),
      HTML('<iframe width="560" height="315" src="https://www.youtube.com/embed/Qq-1m2lp7hI" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>'),
      br(),
      a("YouTube -- Vernier pH Prob", target="_blank", href="https://www.youtube.com/watch?v=1eqVZ2EqhRc"),
      br(),
      HTML('<iframe width="560" height="315" src="https://www.youtube.com/embed/1eqVZ2EqhRc" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>')
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
exp08C <- function(input, output, session, pin) {
  updateTextInput(session, "q1", value = sample(c(0.2, 0.3, 0.4, 0.5), 1))
  
  # render the experiment data table
  output$hot1 <- renderRHandsontable({
    # check to see if the correct volume in mL was entered
    assignedConc = as.numeric(input$q1)
    volumeAceticAcid = as.numeric(input$q2)
    correctVolume = isCorrectVolumeExp08C(assignedConc, volumeAceticAcid)
    
    if(correctVolume) {
      getTableDataExp08C(assignedConc, pin)

      # Display the bar plot of Ka data
      output$plot1 <- renderPlotly({
        x = c('0.2M', '0.3M', '0.4M', '0.5M')
        y = kaValuesExp08C
        fig = getBarPlot(x, y, "Ka for Acetic Acid")
      })
      
      output$vhot1 <- renderText({paste("Volume Correct:", volumeAceticAcid, "mL")})
      rhandsontable(exp08CDF, stretchH = "all")
    } else {
      output$vhot1 <- renderText({paste("Incorrect Volume:", volumeAceticAcid, "mL")})
      return(NULL)
    }
  })
  
  # render the results data table
  output$hot2 <- renderRHandsontable({
    if(isAdminUser(pin)) {
      # check the volume so this table gets updated
      assignedConc = as.numeric(input$q1)
      volumeAceticAcid = as.numeric(input$q2)
      correctVolume = isCorrectVolumeExp08C(assignedConc, volumeAceticAcid)
      
      if(correctVolume) {
        output$vhot2 <- renderText({resultsExp08C})
        rhandsontable(resultsExp08CDF, stretchH = "all")
      } else {
        return(NULL)
      }
    }
  })
  
  # Download the data to users computer as csv
  output$downloadData <- downloadHandler(
    filename = function() {
      paste0("Experiment08_", pin, ".csv")
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
    abstractPrompt = paste("Generate a 200-300 word scientific abstract about the ",
    "ACID DISSOCIATION CONSTANT OF ACETIC ACID and Concentration of Acitic Acid In Vinegar.",
    "Only return the Abstract text.",
    "Here are the main results: ", resultsExp08C, "\n",
    "Here is the csv data:\n", csvString)
    
    print(abstractPrompt)
    
    # display the abstract after call the LLM API
    displayAbstract(abstractPrompt, model, temp)
  })
}
# check to see if the column entered is correct
isCorrectVolumeExp08C = function(assignedConc, volumeAceticAcid) {
  correctVolume = (assignedConc*100)/2.0
  cat("EXP08C Correct Volume (mL):", correctVolume, volumeAceticAcid, "\n")
  
  if(!is.na(volumeAceticAcid) && correctVolume == volumeAceticAcid) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

# calculate the Ka Values
getResultsExp08C = function(data) {
  pH.Data = data[1:4]
  Acetic.Acid.Conc = c(0.2, 0.3, 0.4, 0.5)
  
  h.ion = 10^(-1*pH.Data)
  Hydronium.Ion = formatC(h.ion, format = "E", digits = 2)
  
  ka = (h.ion*h.ion)/Acetic.Acid.Conc
  Calculated.Ka = formatC(ka, format = "E", digits = 2)
  
  pion = (h.ion/Acetic.Acid.Conc)*100
  Percent.Ionization = paste0(formatC(pion, digits = 2),'%')
  
  # initialize the results dataframe
  resultsExp08CDF <<- data.frame(Acetic.Acid.Conc, pH.Data, 
                                 Hydronium.Ion, Calculated.Ka, Percent.Ionization)
  
  # create the results text 
  avgKa = mean(ka)
  vinegarH = 10^(-1*data[5])
  vinegarM = (vinegarH*vinegarH)/avgKa
  
  avgKa.F = formatC(avgKa, format = "E", digits = 2)
  vinegarH.F = formatC(vinegarH, format = "E", digits = 2)
  vinegarM.F = formatC(vinegarM, digits = 4)
  perror = percentError(0.833, vinegarM)
  
  resultsExp08C <<- paste("Average Ka:", avgKa.F, 
                          ", Vinegar H+:", vinegarH.F,
                          ", Calculated Vinegar (M):", vinegarM.F,
                          ", Actual Vinegar (M): 0.833",
                          "/", perror)
  
  # return the calculated Ka
  return(Calculated.Ka)
}

# function to return random data set
getDataExp08C = function(assignedConc, pin) {
  Exp08C.Temperature <<- 27.0
  expData = list()
  
  # the pH Data for various concentration (0.2, 0.3, 0.4, 0.5) of 
  # Acetic And Vinegar
  data = c(3.31, 3.23, 3.16, 3.08, 3.02)
  
  if(!showSampleData(pin)) {
    data = c(0.0, 0.0, 0.0, 0.0, 0.0)  
  }
  
  noisyData = addNoise(data, 0.01)
  
  kaResults = getResultsExp08C(noisyData)
  kaValuesExp08C <<- as.numeric(kaResults)
  
  ka02M = kaResults[1]
  ka03M = kaResults[2]
  ka04M = kaResults[3]
  ka05M = kaResults[4]
  avgKa = '?'
  
  if(assignedConc == 0.2) {
    measuredPH = noisyData[1]
    kaValuesExp08C[1] <<- 0
    ka02M = '?'
  } else if(assignedConc == 0.3) {
    measuredPH = noisyData[2]
    kaValuesExp08C[2] <<- 0
    ka03M = '?'
  } else if(assignedConc == 0.4) {
    measuredPH = noisyData[3]
    kaValuesExp08C[3] <<- 0
    ka04M = '?'
  } else {
    measuredPH = noisyData[4]
    kaValuesExp08C[4] <<- 0
    ka05M = '?'
  }
  
  measuredPH = round(measuredPH, digits = 2)
  vinegarPH = round(noisyData[5], digits = 2)
  
  expData$Variable.Name = c("Temperature", "Acetic Acid (M)","Measured pH", "Vinegar pH", "0.2M Ka","0.3M Ka", "0.4M Ka","0.5M Ka", "Avg Ka")
  expData$Variable.Value = c(Exp08C.Temperature, assignedConc,
                             measuredPH, vinegarPH, 
                             ka02M, ka03M, ka04M, ka05M, avgKa)
  return(expData)
}

# function to get the initial data
getTableDataExp08C = function(assignedConc, pin) {
  cat("Getting EXP8C Table data ...\n")
  expData = getDataExp08C(assignedConc, pin)
  
  Variable.Name =  expData$Variable.Name 
  Variable.Value = expData$Variable.Value
  
  exp08CDF <<- data.frame(Variable.Name, Variable.Value)
}