# Experiment 5C UI/Server module code
# Module used for development and testing
#
# https://shiny.rstudio.com/articles/modules.html
# 

# The UI code
exp05CUI <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  # wrap ui components in tablist
  tagList(
    h3("Experiment 5 -- USING FREEZING-POINT DEPRESSION TO FIND MOLECULAR WEIGHT"),
    
    fluidRow(
      box(title = "Pure Solvent", status = "primary", width = 6,
          img(src='images/LauricAcidPure.png', width = "370px", height = "240px"),
          span(textOutput(ns("v1")), style="color:blue")
      ),
      
      box(title = "Mixture", status = "primary", width = 6,
          img(src='images/LauricAcidMixture.png', width = "370px", height = "240px"),
          span(textOutput(ns("v2")), style="color:blue")
      ),
    ),
    
    
    fluidRow(
      box(title = "Experiment Data", status = "primary", width = 12,
        rHandsontableOutput(ns("hot1")),
        span(htmlOutput(ns("vhot1")), style="color:blue"),
          
        br(),
          
        downloadButton(ns("downloadData"), "Download Data")
      ),
    ),
    
    fluidRow(
      a("YouTube -- Determining Molar Mass of Unknown using Freezing Point Depression", target="_blank", href="https://www.youtube.com/watch?v=khy9sYGqf5w"),
      br(),
      HTML('<iframe width="560" height="315" src="https://www.youtube.com/embed/khy9sYGqf5w" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>'),
      br(),
      a("YouTube -- Determination of Molar Mass by Freezing Point Depression", target="_blank", href="https://www.youtube.com/watch?v=TUWO4zbnnfU"),
      br(),
      HTML('<iframe width="560" height="315" src="https://www.youtube.com/embed/TUWO4zbnnfU" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>')
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
exp05C <- function(input, output, session, pin) {
  # render the datatable for electrolytes
  output$hot1 <- renderRHandsontable({
    if (!is.null(input$hot1)) {
      DF = hot_to_r(input$hot1)
    } else {
      DF = getTableDataExp05C(pin)
    }
    
    # update the text messages
    fp.pure = as.numeric(DF[3,2])
    output$v1 <- renderText({ paste("Average Freezing Point:", fp.pure, "Deg C") })
    
    fp.mixture = as.numeric(DF[4,2])
    output$v2 <- renderText({ paste("Freezing Point:", fp.mixture, "Deg C") })
    
    # if admin show the solution
    if(isAdminUser(pin)) {
      deltaT = fp.pure - fp.mixture
      DF[5,2] = formatC(deltaT, format="f", digits = 1)
      
      molality = deltaT/3.9
      DF[6,2] = formatC(molality, format="f", digits = 1)
      
      mass.lauric = as.numeric(DF[1,2])/1000.0
      moles.benzoic = mass.lauric*molality
      DF[7,2] = formatC(moles.benzoic, format="f", digits = 4)
      
      mass.benzoic = as.numeric(DF[2,2])
      mw.benzoic = mass.benzoic/moles.benzoic
      DF[8,2] = formatC(mw.benzoic, format="f", digits = 2)
      
      mw.accepted = 122.12 # molecular weight benzoic acid g/mol
      DF[9,2] = mw.accepted
      
      # calculate % error
      p.error = (abs(mw.benzoic - mw.accepted)/mw.accepted)*100
      DF[10,2] = formatC(p.error, format="f", digits = 1)
    }
    
    rhandsontable(DF, stretchH = "all", rowHeaders = FALSE)
  })
  
  # Download the data to users computer as csv
  output$downloadData <- downloadHandler(
    filename = function() {
      paste0("Experiment05C_", pin, ".csv")
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
                           "USING FREEZING-POINT DEPRESSION TO FIND MOLECULAR WEIGHT OF Benzoic Acid",
                           "Only return the Abstract text.",
                           "Here is the csv data:\n", csvString)
    
    print(abstractPrompt)
    
    # display the abstract after call the LLM API
    displayAbstract(abstractPrompt, model, temp)
  })
}

# function to get the initial data for table 1
getTableDataExp05C = function(pin) {
  v1 = c("Mass of Lauric Acid (g)", 
         "Mass of Benzoic Acid (g)", 
         "Freezing Point of Pure Lauric Acid (Deg C)", 
         "Freezing Point of Benzoic-Lauric Acid Mixture (Deg C)", 
         "Freezing Temperature Depression (Delta T)", 
         "Molality, m", 
         "Moles of Benzoic Acid",
         "Molecular Weight of Benzoic Acid (Experimental -- g/mol)",
         "Molecular Weight of Benzoic Acid (Accepted -- g/mol)",
         "Percent Error")
  
  v2 = c(rep("---", 10))
  
  # load data
  mass.lauric = addNoise(c(7.9), 0.15)
  v2[1] = formatC(mass.lauric, format="f", digits = 4)
  
  mass.benzoic = addNoise(c(1.0), 0.15)
  v2[2] = formatC(mass.benzoic, format="f", digits = 4)
  
  fp.pure = addNoise(c(42.5), 0.05)
  v2[3] = formatC(fp.pure, format="f", digits = 1)
  
  fp.mixture = addNoise(c(39.2), 0.10)
  v2[4] = formatC(fp.mixture, format="f", digits = 1)
  
  if(!showSampleData(pin)) {
    v2[1] = "0.0000"
    v2[2] = "0.0000"
    v2[3] = "0.0"
    v2[4] = "0.0"
  }
  
  # check to make sure we not returning data which results with negative values
  deltaT = fp.pure - fp.mixture
  if(deltaT < 0) {
    return(getTableDataExp05C(pin))  
  } else {
    df = data.frame(v1, v2, stringsAsFactors = FALSE)
  
    # rename the column names here
    colnames(df) <- c("Measured/Calculated Variable",
                    "Numerical Value")
    return(df)
  }
}