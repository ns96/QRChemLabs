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