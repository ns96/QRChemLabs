#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(plotly)
library(rhandsontable)
library(qrcode)

# Define server logic to load the proper module
shinyServer(function(input, output, session) {
  result <- reactive({
    # get the current semester
    query <- parseQueryString(session$clientData$url_search)
    if(!is.null(query[['semester']])) {
      currentSemester <<- query[['semester']]
      print(paste("Current Semester", currentSemester))
    }
    
    courseCode = input$courseCode
    pin = input$userpin
    userPin = paste0(courseCode, pin)
    #print(userPin)
    
    if(pin %in% devPins || userPin %in% validPins) {
      # CHEM1110 modules
      callModule(exp00, "experiment0", pin = userPin)
      callModule(exp01, "experiment1", pin = userPin)
      callModule(exp02, "experiment2", pin = userPin)
      callModule(exp03, "experiment3", pin = userPin)
      callModule(exp04, "experiment4", pin = userPin)
      callModule(exp05, "experiment5", pin = userPin)
      callModule(exp06, "experiment6", pin = userPin)
      callModule(exp07, "experiment7", pin = userPin)
      callModule(exp08, "experiment8", pin = userPin)
      callModule(exp09, "experiment9", pin = userPin)
      callModule(exp10, "experiment10", pin = userPin)
      callModule(exp11, "experiment11", pin = userPin)
      callModule(exp12, "experiment12", pin = userPin)
      callModule(exp13, "experiment13", pin = userPin)
      
      # CHEM1000 modules
      callModule(exp04B, "experiment4B", pin = userPin)
      callModule(exp04D, "experiment4D", pin = userPin)
      callModule(exp09B, "experiment9B", pin = userPin)
      callModule(exp09, "experiment10B", pin = userPin)
      callModule(exp10, "experiment11B", pin = userPin)
      callModule(exp12B, "experiment12B", pin = userPin)
      callModule(exp13B, "experiment13B", pin = userPin)
      
      # CHEM1210 data serving modules
      callModule(exp01C, "experiment1C", pin = userPin)
      callModule(exp02C, "experiment2C", pin = userPin)
      callModule(exp03C, "experiment3C", pin = userPin)
      callModule(exp04C, "experiment4C", pin = userPin)
      callModule(exp05C, "experiment5C", pin = userPin)
      callModule(exp06C, "experiment6C", pin = userPin)
      callModule(exp07C, "experiment7C", pin = userPin)
      callModule(exp08C, "experiment8C", pin = userPin)
      callModule(exp0910C, "experiment910C", pin = userPin)
      callModule(exp11C, "experiment11C", pin = userPin)
      callModule(exp12C, "experiment12C", pin = userPin)
      callModule(exp13C, "experiment13C", pin = userPin)
      
      result <- paste(" ", currentSemester, "/", "VALID PIN")
    } else {
      result <- paste(" ", currentSemester, "/", "PIN ERROR")
    }
  })
  
  output$status <- renderText(result())
})
