#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shinydashboard)
library(plotly)
library(rhandsontable)
library(titrationCurves)

# Define UI for application that display chemistry tables
shinyUI(dashboardPage(
  dashboardHeader(title = "QRChem Labs v0.7.6 (04/14/2025)",
                  titleWidth = 325),
  
  dashboardSidebar(
    selectInput("courseCode", "Course Code:", courseCodes),
    
    passwordInput("userpin", "Group Pin:", value = devPins[2]), # development is 1,3,4 otherwise 2 for production
    #passwordInput("userpin", "Group Pin:", value = ""),        # production
    
    # menu items for experiments
    sidebarMenu(
      menuItem("Experiment 01 (CHEM1NN0)", tabName = "experiment1", icon = icon("calculator")),
      menuItem("Experiment 02 (CHEM1NN0)", tabName = "experiment2", icon = icon("cube")),
      menuItem("Experiment 03 (CHEM1NN0)", tabName = "experiment3", icon = icon("filter")),
      menuItem("Experiment 04 (CHEM1110)", tabName = "experiment4", icon = icon("tint")),
      #menuItem("Experiment 04 (CHEM1000)", tabName = "experiment4B", icon = icon("vial")),
      menuItem("Experiment 04 (CHEM1000)", tabName = "experiment4D", icon = icon("thermometer-full")),
      menuItem("Experiment 05 (CHEM1NN0)", tabName = "experiment5", icon = icon("thermometer-full")),
      menuItem("Experiment 06 (CHEM1NN0)", tabName = "experiment6", icon = icon("percent")),
      menuItem("Experiment 07 (CHEM1NN0)", tabName = "experiment7", icon = icon("filter")),
      menuItem("Experiment 08 (CHEM1NN0)", tabName = "experiment8", icon = icon("fire")),
      menuItem("Experiment 09 (CHEM1110)", tabName = "experiment9", icon = icon("percent")),
      menuItem("Experiment 09 (CHEM1000)", tabName = "experiment9B", icon = icon("percent")),
      menuItem("Experiment 10 (CHEM1110)", tabName = "experiment10", icon = icon("percent")),
      menuItem("Experiment 10 (CHEM1000)", tabName = "experiment10B", icon = icon("percent")),
      menuItem("Experiment 11 (CHEM1110)", tabName = "experiment11", icon = icon("thermometer-empty")),
      menuItem("Experiment 11 (CHEM1000)", tabName = "experiment11B", icon = icon("percent")),
      menuItem("Experiment 12 (CHEM1110)", tabName = "experiment12", icon = icon("thermometer-empty")),
      menuItem("Experiment 12 (CHEM1000)", tabName = "experiment12B", icon = icon("cookie-bite")),
      menuItem("Experiment 13 (CHEM1110)", tabName = "experiment13", icon = icon("asterisk")),
      menuItem("Experiment 13 (CHEM1000)", tabName = "experiment13B", icon = icon("lemon")),
      
      hr(),
      
      menuItem("Experiment 01 (CHEM1210)", tabName = "experiment1C", icon = icon("car-battery")),
      menuItem("Experiment 02 (CHEM1210)", tabName = "experiment2C", icon = icon("car-battery")),
      menuItem("Experiment 03 (CHEM1210)", tabName = "experiment3C", icon = icon("lightbulb")),
      menuItem("Experiment 04 (CHEM1210)", tabName = "experiment4C", icon = icon("thermometer-quarter")),
      menuItem("Experiment 05 (CHEM1210)", tabName = "experiment5C", icon = icon("thermometer-full")),
      menuItem("Experiment 06 (CHEM1210)", tabName = "experiment6C", icon = icon("chart-line")),
      menuItem("Experiment 07 (CHEM1210)", tabName = "experiment7C", icon = icon("chart-bar")),
      menuItem("Experiment 08 (CHEM1210)", tabName = "experiment8C", icon = icon("chart-bar")),
      menuItem("Experiment 09 (CHEM1210)", tabName = "experiment910C", icon = icon("flask")),
      menuItem("Experiment 11 (CHEM1210)", tabName = "experiment11C", icon = icon("calculator")),
      menuItem("Experiment 12 (CHEM1210)", tabName = "experiment12C", icon = icon("lemon")),
      menuItem("Experiment 13 (CHEM1210)", tabName = "experiment13C", icon = icon("calculator")),  

      hr(),
      
      #menuItem("Experiment 05 (CHEM1000)", tabName = "experiment5B", icon = icon("vial")),
      #menuItem("Experiment 13 (CHEM1000)", tabName = "experiment13B", icon = icon("cookie-bite")),
      #menuItem("Experiment 14 (CHEM1000)", tabName = "experiment14B", icon = icon("lemon")),
      
      menuItem("Experiment 00", tabName = "experiment0", icon = icon("code"))
    ),
    hr(),
    textOutput("status")
  ),
  
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "experiment1",
              exp01UI("experiment1")
      ),
      
      # Second tab content
      tabItem(tabName = "experiment2",
              exp02UI("experiment2")
      ),
      
      # third tab content
      tabItem(tabName = "experiment3",
              exp03UI("experiment3")
      ),
      
      # forth tab content
      tabItem(tabName = "experiment4",
              exp04UI("experiment4")
      ),
      
      # fifth tab content
      tabItem(tabName = "experiment5",
              exp05UI("experiment5")
      ),
      
      # sixth tab content
      tabItem(tabName = "experiment6",
              exp06UI("experiment6")
      ),
      
      # seventh tab content
      tabItem(tabName = "experiment7",
              exp07UI("experiment7")
      ),
      
      # eight tab content
      tabItem(tabName = "experiment8",
              exp08UI("experiment8")
      ),
      
      # ninth tab content
      tabItem(tabName = "experiment9",
              exp09UI("experiment9")
      ),
      
      # tenth tab content
      tabItem(tabName = "experiment10",
              exp10UI("experiment10")
      ),
      
      # eleventh tab content
      tabItem(tabName = "experiment11",
              exp11UI("experiment11")
      ),
      
      # twelfth tab content
      tabItem(tabName = "experiment12",
              exp12UI("experiment12")
      ),
      
      # thirteenth tab content
      tabItem(tabName = "experiment13",
              exp13UI("experiment13")
      ),
      
      #
      # CHEM1000 experiment modules
      #
      
      tabItem(tabName = "experiment4B",
              exp04BUI("experiment4B")
      ),
      
      tabItem(tabName = "experiment4D",
              exp04DUI("experiment4D")
      ),
      
      tabItem(tabName = "experiment9B",
              exp09BUI("experiment9B")
      ),
      
      tabItem(tabName = "experiment10B",
              exp09UI("experiment10B", lab.number = 10)
      ),
      
      tabItem(tabName = "experiment11B",
              exp10UI("experiment11B", lab.number = 11)
      ),
      
      tabItem(tabName = "experiment12B",
              exp12BUI("experiment12B")
      ),
      
      tabItem(tabName = "experiment13B",
              exp13BUI("experiment13B")
      ),
      
      #
      # CHEM1210 experiment modules
      #
      tabItem(tabName = "experiment1C",
              exp01CUI("experiment1C")
      ),
      
      tabItem(tabName = "experiment2C",
              exp02CUI("experiment2C")
      ),
      
      tabItem(tabName = "experiment3C",
              exp03CUI("experiment3C")
      ),
      
      tabItem(tabName = "experiment4C",
              exp04CUI("experiment4C")
      ),
      
      tabItem(tabName = "experiment5C",
              exp05CUI("experiment5C")
      ),
      
      tabItem(tabName = "experiment6C",
              exp06CUI("experiment6C")
      ),
      
      tabItem(tabName = "experiment7C",
              exp07CUI("experiment7C")
      ),
      
      tabItem(tabName = "experiment8C",
              exp08CUI("experiment8C")
      ),
      
      tabItem(tabName = "experiment910C",
              exp0910CUI("experiment910C")
      ),
      
      tabItem(tabName = "experiment11C",
              exp11CUI("experiment11C")
      ),
      
      tabItem(tabName = "experiment12C",
              exp12CUI("experiment12C")
      ),
      
      tabItem(tabName = "experiment13C",
              exp13CUI("experiment13C")
      ),
      
      # development module
      tabItem(tabName = "experiment0",
              exp00UI("experiment0")
      )
    )
  )
))
