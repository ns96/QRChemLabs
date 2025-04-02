# helpers R scripts

library(jsonlite)
library(RMariaDB)

# https://stackoverflow.com/questions/37218319/plotly-not-creating-linear-trend-line
# function to generate plotly
getRegressionPlot = function(df, ptitle, xlabel, ylabel) {
  fit <- lm(y ~ x, data = df)
  
  p <- plot_ly(df, x = ~x, y = ~y, type = 'scatter', mode = 'markers') %>%
    add_trace(x = ~x, y = fitted(fit), mode = "lines") %>%
    layout(title = ptitle,
           xaxis = list(title = xlabel),
           yaxis = list(title = ylabel)) %>%
    layout(showlegend = FALSE)
  
  return(p)
}

# function to plot a scatter plot along with a fitted line
getFittedLinePlot = function(df, ptitle, xlabel, ylabel) {
  p <- plot_ly(df, x = ~x, y = ~y1, type = 'scatter', mode = 'markers') %>%
    add_trace(x = ~x, y = ~y2, mode = "lines") %>%
    layout(title = ptitle,
           xaxis = list(title = xlabel),
           yaxis = list(title = ylabel)) %>%
    layout(showlegend = FALSE)
  
  return(p)  
}

# function to plot a simple line plot 
getLinePlot = function(df, ptitle, xlabel, ylabel, smooth=TRUE) {
  if(smooth) {
    # Scatterplot with Loess Smoother
    # https://plot.ly/r/graphing-multiple-chart-types/
    p <- plot_ly(df, x = ~x_data, color = I("black")) %>%
      add_markers(y = ~y_data, text = rownames(df), showlegend = FALSE) %>%
      add_lines(y = ~fitted(loess(y_data ~ x_data)),
                line = list(color = '#07A4B5'))
  } else {
    p <- plot_ly(df, x = ~x_data, y = ~y_data, type = 'scatter', mode = 'lines+markers')
  }
  
  p <- p %>%
    layout(title = ptitle,
           xaxis = list(title = xlabel),
           yaxis = list(title = ylabel))
  return(p)
}

# function to get a pie plot
getPiePlot = function(df, ptitle) {
  p <- plot_ly(df, labels = ~Compound, values = ~Amount, type = 'pie') %>%
    layout(xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
           yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  
  return(p)
}

# function to get a bar plot
getBarPlot = function(x_data, y_data, pname) {
  p <- plot_ly(x = x_data, y = y_data, name = pname, type = 'bar') %>%
    layout(yaxis = list(title = pname))
  return(p)
}

# function to get a smooth line plot
getSmoothPlot = function(data, xlabel, ylabel, n1 = "real", n2 = "exp") {
  p <- plot_ly() %>%
    add_lines(x = data$x1, y = data$y1, name = n1, line = list(shape = "spline")) %>%
    add_lines(x = data$x2, y = data$y2, name = n2, line = list(shape = "spline")) %>%
    layout(xaxis = list(title = xlabel), yaxis = list(title = ylabel))
  
  return(p)
}



# function to generate message if answer is valid
showValid <- function(valid, ans, pin) {
  # check to see if to reset the pin to the adminPin
  if(isAdminUser(pin)) {
    pin = adminPin
  }
  
  validText = NULL
  if(is.numeric(ans)) {
    ans = round(ans, 2)
  }
  
  if(!is.na(valid) && valid) {
    if(pin == adminPin) {
      validText = paste("(VALID) =>", ans)
    } else {
      validText = "VALID"
    } 
  } else {
    if(pin == adminPin) {
      validText = paste("(INVALID) =>", ans)
    } else {
      validText = "INVALID"
    }
  }
  
  return(validText)
}

# function to get the percent error
percentError <- function(actual, measured) {
  error = (abs(actual - measured)/actual)*100
  return(paste("Percent Error: ", sprintf(error, fmt = '%#.1f'), "%"))
}

# function to return the vector with a percent of percentage
# direction specifies if we positive = 1, or negative = -1
addNoise <- function(actualData, percent, r.values=FALSE, r.digits = 0, direction = 0) {
  noiseSigma = percent * actualData
  
  if(direction == 0) {
    noise = noiseSigma * runif(length(actualData), -1, 1)
  } else {
    noise = noiseSigma * direction
  }
  
  noisyData = actualData + noise
  
  # see if to round the numbers
  if(r.values) {
    noisyData = round(noisyData, digits = r.digits)  
  }
  
  return(noisyData)
}

# function to do linear fit and return parameters in list
doLinearFit <- function(x, y) {
  fit = lm(formula = y ~ x)
  
  fit.numbers = list()
  fit.numbers$intercept = summary(fit)$coefficients[1,1]
  fit.numbers$slope = summary(fit)$coefficients[2,1]
  fit.numbers$rsquare = summary(fit)$r.square
  
  cat('Fit Results:', fit.numbers$intercept, fit.numbers$slope, fit.numbers$rsquare, "\n")
  return(fit.numbers)
}

# function to do a power fit
doPowerFit <- function(df) {
  fit <- nls(y~b*x^z, start = list(b = 100, z = -1), data = df)
  
  fit.numbers = list()
  fit.numbers$b = summary(fit)$coefficients[1,1]
  fit.numbers$z = summary(fit)$coefficients[2,1]
  fit.numbers$predicted = fit$m$predict(df$x)
  
  equation = paste0("y=Ax^B || y = ", round(fit.numbers$b), "x^", round(fit.numbers$z, 2))
  fit.numbers$equaton = equation
  
  return(fit.numbers)
}

# function to return html formatted output of saved data
getSavedData = function(pin, expName, html.format = TRUE) {
  df = loadFromDB(pin, expName)
  
  q.labels = vector()
  store.labels = TRUE
  
  htmlString = ""
  
  # this should be done using lapply
  for(i in 1:nrow(df)) { # change to 1:nrow(df) to process all the data instead of last one
    qlist = fromJSON(df[i, 5])
    q.text = ""
    q.values = vector()
    
    for(q in names(qlist)) {
      # if else statements to change the question labels
      if(startsWith(q, "q")) {
        q.label = gsub("q", "Question ", q)
      } else if(startsWith(q, "u")) {
        q.label = gsub("u", "Unknown ", q)
      } else if(startsWith(q, "t")) {
        q.label = gsub("t", "Test Tube ", q)
      } else if(startsWith(q, "d")) {
        q.label = gsub("d", "Data", q)
      } else {
        q.label = q
      }

      q.value = qlist[[q]]
      if(nchar(q.value) > 50) {
        q.value = paste0(strtrim(q.value, 50), " ...")
      }
      
      q.text = paste0(q.text, "<b><i>", q.label, " : </b></i>", q.value, "<br>")
      
      q.values = c(q.values, q.value)
      
      # store the question labels needed for the dataframe
      if(store.labels) {
        q.labels = c(q.labels, q.label)
      }
    }
    
    # append to the html string
    save.time = df[i,6]
    group.pin = df[i,3]

    htmlString = paste(htmlString, q.text, "<br><b>", "Data # ", i ,"Date/Time:</b>", save.time , " || <b>Group Pin:</b>",
                       group.pin, "<hr>")
    
    # save question value to dataframe with the column name being the pin and save time
    c.title = paste0(group.pin, " || ", save.time)
    
    if(store.labels) {
      sdDF = data.frame("Question" = q.labels)
      store.labels = FALSE
    } 
    
    print(paste("Adding data", c.title))
    sdDF["place.holder"] = q.values
    names(sdDF)[i+1] <- c.title
  }

  # check if to return html or dataframe
  if(html.format) {
    return(htmlString)
  } else {
    return(sdDF)
  }
}

# get the course codes
getCourseCodes = function() {
  if(!is.null(coursesDF)) {
    return(coursesDF[["Code"]])
  } else {
    return(c("D001", "D003", "D005"))
  }
}

# function to check if pin is adminUser pin
isAdminUser = function(pin) {
  if(grepl(adminPin, pin, fixed = T) | 
     grepl("1210", pin, fixed = T) | 
     grepl("1110", pin, fixed = T) |
     grepl("1000", pin, fixed = T)) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

# function to see if to show sample data
showSampleData = function(pin) {
  if(isAdminUser(pin)) {
    return(TRUE)
  } else if(grepl("5555", pin, fixed = T)) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

# get valid pins for the semester
getValidPins = function() {
  pins = vector()
  
  if(!is.null(coursesDF)) {
    # process row in for loop.  should really be done using apply, but this
    # code is just more reliable, not to mention dataset is small enough
    # not to cause any performance problems?
    for(i in 1:nrow(coursesDF)) {
      row <- coursesDF[i, ]
      pins = c(pins, getCoursePins(row[2], row[4]))
    }
  } else {
    # load dummy or hard coded values
    courseCodes = c("D001", "D003", "D005")
    courseLockers = c("B", "F", "H")
  
    # Set valid pins. TO-DO These needs to be read from server
    pins = c(pins, getCoursePins(courseCodes[1], courseLockers[1]))
    pins = c(pins, getCoursePins(courseCodes[2], courseLockers[2]))
    pins = c(pins, getCoursePins(courseCodes[3], courseLockers[3]))
  }
  
  return(pins)
}

# generate pins for a particular course
masterPins = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12")
getCoursePins = function(courseCode, courseLocker) {
  return(paste0(courseCode, courseLocker, masterPins))
}

#
# options for connecting to the mysql database
#

# load the database connection information from yml config file
mysql <- config::get("dbInfo")

databaseName = mysql$database
dataTable = mysql$dataTable
courseTable = mysql$courseTable

# get valid pins for current semester from database
loadCourses = function(semester) {
  tryCatch( {
    # connect to database
    db <- dbConnect(MariaDB(), dbname = databaseName, host = mysql$host, 
                    port = mysql$port, user = mysql$user, password = mysql$password)
    
    query <- sprintf("SELECT * FROM %s WHERE Semester = '%s'", courseTable, semester)
    
    print(query)
    
    # Submit the update query and disconnect
    df <- dbGetQuery(db, query)
    dbDisconnect(db)
    
    coursesDF <<- df # global dataframe variable
  },
  error = function(error_message) {
    print("***Error Reading From Database***")
    print(error_message)
    dbDisconnect(db)
    coursesDF <<- NULL # global dataframe variable set to null
  })
}

# function to save data to backend database
saveToDB = function(pin, expName, questionList) {
  if(any(questionList == "" || anyNA(questionList))) {
    return("Missing Data / Not Saving ...")
  }
  
  tryCatch( {
    jsonData = toJSON(questionList)
    
    # connect to database
    db <- dbConnect(MariaDB(), dbname = databaseName, host = mysql$host, 
                    port = mysql$port, user = mysql$user, password = mysql$password)
    
    query <- sprintf(
      "INSERT INTO %s (%s) VALUES ('%s')",
      dataTable, 
      paste(c("ID", "Semester", "Pin", "ExpName", "Questions"), collapse = ", "),
      paste(c("0", currentSemester, pin, expName, jsonData), collapse = "', '")
    )
    
    print(query)
    
    # Submit the update query and disconnect
    dbExecute(db, query)
    dbDisconnect(db)
    
    return("Data Saved ...")
  },
  error = function(error_message) {
    print("***Error Saving to Database***")
    print(error_message)
    dbDisconnect(db)
    return("Error Saving to Database")
  },
  warning = function(warning_message) {
    print("***Warning Saving to Database***")
    print(warning_message)
    dbDisconnect(db)
    return("Warning Saving to Database")
  })
}

# function to load data from backend database
loadFromDB = function(pin, expName) {
  # check to see if to reset the pin to the adminPin
  if(grepl(adminPin, pin)) {
    pin = adminPin
  }
  
  tryCatch( {
    # connect to database
    db <- dbConnect(MariaDB(), dbname = databaseName, host = mysql$host, 
                    port = mysql$port, user = mysql$user, password = mysql$password)
    
    if(adminPin != pin) {
      query <- sprintf("SELECT * FROM %s WHERE Semester = '%s' AND Pin = '%s' AND ExpName ='%s'",
                       dataTable, currentSemester, pin, expName)
    } else {
      # admin account return all the data
      query <- sprintf("SELECT * FROM %s WHERE Semester = '%s' AND ExpName ='%s'",
                       dataTable, currentSemester, expName)
    }
    
    print(query)
    
    # Submit the get query and disconnect
    df <- dbGetQuery(db, query)
    dbDisconnect(db)
    
    return(df)
  },
  error = function(error_message) {
    print("***Error Reading From Database***")
    print(error_message)
    dbDisconnect(db)
    return(NULL)
  },
  warning = function(warning_message) {
    print("***Warning Reading from Database***")
    print(warning_message)
    return("Warning Reading From Database")
  })
}

# ************************************************
# Interesting links for this project
# Reactive variables to get around scope of reactive/observer block of code
# https://stackoverflow.com/questions/50253655/reset-r-shiny-actionbutton-to-use-it-more-than-once

# 
# DEBUG Code
#
#currentSemester = "FALL_2019"
#loadCourses(currentSemester)
