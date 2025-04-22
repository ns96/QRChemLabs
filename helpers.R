# helpers R functions

library(jsonlite)
library(RMariaDB)
library(httr)

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
showValid <- function(valid, ans, pin, rdigits = 2) {
  # check to see if to reset the pin to the adminPin
  if(isAdminUser(pin)) {
    pin = adminPin
  }
  
  validText = NULL
  if(is.numeric(ans)) {
    ans = format(round(ans, rdigits), nsmall = rdigits)
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
  
  #cat('Fit Results:', fit.numbers$intercept, fit.numbers$slope, fit.numbers$rsquare, "\n")
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

# function to check if pin is adminUser pin or a dev pin
isAdminUser = function(pin) {
  # only take the last 4 characters of the pin
  pin = substr(pin, nchar(pin)-3, nchar(pin))
  
  if(grepl(adminPin, pin, fixed = T)) {
    return(TRUE)
  } else if(pin %in% devPins) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

# function to see if to show sample data
showSampleData = function(pin) {
  if(isAdminUser(pin)) {
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
  pins = paste0(courseCode, courseLocker, masterPins)
  return(pins)
}

#
# options for connecting to the mysql database
#

# load the database connection information from yml config file
# https://solutions.posit.co/connections/db/best-practices/managing-credentials/
mysql <- config::get("dbInfo")
#mysql <- config::get("dbTest")

databaseName = mysql$database
dataTable = mysql$dataTable
courseTable = mysql$courseTable
semesterTable = mysql$semesterTable
userPinTable = mysql$userPinTable

# load the admin and dev pins and other pins from the database
loadPinsFromDB = function() {
  # connect to database
  db <- dbConnect(MariaDB(), dbname = databaseName, host = mysql$host, 
                  port = mysql$port, user = mysql$user, password = mysql$password)
  
  query <- sprintf("SELECT * FROM %s", userPinTable)
  
  print(query)
  
  # Submit the update query and disconnect
  df <- dbGetQuery(db, query)
  dbDisconnect(db)
  
  # iterate through return pins and place them in the correct global variable
  for(i in 1:nrow(df)) {
    row <- df[i, ]
    
    # check if the pin is the admin pin
    if(row[3] == 1) {
      adminPin <<- row[2]
    }
  
    # get the dev pins
    if(row[4] == 1) {
      devPins <<- c(devPins, row[2])
    }
  }
  
  print(paste("Admin Pin:", adminPin))
  print(paste("Dev Pins:", devPins))
}

# load the current semester from the database
loadSemester = function() {
  currentSemester = "UNKNOWN"
  
  tryCatch( {
    # connect to database
    db <- dbConnect(MariaDB(), dbname = databaseName, host = mysql$host, 
                    port = mysql$port, user = mysql$user, password = mysql$password)
    
    query <- sprintf("SELECT * FROM %s WHERE Current = 1", semesterTable)
    
    print(query)
    
    # Submit the update query and disconnect
    df <- dbGetQuery(db, query)
    dbDisconnect(db)
    
    currentSemester <- df[1, 2]
    print(paste("Current Semester:", currentSemester))
  },
  error = function(error_message) {
    print("***Error Reading From Database***")
    print(error_message)
    dbDisconnect(db)
  })
  
  return(currentSemester)
}

# get dataframe with all the semesters
getSemesters = function() {
  tryCatch( {
    # connect to database
    db <- dbConnect(MariaDB(), dbname = databaseName, host = mysql$host, 
                    port = mysql$port, user = mysql$user, password = mysql$password)
    
    query <- sprintf("SELECT * FROM %s", semesterTable)
    
    print(query)
    
    # Submit the update query and disconnect
    df <- dbGetQuery(db, query)
    dbDisconnect(db)
    
    return(df)
  },
  error = function(error_message) {
    print("***Error Reading From Database***")
    print(error_message)
    dbDisconnect(db)
  })
}

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

# function to save data to the backend database
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

# load the LLM keys from config file
llmKeys <- config::get("llmKeys")

# function to send prompts to Google's Gemini and support for passing in the temperature
#' Ask Google Gemini and yes it was generated by Gemini -- why not!
#'
#' Sends a prompt to the specified Google Gemini model using the generative
#' language API and retrieves the generated text response.
#'
#' @param prompt Character string. The user's prompt for the model.
#' @param api_key Character string. Your Google AI Studio or Google Cloud API key.
#'        It's recommended to store this securely (e.g., environment variable)
#'        rather than hardcoding directly in scripts.
#' @param model Character string. The name of the Gemini model to use
#'        (e.g., "gemini-pro", "gemini-1.0-pro", "gemini-1.5-pro-latest").
#'        Defaults to "gemini-2.0-flash". Check Google's documentation for available models.
#' @param temperature Numeric. Controls the randomness of the output. Lower values
#'        (e.g., 0.2) make the output more deterministic/focused, higher values (e.g., 0.9)
#'        make it more creative/diverse. Typically between 0 and 1, though the API might
#'        support slightly higher values. Defaults to 0.7. Set to NULL to use the API's default.
#' @param verbose Logical. If TRUE, prints the status messages during the request. Defaults to FALSE.
#'
#' @return A character string containing the model's generated response.
#'         Returns `NA_character_` with a warning if the response couldn't be parsed
#'         or if content was blocked by safety settings.
#'         An error (`stop()`) is raised if the API request itself fails (e.g., bad API key, network issue).

askGemini <- function(prompt, api_key = llmKeys$gemini, model = "gemini-2.0-flash", temperature = 0.7, verbose = FALSE) {
  # --- Input Validation ---
  if (!is.character(prompt) || nchar(prompt) == 0) {
    stop("Error: 'prompt' must be a non-empty character string.")
  }
  if (!is.character(api_key) || nchar(api_key) == 0) {
    stop("Error: 'api_key' must be a non-empty character string.")
  }
  if (!is.character(model) || nchar(model) == 0) {
    stop("Error: 'model' must be a non-empty character string.")
  }
  if (!is.null(temperature) && (!is.numeric(temperature) || temperature < 0)) {
    warning("Temperature should be a non-negative number. Proceeding anyway.")
  }
  
  # --- API Endpoint ---
  # Using v1beta which is commonly available. Check documentation for v1 if preferred.
  api_url <- sprintf("https://generativelanguage.googleapis.com/v1beta/models/%s:generateContent?key=%s",
                     model, api_key)
  if (verbose) message("API URL: ", api_url)
  
  # --- Construct Request Body ---
  request_body_list <- list(
    contents = list(
      list(parts = list(list(text = prompt)))
    )
  )
  
  # Add temperature to generationConfig if provided
  if (!is.null(temperature)) {
    request_body_list$generationConfig <- list(temperature = temperature)
  }
  
  # Convert the R list to a JSON string
  # auto_unbox = TRUE ensures single-element vectors become JSON scalars (like temperature)
  json_body <- jsonlite::toJSON(request_body_list, auto_unbox = TRUE)
  if (verbose) message("Request Body (JSON):\n", json_body)
  
  # --- Make the API Request ---
  if (verbose) message("Sending request to Gemini API...")
  response <- httr::POST(
    url = api_url,
    httr::add_headers(`Content-Type` = "application/json"),
    body = json_body # httr handles encoding correctly when Content-Type is set
    # and body is a character string representing JSON.
    # Removed encode = "json" as we manually created JSON string.
  )
  
  # --- Process the Response ---
  status_code <- httr::status_code(response)
  if (verbose) message("API Response Status Code: ", status_code)
  
  # Check for successful status code (200 OK)
  if (status_code != 200) {
    # Try to get error details from the response body
    error_content <- httr::content(response, "text", encoding = "UTF-8")
    stop(sprintf("API request failed with status %d.\nURL: %s\nResponse: %s",
                 status_code, api_url, error_content))
  }
  
  # Parse the successful JSON response
  parsed_response <- httr::content(response, as = "parsed", type = "application/json")
  if (verbose) {
    message("Parsed Response Structure:")
    str(parsed_response) # Show structure if verbose
  }
  
  
  # --- Extract Generated Text ---
  # Navigate the expected structure of the Gemini API response
  # Check for safety blocks first
  if (!is.null(parsed_response$promptFeedback$blockReason)) {
    reason <- parsed_response$promptFeedback$blockReason
    details <- ""
    if (!is.null(parsed_response$promptFeedback$safetyRatings)) {
      details <- paste(sapply(parsed_response$promptFeedback$safetyRatings, function(r) {
        paste0(r$category, ": ", r$probability)
      }), collapse=", ")
    }
    warning(paste("Content generation blocked by API safety settings. Reason:", reason, ". Details:", details))
    return(NA_character_) # Return NA for blocked content
  }
  
  # Try to extract the text content
  # Structure: response$candidates[[1]]$content$parts[[1]]$text
  generated_text <- tryCatch({
    parsed_response$candidates[[1]]$content$parts[[1]]$text
  }, error = function(e) {
    warning("Could not extract generated text from the API response structure. Error: ", e$message)
    NULL # Return NULL if extraction fails
  })
  
  # Check if text extraction was successful
  if (is.null(generated_text) || !is.character(generated_text)) {
    warning("Failed to extract text. Check API response structure or safety settings.")
    # Optionally print the full response for debugging if text is missing but no block reason found
    if (verbose || is.null(generated_text)) {
      message("Full Parsed Response Content:")
      print(parsed_response)
    }
    return(NA_character_) # Return NA if text is missing/invalid
  }
  
  if (verbose) message("Successfully extracted generated text.")
  return(generated_text)
}

# function to send prompts to OpenAI's ChatGPT API
askChatGPT <- function(prompt, api_key = llmKeys$openai, model = "gpt-4.1-mini", temperature = 0.7) {
  # --- Input Validation ---
  if (!is.character(prompt) || nchar(prompt) == 0) {
    stop("Error: 'prompt' must be a non-empty character string.")
  }
  if (!is.character(api_key) || nchar(api_key) == 0) {
    stop("Error: 'api_key' must be a non-empty character string.")
  }
  if (!is.character(model) || nchar(model) == 0) {
    stop("Error: 'model' must be a non-empty character string.")
  }
  if (!is.null(temperature) && (!is.numeric(temperature) || temperature < 0)) {
    warning("Temperature should be a non-negative number. Proceeding anyway.")
  }
  
  # OpenAI Chat Completions API endpoint
  url <- "https://api.openai.com/v1/chat/completions"
  
  headers <- add_headers(
    "Authorization" = paste("Bearer", api_key),
    "Content-Type" = "application/json"
  )
  
  body <- list(
    model = model,
    messages = list(list(role = "user", content = prompt)),
    temperature = temperature
  )
  
  response <- POST(url, headers, body = toJSON(body, auto_unbox = TRUE))
  
  if (status_code(response) != 200) {
    stop("Request failed: ", content(response, "text"))
  }
  
  parsed <- content(response, as = "parsed", type = "application/json")
  return(parsed$choices[[1]]$message$content)
}

# function to send prompts to DeepSeek's API
askDeepSeek <- function(prompt, api_key=llmKeys$deepseek, model = "deepseek-chat", temperature = 0.7) {
  
  # DeepSeek's endpoint (assuming OpenAI-compatible)
  url <- "https://api.deepseek.com/v1/chat/completions"
  
  # Setup HTTP request headers
  headers <- add_headers(
    "Authorization" = paste("Bearer", api_key),   # DeepSeek API key
    "Content-Type" = "application/json"           # JSON content type
  )
  
  # Build request body
  body <- list(
    model = model,  # e.g., "deepseek-chat" or any available model
    messages = list(
      list(role = "user", content = prompt)  # Single user message
    ),
    temperature = temperature  # Control randomness
  )
  
  # Send POST request
  response <- POST(
    url,
    headers,
    body = toJSON(body, auto_unbox = TRUE)
  )
  
  # Check for successful response
  if (status_code(response) != 200) {
    stop("Request failed: ", content(response, "text"))
  }
  
  # Parse and return the content of the assistant's reply
  parsed <- content(response, as = "parsed", type = "application/json")
  return(parsed$choices[[1]]$message$content)
}

# function to return a fluid row to sending propmts to Gemini and other LLM
# @ns namespace function
getLLMPromptUIRow <- function(ns) {
  row <- fluidRow(
    # add drop down for selecting the llm model
    column(4, selectInput(ns("llmModel"), "Select LLM Model", choices = c("Google Gemini", "ChatGPT", "DeepSeek", "Show Prompt"))),
    
    # add slider input for selecting temperature
    column(4, sliderInput(ns("llmTemp"), "Temperature", min = 0, max = 1, value = 0.7)),
    
    column(4, actionButton(ns("llmGenerate"), "Generate Abstract"))
  )
  
  return (row)
}

# function to send prompt to LLM API and display Abstract
displayAbstract <- function(abstractPrompt, model, temp, pin) {
  dialogTitle  = paste("Generated Abstract --", model)
  
  # check if to use google gemini
  if(model == "Google Gemini") {
    response = askGemini(abstractPrompt, temperature = temp)
  } else if(model == "ChatGPT") {
    response = askChatGPT(abstractPrompt, temperature = temp)
  } else if(model == "DeepSeek") {
    response = askDeepSeek(abstractPrompt, temperature = temp)
  } else {
    # assume we just want to show the prompt
    dialogTitle = "LLM Prompt"
    response = abstractPrompt
  }
  
  # get the word count of the response
  wordCount = countWords(response)
  response = paste0("Word Count: ", wordCount, " || ", response)
  
  # display the generated abstract
  showModal(modalDialog(
    title = dialogTitle,
    response,
    easyClose = TRUE,
    footer = NULL
  ))
}

# Function to count words in a string of text
countWords <- function(text) {
  # Remove leading/trailing whitespace and split by one or more spaces
  words <- unlist(strsplit(trimws(text), "\\s+"))
  
  # Return the number of words
  return(length(words))
}


# ************************************************
# Interesting links for this project
# Reactive variables to get around scope of reactive/observer block of code
# https://stackoverflow.com/questions/50253655/reset-r-shiny-actionbutton-to-use-it-more-than-once
