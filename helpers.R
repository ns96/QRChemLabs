# helpers R functions

library(jsonlite)
library(RMariaDB)
library(httr)
library(ggplot2)

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

# function to plot a scatter plot with a linear fit line for each of two series
# df must have columns x, y1 and y2 -- the same shape getFittedLinePlot uses
# the fit lines are hidden from the legend so only the series names show
getScatterFitPlot = function(df, ptitle, xlabel, ylabel, n1 = "series 1", n2 = "series 2") {
  # the rhandsontable round trip can hand these columns back as characters
  x  = suppressWarnings(as.numeric(df$x))
  y1 = suppressWarnings(as.numeric(df$y1))
  y2 = suppressWarnings(as.numeric(df$y2))
  
  # colours so that each series and its fit line match
  seriesColours = c("#1f77b4", "#d62728")
  
  # linear fit line for a series, or NULL when a fit is not possible
  # (constant data, fewer than two distinct x values, all values missing, ...)
  fitLineFor = function(y) {
    usable = is.finite(x) & is.finite(y)
    
    if(sum(usable) < 2 || length(unique(x[usable])) < 2) {
      return(NULL)
    }
    
    tryCatch({
      fit = lm(y[usable] ~ x[usable])
      theLine = data.frame(x = x[usable], y = as.numeric(fitted(fit)))
      
      # order by x so the line does not zigzag back on itself
      return(theLine[order(theLine$x), ])
    }, error = function(error_message) {
      cat("***Could not fit scatter series:", conditionMessage(error_message), "***\n")
      return(NULL)
    })
  }
  
  line1 = fitLineFor(y1)
  line2 = fitLineFor(y2)
  
  p <- plot_ly() %>%
    add_trace(x = x, y = y1, name = n1, type = 'scatter', mode = 'markers',
              marker = list(color = seriesColours[1]))
  
  if(!is.null(line1)) {
    p <- p %>% add_trace(x = line1$x, y = line1$y, name = paste(n1, "fit"),
                         type = 'scatter', mode = 'lines', showlegend = FALSE,
                         line = list(color = seriesColours[1]))
  }
  
  p <- p %>%
    add_trace(x = x, y = y2, name = n2, type = 'scatter', mode = 'markers',
              marker = list(color = seriesColours[2]))
  
  if(!is.null(line2)) {
    p <- p %>% add_trace(x = line2$x, y = line2$y, name = paste(n2, "fit"),
                         type = 'scatter', mode = 'lines', showlegend = FALSE,
                         line = list(color = seriesColours[2]))
  }
  
  p <- p %>%
    layout(title = ptitle,
           xaxis = list(title = xlabel),
           yaxis = list(title = ylabel))
  
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

# get connected scatter plot
getConnectedScatterPlot = function(data, xlabel, ylabel, n1 = "real", n2 = "exp") {
  p <- plot_ly() %>%
    add_trace(x = data$x1, y = data$y1, name = n1, type = 'scatter', mode = 'lines+markers') %>%
    add_trace(x = data$x2, y = data$y2, name = n2, type = 'scatter', mode = 'lines+markers') %>%
    layout(xaxis = list(title = xlabel), yaxis = list(title = ylabel))
  
  return(p)
}

# function to plot a line with a shaded confidence band around it
# df must have columns x, y, lower and upper -- lower and upper are the band edges
# points is an optional data frame with columns x and y drawn as markers on top
# of the band (e.g. measured lab values)
getConfidenceBandPlot = function(df, ptitle, xlabel, ylabel,
                                 points = NULL,
                                 bandColour = "#1f77b4", pointColour = "#d62728",
                                 xBreaks = waiver(), baseSize = 13) {
  # the rhandsontable round trip can hand these columns back as characters
  x     = suppressWarnings(as.numeric(df$x))
  y     = suppressWarnings(as.numeric(df$y))
  lower = suppressWarnings(as.numeric(df$lower))
  upper = suppressWarnings(as.numeric(df$upper))

  # order by x so the line and the band do not zigzag back on themselves
  theOrder = order(x)
  plotDF = data.frame(x = x, y = y, lower = lower, upper = upper)[theOrder, ]

  p = ggplot(plotDF, aes(x = .data$x)) +
    geom_ribbon(aes(ymin = .data$lower, ymax = .data$upper),
                fill = bandColour, alpha = 0.2) +
    geom_line(aes(y = .data$y), color = bandColour, linewidth = 1.2) +
    scale_x_continuous(breaks = xBreaks) +
    labs(title = ptitle, x = xlabel, y = ylabel) +
    theme_minimal(base_size = baseSize) +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"),
          panel.grid.minor = element_blank())

  # add the optional benchmark points
  if(!is.null(points) && nrow(points) > 0) {
    pointDF = data.frame(x = suppressWarnings(as.numeric(points$x)),
                         y = suppressWarnings(as.numeric(points$y)))

    p = p + geom_point(data = pointDF, aes(x = .data$x, y = .data$y),
                       color = pointColour, size = 3)
  }

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
    # no live data and no usable cache: show nothing rather than the old dummy
    # codes, which would have let made-up pins through as valid logins
    cat("***No course data available -- course list is empty***\n")
    return(character(0))
  }
}

# function to check if pin is adminUser pin or a dev pin
isAdminUser = function(pin) {
  # only take the last 4 characters of the pin
  pin = substr(pin, nchar(pin)-3, nchar(pin))
  
  # require a usable admin pin, otherwise grepl("") would match every pin
  if(!is.null(adminPin) && nzchar(adminPin) && grepl(adminPin, pin, fixed = T)) {
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
  
  if(!is.null(coursesDF) && nrow(coursesDF) > 0) {
    # process row in for loop.  should really be done using apply, but this
    # code is just more reliable, not to mention dataset is small enough
    # not to cause any performance problems?
    #
    # accessed by name so that the live course frame and the cached course frame
    # (which has no ID column) behave identically
    for(i in 1:nrow(coursesDF)) {
      row <- coursesDF[i, ]
      pins = c(pins, getCoursePins(row$Code, row$Locker))
    }
  } else {
    # no live data and no usable cache: accept no pins rather than the old
    # hard-coded dummy courses, which would have let made-up pins log in
    cat("***No course data available -- no valid pins***\n")
    pins = character(0)
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

#
# Reference data cache (semester / courses / pins)
#
# Written after every successful live read and read back only when the database
# is unreachable.  This lets the app start and validate logins during an outage.
#
# The cache lives in the app bundle, so the copy generated at deploy time is what
# the hosted app falls back to.  Writes made while the app is running are NOT
# durable on shinyapps.io (ephemeral filesystem) -- only on a real disk (local
# dev, own VM).  Regenerate before deploying with:
#     Rscript tools/make_reference_cache.R
#

referenceCachePath = "data/qrchem_cache.json"
referenceCacheVersion = 1L

# set to TRUE by any loader that had to fall back to the cache
referenceFromCache = FALSE

# Should the cache be refreshed on this run?
#
# Refreshing happens automatically whenever the app runs on a desktop, so there is
# no separate step to remember before deploying.  It is skipped on hosted Posit
# platforms (shinyapps.io / Connect) where the filesystem is ephemeral and the
# cache shipped inside the bundle must not be rewritten at runtime.
#
# Override with QRCHEM_WRITE_CACHE=1 to force on, or =0 to force off.
shouldWriteReferenceCache = function() {
  override = Sys.getenv("QRCHEM_WRITE_CACHE", unset = NA_character_)
  if(!is.na(override)) {
    return(identical(override, "1"))
  }

  # hosted Posit platforms -- ephemeral filesystem, leave the bundled cache alone
  if(nzchar(Sys.getenv("RSTUDIO_PRODUCT"))) {
    return(FALSE)
  }
  if(nzchar(Sys.getenv("SHINYAPPS_ACCOUNT"))) {
    return(FALSE)
  }
  if(nzchar(Sys.getenv("CONNECT_SERVER"))) {
    return(FALSE)
  }

  # desktop (or an own server with a real disk) -- keep the cache fresh
  return(TRUE)
}

# read the cached reference data; returns NULL when unavailable or invalid
readReferenceCache = function() {
  tryCatch({
    if(!file.exists(referenceCachePath)) {
      cat("No reference cache at", referenceCachePath, "\n")
      return(NULL)
    }

    cache = fromJSON(referenceCachePath, simplifyDataFrame = TRUE)

    # validate before trusting it
    if(is.null(cache$schemaVersion) || cache$schemaVersion != referenceCacheVersion) {
      cat("***Reference cache ignored: unsupported schema version***\n")
      return(NULL)
    }

    if(is.null(cache$semester) || !nzchar(cache$semester)) {
      cat("***Reference cache ignored: no semester***\n")
      return(NULL)
    }

    cat("Using reference cache generated", cache$generatedAt, "\n")
    return(cache)
  },
  error = function(error_message) {
    cat("***Reference cache unreadable:", conditionMessage(error_message), "***\n")
    NULL
  })
}

# build the course data.frame from the cache; NULL when unavailable
cachedCourses = function() {
  cache = readReferenceCache()
  if(is.null(cache)) {
    return(NULL)
  }

  referenceFromCache <<- TRUE

  courses = cache$courses
  if(is.null(courses) || length(courses) == 0) {
    cat("***Reference cache has no courses***\n")
    return(NULL)
  }

  if(!is.data.frame(courses)) {
    courses = as.data.frame(courses, stringsAsFactors = FALSE)
  }

  rownames(courses) = NULL
  return(courses)
}

# write the current live reference data to the cache (atomic)
writeReferenceCache = function() {
  # never overwrite the cache with values that came from the cache
  if(referenceFromCache) {
    cat("Reference data came from the cache - not rewriting it\n")
    return(invisible(FALSE))
  }

  tryCatch({
    snapshot = list(
      schemaVersion = referenceCacheVersion,
      generatedAt = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
      source = "mysql",
      semester = as.character(currentSemester),
      # only the columns the app needs, so the cached frame matches the live one
      courses = if(is.null(coursesDF)) {
        data.frame(Code = character(0), Semester = character(0), Locker = character(0),
                   stringsAsFactors = FALSE)
      } else {
        data.frame(
          Code = as.character(coursesDF$Code),
          Semester = as.character(coursesDF$Semester),
          Locker = as.character(coursesDF$Locker),
          stringsAsFactors = FALSE
        )
      },
      pins = list(
        admin = if(is.null(adminPin)) "" else as.character(adminPin),
        dev = as.character(devPins)
      )
    )

    json = toJSON(snapshot, auto_unbox = TRUE, na = "null", null = "null")

    tmpPath = paste0(referenceCachePath, ".tmp")
    writeLines(json, tmpPath, useBytes = TRUE)

    # atomic swap so a crash can never leave a truncated cache behind
    if(file.exists(referenceCachePath)) {
      file.remove(referenceCachePath)
    }
    file.rename(tmpPath, referenceCachePath)

    cat("Reference cache written:", referenceCachePath, "\n")
    invisible(TRUE)
  },
  error = function(error_message) {
    cat("***Could not write reference cache:", conditionMessage(error_message), "***\n")
    invisible(FALSE)
  })
}

# load the admin and dev pins and other pins from the database
loadPinsFromDB = function() {
  db = NULL
  
  tryCatch( {
    # connect to database
    db <- dbConnect(MariaDB(), dbname = databaseName, host = mysql$host, 
                    port = mysql$port, user = mysql$user, password = mysql$password)
    
    query <- sprintf("SELECT * FROM %s", userPinTable)
    
    print(query)
    
    # Submit the get query
    df <- dbGetQuery(db, query)
    
    # iterate through return pins and place them in the correct global variable
    for(i in 1:nrow(df)) {
      row <- df[i, ]
      
      # check if the pin is the admin pin ([[ ]] to keep these plain strings)
      if(row[[3]] == 1) {
        adminPin <<- as.character(row[[2]])
      }
    
      # get the dev pins
      if(row[[4]] == 1) {
        devPins <<- c(devPins, as.character(row[[2]]))
      }
    }
    
    print(paste("Admin Pin:", adminPin))
    print(paste("Dev Pins:", devPins))
  },
  error = function(error_message) {
    print("***Error Reading From Database***")
    print(error_message)
    
    # fall back to the cached admin and dev pins
    cache = readReferenceCache()
    if(!is.null(cache)) {
      referenceFromCache <<- TRUE
      adminPin <<- if(is.null(cache$pins$admin)) "" else as.character(cache$pins$admin)
      devPins <<- as.character(unlist(cache$pins$dev))
      cat("Using CACHED pins (admin set:", nzchar(adminPin), ", dev pins:", length(devPins), ")\n")
    }
  },
  finally = {
    if(!is.null(db)) try(dbDisconnect(db), silent = TRUE)
  })
}

# load the current semester from the database
loadSemester = function() {
  currentSemester = "UNKNOWN"
  db = NULL
  
  tryCatch( {
    # connect to database
    db <- dbConnect(MariaDB(), dbname = databaseName, host = mysql$host, 
                    port = mysql$port, user = mysql$user, password = mysql$password)
    
    query <- sprintf("SELECT * FROM %s WHERE Current = 1", semesterTable)
    
    print(query)
    
    # Submit the get query (connection is closed in the finally block below)
    df <- dbGetQuery(db, query)
    
    currentSemester <- as.character(df[[2]][1])
    print(paste("Current Semester:", currentSemester))
  },
  error = function(error_message) {
    print("***Error Reading From Database***")
    print(error_message)
    
    # fall back to the cached semester so the app can still start
    cache = readReferenceCache()
    if(!is.null(cache)) {
      currentSemester <<- cache$semester
      referenceFromCache <<- TRUE
      cat("Using CACHED semester:", currentSemester, "\n")
    }
  },
  finally = {
    if(!is.null(db)) try(dbDisconnect(db), silent = TRUE)
  })
  
  return(currentSemester)
}

# get dataframe with all the semesters
# (currently unused, kept for reference)
getSemesters = function() {
  db = NULL
  
  tryCatch( {
    # connect to database
    db <- dbConnect(MariaDB(), dbname = databaseName, host = mysql$host, 
                    port = mysql$port, user = mysql$user, password = mysql$password)
    
    query <- sprintf("SELECT * FROM %s", semesterTable)
    
    print(query)
    
    # Submit the get query
    df <- dbGetQuery(db, query)
    
    return(df)
  },
  error = function(error_message) {
    print("***Error Reading From Database***")
    print(error_message)
    return(NULL)
  },
  finally = {
    if(!is.null(db)) try(dbDisconnect(db), silent = TRUE)
  })
}

# get valid pins for current semester from database
loadCourses = function(semester) {
  db = NULL
  
  tryCatch( {
    # connect to database
    db <- dbConnect(MariaDB(), dbname = databaseName, host = mysql$host, 
                    port = mysql$port, user = mysql$user, password = mysql$password)
    
    query <- sprintf("SELECT * FROM %s WHERE Semester = '%s'", courseTable, semester)
    
    print(query)
    
    # Submit the get query
    df <- dbGetQuery(db, query)
    
    coursesDF <<- df # global dataframe variable
  },
  error = function(error_message) {
    print("***Error Reading From Database***")
    print(error_message)
    
    # fall back to the cached course list
    coursesDF <<- cachedCourses()
  },
  finally = {
    if(!is.null(db)) try(dbDisconnect(db), silent = TRUE)
  })
}

# function to save data to the backend database
saveToDB = function(pin, expName, questionList) {
  if(any(questionList == "" || anyNA(questionList))) {
    return("Missing Data / Not Saving ...")
  }
  
  db = NULL
  
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
    
    # Submit the insert query
    dbExecute(db, query)
    
    return("Data Saved ...")
  },
  error = function(error_message) {
    print("***Error Saving to Database***")
    print(error_message)
    # degrade gracefully: the experiment data is not persisted while the
    # database is unavailable, but the app keeps working
    return("Database unavailable -- data NOT saved")
  },
  warning = function(warning_message) {
    print("***Warning Saving to Database***")
    print(warning_message)
    return("Warning Saving to Database")
  },
  finally = {
    if(!is.null(db)) try(dbDisconnect(db), silent = TRUE)
  })
}

# function to load data from backend database
loadFromDB = function(pin, expName) {
  db = NULL
  
  # check to see if to reset the pin to the adminPin
  if(!is.null(adminPin) && nzchar(adminPin) && grepl(adminPin, pin, fixed = TRUE)) {
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
    
    # Submit the get query
    df <- dbGetQuery(db, query)
    
    return(df)
  },
  error = function(error_message) {
    print("***Error Reading From Database***")
    print(error_message)
    return(NULL)
  },
  warning = function(warning_message) {
    print("***Warning Reading from Database***")
    print(warning_message)
    return("Warning Reading From Database")
  },
  finally = {
    if(!is.null(db)) try(dbDisconnect(db), silent = TRUE)
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
#'        Defaults to "gemini-2.5-flash". Check Google's documentation for available models.
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

askGemini <- function(prompt, api_key = llmKeys$gemini, model = "gemini-2.5-flash", temperature = 0.7, verbose = FALSE) {
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
  
  return(paste(generated_text, ' -- model:', model))
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
  generated_text  = parsed$choices[[1]]$message$content
  return(paste(generated_text, '-- model:', model))
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
  generated_text = parsed$choices[[1]]$message$content
  return(paste(generated_text, '-- model:', model))
}

# function to send prompts to Claude API
askClaude <- function(prompt, api_key = llmKeys$claude, model = "claude-sonnet-4-5-20250929", temperature = 0.7) {
  # Claude API endpoint
  url <- "https://api.anthropic.com/v1/messages"
  
  headers <- add_headers(
    "x-api-key" = api_key,
    "anthropic-version" = "2023-06-01",
    "content-type" = "application/json"
  )
  
  body <- list(
    model = model,
    max_tokens = 1024,
    temperature = temperature,
    messages = list(
      list(
        role = "user",
        content = prompt
      )
    )
  )
  
  response <- POST(url, headers, body = toJSON(body, auto_unbox = TRUE), encode = "raw")  # ADDED: encode = "raw"
  
  if (status_code(response) != 200) {
    stop("Request failed: ", content(response, "text"))
  }
  
  parsed <- content(response, as = "parsed", type = "application/json")
  generated_text = parsed$content[[1]]$text
  return(paste(generated_text, '-- model:', model))
}

# function to return a fluid row to sending propmts to Gemini and other LLM
# @ns namespace function
getLLMPromptUIRow <- function(ns) {
  row <- fluidRow(
    # add drop down for selecting the llm model
    column(4, selectInput(ns("llmModel"), "Select LLM Model", choices = c("Google Gemini", "ChatGPT", "DeepSeek", "Claude", "Show Prompt"))),
    
    # add slider input for selecting temperature
    column(4, sliderInput(ns("llmTemp"), "Temperature", min = 0, max = 1, value = 0.7)),
    
    column(4, actionButton(ns("llmGenerate"), "Generate Abstract"))
  )
  
  return (row)
}

# function to send prompt to LLM API and display Abstract
displayAbstract <- function(abstractPrompt, model, temp, pin) {
  dialogTitle = paste("Generated Abstract --", model)
  
  response <- tryCatch({
      # check if to use Google's Gemini
      if (model == "Google Gemini") {
        response = askGemini(abstractPrompt, temperature = temp)
      } else if (model == "ChatGPT") {
        response = askChatGPT(abstractPrompt, temperature = temp)
      } else if (model == "DeepSeek") {
        response = askDeepSeek(abstractPrompt, temperature = temp)
      } else if (model == "Claude") {
        response = askClaude(abstractPrompt, temperature = temp)
      } else {
        # assume we just want to show the prompt
        dialogTitle = "LLM Prompt ->>"
        response = abstractPrompt
      }
    }, error = function(e) {
      response = cleanLLMError(e$message)
      print(e$message)
      return(response)
    }
  )
  
  # get the word count of the response
  wordCount = countWords(response)
  response = paste0("Word Count: ", wordCount, " || ", response)
  
  # display the generated abstract or the prompt
  showModal(modalDialog(
    title = dialogTitle,
    response,
    easyClose = TRUE,
    footer = NULL
  ))
}

# Function to count words in a string of text
countWords <- function(text) {
  # Remove leading/trailing white space and split by one or more spaces
  words <- unlist(strsplit(trimws(text), "\\s+"))
  
  # Return the number of words
  return(length(words))
}

# function to clean up LLM error messages by redacting API keys and extracting human-readable content
# Gemini pro 2.5 generated code
cleanLLMError <- function(raw_msg) {
  # --- 1. SECURITY: Redact ALL types of API keys first ---
  # This regex covers:
  # - "key=AIza..." (Google)
  # - "Bearer sk-..." (OpenAI/DeepSeek)
  # - "x-api-key: sk-..." (Claude)
  # It looks for the keyword, followed by non-whitespace characters
  safe_msg <- gsub("(key=|Bearer |x-api-key:? )[^[:space:]\"\']+", "\\1[REDACTED]", raw_msg, ignore.case = TRUE)
  
  # --- 2. PARSING: Try to extract the human-readable message ---
  tryCatch({
    # Attempt to find the start of the JSON payload
    # We look for the first curly brace '{' to handle different wrapper formats
    json_start <- regexpr("\\{", safe_msg)
    
    if (json_start > 0) {
      json_str <- substring(safe_msg, json_start)
      parsed <- fromJSON(json_str)
      
      # Universal Error Extraction Logic:
      # OpenAI/DeepSeek: { "error": { "message": "..." } }
      # Claude: { "type": "error", "error": { "message": "..." } }
      # Google: { "error": { "message": "..." } }
      
      if (!is.null(parsed$error$message)) {
        return(parsed$error$message)
      }
    }
  }, error = function(e) {
    # If JSON parsing fails, ignore it and fall through to the safe raw message
  })
  
  # --- 3. FALLBACK: Return the redacted raw string ---
  return(safe_msg)
}


# ************************************************
# Interesting links for this project
# Reactive variables to get around scope of reactive/observer block of code
# https://stackoverflow.com/questions/50253655/reset-r-shiny-actionbutton-to-use-it-more-than-once
