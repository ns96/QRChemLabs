# load the helper script here since this file is loaded first when the
# shiny app is ran
source("helpers.R")

#
#global file used to load modules for the various experiments
#

# specify the semester and load the course information from database
currentSemester = loadSemester()
loadCourses(currentSemester)

# get the course codes
courseCodes = getCourseCodes()

# Set the admin pin. This should set from a DB
adminPin = "1001"
devPins = c(adminPin, "0123", "1210")
validPins = getValidPins()

print("Loading experiment modules ...")

# load chem1110 modules
source("exp00_module.R")
source("exp01_module.R")
source("exp02_module.R")
source("exp03_module.R")
source("exp04_module.R")
source("exp05_module.R")
source("exp06_module.R")
source("exp07_module.R")
source("exp08_module.R")
source("exp09_module.R")
source("exp10_module.R")
source("exp11_module.R")
source("exp12_module.R")
source("exp13_module.R")

# load chem1000 modules
source("exp04B_module.R")
source("exp04D_module.R")
source("exp09B_module.R")
source("exp12B_module.R")
source("exp13B_module.R")

# load chem1210 modules
source("exp01C_module.R")
source("exp02C_module.R")
source("exp03C_module.R")
source("exp04C_module.R")
source("exp05C_module.R")
source("exp06C_module.R")
source("exp07C_module.R")
source("exp08C_module.R")
source("exp09-10C_module.R")
source("exp11C_module.R")
source("exp12C_module.R")
source("exp13C_module.R")

print("Done loading modules ...")