#
# Regenerate data/qrchem_cache.json from the live database.
#
# OPTIONAL.  Running the app on your desktop already refreshes the cache
# automatically, so the normal flow is: run the app, then deploy.  Use this
# script when you want to refresh the cache without launching the app, or when
# you want a deploy to fail loudly because the database is unreachable:
#
#     Rscript tools/make_reference_cache.R
#
# The app falls back to this file when the database is unreachable, which is what
# lets it start and validate logins during a MySQL outage.
#
# This script deliberately FAILS instead of writing a stale cache when the
# database cannot be reached.
#

if(!file.exists("helpers.R")) {
  stop("Run this script from the QRChem app root:  Rscript tools/make_reference_cache.R")
}

source("helpers.R")

cat("Reading reference data from", mysql$host, "...\n")

# same sequence global.R uses
currentSemester = loadSemester()
loadCourses(currentSemester)

adminPin = NULL
devPins = c()
loadPinsFromDB()

# writeReferenceCache() refuses to write when any read fell back to the cache,
# but bail out explicitly here so the reason is unmistakable.
if(referenceFromCache) {
  stop("Reference data came from the cache -- the database was unreachable.\n",
       "Refusing to overwrite the cache. Fix the DB connection and re-run.")
}

writeReferenceCache()

# report what was written
cache = readReferenceCache()

cat("\n--- reference cache summary ---\n")
cat("semester  :", currentSemester, "\n")
cat("courses   :", if(is.null(cache$courses)) 0 else nrow(cache$courses), "\n")
cat("admin pin :", if(nzchar(as.character(adminPin))) "set" else "NOT SET", "\n")
cat("dev pins  :", length(devPins), "\n")
cat("file      :", referenceCachePath, "\n")
cat("generated :", cache$generatedAt, "\n")
