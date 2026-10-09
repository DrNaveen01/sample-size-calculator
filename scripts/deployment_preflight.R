# Read dependency metadata and build the manifest without contacting a host.
if(!requireNamespace("rsconnect",quietly=TRUE)) stop("Install rsconnect for deployment preflight.")
files <- c("app.R",list.files("R",recursive=TRUE,full.names=TRUE),
  list.files("www",recursive=TRUE,full.names=TRUE),list.files("templates",recursive=TRUE,full.names=TRUE))
files <- files[!grepl("/\\.gitkeep$",files)]
deps <- rsconnect::appDependencies(appDir=".",appFiles=files)
stopifnot(all(c("shiny","rmarkdown","base64enc") %in% deps$Package))
rsconnect::writeManifest(appDir=".",appFiles=files)
cat("Dependency parsing and local deployment manifest passed; no deployment performed.\n")
