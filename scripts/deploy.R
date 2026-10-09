# Explicit publication script; never sourced during tests or app startup.
keys <- c("SHINYAPPS_ACCOUNT","SHINYAPPS_TOKEN","SHINYAPPS_SECRET")
if(any(!nzchar(Sys.getenv(keys)))) stop("Configure the three SHINYAPPS repository secrets before deployment.")
rsconnect::setAccountInfo(name=Sys.getenv(keys[1]),token=Sys.getenv(keys[2]),secret=Sys.getenv(keys[3]))
files <- c("app.R",list.files("R",recursive=TRUE,full.names=TRUE),list.files("www",recursive=TRUE,full.names=TRUE),list.files("templates",recursive=TRUE,full.names=TRUE))
files <- files[!grepl("/\\.gitkeep$",files)]
rsconnect::deployApp(appDir=".",appFiles=files,appName="sample-size-calculator",account=Sys.getenv(keys[1]),forceUpdate=TRUE)
