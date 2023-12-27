# usethis::use_build_ignore("deploy")

rsconnect::setAccountInfo(name=Sys.getenv("SHINYAPPS_ACCOUNT"), 
                          token=Sys.getenv('SHINYAPPS_TOKEN'), 
                          secret=Sys.getenv("SHINYAPPS_SECRET"))

rsconnect::deployApp(appName = "Rstrology")