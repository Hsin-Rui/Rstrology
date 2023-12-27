# usethis::use_build_ignore("deploy")

rsconnect::setAccountInfo(name=Sys.getenv('astrocalculator'), 
                          token=Sys.getenv('40FB3997806249234B29E05ADD66D1E5'), 
                          secret=Sys.getenv('iOEbESXhdU0VaW4yQnOajRQW0XvNkIQgY/M1aBIk'))

rsconnect::deployApp(appName = "Rstrology")