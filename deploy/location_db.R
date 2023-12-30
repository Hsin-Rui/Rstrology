# create a location database
# source: https://www.geonames.org/
# database documentation: https://download.geonames.org/export/dump/readme.txt
# TW cities here: https://api.opencube.tw/twzipcode


col_names <- c("geonameid",
               "name",
               "asciiname",
               "alternatenames",
               "latitude",
               "longitude",
               "feature_class",
               "feature_code",
               "country_code",
               "cc2",
               "admin1_code",
               "admin2_code",
               "admin3_code",
               "admin4_code",
               "population",
               "elevation",
               "dem",
               "timezone",
               "modification_date")

cities1000 <- readr::read_delim("deploy/cities1000.txt", 
                         delim = "\t", escape_double = FALSE, 
                         col_names = col_names, trim_ws = TRUE)
cities1000 <- 
  cities1000 %>% 
  dplyr::select(name, alternatenames, admin1_code, admin2_code, latitude, longitude, country_code) %>%
  dplyr::mutate(admin1_code=paste(country_code, admin1_code, sep = "."))

countryInfo <- readr::read_delim("deploy/countryInfo.txt", 
                               delim = "\t", escape_double = FALSE, 
                               trim_ws = TRUE, skip = 49)

admin1CodesASCII <- read_delim("deploy/admin1CodesASCII.txt", 
                               delim = "\t", escape_double = FALSE, 
                               col_names = c("admin1_code", "admin1_name", "admin1_name_ascii", "geonameid"), 
                               trim_ws = TRUE)

admin1CodesASCII %>% dplyr::select(-geonameid, -admin1_name_ascii) %>% 
  dplyr::right_join(cities1000, by="admin1_code")

countryInfo %>% dplyr::rename(country_code=`#ISO`) %>% dplyr::select(country_code, Country) %>% 
  dplyr::right_join(cities1000, by="country_code")

cities1000 %>% dplyr::filter()
