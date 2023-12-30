# create a location database
# source: https://www.geonames.org/
# database documentation: https://download.geonames.org/export/dump/readme.txt
# TW cities here: https://api.opencube.tw/twzipcode

dt <- readr::read_csv("./deploy/tw_zipcode.csv")[,-1] %>%
  filter(lat != 0) %>%
  select(city, lat,lng) %>%
  group_by(city) %>%
  mutate(pos=seq_along(city),
         mean_lat=mean(lat),
         mean_lng=mean(lng),
         diff_lat=lat-mean(lat),
         diff_lng=lng-mean(lng),
         total_diff=diff_lat+diff_lng,
         min_diff=min(total_diff)) %>%
  filter(total_diff==min_diff) %>%
  ungroup() %>%
  mutate(tz="Asia/Taipei",
         country="台灣") %>%
  select(country, city, lat, lng, tz) %>%
  mutate(city=paste(city, paste("lng:",round(lng,1)), paste("lat:", round(lat,1)), sep=", ")) %>%
  select(country, city)
  
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
  dplyr::select(name, alternatenames, admin1_code, admin2_code, latitude, longitude, country_code, population)

countryInfo <- readr::read_delim("deploy/countryInfo.txt", 
                               delim = "\t", escape_double = FALSE, 
                               trim_ws = TRUE, skip = 49)

# admin1CodesASCII <- readr::read_delim("deploy/admin1CodesASCII.txt", 
#                                delim = "\t", escape_double = FALSE, 
#                                col_names = c("admin1_code", "admin1_name", "admin1_name_ascii", "geonameid"), 
#                                trim_ws = TRUE)
# 
# admin1CodesASCII %>% dplyr::select(-geonameid, -admin1_name_ascii) %>% 
#   dplyr::right_join(cities1000, by="admin1_code")

country <- countryInfo %>% dplyr::rename(country_code=`#ISO`,
                              pop_country=Population) %>% 
  filter(!is.na(country_code)) %>%
  dplyr::select(country_code, Country, pop_country) %>%
  arrange(desc(pop_country)) %>%
  mutate(pop_ranking=seq_along(country_code))

cities1000 <- cities1000 %>% left_join(country, by="country_code")
names(cities1000)
cities1000 <- cities1000 %>%
  filter(country_code != "TW")

cities1000 <- 
  cities1000 %>%
  mutate(country=Country,
         city=paste(name, paste("lat:", round(latitude, 1)), paste("lng:", round(longitude,1)), sep=", ")) %>%
  select(country, city)

cities <- rbind(dt, cities1000)

usethis::use_data(cities)
