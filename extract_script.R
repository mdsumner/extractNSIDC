############################################
### Extract NSIDC geotiff data to points ###
############################################
## https://gist.github.com/jamesgrecian/cbc764e586f9da52c6b3105cf005aa51

#Load libraries
require(tidyverse)
require(lubridate)
require(httr)
require(raster)
require(RCurl)
require(rgdal)
require(sf)
require(viridis)

#Load in sample data
dat = structure(list(id = c("1", "1", "1", "1", "1", "1", "1", "1", 
                            "1", "1", "1", "1", "1", "1", "1", "1", "1", "1"), date = structure(c(1496293200, 
                                                                                                  1496379600, 1496466000, 1496552400, 1496638800, 1496725200, 1496811600, 
                                                                                                  1496898000, 1496984400, 1497070800, 1497157200, 1497243600, 1497330000, 
                                                                                                  1497416400, 1497502800, 1497589200, 1497675600, 1497762000), class = c("POSIXct", 
                                                                                                                                                                         "POSIXt"), tzone = "UTC"), lon = c(7.67676622815245, 8.32651129486591, 
                                                                                                                                                                                                            9.30056600314477, 9.45250360572033, 9.60916520099151, 9.77475128937539, 
                                                                                                                                                                                                            10.1061720842052, 10.1710542009332, 9.90688750274992, 9.81325388550969, 
                                                                                                                                                                                                            10.4855223523313, 11.5879347141201, 11.1764074647293, 11.1285814662996, 
                                                                                                                                                                                                            11.0955496956579, 11.2381946111766, 11.0354486782552, 10.9479339290118
                                                                                                                                                                         ), lat = c(79.1491984152386, 79.2639703417475, 79.3592790529816, 
                                                                                                                                                                                    79.3924837009205, 79.4415010492375, 79.5094211441373, 79.6192990247995, 
                                                                                                                                                                                    79.6480471177488, 79.6914819415593, 79.769581367991, 79.9154387708053, 
                                                                                                                                                                                    79.9810654837584, 80.0169452297861, 79.9918142858824, 79.9980199574213, 
                                                                                                                                                                                    79.9860598615852, 79.9910041289865, 79.9753439508087), lon.se = c(0.0633655756906932, 
                                                                                                                                                                                                                                                      0.114755482879892, 0.0757837818685542, 0.0900480403465911, 0.0645958828587992, 
                                                                                                                                                                                                                                                      0.0881363142635468, 0.0286578317608318, 0.0699719826533174, 0.0578032562549854, 
                                                                                                                                                                                                                                                      0.0540794853847073, 0.0626218863526746, 0.111825808555079, 0.0422916670580266, 
                                                                                                                                                                                                                                                      0.060029548866816, 0.0752812171499952, 0.045144065564274, 0.0407297531852337, 
                                                                                                                                                                                                                                                      0.0535020480527324), lat.se = c(0.0138951487651622, 0.0256553862829213, 
                                                                                                                                                                                                                                                                                      0.0162791965430388, 0.0208636195015253, 0.0145311803500762, 0.0197045213192155, 
                                                                                                                                                                                                                                                                                      0.00823009511074715, 0.0147462106154249, 0.0128565358466395, 
                                                                                                                                                                                                                                                                                      0.0120828817775333, 0.013947113178654, 0.0253086237821025, 0.00989262859261941, 
                                                                                                                                                                                                                                                                                      0.0139588308974151, 0.0166477882149834, 0.0100829843756892, 0.00889804253914898, 
                                                                                                                                                                                                                                                                                      0.0119026752050172)), row.names = c(NA, -18L), spec = structure(list(
                                                                                                                                                                                                                                                                                        cols = structure(list(id = structure(list(), class = c("collector_character", 
                                                                                                                                                                                                                                                                                                                                               "collector")), date = structure(list(format = ""), .Names = "format", class = c("collector_datetime", 
                                                                                                                                                                                                                                                                                                                                                                                                                               "collector")), lon = structure(list(), class = c("collector_double", 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                "collector")), lat = structure(list(), class = c("collector_double", 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 "collector")), lon.se = structure(list(), class = c("collector_double", 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     "collector")), lat.se = structure(list(), class = c("collector_double", 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         "collector"))), .Names = c("id", "date", "lon", "lat", "lon.se", 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    "lat.se")), default = structure(list(), class = c("collector_guess", 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      "collector"))), .Names = c("cols", "default"), class = "col_spec"), .Names = c("id", 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     "date", "lon", "lat", "lon.se", "lat.se"), class = c("tbl_df", 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          "tbl", "data.frame"))

#First append raster file name based on year month day
dat = dat %>%
  mutate(year = year(date), month = month(date), mon = month(date, label = T, abbr = T), day = day(date)) %>%
  mutate(month = ifelse(month < 10, paste("0", month, sep=""), paste(month))) %>%
  mutate(day = ifelse(day < 10, paste("0", day, sep=""), paste(day))) %>%
  mutate(fn = paste0("ftp://anonymous:wjg5@sidads.colorado.edu/DATASETS/NOAA/G02135/north/daily/geotiff/",
                     year, "/", month, "_", mon, "/N_", year, month, day, "_concentration_v3.0.tif"))


dir.create("geotiff", showWarnings = FALSE)
## get all the files, though avoid re-downloading
download_it <- function(x, path = "geotiff") {
  file <- file.path(path, basename(x))
  if (!file.exists(file)) {
    curl::curl_download(x, file)
    return(TRUE)
  } 
  FALSE
} 
## TRUE if we downloaded it, FALSE if it was already there
test <- purrr::map_lgl(dat$fn, download_it)


## now build a file-db
files <- tibble(fullname = list.files(normalizePath("geotiff"), full.names = TRUE, pattern = "tif$"), 
                date = as.POSIXct(strptime(basename(fullname), "N_%Y%m%d", tz = "UTC")))

## map our data to this file-db (we might have multiple points per file)
dat$fullname <- files$fullname[findInterval(dat$date, files$date)]

## investigate the data
(rdummy <- raster(dat$fullname[1]))
## project the query points
dat[c("X", "Y")] <- as_tibble(rgdal::project(as.matrix(dat[c("lon", "lat")]), projection(rdummy)))
plot(rdummy, addfun = function() points(dat[c("X", "Y")]))
## now, extract per file 
dat2 <- purrr::map_df(split(dat, dat$fullname)[unique(dat$fullname)], 
           function(.x) {
           print(.x$fullname[1])
             .x["concentration"] <- raster::extract(raster(.x$fullname[1]), as.matrix(.x[c("X", "Y")]))
             .x
           })
ggplot(dat2, aes(X, Y, colour = concentration)) + geom_point() + coord_equal()
ggsave("extract_plot.png")
saveRDS(dat2, "extracted_data.rds")
