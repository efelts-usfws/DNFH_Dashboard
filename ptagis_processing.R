
library(vroom) 
library(dataRetrieval)
library(conflicted)
library(dplyr)
library(stringr)
library(lubridate)


conflicts_prefer(vroom::locale,
                 dplyr::filter)

# set the timeout above default of 60 seconds
# because sometimes the API calls are slow

options(timeout=300)

dir.create("data",recursive=TRUE, showWarnings = FALSE)

ptagis.dat <- readRDS("data/ptagis_sites")

## grabbing water data from USGS gaging station
## at Peck

peck.site <- "13341050"

# temp is coded as 00010, discharge is 00060

parm.cd <- c("00010","00060")

# put today's date into text format to 
# feed into the water data query

today.text <- as.character(today(tz="America/Los_Angeles"))

peck.daily <- readNWISdv(siteNumber=peck.site,
                           parameterCd=parm.cd,
                           startDate="1990-01-01",
                           endDate=today.text) |> 
  select(date=Date,
         mean_temp=X_00010_00003,
         mean_discharge=X_00060_00003) |> 
  mutate(date=as_date(date),
         year=year(date))

current_year <- year(today())

peck.dat <- peck.daily |> 
   filter(year==current_year) |> 
   mutate(group=1)
