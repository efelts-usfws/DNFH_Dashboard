
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