
# this script is built to query marking records for
# all chinook and steelhead raised at Dworshak, Kooskia,
# and Clearwater hatcheries; it is used to compile
# juvenile capture histories, estimates of juvenile survival,
# summaries of emigration timing, adult return histories and
# adult migration timing

# load libraries

library(vroom) 
library(dataRetrieval)
library(conflicted)
library(stringr)
library(lubridate)
library(dplyr)
library(readr)
library(tidyr)
library(forcats)
library(purrr)

# set conflict preferences

conflicts_prefer(vroom::locale,
                 dplyr::filter)

# read in PTAGIS metadata for reference

ptagis.dat <- readRDS("data/ptagis_sites")

#