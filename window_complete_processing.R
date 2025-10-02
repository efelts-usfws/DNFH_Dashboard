
library(readr)
library(magrittr)
library(dplyr)
library(purrr)
library(stringr)
library(tidyr)
library(lubridate)
library(conflicted)

conflicts_prefer(readr::cols,
                 readr::col_guess)

completed.files <- list.files("historical/window_complete",pattern="\\.csv$",full.names = T)


read_and_fix <- function(file) {
  df <- read_csv(file, col_types = cols(.default = col_guess()))
  
  if ("Date" %in% names(df)) {
    df <- df %>%
      mutate(Date = parse_date_time(Date, orders = c("ymd", "mdy", "dmy")))
  }
  
  return(df)
}

# need a reference to today's date to apply 
# conditional filters to steelhead

today <- Sys.Date()
july_first <- as.Date(paste0(lubridate::year(today), "-07-01"))


# get the data read in and pivot longer

completed.dat <- map_dfr(completed.files, read_and_fix) |> 
  filter(!is.na(Date)) |> 
  mutate(year=year(Date)) |> 
  select(dam=Project,date=Date,year,
         run_type=`Chinook Run`,temp=TempC,
         Chinook=Chin,Jack_Chinook=JChin,
         Steelhead=Stlhd,Wild_Steelhead=WStlhd,
         Sockeye=Sock,Coho,Jack_Coho=JCoho,
         Shad,Lamprey=Lmpry,`Bull Trout`=BTrout,
         Chum,Pink
  ) |> 
  pivot_longer(cols=Chinook:Pink,
               names_to = "species",
               values_to = "count") |> 
  mutate(life_stage=case_when(
    species  %in% c("Jack_Chinook","Jack_Coho") ~ "Jack",
    species %in% c("Chinook","Coho") ~ "Adult",
    TRUE ~ "All"
  ),
  origin=case_when(
    species=="Wild_Steelhead" ~ "Wild",
    TRUE ~ "All",
  ),
  species=case_when(
    species %in% c("Wild_Steelhead","Jack_Chinook","Jack_Coho") ~ word(species,2,sep="_"),
    TRUE ~ species
  ),
  run_type=case_when(
    run_type == "Sp" ~ "Spring",
    run_type=="Su" ~ "Summer",
    run_type=="Fa" ~ "Fall",
    TRUE ~ NA
  ),
  species=case_when(
    species=="Chinook" ~ str_c(run_type,species,sep=" "),
    TRUE ~ species
  ),
  july_first=make_date(year(date),7,1),
  dummy_date=case_when(
    species=="Steelhead" & date < july_first ~as.Date(format(date,"1977-%m-%d")),
    TRUE ~ as.Date(format(date,"1976-%m-%d"))),
  spawn_year=case_when(
    species=="Steelhead" & date >=  july_first ~ year(date)+1,
    TRUE ~ year(date)
  )) |> 
  arrange(spawn_year,dam,dummy_date,species,life_stage,
          origin) |> 
  group_by(spawn_year,dam,species,life_stage,origin) |> 
  mutate(count=if_else(is.na(count),0,count),
         running_total=cumsum(count),
         annual_total=sum(count)) %>%
  filter(
    year(date) < year(today)|
      (species ==  "Steelhead" & year(date) == year(today)&date<july_first)
  )

test.steel <- completed.dat |> 
  filter(year==2024,
         species=="Steelhead",
         dam=="Lower Granite")


# save the completed data as an RDS file

saveRDS(completed.dat,
        "data/window_counts_complete")

