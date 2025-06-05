
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
library(arrow)

# set conflict preferences

conflicts_prefer(vroom::locale,
                 dplyr::filter)

# read in PTAGIS metadata for reference

ptagis.dat <- readRDS("data/ptagis_sites")

# First step is to get relevant information on
# every marked fish and do some formatting of 
# columns like dates and such

marking.dat_20_24 <- read_csv("historical/DNFH Marking Historical_2020-2024.csv") |> 
  mutate(release_date=mdy(`Release Date MMDDYYYY`),
         mark_date=mdy(`Mark Date MMDDYYYY`),
         hatchery=word(`Mark Site Name`,1,sep=" "),
         release_sitecode=word(`Release Site Name`,1,sep=" "),
         tag_file=`Mark File Name`) |> 
  select(pit_id=`Tag Code`,hatchery,
         mark_date,release_site=`Release Site Name`,
         release_sitecode,release_date,mark_lifestage=`Mark Life Stage Value`,
         species=`Species Name`,
         length_mm=`Length mm`,tag_file) 


# check to see if there's any release sites that don't make sense

release_sites_20_24 <- marking.dat_20_24 |> 
  group_by(release_site,
           release_date) |> 
  tally()


# check to make sure there are no NA or Unknown in life stage

life_stages_20_24 <- marking.dat_20_24 |> 
  group_by(mark_lifestage) |> 
  tally()

# want to distinguiish release groups, check by hatchery, species,
# release site and release date to see which will make sense

release_grp_tally_20_24 <- marking.dat_20_24 |> 
  group_by(hatchery,species,release_sitecode,release_date) |> 
  tally() |> 
  mutate(release_year=year(release_date)) |> 
  arrange(hatchery,species,release_sitecode,release_date) |> 
  group_by(hatchery,species,release_sitecode,release_year) |> 
  mutate(release_order=row_number())

# join the release order info to the main data from so
# correct release groups can be assigned

marking.join_20_24 <- marking.dat_20_24 |> 
  left_join(release_grp_tally_20_24,
            by=c("hatchery","species","release_sitecode",
                 "release_date"))

marking.export_20_24 <- marking.join_20_24 |> 
  mutate(release_group=case_when(
    
    release_sitecode=="CLEARC" ~ "Clear Creek",
    release_sitecode=="CLWHNF" & release_year == 2020 ~ "North Fork Clearwater River Single",
    release_sitecode=="CLWHNF" & !release_year==2020 &  release_order == 1 ~ "North Fork Clearwater River Early",
    release_sitecode=="CLWHNF" & !release_year==2020 &  release_order == 2 ~ "North Fork Clearwater River Late",
    release_sitecode=="CLWRSF" ~ "South Fork Clearwater River",
    release_sitecode=="DWORMS" & species=="Steelhead" ~ "Clearwater River",
    release_sitecode=="DWORMS" & species=="Chinook" & release_order==1 ~ "Clearwater River Early",
    release_sitecode=="DWORMS" & species=="Chinook" & release_order==2 ~ "Clearwater River Late",
    release_sitecode=="DWORNF" & release_year == 2020 ~ "North Fork Clearwater River Single",
    release_sitecode=="DWORNF" & !release_year == 2020 & release_order==1 ~ "North Fork Clearwater River Early",
    release_sitecode=="DWORNF" & !release_year == 2020 & release_order==2 ~ "North Fork Clearwater River Late",
    release_sitecode=="KOOS" ~ "Kooskia",
    release_sitecode=="LOLOC" ~ "Lolo Creek",
    release_sitecode=="MEAD2C" ~ "Meadow Creek",
    release_sitecode=="NEWSOC" ~ "Newsome Creek",
    release_sitecode=="POWP" ~ "Powell",
    release_sitecode=="REDP" ~ "Red River",
    release_sitecode=="SELWY1" ~ "Selway River",
    release_sitecode=="SNAKE2" ~ "Snake Kelts"
    
    
  )) |> 
  select(pit_id,hatchery,mark_date,release_site,
         release_sitecode,release_date,mark_lifestage,
         species,length_mm,tag_file,release_year,
         release_group)

# export the marking data as an RDS file

saveRDS(marking.export_20_24,
        "historical/mark_2020_2024")

# Now get lists of pit ids to run complete tag histories for;
# need to split these up into 10000 at a time - PTAGIS says it will allow
# up to 15000 but often that has so many detections it throws an error, so 
# going enough below to not deal with that

tag.file <- marking.export_20_24 |> 
  select(pit_id) 


chunk <- 10000

n <- nrow(tag.file)

r <- rep(1:ceiling(n/chunk),each=chunk)[1:n]

tag.split <- split(tag.file,r)

for(i in 1:length(tag.split)){
  
  write.table(tag.split[i],file=str_c("historical/int_files_20_24/",
                                      names(tag.split)[i],".txt",sep=""),
              quote=F,row.names=F,col.names=F)
}

# read in compiled complete tag histories

# make a list of all the files to be able to 
# iterate over

int.files <- list.files("historical/complete_history_20_24", pattern="\\.csv$", full.names = T)

# read in all of the detections, and get the first observations
# for each fish by day and site. The original query searched detections by individual antennas, 
# and at many of these sites that means there may be several individual detections within 
# as short as a 1-2 minute span at a given site. For pretty much all purposes I can think of, 
# it will suffice to limit these records to the first detection at a site within a given date.

int_20_24.dat <- map(int.files,read_csv) |> 
  bind_rows() |> 
  filter(!`Event Type Name`=="Mark") |> 
  mutate(obs_type = str_to_lower(`Event Type Name`),
         obs_datetime=mdy_hms(`Event Date Time Value`),
         obs_date=as.Date(obs_datetime),
         obs_year=year(obs_date)) |> 
  select(pit_id=`Tag Code`,
         obs_type,obs_datetime,obs_date,
         obs_year,obs_sitecode=`Event Site Code Value`) |> 
  group_by(pit_id,obs_date,obs_sitecode) |> 
  slice(which.min(obs_datetime))



# make a vector of main stem detection sites linked with dam
# name to put emigration and return detections into the 8 dams
# and the towed estuary array 
  
dam_key <- tibble(sitecode=c("GRS","GRJ","GRA","LGR","LGRLDR",
                             "GOJ","GOA",
                             "LMJ","LMA",
                             "ICH",
                             "MCJ","MC2","MC1","MCN",
                             "JDJ","JO1","JO2",
                             "TD1","TD2",
                             "B2J","BCC","BO1","BO2","BO4","BONAFF",
                             "TWX"),
                  dam=c("Lower Granite","Lower Granite","Lower Granite","Lower Granite","Lower Granite",
                        "Little Goose","Little Goose",
                        "Lower Monumental","Lower Monumental",
                        "Ice Harbor",
                        "McNary","McNary","McNary","McNary",
                        "John Day","John Day","John Day",
                        "The Dalles","The Dalles",
                        "Bonneville","Bonneville","Bonneville","Bonneville","Bonneville","Bonneville",
                        "Estuary"),
                  dam_code=c("LGR","LGR","LGR","LGR","LGR",
                             "LGS","LGS",
                             "LOMO","LOMO",
                             "ICH",
                             "MCN","MCN","MCN","MCN",
                             "JD","JD","JD",
                             "TDA","TDA",
                             "BONN","BONN","BONN","BONN","BONN","BONN",
                             "TWX")) |> 
  mutate(dam_code=factor(dam_code,
                         levels=c("LGR","LGS","LOMO","ICH",
                                  "MCN","JD","TDA","BONN",
                                  "TWX")))  


# now join the complete detection histories to the dam key

int_20_24.join <- int_20_24.dat |> 
  left_join(dam_key,by=c("obs_sitecode"="sitecode"))

# now join the detections to the marking data
# so records for those that were never detected
# are also retained

mark_20_24.join <- marking.export_20_24 |> 
  left_join(int_20_24.join,by="pit_id") |> 
  mutate(years_at_large=obs_year-release_year)


# this is a useful point at which to save for future use

saveRDS(int_20_24.dat,"historical/daily_detections_20_24")


# now create summaries through the hydrosystem for juvenile detections
# first make the summaries for those fish that were detected in the
# hydrosystem as juveniles; then make a data frame with all NA
# and 0's in capture history for any fish that weren't detected

juv_dam_20_24.dat <- mark_20_24.join |>
  filter(years_at_large==0,
         !is.na(dam_code)) |>
  filter(mark_lifestage=="Juvenile") |>
  group_by(pit_id,dam_code) |>
  summarize(first_detection=min(obs_datetime,na.rm=T)) |> 
  select(pit_id,dam_code,first_detection) |>
  pivot_wider(names_from=dam_code,
              values_from = first_detection) |> 
  left_join(marking.export_20_24,by="pit_id") |> 
  mutate(release_date=as.POSIXct(release_date)) |> 
  mutate(LGR_ch=ifelse(is.na(LGR),"0","1"),
         LGS_ch=ifelse(is.na(LGS),"0","1"),
         LOMO_ch=ifelse(is.na(LOMO),"0","1"),
         ICH_ch=ifelse(is.na(ICH),"0","1"),
         MCN_ch=ifelse(is.na(MCN),"0","1"),
         JD_ch=ifelse(is.na(JD),"0","1"),
         TDA_ch=ifelse(is.na(TDA),"0","1"),
         BONN_ch=ifelse(is.na(BONN),"0","1"),
         TWX_ch=ifelse(is.na(TWX),"0","1"),
         release_time=as.numeric(release_date),
         LGR_time=as.numeric(LGR),
         LGS_time=as.numeric(LGS),
         LOMO_time=as.numeric(LOMO),
         ICH_time=as.numeric(ICH),
         MCN_time=as.numeric(MCN),
         JD_time=as.numeric(JD),
         TDA_time=as.numeric(TDA),
         BONN_time=as.numeric(BONN),
         TWX_time=as.numeric(TWX),
         lgr_traveltime=floor((LGR_time-release_time)/86400),
         lgs_traveltime=floor((LGS_time-release_time)/86400),
         lomo_traveltime=floor((LOMO_time-release_time)/86400),
         ich_traveltime=floor((ICH_time-release_time)/86400),
         mcn_traveltime=floor((MCN_time-release_time)/86400),
         jd_traveltime=floor((JD_time-release_time)/86400),
         tda_traveltime=floor((TDA_time-release_time)/86400),
         bonn_traveltime=floor((BONN_time-release_time)/86400),
         twx_traveltime=floor((TWX_time-release_time)/86400)) 

# for any pit tag ids that were 
# not in that summary, give an NA
# for everything


# find all the pit ids that did not appear
# anywhere

juv_na_20_24.antijoin <- marking.export_20_24 |> 
  anti_join(juv_dam_20_24.dat,by="pit_id")

# create a template from the populated data frame

na_template <- juv_dam_20_24.dat[0,]

# make the number of rows needed in the empty data frame

na_rows <- na_template[rep(1,nrow(juv_na_20_24.antijoin)),]

# now populate the pit_id in that data frame with those
# in the anti join

na_rows$pit_id <- juv_na_20_24.antijoin$pit_id

# bind together all the travel time parts then join back 
# in marking/release info

juv_dam_20_24.bind <- bind_rows(juv_dam_20_24.dat,
                                na_rows) |> 
  select(pit_id,LGR,LGS,LOMO,ICH,MCN,
         JD,TDA,BONN,TWX,LGR_ch,LGS_ch,LOMO_ch,ICH_ch,MCN_ch,JD_ch,
         TDA_ch,BONN_ch,TWX_ch,release_time,LGR_time,LGS_time,LOMO_time,
         ICH_time,MCN_time,JD_time,TDA_time,BONN_time,TWX_time,
         lgr_traveltime,lgs_traveltime,
         lomo_traveltime,ich_traveltime,mcn_traveltime,jd_traveltime,tda_traveltime,
         bonn_traveltime,twx_traveltime) |> 
  left_join(marking.export_20_24,by="pit_id") |> 
  select(pit_id,species,tag_file,hatchery,release_date,release_year,
         mark_date,release_sitecode,release_group,length_mm,LGR,LGS,LOMO,ICH,MCN,
         JD,TDA,BONN,TWX,LGR_ch,LGS_ch,LOMO_ch,ICH_ch,MCN_ch,JD_ch,
         TDA_ch,BONN_ch,TWX_ch,release_time,LGR_time,LGS_time,LOMO_time,
         ICH_time,MCN_time,JD_time,TDA_time,BONN_time,TWX_time,
         lgr_traveltime,lgs_traveltime,
         lomo_traveltime,ich_traveltime,mcn_traveltime,jd_traveltime,tda_traveltime,
         bonn_traveltime,twx_traveltime)

# save to RDS format

saveRDS(juv_dam_20_24.bind,
        "historical/juv_hydro_20_24")

# work out travel timing plot

travel.dat <- read_rds("data/travel")

historical_travel.dat <- read_rds("historical/juv_hydro_20_24")




# do the emigration summaries here and save to RDS so
# it doesn't slow down app launches

historical_travel.sum <- historical_travel.dat |> 
  group_by(hatchery,species,release_year,release_group) |> 
  summarize(median_lgr=floor(median(lgr_traveltime,na.rm=T)),
            median_lgr_date=as_date(median(LGR,na.rm=T)),
            lgr_detections=sum(!is.na(LGR)),
            median_bonn=floor(median(bonn_traveltime,na.rm=T)),
            median_bonn_date=as_date(median(BONN,na.rm=T)),
            bonn_detections=sum(!is.na(BONN)),
            earliest_release=format(min(release_date),"%B %d"),
            latest_release=format(max(release_date),"%B %d"))

saveRDS(historical_travel.sum,
        "historical/juv_emigration_20_24")


# Now summarize adult detections


adult_detections_20_24.dat <- int_20_24.join |> 
  left_join(marking.export_20_24,by="pit_id") |> 
  mutate(release_year=year(release_date),
         years_at_large=obs_year-release_year) |> 
  filter(years_at_large>0,
         obs_type %in% c("recapture","passive recapture","observation")) |> 
  mutate(day_of_year=yday(obs_datetime),
         dummy_date=case_when(
           
           species=="Steelhead" & day_of_year< 183 ~ as.Date(day_of_year,origin="1977-01-01"),
           
          TRUE ~ as.Date(day_of_year-1, origin="1976-01-01")                    
                              
                              
                              ),
         spawn_year=case_when(
           
           species=="Steelhead" & day_of_year>=183 ~ (obs_year+1),
           TRUE ~ obs_year
           
         )) |> 
  mutate(ocean_age=case_when(
    
    species=="Steelhead" ~ (spawn_year-release_year-1),
    TRUE ~ (spawn_year-release_year)
    
  ))

age_summary <- adult_detections_20_24.dat |> 
  group_by(species,ocean_age) |> 
  tally()

# export adult detections to RDS; dropping SY 2025 stuff 
# because it's not actually completed when i'm building 
# this script (June '25)

adult_export_20_24.dat <- adult_detections_20_24.dat |> 
  filter(!spawn_year==2025)

saveRDS(adult_export_20_24.dat,
        "historical/adult_detections_20_24")


# think about making plots and what will be needed,
# here Granite adult as an example; using max datetime
# observation at a dam to say when they passed...for most the
# min/max will be the same but in the case of
# fallback/reascension this will get when they actually left


lgr_plot.dat <- adult_detections_20_24.dat |>
  filter(obs_sitecode %in% c("GRA","LGRLDR"),
         !release_group=="Snake Kelts") |>
  group_by(pit_id,obs_year) |> 
  slice(which.max(obs_datetime)) |> 
  ungroup() |> 
  group_by(dummy_date,spawn_year,species,hatchery) |> 
  summarize(daily_total=n()) |> 
  ungroup() |> 
  group_by(spawn_year,species,hatchery) |> 
  arrange(spawn_year,dummy_date,species,hatchery) |> 
  mutate(running_total=cumsum(daily_total),
         annual_total=sum(daily_total)) |> 
  filter(!spawn_year==2025) |> 
  mutate(dam="Lower Granite")


# same thing applied to bonneville

bonn_plot.dat <- adult_detections_20_24.dat |>
  filter(obs_sitecode %in% c("BO4","BO3","BO1",
                             "BO2"),
         !release_group=="Snake Kelts") |>
  group_by(pit_id,obs_year,obs_sitecode) |> 
  slice(which.max(obs_datetime)) |> 
  ungroup() |> 
  group_by(dummy_date,spawn_year,species,hatchery) |> 
  summarize(daily_total=n()) |> 
  ungroup() |> 
  group_by(spawn_year,species,hatchery) |> 
  arrange(spawn_year,dummy_date,species,hatchery) |> 
  mutate(running_total=cumsum(daily_total),
         annual_total=sum(daily_total))|> 
  filter(!spawn_year==2025) |> 
  mutate(dam="Bonneville")

# same thing applied to the Dworshak ladder

dwor_plot.dat <- adult_detections_20_24.dat |>
  filter(obs_sitecode %in% c("DWL"),
         !release_group=="Snake Kelts") |>
  group_by(pit_id,obs_year,obs_sitecode) |> 
  slice(which.max(obs_datetime)) |> 
  ungroup() |> 
  group_by(dummy_date,spawn_year,species,hatchery) |> 
  summarize(daily_total=n()) |> 
  ungroup() |> 
  group_by(spawn_year,species,hatchery) |> 
  arrange(spawn_year,dummy_date,species,hatchery) |> 
  mutate(running_total=cumsum(daily_total),
         annual_total=sum(daily_total)) |> 
  filter(!spawn_year==2025) |> 
  mutate(dam="Dworshak Ladder")

# save those plat data to RDS

complete_plot.dat <- bind_rows(lgr_plot.dat,
                               bonn_plot.dat,
                               dwor_plot.dat)


saveRDS(complete_plot.dat,
        "historical/complete_adult_plot_20_24")


### below here will eventually go away, just 
# working through plots to put into the shiny
# app

lgr_total.plot <- lgr_plot.dat |> 
  filter(species=="Steelhead") |> 
  ggplot(aes(x=dummy_date,y=daily_total,
             fill=as.factor(spawn_year)))+
  geom_col(aes(text=str_c(" Date:", format(dummy_date, "%B %d"),
                          "<br>",
                          "Number Passed:",daily_total,
                          sep=" ")))+
  scale_fill_okabe_ito()+
  facet_grid(hatchery~spawn_year,
             scales="free_y")+
  theme_bw()+
  labs(y="Daily Adults Passed",
       fill="")
lgr_total.plot

ggplotly(lgr_total.plot,
         tooltip = c("text"))

# now line for accumulation plot


lgr_accumulation_plot <- lgr_plot.dat |> 
  filter(species=="Chinook") |> 
  ggplot(aes(x=dummy_date,y=running_total,group=spawn_year,
             color=as.factor(spawn_year)))+
  geom_line(linewidth=1.2)+
  scale_color_okabe_ito()+
  facet_wrap(~hatchery,
             scales="free_y")+
  theme_bw()
lgr_accumulation_plot



# do steelhead as an example plot
# 
# library(ggokabeito)
# 
sthd.travel <- travel.sum |>
  filter(species=="Steelhead") |>
  ggplot(aes(x=release_year,y=median_lgr))+
  geom_col(aes(fill=release_group),
           position="dodge",
           color="black")+
  facet_wrap(~hatchery)+
  theme_bw()+
  scale_fill_okabe_ito()+
  # scale_x_continuous(breaks=seq(min(release_year),
  #                               max(release_year),1))+
  theme(axis.text.x = element_text(angle=45, hjust=1))+
  labs(x="Release Year",
       y="Median Travel Time to Lower Granite Dam (days)",
       fill="Release Group")
sthd.travel
# 
# look at chinook; maybe filter clearwater to just
# the comparables

chn.travel <- travel.sum |>
  filter(species=="Chinook",
         !release_group %in% c("Powell","Red River","Selway River",
                               "Clear Creek")) |>
  ggplot(aes(x=release_year,y=median_lgr))+
  geom_col(aes(fill=release_group,
               text=str_c(" Release Year:", release_year,
                          "<br>","Release Dates:",earliest_release,"-",latest_release,
                          "<br>","Median Travel Time to LGR:",median_lgr,"days",sep=" ")),
           position="dodge",
           color="black")+
  scale_fill_okabe_ito()+
  facet_wrap(~hatchery)+
  theme_bw()+
  theme(axis.text.x = element_text(angle=45, hjust=1))+
  labs(x="",
       y="Median Travel Time to Lower Granite Dam (days)",
       fill="Release Group")
chn.travel

ggplotly(chn.travel,
         tooltip = c("text"))


# think about how to grab returning adults?

detections.dat <- read_rds("historical/daily_detections_20_24")
