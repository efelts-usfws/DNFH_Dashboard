
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

conflicts_prefer(vroom::locale,
                 dplyr::filter)

# set the timeout above default of 60 seconds
# because sometimes the API calls are slow

options(timeout=300)

dir.create("data",recursive=TRUE, showWarnings = FALSE)

ptagis.dat <- readRDS("data/ptagis_sites")

# read in marking data

# after the first release, will run the commented out
# code and dump into RDS; should only need this onve
# a year untless there are corrections to the data

# dnfh.csv <- read_csv("data/Current Year DNFH Mark.csv") |>
#   mutate(release_date=mdy(`Release Date MMDDYYYY`),
#          mark_date=mdy(`Mark Date MMDDYYYY`),
#          hatchery=word(`Mark Site Name`,1,sep=" "),
#          release_sitecode=word(`Release Site Name`,1,sep=" "),
#          tag_file=`Mark File Name`) |>
#   select(pit_id=`Tag Code`,hatchery,
#          mark_date,release_site=`Release Site Name`,
#          release_sitecode,release_date,
#          species=`Species Name`,
#          length_mm=`Length mm`,
#          tag_file)
# 
# 
#  saveRDS(dnfh.csv,"data/dnfh_currentyr_mark")

# get marking data from the current year


dnfh_mark.dat <- read_rds("data/dnfh_currentyr_mark")


# bring in downstream detections at LGR
# from web API and get the first
# detection at LGR for each

# make a table to identify which dam each site is associated with

dam_key <- tibble(sitecode=c("GRS","GRJ","GRA",
                             "GOJ","GOA",
                             "LMJ","LMA",
                             "ICH",
                             "MCJ","MC2","MC1",
                             "JDJ","JO1","JO2",
                             "TD1","TD2",
                             "B2J","BCC","BO1","BO2","BO4",
                             "TWX"),
                  dam=c("Lower Granite","Lower Granite","Lower Granite",
                        "Little Goose","Little Goose",
                        "Lower Monumental","Lower Monumental",
                        "Ice Harbor",
                        "McNary","McNary","McNary",
                        "John Day","John Day","John Day",
                        "The Dalles","The Dalles",
                        "Bonneville","Bonneville","Bonneville","Bonneville","Bonneville",
                        "Estuary"),
                  dam_code=c("LGR","LGR","LGR",
                             "LGS","LGS",
                             "LOMO","LOMO",
                             "ICH",
                             "MCN","MCN","MCN",
                             "JD","JD","JD",
                             "TDA","TDA",
                             "BONN","BONN","BONN","BONN","BONN",
                             "TWX")) |> 
  mutate(dam_code=factor(dam_code,
                         levels=c("LGR","LGS","LOMO","ICH",
                                  "MCN","JD","TDA","BONN",
                                  "TWX")))

lowersnake_detections.dat <- vroom(file = "https://api.ptagis.org/reporting/reports/efelts60/file/DNFH%20Lower%20Snake%20Detections.csv",
                            delim = ",",
                            locale = locale(encoding= "UTF-16LE")) |> 
  mutate(obs_datetime=mdy_hms(`Obs Time`),
         sitecode=word(Site,1,sep=" ")) |> 
  select(pit_id=Tag,
         obs_datetime,
         site=Site,
         sitecode) |> 
  group_by(pit_id,site,sitecode) |> 
  slice(which.min(obs_datetime)) 


columbia_detections.dat <- vroom(file = "https://api.ptagis.org/reporting/reports/efelts60/file/DNFH%20Columbia%20Detections.csv",
                                        delim = ",",
                                        locale = locale(encoding= "UTF-16LE")) |> 
  mutate(obs_datetime=mdy_hms(`Obs Time`),
         sitecode=word(Site,1,sep=" ")) |> 
  select(pit_id=Tag,
         obs_datetime,
         site=Site,
         sitecode) |> 
  group_by(pit_id,site,sitecode) |> 
  slice(which.min(obs_datetime))


detections.bind <- 
  bind_rows(lowersnake_detections.dat,
            columbia_detections.dat) |> 
  left_join(dam_key,by="sitecode") |> 
  group_by(pit_id,dam_code) |> 
  summarize(first_detection=min(obs_datetime,na.rm=T)) |>
  ungroup() |> 
  complete(pit_id,dam_code,fill=list(first_detection = NA))


# Will join detection data to initial marking
# data; just starting with granite here
# but will expand to the rest used in
# CJS modeling

detections.join <- dnfh_mark.dat |> 
  mutate(release_year=year(release_date)) |> 
  left_join(detections.bind,by="pit_id") |> 
  select(pit_id,species,tag_file,hatchery,release_date,release_year,mark_date,
         release_sitecode,length_mm,
         dam_code,first_detection) |> 
  pivot_wider(names_from=dam_code,
              values_from=first_detection) |> 
  select(pit_id,species,tag_file,hatchery,release_date,release_year,mark_date,
         release_sitecode,length_mm,LGR,LGS,
         LOMO,ICH,MCN,JD,TDA,BONN,TWX) |> 
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
         twx_traveltime=floor((TWX_time-release_time)/86400),)



# build a QAQC check to see if any of the 
# fish that are shown as being released in the main
# stem were detected upstream and/or before they
# were supposed to be released. The main ones to 
# look for will be near where actual released occur,
# so Clear Creek an SF Clearwater arrays

# get all tags that were shown as being released
# on site

onsite.dat <- dnfh_mark.dat |> 
  filter(release_sitecode %in% c("DWORMS","DWORNF"))


# check to see how many were detected at CLC

upstream.dat <- vroom(file = "https://api.ptagis.org/reporting/reports/efelts60/file/Upstream%20Detections%20DNFH.csv",
                      delim = ",",
                      locale = locale(encoding= "UTF-16LE")) |> 
  rename(pit_id=`Tag`)


onsite_upstream_filter <- onsite.dat |> 
  filter(pit_id %in% upstream.dat$pit_id)

# drop any of those records from further analysis

detections.filtered <- detections.join |> 
  filter(!pit_id %in% onsite_upstream_filter$pit_id)


# think that's what's needed to make travel time plot,
# test on LGR here: for chinook early vs. late, just
# manually setting those for the relevant groups from
# Dworshak and Clearwater 

release_summary <- detections.filtered |> 
  group_by(species,hatchery,release_sitecode,release_date) |> 
  tally()


travel.dat <- detections.filtered  |> 
  mutate(release_grp_plot=case_when(
           release_sitecode=="DWORMS" ~ "Clearwater River",
           release_sitecode=="CLEARC" ~ "Clear Creek",
           release_sitecode=="CLWRSF" ~ "South Fork Clearwater River",
           release_sitecode=="NEWSOC" ~ "Newsome Creek",
           release_sitecode=="MEAD2C" ~ "Meadow Creek",
           release_sitecode=="KOOS" ~ "Kooskia",
           release_sitecode=="DWORNF" & release_date==as.Date("2025-03-27") ~ "North Fork Clearwater River Early",
           release_sitecode=="DWORNF" & release_date==as.Date("2025-04-10") ~ "North Fork Clearwater River Late",
           release_sitecode=="CLWHNF" & release_date==as.Date("2025-03-27") ~ "North Fork Clearwater River Early",
           release_sitecode=="CLWHNF" & release_date==as.Date("2025-04-10") ~ "North Fork Clearwater River Late",
           release_sitecode=="POWP" ~ "Powell",
           release_sitecode=="REDP" ~ "Red River",
           release_sitecode=="SELWY1" ~ "Selway River"
         )) |> 
  select(pit_id,species,tag_file,hatchery,release_date,release_year,mark_date,
         release_sitecode,release_group=release_grp_plot,length_mm,
         LGR,LGS,LOMO,ICH,MCN,JD,TDA,BONN,TWX,LGR_ch,LGS_ch,LOMO_ch,
         ICH_ch,MCN_ch,JD_ch,TDA_ch,BONN_ch,TWX_ch,release_time,LGR_time,
         LGS_time,LOMO_time,ICH_time,MCN_time,JD_time,TDA_time,BONN_time,
         TWX_time,lgr_traveltime,lgs_traveltime,lomo_traveltime,ich_traveltime,
         mcn_traveltime,jd_traveltime,tda_traveltime,bonn_traveltime,twx_traveltime)


# write that as an output for use in shiny

saveRDS(travel.dat,"data/travel")


# Now also want to track Adult returns, which will
# be queried by ocean age, basically find anything
# that is detected at > 0 years at large

# read in adult PTAGIS API

# need the historical marking data

mark_complete.dat <- read_rds("data/mark_2020_2024")

adult_detections.dat <- vroom(file = "https://api.ptagis.org/reporting/reports/efelts60/file/DNFH%20Adult%20Inseason.csv",
                              delim = ",",
                              locale = locale(encoding= "UTF-16LE")) |> 
  mutate(obs_sitecode=word(Site,1,sep=" "),
         obs_datetime=mdy_hms(`Obs Time`)) |> 
  select(pit_id=Tag,
         obs_sitecode,obs_datetime,
         species=`Species Name`) |> 
  filter(!species=="Coho") |> 
  select(-c(species)) |> 
  left_join(mark_complete.dat,by=c("pit_id")) |> 
  mutate(obs_year=year(obs_datetime),
         release_year=year(release_date),
         years_at_large=obs_year-release_year) |> 
  filter(years_at_large>0) |> 
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


sthd.search <- adult_detections.dat |> 
  filter(spawn_year==2026)


# make a conditional for which spawn year we're in,
# because it varies by species

today <- today()
year_today <- year(today)
month_today <- month(today)

if(month_today >= 7){
  chn_spawn_year <- year_today
  sthd_spawn_year <- year_today+1
} else {
  chn_spawn_year <- year_today
  sthd_spawn_year <- year_today
}


lgr_plot.dat <- adult_detections.dat |>
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
         annual_total=sum(daily_total))|> 
  filter(species=="Chinook" & spawn_year==chn_spawn_year|
           species=="Steelhead" & spawn_year==sthd_spawn_year) |> 
  mutate(dam="Lower Granite")
  

# same thing applied to bonneville

bonn_plot.dat <- adult_detections.dat |>
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
filter(species=="Chinook" & spawn_year==chn_spawn_year|
         species=="Steelhead" & spawn_year==sthd_spawn_year) |> 
  mutate(dam="Bonneville")

# same thing applied to the Dworshak ladder

dwor_plot.dat <- adult_detections.dat |>
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
  filter(species=="Chinook" & spawn_year==chn_spawn_year|
           species=="Steelhead" & spawn_year==sthd_spawn_year) |> 
  mutate(dam="Dworshak Ladder")

adult_bind <- bind_rows(lgr_plot.dat,
                        bonn_plot.dat,
                        dwor_plot.dat)

saveRDS(adult_bind,
        "data/adult_plot_inseason")



# library(ggplot2)
# library(plotly)
# # 
# travel.plot <- ggplot()+
#   geom_density(data=dist_plot.dat,adjust=1.5,alpha=0.5,
#                aes(x=LGR,group=release_grp_plot,fill=release_grp_plot))+
#   geom_vline(data=vline.dat,aes(xintercept=as.numeric(release_date),
#                                 group=release_grp_plot,
#                                 color=release_grp_plot),
#              linewidth=1.5)+
#   geom_vline(data=lgr_median.dat,
#              aes(xintercept = as.numeric(median_lgr),
#                  group=release_grp_plot,
#                  color=release_grp_plot),
#              linetype="dashed",
#              linewidth=1.25)+
#   scale_x_datetime(limits = c(xmin,xmax),
#                date_breaks="1 week",
#                date_labels="%b %d")+
#   scale_fill_manual(values=c("blue","red","purple"))+
#   scale_color_manual(values=c("blue","red","purple"))+
#   theme_bw()+
#   labs(x="Arrival Date at Lower Granite Dam",
#        y="Proportion of total arrivals",
#        fill="",
#        color="")+
#   theme(axis.text.x=element_text(angle=45,hjust=1))
# travel.plot
# 
# ggplotly(travel.plot, tooltip=NULL)







## grabbing water data from USGS gaging station
## at Peck, North Fork Clearwater



water.sites <- tibble(name=c("Clearwater (Peck)","Clearwater (Orofino)",
                             "Lolo Creek","SF Clearwater",
                             "Clear Creek","Selway",
                             "Lochsa"),
                      sitenumber=c("13341050","13340000",
                                   "13339500","13338500",
                                   "13337095","13336500",
                                   "13337000"))

# temp is coded as 00010, discharge is 00060

parm.cd <- c("00010","00060")

# put today's date into text format to 
# feed into the water data query

today.text <- as.character(today(tz="America/Los_Angeles"))

# read in site info for all the sites



daily.map <- map_dfr(water.sites$sitenumber, 
                     ~ readNWISdv(siteNumbers=.x,
                            parameterCd=parm.cd,
                            startDate="1990-01-01",
                            endDate=today.text)) |> 
  select(sitenumber=site_no,
         date=Date,
         mean_temp=X_00010_00003,
         mean_discharge=X_00060_00003) |> 
  mutate(date=as_date(date),
         year=year(date)) |> 
  left_join(water.sites,by="sitenumber")



current_year <- year(today())

water.dat <- daily.map |> 
   filter(year==current_year) |> 
   mutate(group=sitenumber)

saveRDS(water.dat,
        "data/water")

# process window count data from DART

bonneville_link <- "https://cbr.washington.edu/dart/cs/php/rpt/adult_daily.php?sc=1&outputFormat=csv&year=2025&proj=BON&span=no&startdate=1%2F1&enddate=12%2F31&run=&syear=2025&eyear=2025"

# need to add in a 2026 link but keep 2025 for STHD spawn year stuff

bonneville_link2 <- "https://www.cbr.washington.edu/dart/cs/php/rpt/adult_daily.php?sc=1&outputFormat=csv&year=2026&proj=BON&span=no&startdate=1%2F1&enddate=12%2F31&run=&syear=2026&eyear=2026"

lgr_link <- "https://cbr.washington.edu/dart/cs/php/rpt/adult_daily.php?sc=1&outputFormat=csv&year=2025&proj=LWG&span=no&startdate=1%2F1&enddate=12%2F31&run=&syear=2025&eyear=2025"

lgr_link2 <- "https://www.cbr.washington.edu/dart/cs/php/rpt/adult_daily.php?sc=1&outputFormat=csv&year=2026&proj=LWG&span=no&startdate=1%2F1&enddate=12%2F31&run=&syear=2026&eyear=2026"


current.links <- list(bonneville_link,lgr_link,bonneville_link2,lgr_link2)

today <- Sys.Date()
july_first <- as.Date(paste0(lubridate::year(today), "-07-01"))


# make a reference value for which SY we're in 
# depending on species

current_sy <- tibble(species=c("Steelhead","Chinook","Bull Trout",
                               "Chum","Coho","Lamprey_Day","Lamprey_Night",
                               "Pink","Shad","Sockeye","Spring Chinook",
                               "Summer Chinook","Fall Chinook")) |> 
  mutate(spawn_year=case_when(
    
    species=="Steelhead" & yday(today()) >= 183 ~ year(today())+1,
    
    TRUE ~ year(today())
  ))

current.dat <- map(current.links,read_csv) |> 
  bind_rows() |> 
  filter(!is.na(Date)) |> 
  mutate(year=year(Date)) |> 
  select(dam=Project,date=Date,year,
         run_type=`Chinook Run`,temp=TempC,
         Chinook=Chin,Jack_Chinook=JChin,
         Steelhead=Stlhd,Wild_Steelhead=WStlhd,
         Sockeye=Sock,Coho,Jack_Coho=JCoho,
         Shad,Lamprey_Day=LmpryDay,Lamprey_Night=LmpryNight,
         `Bull Trout`=BTrout,
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
  ,
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
         annual_total=sum(count)) |> 
  left_join(current_sy,by="species") |> 
  filter(spawn_year.x==spawn_year.y)

# read in completed years so the current year can be bound to it

completed.dat <- read_rds("data/window_counts_complete") 

all.dat_bind <- current.dat |> 
  bind_rows(completed.dat)

# save the combined data as RDS

saveRDS(all.dat_bind,
        "data/window_daily")
