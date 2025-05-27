
library(vroom) 
library(dataRetrieval)
library(conflicted)
library(stringr)
library(lubridate)
library(dplyr)
library(readr)
library(tidyr)
library(forcats)

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
                             "TD1",
                             "B2J","BCC","BO1","BO2","BO4"),
                  dam=c("Lower Granite","Lower Granite","Lower Granite",
                        "Little Goose","Little Goose",
                        "Lower Monumental","Lower Monumental",
                        "Ice Harbor",
                        "McNary","McNary","McNary",
                        "John Day","John Day","John Day",
                        "The Dalles",
                        "Bonneville","Bonneville","Bonneville","Bonneville","Bonneville"),
                  dam_code=c("LGR","LGR","LGR",
                             "LGS","LGS",
                             "LOMO","LOMO",
                             "ICH",
                             "MCN","MCN","MCN",
                             "JD","JD","JD",
                             "TDA",
                             "BONN","BONN","BONN","BONN","BONN")) |> 
  mutate(dam_code=factor(dam_code,
                         levels=c("LGR","LGS","LOMO","ICH",
                                  "MCN","JD","TDA","BONN")))

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
         LOMO,ICH,MCN,JD,BONN) |> 
  mutate(release_date=as.POSIXct(release_date)) |> 
  mutate(LGR_ch=ifelse(is.na(LGR),"0","1"),
          LGS_ch=ifelse(is.na(LGS),"0","1"),
          LOMO_ch=ifelse(is.na(LOMO),"0","1"),
          ICH_ch=ifelse(is.na(ICH),"0","1"),
          MCN_ch=ifelse(is.na(MCN),"0","1"),
          JD_ch=ifelse(is.na(JD),"0","1"),
          BONN_ch=ifelse(is.na(BONN),"0","1"),
         release_time=as.numeric(release_date),
         LGR_time=as.numeric(LGR),
         LGS_time=as.numeric(LGS),
         LOMO_time=as.numeric(LOMO),
         ICH_time=as.numeric(ICH),
         MCN_time=as.numeric(MCN),
         JD_time=as.numeric(JD),
         BON_time=as.numeric(BONN),
         lgr_traveltime=floor((LGR_time-release_time)/86400),
         lgs_traveltime=floor((LGS_time-release_time)/86400),
         lomo_traveltime=floor((LOMO_time-release_time)/86400),
         ich_traveltime=floor((ICH_time-release_time)/86400),
         mcn_traveltime=floor((MCN_time-release_time)/86400),
         jd_traveltime=floor((JD_time-release_time)/86400),
         bon_traveltime=floor((BON_time-release_time)/86400))



# build a QAQC check to see if any of the 
# fish that are shown as being released in the main
# stem were detected upstream and/or before they
# were supposed to be released. The main ones to 
# look for will be near where actual released occur,
# so Clear Creek an SF Clearwater arrays

# get all tags that were shown as being released
# on s-te

onsite.dat <- dnfh_mark.dat |> 
  filter(release_sitecode %in% c("DWORMS","DWORNF"))


# check to see how many were detected at CLC

upstream.dat <- vroom(file = "https://api.ptagis.org/reporting/reports/efelts60/file/Upstream%20Detections%20STHD.csv",
                      delim = ",",
                      locale = locale(encoding= "UTF-16LE")) |> 
  rename(pit_id=`Tag`)


onsite_upstream_filter <- onsite_sthd.dat |> 
  filter(pit_id %in% upstream.dat$pit_id)

# drop any of those records from further analysis

sthd_detections.filtered <- sthd_detections.join |> 
  filter(!pit_id %in% onsite_upstream_filter$pit_id)


# think that's what's needed to make travel time plot,
# test on LGR here:

sthd_travel.dat <- sthd_detections.filtered  |> 
  mutate(release_grp_plot=case_when(
           release_sitecode=="DWORMS" ~ "On-Site",
           release_sitecode=="CLEARC" ~ "Clear Creek",
           release_sitecode=="CLWRSF" ~ "South Fork Clearwater",
           release_sitecode=="NEWSOC" ~ "Newsome Creek",
           release_sitecode=="MEAD2C" ~ "Meadow Creek"
         ))

# write that as an output for use in shiny

saveRDS(sthd_travel.dat,"data/sthd_travel")

lgr_median.dat <- sthd_travel.dat |> 
  filter(hatchery=="DWOR") |> 
  group_by(release_grp_plot) |> 
  summarize(median_lgr=median(LGR,na.rm=T))


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

saveRDS(peck.dat,
        "data/peck_water")


