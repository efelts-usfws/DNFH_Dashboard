
library(shiny)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(lubridate)
library(bslib)
library(DT)
library(shinyWidgets)
library(conflicted)
library(plotly)
library(dataRetrieval)
library(bsicons)
library(viridis)
library(scales)
library(fontawesome)
library(readr)
library(ggokabeito)

conflicts_prefer(DT::renderDT,
                 dplyr::filter,
                 dplyr::lag,
                 plotly::layout)

# read in water data

water.dat <- read_rds("data/water")

latest_discharge <- water.dat |> 
  group_by(name) |> 
  slice(which.max(date))

# read in emigration data

emigration.dat <- read_rds("data/travel") 

# get the latest date a given species is
# encountered at LGR and Bonneville to
# set the max on the emigration plot; add a week
# buffer for the flot

emigration.max <- emigration.dat |> 
  group_by(species) |> 
  summarize(LGR_max=max(LGR,na.rm=T)+days(7),
            BON_max=max(BONN,na.rm=T)+days(7))


# summarize current year emigration for comparison plots

current_emigration_summaries <- emigration.dat |> 
  group_by(hatchery,species,release_year,release_group) |> 
  summarize(median_lgr=floor(median(lgr_traveltime,na.rm=T)),
            median_lgr_date=as_date(median(LGR,na.rm=T)),
            lgr_detections=sum(!is.na(LGR)),
            median_bonn=floor(median(bonn_traveltime,na.rm=T)),
            median_bonn_date=as_date(median(BONN,na.rm=T)),
            bonn_detections=sum(!is.na(BONN)),
            earliest_release=format(min(release_date),"%B %d"),
            latest_release=format(max(release_date),"%B %d"))


# read in historical emigration data summaries

emigration_summaries_historical.dat <- read_rds("data/juv_emigration_20_24")


# bind historical to current emigration
# summaries 

emigration_summaries.dat <- bind_rows(emigration_summaries_historical.dat,
                                      current_emigration_summaries)

# automatically get the min for slider
# to be first of current year

min_date <- as_date(paste0(format(Sys.Date(), "%Y"),"-01-01"))

min_range_date <- as_date(paste0(format(Sys.Date(), "%Y"),"-03-01"))


# read in adult plotting data and extend to
# the end of a given spawning season

# helper df for limits on max for dummy date depending
# on species

species_max_dates <- tibble(
  species=c("Chinook","Steelhead",
            "Chinook","Steelhead",
            "Chinook","Steelhead"),
  dam=c("Lower Granite","Lower Granite",
        "Bonneville","Bonneville",
        "Dworshak Ladder","Dworshak Ladder"),
  max_date=as.Date(c("1976-07-15","1977-05-01",
                     "1976-07-15","1977-05-01",
                     "1976-09-15","1977-05-01"))
)

# to append additional years as they come

complete_adult.dat25 <- read_rds("data/complete_adult_plot_25") |> 
  filter(species=="Steelhead")

complete_adult.dat <- read_rds("data/complete_adult_plot_20_24") |> 
  bind_rows(complete_adult.dat25) |> 
  left_join(species_max_dates,by=c("species","dam")) |> 
  group_by(species,hatchery,spawn_year,dam) |> 
  mutate(min_date=min(dummy_date,na.rm=TRUE)) |> 
  complete(
    dummy_date=seq(min(min_date), max(max_date), by="day")
  ) |> 
  ungroup() |> 
  select(-c(min_date,max_date)) |> 
  mutate(across(daily_total,~replace_na(.x,0))) |> 
  group_by(species,hatchery,spawn_year,dam) |> 
  fill(c("running_total","annual_total"),.direction="down")

# read in in-progress adult plotting data

# to complete through current date need to set
# dummy date and have that be dependent on species


if(yday(today()) >= 106 & yday(today()) <= 212) {
  default_adult_species <-  "Chinook"
} else {
  
  default_adult_species <- "Steelhead"
  
}

month(today()-days(1))

# need to make this robust to spawn year...if current month is July - December it
# will need to be different than if it's jan-june for steelhead

inseason_max_dates <- tibble(species=c("Chinook","Steelhead")) |> 
  mutate(day_of_year=yday(today()-days(1)),
         season_month=month(today()-days(1)),
         inseason_max=case_when(
           
           species=="Steelhead" & season_month<7 ~ as.Date(day_of_year,origin="1977-01-01"),
           TRUE ~ as.Date(day_of_year, origin="1976-01-01")
           
           
         )) 




inseason_adult.dat <- read_rds("data/adult_plot_inseason")|> 
  left_join(inseason_max_dates,by="species") |> 
  group_by(species,hatchery,spawn_year,dam) |> 
  mutate(min_date=min(dummy_date,na.rm=TRUE)) |> 
  complete(dummy_date=seq(min(min_date),max(inseason_max),by="day")) |> 
  ungroup() |> 
  select(-c(min_date,inseason_max)) |> 
  mutate(across(daily_total,~replace_na(.x,0))) |> 
  group_by(species,hatchery,spawn_year,dam) |> 
  fill(c("running_total","annual_total"),.direction="down")

# right now only using SY 23 and up bc that's whats
# complete for both species at this point

adult.bind <- complete_adult.dat |> 
  bind_rows(inseason_adult.dat) |> 
  filter(spawn_year>=2023)


# make a reference value for which SY we're in 
# depending on species

current_sy <- tibble(species=c("Steelhead","Chinook")) |> 
  mutate(spawn_year=case_when(
  
  species=="Steelhead" & yday(today()) >= 183 ~ year(today())+1,
  
  TRUE ~ year(today())
))

# make a reference for the last week 

last_week <- yday(today()-days(7))

# make the default selection for adult returning
# depend on today's date; for now setting to be
# Chinook if April 15-July 31 otherwise steelhead



if(yday(today()) >= 106 & yday(today()) <= 212) {
  default_adult_species <-  "Chinook"
} else {
  
  default_adult_species <- "Steelhead"
  
}



# Build user interface

ui <- page_navbar(
  
  title="Dworshak National Fish Hatchery Evaluations",
  
  theme=bs_theme(bootswatch ="lux"),
  
  id="nav",
  
  sidebar=sidebar(width=300,
                  
                  conditionalPanel("input.nav===`Current Year Juveniles`",
                  
                  accordion(
                    
                    accordion_panel(
                      
                      "Inputs",
                      
                      selectInput(inputId = "species_filter",
                                     label="Choose a species",
                                     choices=c("Chinook",
                                               "Steelhead"),
                                     selected="Steelhead")

                      
                    )
                    )),
                  
                  conditionalPanel("input.nav===`Current Year Adults`",
                                   
                                   accordion(
                                     
                                     accordion_panel(
                                       
                                       "Inputs",
                                       
                                       sliderInput(inputId = "adult_years",
                                                   label="Choose a range of Spawn Years",
                                                   min=min(adult.bind$spawn_year),
                                                   max=max(adult.bind$spawn_year),
                                                   value=c(min(adult.bind$spawn_year),
                                                           max(adult.bind$spawn_year)),
                                                   sep=""),
                                       
                                       selectInput(inputId = "adult_species_filter",
                                                   label="Choose a species",
                                                   choices=c("Chinook",
                                                             "Steelhead"),
                                                   selected=default_adult_species),
                                       
                                       selectInput(inputId = "adult_location",
                                                   "Choose a location of detections",
                                                   choices=c("Lower Granite",
                                                             "Bonneville",
                                                             "Dworshak Ladder"),
                                                   selected = "Lower Granite")
                                       
                                       
                                     )
                                   )),
                  
                  conditionalPanel("input.nav===`Water Data`",
                                   
                  
                  accordion(
                    
                    accordion_panel(
                      
                      "Inputs",
                      
                      
                      sliderInput(inputId="user_dates",
                                  label="Choose a Date Range for water Data",
                                  min=min_date,
                                  max=today(),
                                  value=c(min_range_date,today())),
                      
                      selectInput(inputId = "water_site",
                                  label="Choose a gaging station",
                                  choices=sort(unique(water.dat$name)),
                                  selected="Clearwater (Peck)")
                      
                    )
                  )
                  
                  ),
                  
                  conditionalPanel("input.nav===`Emigration Comparison`",
                                   
                    accordion(
                      
                      accordion_panel(
                        
                        "Inputs",
                        
                        sliderInput(inputId="comparison_years",
                                    label="Choose a Range of Release Years",
                                    min=min(emigration_summaries.dat$release_year),
                                    max=max(emigration_summaries.dat$release_year),
                                    value=c((max(emigration_summaries.dat$release_year)-5),
                                             max(emigration_summaries.dat$release_year)),
                                    sep=""),
                        
                        selectInput(inputId = "comparison_species",
                                    label="Choose a species",
                                    choices=c("Chinook",
                                              "Steelhead"),
                                    selected="Steelhead")
                       
                      )
                      )
                  )            
                                   
                  ),
  
  nav_panel("Current Year Juveniles",
            
            layout_columns(
              
              value_box(
                
                title="PIT Tag Summaries",
                value=textOutput("selected_species"),
                textOutput("tag_count"),
                showcase = fa("fish-fins")
                
              )
              
            ),
            
            page_fillable(
              
              layout_columns(
                
                col_widths=c(6,6),
           
              card(
                
                card_header("Timing to Lower Granite Dam"),
                plotlyOutput("lgr_timing_plot"),
                full_screen = TRUE
                
              ),
              
              card(
                
                card_header("Timing to Bonneville Dam"),
                plotlyOutput("bon_timing_plot"),
                full_screen = TRUE
                
              )
              
              
            )
            
            )
            
            
            ),
  
  nav_panel("Current Year Adults",
            
            layout_columns(
              
              value_box(
                
                title="Number Passed, Current Year-to-date",
                value=textOutput("adult_ytd_dwr"),
                p(textOutput("adult_ytd_koos")),
                p(textOutput("adult_ytd_clwh")),
                showcase = fa("fish-fins")
                
                
              ),
              
              
              value_box(
                
                title="New in the last week",
                value=textOutput("adult_lw_dwr"),
                p(textOutput("adult_lw_koos")),
                p(textOutput("adult_lw_clwh")),
                showcase = bs_icon("graph-up-arrow")
                
                
                
              
            )),
            
            page_fillable(
              
              layout_columns(
                
                col_widths=c(6,6),
                
                
                card(
                  
                  card_header("Daily Passage"),
                  plotlyOutput("lgr_adult_count"),
                  full_screen = TRUE
                  
                ),
                
                
                card(
                  
                  card_header("Cumulative Counts"),
                  plotlyOutput("lgr_adult_cum"),
                  full_screen = TRUE
                  
                )
                
                
              )
              
              
            )
    
    
  ),
  
  nav_panel("Emigration Comparison",
            
            page_fillable(
              
              layout_columns(
                
                col_widths = c(6,6),
                
                card(
                  
                  card_header("Timing to Lower Granite"),
                  plotlyOutput("lgrmedian_comp_plot"),
                  full_screen=TRUE
                  
                  
                ),
                
                card(
                  
                  card_header("Timing to Bonneville"),
                  plotlyOutput("bonnmedian_comp_plot"),
                  full_screen = TRUE
                  
                )
                
                
              )
              
            )
    
    
    
    
  ),
  
  nav_panel("Water Data",
            
            
            layout_columns(
              
              value_box(
                
                title="Water Conditions",
                value=textOutput("water_site"),
                textOutput("discharge"),
                textOutput("temp"),
                showcase=fa("tint")
                
                
              )
              
              
            ),
            
            
            page_fillable(
              
              
              layout_columns(
                
                col_widths = c(6,6),
                
                card(
                  card_header("Discharge"),
                  plotlyOutput("flow_plot"),
                  full_screen=TRUE
                ),
                
                card(
                  
                  card_header("Temperature"),
                  plotlyOutput("temp_plot"),
                  full_screen=TRUE
                  
                )
                
                
              )
              
              
            )
  )

)

server <- function(input,output,session){
  
  

  
  # make a reactive of emigration data that is 
  # filtered on user input
  
  emigration_reactive <- reactive({
    
    if(input$species_filter=="Steelhead")
      
    {
      
      emigration.dat |> 
      filter(species == input$species_filter,
             hatchery=="DWOR")
      
    }
    
    else{
      
      emigration.dat |> 
        filter(species==input$species_filter,
               release_group %in% c("North Fork Clearwater River Early",
                                       "North Fork Clearwater River Late",
                                       "Kooskia"))
      
      
    }
    
  })
  
  # get text output of selected species
  
  output$selected_species <- renderText({
    
    dat <- emigration_reactive()
    
    first(dat$species)
    
  })
  
  # get count of number tagged for value box
  
  output$tag_count <- renderText({
    
    dat <- emigration_reactive() |> 
      filter(hatchery=="DWOR")
    
    count <- nrow(dat)
    
    str_c("Number Tagged: ",comma(count))
    
    
  })
  

  # build the passage plot for LGR
  
  output$lgr_timing_plot <- renderPlotly({
  
    req(input$species_filter)
    
    dat <- emigration_reactive() |> 
      filter(!is.na(LGR)) |> 
      mutate(release_group=factor(release_group,
                                     levels = unique(release_group))) 
      
    
    vline.dat <- dat |> 
      group_by(hatchery,release_group) |> 
      summarize(release_date=first(release_date),
                .groups= "drop") 
      
    lgr_median.dat <- dat |> 
      group_by(hatchery,release_group) |> 
      summarize(median_lgr=median(LGR,na.rm=T),
                release_date=first(release_date),
                travel_days=as.numeric(floor(median_lgr-release_date)),
                .groups="drop") 
    
    xmin <- min(vline.dat$release_date)-days(2)
    xmax <- emigration.max |> 
      filter(species==input$species_filter) |> 
      pull(LGR_max)
    
    travel.plot <- ggplot()+
      geom_density(data=dat,adjust=0.5,alpha=0.5,
                   aes(x=LGR,fill=release_group,
                       color=release_group,
                       name=release_group,
                       legendgroup=release_group))+
      geom_vline(data=vline.dat,aes(xintercept=as.numeric(release_date),
                                    color=release_group,
                                    fill=release_group,
                                    name=release_group,
                                    legendgroup=release_group),
                 linewidth=1.5)+
      geom_vline(data=lgr_median.dat,
                 aes(xintercept = as.numeric(median_lgr),
                     text=str_c(" Release Group:",release_group,
                                "<br>",
                                "Median Date LGR:", format(median_lgr,"%Y-%m-%d"),
                                "<br>",
                                "Release Date:",format(release_date,"%Y-%m-%d"),
                                "<br>",
                                "Median Days to LGR:",travel_days,
                                sep=" "),
                     color=release_group,
                     fill=release_group,
                     name=release_group,
                     legendgroup=release_group),
                 linetype="dashed",
                 linewidth=1.25)+
      scale_x_datetime(limits = c(xmin,xmax),
                       date_breaks="1 week",
                       date_labels="%b %d")+
      facet_wrap(~hatchery,ncol=1)+
      scale_fill_manual(values=c("blue","red","purple"))+
      scale_color_manual(values=c("blue","red","purple"))+
      theme_bw()+
      labs(x="",
           y="Proportion of total arrivals",
           fill="",
           color="Release Group")+
      theme(axis.text.x=element_text(angle=45,hjust=1))
    travel.plot
    
    ggplotly(travel.plot, tooltip=c("text"))
    

    
  })
  
  # build the passage plot for Bonneville
  
  output$bon_timing_plot <- renderPlotly({
    
    dat <- emigration_reactive() |> 
      filter(!is.na(BONN)) |> 
      mutate(release_group=factor(release_group,
                                     levels = unique(release_group))) 
    
    
    vline.dat <- dat |> 
      group_by(hatchery,release_group) |> 
      summarize(release_date=first(release_date),
                .groups= "drop") 
    
    bon_median.dat <- dat |> 
      group_by(hatchery,release_group) |> 
      summarize(median_bon=median(BONN,na.rm=T),
                release_date=first(release_date),
                travel_days=as.numeric(floor(median_bon-release_date)),
                .groups="drop") 
    
    xmin <- min(vline.dat$release_date)-days(2)
    xmax <- emigration.max |> 
      filter(species==input$species_filter) |> 
      pull(BON_max)
    
    
    travel.plot <- ggplot()+
      geom_density(data=dat,adjust=0.5,alpha=0.5,
                   aes(x=BONN,fill=release_group,
                       color=release_group,
                       name=release_group,
                       legendgroup=release_group))+
      geom_vline(data=vline.dat,aes(xintercept=as.numeric(release_date),
                                    color=release_group,
                                    fill=release_group,
                                    name=release_group,
                                    legendgroup=release_group),
                 linewidth=1.5)+
      geom_vline(data=bon_median.dat,
                 aes(xintercept = as.numeric(median_bon),
                     text=str_c(" Release Group:",release_group,
                                "<br>",
                                "Median Date BONN:", format(median_bon,"%Y-%m-%d"),
                                "<br>",
                                "Release Date:",format(release_date,"%Y-%m-%d"),
                                "<br>",
                                "Median Days to BONN:",travel_days,
                                sep=" "),
                     color=release_group,
                     fill=release_group,
                     name=release_group,
                     legendgroup=release_group),
                 linetype="dashed",
                 linewidth=1.25)+
      scale_x_datetime(limits = c(xmin,xmax),
                       date_breaks="1 week",
                       date_labels="%b %d")+
      facet_wrap(~hatchery,
                 ncol=1)+
      scale_fill_manual(values=c("blue","red","purple"))+
      scale_color_manual(values=c("blue","red","purple"))+
      theme_bw()+
      labs(x="",
           y="Proportion of total arrivals",
           fill="",
           color="")+
      theme(axis.text.x=element_text(angle=45,hjust=1))
    travel.plot
    
    ggplotly(travel.plot, tooltip=c("text"))
    
    
    
  })
  
  # reactive data for adult detecitons
  
  adult_reactive <- reactive({
    
   adult.bind |> 
      filter(dam == input$adult_location,
             species == input$adult_species_filter,
             spawn_year>=min(input$adult_years),
             spawn_year<=max(input$adult_years))
    
    
  })
  
  # get summary values for number of adults over the 
  # various dams year-to-date.
  
  output$adult_ytd_dwr <- renderText({
    
    sy.dat <- current_sy |> 
      filter(species == input$adult_species_filter)
    
    dat <- adult_reactive() |> 
    filter(hatchery=="DWOR",
           spawn_year==sy.dat$spawn_year) |> 
      slice(which.max(running_total))
    
      str_c("Dworshak: ",dat$annual_total)
      
    })
  
  output$adult_ytd_koos <- renderText({
    
    sy.dat <- current_sy |> 
      filter(species == input$adult_species_filter)
    
    dat <- adult_reactive() |> 
      filter(hatchery=="KOOS",
             spawn_year==sy.dat$spawn_year) |> 
      slice(which.max(running_total))
    
    req(nrow(dat)>0)
    
    str_c("Kooskia: ",dat$annual_total)
    
  })
  
  
  output$adult_ytd_clwh <- renderText({
    
    sy.dat <- current_sy |> 
      filter(species == input$adult_species_filter)
    
    dat <- adult_reactive() |> 
      filter(hatchery=="CLWH",
             spawn_year==sy.dat$spawn_year) |> 
      slice(which.max(running_total))
    
    str_c("Clearwater: ",dat$annual_total)
    
  })
  
  # get summary values for number of new adults over the 
  # various dams in the last week
  
  output$adult_lw_dwr <- renderText({
    
    sy.dat <- current_sy |> 
      filter(species == input$adult_species_filter)
    
    dat <- adult_reactive() |> 
      filter(hatchery=="DWOR",
             spawn_year==sy.dat$spawn_year) |> 
      filter(yday(dummy_date)>=last_week,
             yday(dummy_date)<=yday(today())) |> 
      group_by(hatchery) |> 
      summarize(new_count=sum(daily_total))
    
    req(nrow(dat)>0)
    
    str_c("Dworshak: ",dat$new_count)
    
  })
  
  output$adult_lw_koos <- renderText({
    
    sy.dat <- current_sy |> 
      filter(species == input$adult_species_filter)
    
    dat <- adult_reactive() |> 
      filter(hatchery=="KOOS",
             spawn_year==sy.dat$spawn_year) |> 
      filter(yday(dummy_date)>=last_week,
             yday(dummy_date)<=yday(today())) |> 
      group_by(hatchery) |> 
      summarize(new_count=sum(daily_total))
    
    req(nrow(dat)>0)
    
    str_c("Kooskia: ",dat$new_count)
    
  })
  
  output$adult_lw_clwh <- renderText({
    
    sy.dat <- current_sy |> 
      filter(species == input$adult_species_filter)
    
    dat <- adult_reactive() |> 
      filter(hatchery=="CLWH",
             spawn_year==sy.dat$spawn_year) |> 
      filter(yday(dummy_date)>=last_week,
             yday(dummy_date)<=yday(today())) |> 
      group_by(hatchery) |> 
      summarize(new_count=sum(daily_total))
    
    req(nrow(dat)>0)
    
    str_c("Clearwater: ",dat$new_count)
    
  })
  
  
  # reactive plot for adult daily counts
  
  output$lgr_adult_count <- renderPlotly({
    
    
    dat <- adult_reactive()
    
    plot1 <- dat |> 
      ggplot(aes(x=dummy_date,y=daily_total,
                 fill=as.factor(spawn_year)))+
      geom_col(aes(text=str_c(" Date:", format(dummy_date, "%B %d"),
                              "<br>",
                              "Number Passed:",daily_total,
                              sep=" ")),show.legend = F)+
      scale_fill_okabe_ito()+
      facet_grid(hatchery~spawn_year,
                 scales="free_y")+
      theme_bw()+
      scale_x_date(date_labels= "%b")+
      labs(x="",
           y="Daily Adults Passed",
           fill="")
    
    ggplotly(plot1,
             tooltip=c("text")) |> 
      layout(showlegend=FALSE)
    
    
  })
  
  # reactive plot for cumulative adults
  
  output$lgr_adult_cum <- renderPlotly({
    
    dat <- adult_reactive() |> 
      left_join(current_sy,by="species") |> 
      mutate(yr_category=case_when(
        
        spawn_year.x==spawn_year.y ~ "Current",
        TRUE ~ "Previous"
        
      ))
    
    plot1 <- dat |> 
      ggplot(aes(x=dummy_date,y=running_total,group=spawn_year.x,
                 color=as.factor(yr_category)))+
      geom_line(aes(text=str_c(" Date:", format(dummy_date, "%B %d"),
                               "<br>",
                               "Spawn Year:",spawn_year.x,
                               "<br>",
                               "Number Passed:",running_total,
                               sep=" ")))+
      scale_color_manual(values=c("steelblue","gray70"))+
      facet_wrap(~hatchery,
                 scales="free_y")+
      theme_bw()+
      labs(x="",y="Cumulative Number Passed",
           color="Spawn Year")+
      theme(axis.text.x = element_text(angle=45,hjust=1))+
      scale_x_date(date_breaks = "1 week", date_labels= "%b %d")
    
    ggplotly(plot1,
             tooltip = c("text"))
    
  })
  
  # reactive data frame for emigration comparison plots
  
  emigration_comp_reactive <- reactive({
    
    dat <- emigration_summaries.dat |> 
      filter(release_year>=min(input$comparison_years),
             release_year<=max(input$comparison_years),
             species==input$comparison_species)
    
    if(input$comparison_species=="Chinook")
      
    {
      
      dat |> 
        filter(!release_group %in% c("Powell","Red River","Selway River",
                                    "Clear Creek"))
      
    }
    
    else{
      
      dat |> 
        filter(!release_group %in% c("Meadow Creek","Snake Kelts","Newsome Creek"))
      
    }
    
    
    
  })
  
  # construct median time to LGR plot comparing years within release groups
  
  output$lgrmedian_comp_plot <- renderPlotly({
    
    dat <- emigration_comp_reactive()
    
    plot1 <- dat |> 
      ggplot(aes(x=release_year,y=median_lgr))+
      geom_col(aes(fill=release_group,
                   text=str_c(" Release Year:", release_year,
                              "<br>","Release Dates:",earliest_release,"-",latest_release,
                              "<br>","Median Travel Time to LGR:",median_lgr,"days",sep=" ")),
               position="dodge",
               color="black")+
      facet_wrap(~hatchery)+
      theme_bw()+
      scale_fill_okabe_ito()+
      scale_x_continuous(breaks=seq(min(dat$release_year),
                                     max(dat$release_year),1))+
      theme(axis.text.x = element_text(angle=45, hjust=1))+
      labs(x="",
           y="Median Travel Time to Lower Granite Dam (days)",
           fill="Release Group")
    plot1
    
    ggplotly(plot1,
             tooltip = c("text"))
    
    
  })
  
  # same plot comparing years but timing to Bonneville
  
  
  output$bonnmedian_comp_plot <- renderPlotly({
    
    dat <- emigration_comp_reactive()
    
    plot1 <- dat |> 
      ggplot(aes(x=release_year,y=median_bonn))+
      geom_col(aes(fill=release_group,
                   text=str_c(" Release Year:", release_year,
                              "<br>","Release Dates:",earliest_release,"-",latest_release,
                              "<br>","Median Travel Time to BONN:",median_bonn,"days",sep=" ")),
               position="dodge",
               color="black")+
      facet_wrap(~hatchery)+
      theme_bw()+
      scale_fill_okabe_ito()+
      scale_x_continuous(breaks=seq(min(dat$release_year),
                                    max(dat$release_year),1))+
      theme(axis.text.x = element_text(angle=45, hjust=1))+
      labs(x="",
           y="Median Travel Time to Bonneville Dam (days)",
           fill="Release Group")
    plot1
    
    ggplotly(plot1,
             tooltip = c("text"))
    
    
  })
  
  
  # filter water data reactively based on
  # which site is selected by the user
  
  water_reactive <- reactive({
    
    water.dat |> 
      filter(name == input$water_site)
    
    
  })
  
  # get text output of selected water site
  
  output$water_site <- renderText({
    
    dat <- water_reactive()
    
    first(dat$name)
    
  })
  
  # get text output of latest discharge value
  
  output$discharge <- renderText({
    
    dat <- water_reactive() |> 
      slice(which.max(date))
    
    str_c("Discharge: ",comma(dat$mean_discharge)," CFS")
    
    
  })
  
  # get text output of latest temp value
  
  output$temp <- renderText({
    
    dat <- water_reactive() |> 
      slice(which.max(date)) |> 
      mutate(mean_temp_f=(mean_temp*(9/5))+32)
    
    str_c("Temperature: ",dat$mean_temp," C","
          (",round(dat$mean_temp_f,1)," F)")
    
    
  })
  
  # get flow plot reactively
  
  flowplot_reactive <- reactive({
    
    dat <- water_reactive()
    
    plot_min <- min(input$user_dates)
    plot_max <- max(input$user_dates)
    
    flow.plot <- dat %>% 
      mutate(date=as_date(date)) %>% 
      ggplot(aes(x=date,y=mean_discharge,group=group))+
      geom_line(aes(text=str_c(" Date:",date,
                               "<br>","Mean Discharge (cfs): ",comma(mean_discharge),
                               sep=" ")))+
      scale_x_date(date_breaks = "1 week", date_labels="%b %d",
                   limits=c(as.Date(plot_min),as.Date(plot_max)))+
      theme_bw()+
      theme(axis.text.x=element_text(angle=45,hjust=1))+
      labs(x="",y="Mean Daily Discharge")
    
    
  })
  
  output$flow_plot <- renderPlotly({
    
    plot1 <- flowplot_reactive()
    
    ggplotly(plot1,
             tooltip=c("text"))
    
  })
  
  tempplot_reactive <- reactive({
    
    dat <- water_reactive()
    
    plot_min <- min(input$user_dates)
    plot_max <- max(input$user_dates)
    
    temp.plot <- dat %>% 
      mutate(date=as_date(date),
             mean_temp_f=(mean_temp*(9/5))+32) %>% 
      ggplot(aes(x=date,y=mean_temp,group=group))+
      geom_line(aes(text=str_c(" Date:",date,
                               "<br>","Mean Temp (C): ",mean_temp,
                               "<br>","Mean Temp (F):",round(mean_temp_f,1),
                               sep=" ")))+
      scale_x_date(date_breaks = "1 week", date_labels="%b %d",
                   limits=c(as.Date(plot_min),as.Date(plot_max)))+
      theme_bw()+
      theme(axis.text.x=element_text(angle=45,hjust=1))+
      labs(x="",y="Mean Daily Temperature")
    
  })
  
  
  output$temp_plot <- renderPlotly({
    
    plot1 <- tempplot_reactive()
    
    ggplotly(plot1,
             tooltip=c("text"))
    
  })
}

shinyApp(ui, server)

