
library(shiny)
library(dplyr)
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


# automatically get the min for slider
# to be first of current year

min_date <- as_date(paste0(format(Sys.Date(), "%Y"),"-01-01"))

min_range_date <- as_date(paste0(format(Sys.Date(), "%Y"),"-03-01"))


# Build user interface

ui <- page_navbar(
  
  title="Dworshak National Fish Hatchery Evaluations",
  
  theme=bs_theme(bootswatch ="lux"),
  
  id="nav",
  
  sidebar=sidebar(width=300,
                  
                  conditionalPanel("input.nav===`Emigration Summaries`"),
                  
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
                                  selected="Clearwater (Peck)"),
                      
                      selectInput(inputId = "species_filter",
                                     label="Choose a species",
                                     choices=c("Chinook",
                                               "Steelhead"),
                                     selected="Steelhead")
                      
                      
                    )
                    
                    
                  )
                  
                  
                  ),
  
  nav_panel("Emigration Summaries",
            
            layout_columns(
              
              value_box(
                
                title="PIT Tag Summaries",
                value=textOutput("selected_species"),
                textOutput("tag_count"),
                showcase = fa("fish-fins")
                
              ),
              
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
                
                col_widths=c(6,6,6,6),
              
              card(
                card_header("Discharge"),
                plotlyOutput("flow_plot"),
                full_screen=TRUE
              ),
              
              card(
                
                card_header("Temperature"),
                plotlyOutput("temp_plot"),
                full_screen=TRUE
                
              ),
              
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
            
            
            )
  
  
)

server <- function(input,output,session){
  
  
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
               release_grp_plot %in% c("North Fork Clearwater River Early",
                                       "North Fork Clearwater River Late"))
      
      
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
  
    dat <- emigration_reactive() |> 
      filter(!is.na(LGR)) |> 
      mutate(release_grp_plot=factor(release_grp_plot,
                                     levels = unique(release_grp_plot))) 
      
    
    vline.dat <- dat |> 
      group_by(hatchery,release_grp_plot) |> 
      summarize(release_date=first(release_date),
                .groups= "drop") 
      
    lgr_median.dat <- dat |> 
      group_by(hatchery,release_grp_plot) |> 
      summarize(median_lgr=median(LGR,na.rm=T),
                release_date=first(release_date),
                travel_days=as.numeric(floor(median_lgr-release_date)),
                .groups="drop") 
    
    xmin <- min(vline.dat$release_date)-days(2)
    xmax <- today()+days(3)
    
    travel.plot <- ggplot()+
      geom_density(data=dat,adjust=0.5,alpha=0.5,
                   aes(x=LGR,fill=release_grp_plot,
                       color=release_grp_plot,
                       name=release_grp_plot,
                       legendgroup=release_grp_plot))+
      geom_vline(data=vline.dat,aes(xintercept=as.numeric(release_date),
                                    color=release_grp_plot,
                                    fill=release_grp_plot,
                                    name=release_grp_plot,
                                    legendgroup=release_grp_plot),
                 linewidth=1.5)+
      geom_vline(data=lgr_median.dat,
                 aes(xintercept = as.numeric(median_lgr),
                     text=str_c(" Release Group:",release_grp_plot,
                                "<br>",
                                "Median Date LGR:", format(median_lgr,"%Y-%m-%d"),
                                "<br>",
                                "Release Date:",format(release_date,"%Y-%m-%d"),
                                "<br>",
                                "Median Days to LGR:",travel_days,
                                sep=" "),
                     color=release_grp_plot,
                     fill=release_grp_plot,
                     name=release_grp_plot,
                     legendgroup=release_grp_plot),
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
      mutate(release_grp_plot=factor(release_grp_plot,
                                     levels = unique(release_grp_plot))) 
    
    
    vline.dat <- dat |> 
      group_by(hatchery,release_grp_plot) |> 
      summarize(release_date=first(release_date),
                .groups= "drop") 
    
    bon_median.dat <- dat |> 
      group_by(hatchery,release_grp_plot) |> 
      summarize(median_bon=median(BONN,na.rm=T),
                release_date=first(release_date),
                travel_days=as.numeric(floor(median_bon-release_date)),
                .groups="drop") 
    
    xmin <- min(vline.dat$release_date)-days(2)
    xmax <- today()+days(3)
    
    travel.plot <- ggplot()+
      geom_density(data=dat,adjust=0.5,alpha=0.5,
                   aes(x=BONN,fill=release_grp_plot,
                       color=release_grp_plot,
                       name=release_grp_plot,
                       legendgroup=release_grp_plot))+
      geom_vline(data=vline.dat,aes(xintercept=as.numeric(release_date),
                                    color=release_grp_plot,
                                    fill=release_grp_plot,
                                    name=release_grp_plot,
                                    legendgroup=release_grp_plot),
                 linewidth=1.5)+
      geom_vline(data=bon_median.dat,
                 aes(xintercept = as.numeric(median_bon),
                     text=str_c(" Release Group:",release_grp_plot,
                                "<br>",
                                "Median Date BONN:", format(median_bon,"%Y-%m-%d"),
                                "<br>",
                                "Release Date:",format(release_date,"%Y-%m-%d"),
                                "<br>",
                                "Median Days to BONN:",travel_days,
                                sep=" "),
                     color=release_grp_plot,
                     fill=release_grp_plot,
                     name=release_grp_plot,
                     legendgroup=release_grp_plot),
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
}



shinyApp(ui, server)

