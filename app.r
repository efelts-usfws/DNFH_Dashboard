
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

emigration.dat <- read_rds("data/travel") |> 
  filter(hatchery=="DWOR")

# build some pieces for the timing plot

vline.dat <- emigration.dat |> 
  group_by(hatchery,species,release_grp_plot) |> 
  summarize(release_date=first(release_date))

lgr_median.dat <- emigration.dat |> 
  group_by(release_grp_plot,
           species) |> 
  summarize(median_lgr=median(LGR,na.rm=T))

xmin <- min(vline.dat$release_date)-days(2)
xmax <- today()+days(3)

detected_count <- emigration.dat |> 
  rowwise() |> 
  filter(any((!is.na(c_across(LGR:BONN)))))

# need to change logic here to apply to multiple species

detected_percent <- str_c((round(nrow(detected_count)/
                                        nrow(emigration.dat)*100)),
                               "%",sep=" ")


lgr_count <- emigration.dat |> 
  group_by(hatchery,species) |> 
  summarize(lgr=sum(!is.na(LGR)))

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
                  
                  conditionalPanel("input.nav===`Steelhead Emigration`"),
                  
                  accordion(
                    
                    accordion_panel(
                      
                      "Inputs",
                      
                      
                      sliderInput(inputId="user_dates",
                                  label="Choose a Date Range for water Data",
                                  min=min_date,
                                  max=today(),
                                  value=c(min_range_date,today()))
                      
                      
                    )
                    
                    
                  )
                  
                  
                  ),
  
  nav_panel("Emigration Summaries",
            
            layout_columns(
              
              value_box(
                
                title="PIT Tagged Steelhead",
                value=str_c("Marked: ",comma(nrow(sthd_emigration.dat))),
                p(str_c("Detected in Hydrosystem: ",comma(nrow(sthd_detected_count)))),
                p(str_c("Percent Detected: ",sthd_detected_percent)),
                showcase = fa("fish-fins")
                
              ),
              
              value_box(
                
                title="Clearwater River Conditions",
                value=str_c("Discharge: ",
                            comma(latest_discharge$mean_discharge),
                            " CFS"),
                p(str_c("Temperature: ",
                        latest_discharge$mean_temp,
                        " C"))
                
                
              )
              
              
            ),
            
            page_fillable(
              
              layout_columns(
                
                col_widths=c(6,6,12),
              
              card(
                card_header("Discharge, Clearwater River at Peck"),
                plotlyOutput("flow_plot"),
                full_screen=TRUE
              ),
              
              card(
                
                card_header("Temperature, Clearwater River at Peck"),
                plotlyOutput("temp_plot"),
                full_screen=TRUE
                
              ),
              
              card(
                
                card_header("Timing to Lower Granite Dam"),
                plotlyOutput("lgr_timing_plot"),
                full_screen = TRUE
                
              )
              
              
            )
            
            )
            
            
            )
  
  
)

server <- function(input,output,session){
  
  
  # get flow plot reactively
  
  flowplot_reactive <- reactive({
    
    plot_min <- min(input$user_dates)
    plot_max <- max(input$user_dates)
    
    flow.plot <- peck.dat %>% 
      mutate(date=as_date(date)) %>% 
      ggplot(aes(x=date,y=mean_discharge,group=group))+
      geom_line(aes(text=str_c(" Date:",date,
                               "<br>","Mean Discharge (cfs): ",mean_discharge,
                               sep=" ")))+
      scale_x_date(date_breaks = "1 week", date_labels="%b %d",
                   limits=c(as.Date(plot_min),as.Date(plot_max)))+
      theme_bw()+
      theme(axis.text.x=element_text(angle=45,hjust=1))+
      labs(x="",y="Mean Discharge at Peck Gaging Station")
    
    
  })
  
  output$flow_plot <- renderPlotly({
    
    plot1 <- flowplot_reactive()
    
    ggplotly(plot1,
             tooltip=c("text"))
    
  })
  
  tempplot_reactive <- reactive({
    
    plot_min <- min(input$user_dates)
    plot_max <- max(input$user_dates)
    
    temp.plot <- peck.dat %>% 
      mutate(date=as_date(date)) %>% 
      ggplot(aes(x=date,y=mean_temp,group=group))+
      geom_line(aes(text=str_c(" Date:",date,
                               "<br>","Mean Temp (C): ",mean_temp,
                               sep=" ")))+
      scale_x_date(date_breaks = "1 week", date_labels="%b %d",
                   limits=c(as.Date(plot_min),as.Date(plot_max)))+
      theme_bw()+
      theme(axis.text.x=element_text(angle=45,hjust=1))+
      labs(x="",y="Mean Temperature at Yankee Fork Gaging Station")
    
  })

  
  output$temp_plot <- renderPlotly({
    
    plot1 <- tempplot_reactive()
    
    ggplotly(plot1,
             tooltip=c("text"))
    
  })
  
  # build the passage plot, don't think it needs to 
  # react to anythin
  
  output$lgr_timing_plot <- renderPlotly({
    
    travel.plot <- ggplot()+
      geom_density(data=sthd_emigration.dat,adjust=1.5,alpha=0.5,
                   aes(x=LGR,group=release_grp_plot,fill=release_grp_plot))+
      geom_vline(data=vline.dat,aes(xintercept=as.numeric(release_date),
                                    group=release_grp_plot,
                                    color=release_grp_plot),
                 linewidth=1.5)+
      geom_vline(data=lgr_median.dat,
                 aes(xintercept = as.numeric(median_lgr),
                     group=release_grp_plot,
                     color=release_grp_plot),
                 linetype="dashed",
                 linewidth=1.25)+
      scale_x_datetime(limits = c(xmin,xmax),
                       date_breaks="1 week",
                       date_labels="%b %d")+
      scale_fill_manual(values=c("blue","red","purple"))+
      scale_color_manual(values=c("blue","red","purple"))+
      theme_bw()+
      labs(x="Arrival Date at Lower Granite Dam",
           y="Proportion of total arrivals",
           fill="",
           color="")+
      theme(axis.text.x=element_text(angle=45,hjust=1))
    travel.plot
    
    ggplotly(travel.plot, tooltip=NULL)
    
    
    
    
  })
  
  
}



shinyApp(ui, server)
