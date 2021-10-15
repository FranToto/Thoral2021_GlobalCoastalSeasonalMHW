#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(plotly)
library(sf)
library(sp)
library(viridis)
library(maps)
library(DT)

## Load full MHW Attributes outputs
MHW_dplyr <- read_csv('MHW_Events_Global_FULL_Realm_SmallSize.csv') %>% 
    mutate(REALM = factor(REALM,levels=c("Arctic", "Temperate Northern Atlantic", "Temperate Northern Pacific","Tropical Atlantic",
                                         "Eastern Indo-Pacific","Central Indo-Pacific","Western Indo-Pacific","Tropical Eastern Pacific", 
                                         "Temperate South America","Temperate Southern Africa","Temperate Australasia","Southern Ocean")),
           Season = factor(Season,levels=c("Summer","Autumn","Winter","Spring")),
           Metrics = factor(Metrics,levels=c("Number_Events", "meanDuration", "meanMaxInt", "meanCumInt")))


##

## Load Coastal Realms (Costello et al., 2007)
coastal_spal <- st_read('Marine_Ecoregions_Of_the_World__MEOW_.shp') %>% 
    mutate(REALM = factor(REALM,levels=c("Arctic", "Temperate Northern Atlantic", "Temperate Northern Pacific","Tropical Atlantic",
                                         "Eastern Indo-Pacific","Central Indo-Pacific","Western Indo-Pacific","Tropical Eastern Pacific", 
                                         "Temperate South America","Temperate Southern Africa","Temperate Australasia","Southern Ocean")))

coastal_spal_lonlat <- st_transform(coastal_spal,CRS("+proj=longlat +datum=WGS84 +no_defs ")) 
##

## New facet label names for Metrics
metrics.labs <- c(`meanCumInt` = "Cumulative Intensity (DegC Days)",
                  `meanDuration` = "Duration (Days)",
                  `meanMaxInt` = "Maximum Intensity (DegC)",
                  `Number_Events` = "Number of Events")
##

## Load Trends
MHW_Trends <- read_csv('MHW_Trends_FULL.csv') #Contains output of trend analysis (Slope, breakpoint, p-values) - for global and NZ
##

# Define UI for application that draws a histogram
ui <- fluidPage(
    navbarPage("Coastal MHW", id="nav",
               
               tabPanel("Interactive map",
                        div(class="outer",
                            
                            # If not using custom CSS, set height of leafletOutput to a number instead of percent
                            plotlyOutput("map", width=1600, height=800),
                            
                            
                            tags$div(id="cite",
                                     'Data compiled for ', tags$em('Thoral et al., 2021 - Global long-term trends in coastal marine heatwaves.'), 'in prep.'
                            )
                        )
               ),
               
               tabPanel("Trends explorer",
                        fluidRow(
                            column(3,
                                   selectInput("realms", "Realms", c("Arctic", "Temperate Northern Atlantic", "Temperate Northern Pacific","Tropical Atlantic",
                                                                     "Eastern Indo-Pacific","Central Indo-Pacific","Western Indo-Pacific","Tropical Eastern Pacific", 
                                                                     "Temperate South America","Temperate Southern Africa","Temperate Australasia","Southern Ocean"), 
                                               selected='Arctic',multiple=F)
                            )
                        ),
                        #plotlyOutput("trendplot", width="100%", height="100%"),
                        plotlyOutput("trendplot", width=1000, height=1000),
                        
                        #plotlyOutput("trendplot", width="auto", height="auto"),
                        hr(),
                        DT::dataTableOutput("table")
                        #dataTableOutput("table")
                        
               ),
               
               conditionalPanel("false", icon("crosshair"))
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    ## Interactive Map ###########################################
    output$map <- renderPlotly({
        p <- ggplot() + geom_sf(data=coastal_spal_lonlat,aes(fill=REALM)) + 
            #scale_fill_viridis(begin = 0.1, end = 1,option="turbo",discrete = T) + 
            borders('world',fill='grey') +
            #geom_sf(data=MHW_dplyr_sf_pts,size=.3) +
            #annotation_scale(pad_x = unit(0.05, "in"),pad_y = unit(0.05, "in"), width_hint = 0.2)  +
            theme(panel.background = element_rect(fill = 'aliceblue')) +
            #annotation_north_arrow(location = "bl", which_north = "true", 
            #                      pad_x = unit(0.5, "in"), pad_y = unit(0.3, "in"),
            #                       style = north_arrow_fancy_orienteering) +
            xlab('') + ylab('') +
            theme(legend.position = "bottom",legend.title = element_blank(),
                  legend.text = element_text(size=12))
        ggplotly(p) %>% layout(legend = list(
            orientation = "h", x = 0.4, y = -0.2))
    })
   
    ## Interactive Plot ###########################################
    MHW_dplyr_realm <- reactive({
         MHW_dplyr %>% 
            dplyr::filter(REALM==input$realms)

    })
    
    output$trendplot <- renderPlotly({    
        p <- ggplot(MHW_dplyr_realm(),aes(Year,values,col=Season)) + 
            facet_wrap(vars(Metrics),scales='free',ncol=3,labeller = as_labeller(metrics.labs)) + 
            geom_line(size=.5) + 
            geom_smooth(se=T,size=1.5) + 
            ggtitle('MHW Metrics trends per season for a given Coastal Realm') + 
            ylab('') + xlab('Year') + 
            scale_colour_viridis(begin = 0, end = .75,option="inferno",discrete = T) +
            theme_bw() + 
            theme(legend.position="bottom",
                  legend.title = element_text(size=16),
                  legend.text = element_text(size=16),
                  strip.text = element_text(size=12), 
                  axis.text=element_text(size=12),
                  axis.title = element_text(size=16))
        ggplotly(p) %>% layout(legend = list(
            orientation = "h", x = 0.4, y = -0.2))
    })
    
    ## Stats Table ###########################################
    MHW_Trends_realm <- reactive({
        MHW_Trends %>% 
            dplyr::filter(Region_Name==input$realms) 
    })
    
    output$table <- DT::renderDataTable(datatable(MHW_Trends_realm()) %>% 
                                            DT::formatStyle('P_Value',target = 'row',fontWeight = styleInterval(.05, c('bold', 'normal'))) %>%
                                            DT::formatStyle('Sens_Slope',target = 'row',backgroundColor = styleInterval(0, c('lightblue', 'lightpink')))
                                        )
        
    
}

# Run the application 
shinyApp(ui = ui, server = server)
