#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinybusy)
library(tidyverse)
library(leaflet)
library(leafpop)
library(plotly)
library(sf)
library(sp)
library(viridis)
library(maps)
library(DT)
#library(rnaturalearth)

## Load full MHW Attributes outputs
MHW_dplyr <- read_csv('MHW_Trends_FULL_Realm_Pixels_Reviews_check_SmallSize.csv') %>% 
    mutate(REALM = factor(REALM,levels=c("Arctic", "Temperate Northern Atlantic", "Temperate Northern Pacific","Tropical Atlantic",
                                         "Eastern Indo-Pacific","Central Indo-Pacific","Western Indo-Pacific","Tropical Eastern Pacific", 
                                         "Temperate South America","Temperate Southern Africa","Temperate Australasia","Southern Ocean")),
           Season = factor(season,levels=c("Summer","Autumn","Winter","Spring")),
           Metrics = factor(Metrics,levels=c("Number_MHW_days", "Nevents", "Mean_Intensity", "Maximum_Intensity", "Cumulative_Intensity")))
##--> To create cf trend script + change here

##

## Load Coastal Realms (Costello et al., 2007)
coastal_spal <- st_read('Marine_Ecoregions_Of_the_World__MEOW_.shp') %>% 
    mutate(REALM = factor(REALM,levels=c("Arctic", "Temperate Northern Atlantic", "Temperate Northern Pacific","Tropical Atlantic",
                                         "Eastern Indo-Pacific","Central Indo-Pacific","Western Indo-Pacific","Tropical Eastern Pacific", 
                                         "Temperate South America","Temperate Southern Africa","Temperate Australasia","Southern Ocean")))

coastal_spal_lonlat <- st_transform(coastal_spal,CRS("+proj=longlat +datum=WGS84 +no_defs ")) 

coastal_spal_lonlat <- coastal_spal_lonlat %>% group_by(REALM) %>% summarize(geometry = st_combine(geometry))

#factpal <- colorFactor(viridis(12,begin = 0, end = .75), coastal_spal_lonlat$REALM)
factpal <- colorFactor(hcl(seq(15,330,length=12),l = 65, c = 100), coastal_spal_lonlat$REALM)
 
##

## Load coastline
#coast <- ne_coastline(scale = "medium", returnclass = "sf") %>% 
#    st_cast(.,"MULTIPOLYGON")

## New facet label names for Metrics
metrics.labs <- c(`Cumulative_Intensity` = "Cumulative Intensity (DegC Days)",
                  `Number_MHW_days` = "MHW days",
                  `Mean_Intensity` = "Mean Intensity (DegC)",
                  `Maximum_Intensity` = "Maximum Intensity (DegC)",
                  `Nevents` = "Number of Events")
##

## Load Trends
MHW_Trends <- read_csv('MHW_Trends_FULL_Realm_Pixels_Reviews_check_clean.csv') #Contains output of trend analysis (Slope, breakpoint, p-values) - for global and NZ


##

# Define UI for application that draws a histogram
ui <- fluidPage(
    navbarPage("Coastal MHW", id="nav",
               
               tabPanel("Interactive map",
                        div(class="outer",
                            h2("Click on the polygons to see seasonal trends in MHW metrics."),
                            h4("Then go to the Trend Explorer Tab for exploring the data more."),
                            # If not using custom CSS, set height of leafletOutput to a number instead of percent
                            #plotlyOutput("map", width=1600, height=800),
                            leafletOutput("map", width=1600, height=800),
                            
                            tags$div(id="cite",
                                     'Data compiled for ', tags$em('Thoral et al., 2022 - Unravelling Seasonal Trends in Coastal Marine Heatwave Metrics Across Global Biogeographical Regions.'), 'under review'
                            )
                        ),
                        add_busy_spinner(timeout=1000,color='blue')
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
                        plotlyOutput("trendplot", width=800, height=800),
                        
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

    #To pop a plot on map at click, cf:https://community.rstudio.com/t/leafpop-popupgraph-doenst-work-with-plumber-r/89073/2 using package leafpop::
    my_list <- list()  
    loop<-for (i in unique(MHW_dplyr$REALM)) {
        MHW_dplyr_realm_sub <- MHW_dplyr %>% filter(REALM == i)
        
        plot <- ggplot(MHW_dplyr_realm_sub,aes(year,values,col=season)) + 
            facet_wrap(vars(Metrics),scales='free',ncol=3,labeller = as_labeller(metrics.labs)) + 
            geom_line(size=.5) + 
            geom_smooth(se=T,size=1.5) + 
            ylab(paste0(unique(MHW_dplyr_realm_sub$REALM))) + xlab('Year') + 
            scale_colour_viridis(begin = 0, end = .75,option="inferno",discrete = T) +
            theme_bw() + 
            theme(legend.position="bottom",
                  legend.title = element_text(size=12),
                  legend.text = element_text(size=12),
                  strip.text = element_text(size=12), 
                  axis.text=element_text(size=12),
                  axis.title = element_text(size=12))+
            guides(colour = guide_legend(override.aes = list(shape=15,size=4)))
        
        my_list[[i]] <- plot
    }
    
    output$map <- renderLeaflet({
        leaflet(coastal_spal_lonlat) %>% 
            setView(lng = 90, lat = 0, zoom = 2) %>%
            addTiles() %>% 
            addPolygons(#data=coastal_spal_lonlat,
                        stroke = T, smoothFactor = 0.2, fillOpacity = 1,fillColor = ~factpal(REALM),
                       highlightOptions = highlightOptions(color = "white", weight = 2,
                                                           bringToFront = TRUE),
                       #layerId = ~REALM,
                       popup = paste0(" Realm: ",coastal_spal_lonlat$REALM,"<br/>", popupGraph(my_list,width=800,height=500))) %>% 
                       #popup = paste0(" Realm: ",coastal_spal_lonlat$REALM, "<br/>  Province: ",coastal_spal_lonlat$PROVINCE)) %>% 
            
            addProviderTiles("Esri.WorldPhysical") #%>%
            #addPolygons(data=coast,fill='transparent')
            #addPopupGraphs(my_list, type='html')
    })
    
    ## Interactive Plot ###########################################
    MHW_dplyr_realm <- reactive({
         MHW_dplyr %>% 
            dplyr::filter(REALM==input$realms)

    })
    
    output$trendplot <- renderPlotly({    
        p <- ggplot(MHW_dplyr_realm(),aes(year,values,col=season)) + 
            facet_wrap(vars(Metrics),scales='free',ncol=3,labeller = as_labeller(metrics.labs)) + 
            geom_line(size=.5) + 
            geom_smooth(se=T,size=1.5) + 
            ylab('') + xlab('Year') + 
            scale_colour_viridis(begin = 0, end = .75,option="inferno",discrete = T) +
            theme_bw() + 
            theme(legend.position="bottom",
                  legend.title = element_text(size=16),
                  legend.text = element_text(size=16),
                  strip.text = element_text(size=12), 
                  axis.text=element_text(size=12),
                  axis.title = element_text(size=16))+
            guides(colour = guide_legend(override.aes = list(shape=15,size=8)))
        ggplotly(p) %>% layout(legend = list(
            orientation = "h", x = 0.4, y = -0.2))
    })
    
    ## Stats Table ###########################################
    MHW_Trends_realm <- reactive({
        MHW_Trends %>% 
            dplyr::filter(Region_Name==input$realms) 
    })
    
    output$table <- DT::renderDataTable(datatable(MHW_Trends_realm(),
                                                  options = list(
                                                      pageLength = 30)) %>% 
                                            DT::formatStyle('P_Value',target = 'row',fontWeight = styleInterval(.05, c('bold', 'normal'))) %>%
                                            DT::formatStyle('P_Value_pre',target = 'cell',fontWeight = styleInterval(.05, c('bold', 'normal'))) %>%
                                            DT::formatStyle('P_Value_post',target = 'cell',fontWeight = styleInterval(.05, c('bold', 'normal'))) %>%
                                            DT::formatStyle('Trend_Decadal',target = 'row',backgroundColor = styleInterval(0, c('lightblue', 'lightpink'))) %>% 
                                            DT::formatStyle('Trend_Decadal_pre',target = 'cell',backgroundColor = styleInterval(0, c('lightblue', 'lightpink'))) %>% 
                                            DT::formatStyle('Trend_Decadal_post',target = 'cell',backgroundColor = styleInterval(0, c('lightblue', 'lightpink'))) %>% 
                                            formatRound(c(5,6,8:12), 4) %>% 
                                            formatStyle(columns = c(1:12), 'text-align' = 'center')
                                        )
        
    
}

# Run the application 
shinyApp(ui = ui, server = server)
