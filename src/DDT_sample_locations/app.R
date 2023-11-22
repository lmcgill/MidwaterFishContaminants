#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

## Load in required libraries/ R packages 
library(shiny)
library(dplyr)
library(leaflet)
library(ggplot2)
library(cowplot)
library(tidyr)
library(lubridate)
if(!require(shinythemes)) install.packages("shinythemes", repos = "http://cran.us.r-project.org")
if(!require(shinyWidgets)) install.packages("shinyWidgets", repos = "http://cran.us.r-project.org")

hydros <- readRDS("data/erdNOAAhydros.rds") %>% 
  dplyr::filter(cast_type == "hydrocast") %>% 
  dplyr::mutate(line.cruise = paste(line.station, cruise, sep=", ")) 

  

setwd("/Users/lillianmcgill/Documents/MidwaterFishContaminants/src/DDT_sample_locations")
ui = bootstrapPage(
  navbarPage(theme = shinytheme("flatly"), collapsible = TRUE,
             HTML('<a style="text-decoration:none;cursor:default;color:#FFFFFF;" class="active" href="#">DDT+ Sample Explorer</a>'), id="nav",
             # Specify titles and that we want multiple panels 
             windowTitle = "DDT+ Sample Explorer",
             tabPanel("Myctophid Map", 

                      sidebarLayout(
                        sidebarPanel(
                          id = "sidebar",
                          tags$h1("Subset the data"),
                          tags$br(),
                          HTML(paste0("This map shows the number of years with at least one myctophid caught for a given year-site combination.
                            Note that data ONLY goes to 1978 at present. Weight is determiend by indidivual length and a kind of crummy length-weight relationship from other CalCOFI data.")), 
                          tags$br(),
                          tags$br(),
                          
                          HTML(paste0("You can select either one or all of the most common species (Stenobrachius leucopsarus, Triphoturus mexicanus, and Nannobrachium ritteri)")),
                          tags$br(),
                          tags$br(),
                          radioButtons(inputId = "plot_select", 
                                       label = "Species Options:",
                                       choices = 
                                         c("All" = "both",
                                           "Stenobrachius leucopsarus" = "ste_leu", 
                                           "Triphoturus mexicanus" = "tri_mex", 
                                           "Nannobrachium ritteri" = "nan_rit")),
                          tags$br(),
                          tags$br(),
                          
                          HTML(paste0("You can select the minimum size of individual fish")),
                          tags$br(),

                          sliderInput("minsize", "Minimum Size (mm):",
                                      min = 0, max = 86, value = 0
                          ),

                          width = 3
                          
                        ), 
                        
                      mainPanel(
                        HTML(paste0("Click a station to view myctophid time series.")),
                
                        fluidRow(column(6, leafletOutput("mymap"),   
                                        absolutePanel(top = 10, left = 60, draggable = TRUE)), 
                                 fluidRow(column(6,plotOutput("plot"))), 
                                 tags$br(),
                                 HTML(paste0("The numbers below show the total number of individuals (across all sites in the 75-station CalCOFI grid), and a 
                                    very rough approximation of their total weigth. The numbers will change with options on the left hand side.")),
                                 fluidRow(tableOutput("num.indiv")), 
                                 
                                 
                                 ), 
                       
                        
                        width=9
                      ),
                      

                      
             ),
             ),
             tabPanel("Myctophid Seasonality", 
                      span(tags$i(h6("This graph just shows the breakdown of how many myctophids are caught annually (across all sites) by net and month.")), style="color:#045a8d"),
                      mainPanel(
                        plotOutput("seasonal.plot")
                        #verbatimTextOutput("text")
                      ), 
             ), 
             tabPanel("Temperature and Salinity", 
                      
                      sidebarLayout(
                        sidebarPanel(
                          id = "sidebar",
                          tags$h1("Chose a site and cruise"),
                          tags$br(),
                          HTML(paste0("This plot will show a temperature vs. salinity plot for two/ cruise combinations. Start typing the your desired 
                                      combination in the format `Site, Cruise` and click it to select. All data is from CalCOFI hydro cast bottle data.")), 
                          tags$br(),
                          tags$br(),
                          selectizeInput("v1", 
                                         label = "Select Site/Cruise", 
                                         choices = unique(hydros$line.cruise[order(hydros$line.station)]), 
                                         selected = character(0), 
                                         multiple = TRUE),
                          
                          # selectInput("v1", 
                          #             label = "Select Site 1", 
                          #             choices = unique(hydros$line.cruise[order(hydros$line.station)]), 
                          #             selected = unique(hydros$line.cruise[order(hydros$line.station)])[1],
                          #             multiple = TRUE),
                          # 
                          width = 3
                          
                        ), 
                        
                        mainPanel(
                          plotOutput("temp.salinity", height=600),
    
                          
                          
                          width=9
                        ),
                        
                        
                        
                      ),
             )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

    data.calcofi <- reactive({
      
      req(input$plot_select)
      species = switch(input$plot_select, 
                       both = c("Stenobrachius leucopsarus", "Triphoturus mexicanus", "Nannobrachium ritteri"), 
                       ste_leu = c("Stenobrachius leucopsarus"), 
                       tri_mex = c("Triphoturus mexicanus"), 
                       nan_rit = c("Nannobrachium ritteri")) 
      
      size = input$minsize
      
      data.calcofi = readRDS("data/juvenile_size_data.rds")  %>% 
        dplyr::filter(SpeciesName %in% species) %>% 
        dplyr::filter(Size >= size) %>% 
        #dplyr::filter(SpeciesName %in% c("Stenobrachius leucopsarus")) %>% 
        
        dplyr::mutate(year = year(Datetime)) %>% 
        dplyr::mutate(month = month(Datetime)) %>% 
        group_by(line.station, year) %>% 
        dplyr::mutate(T_JA_calcofi = sum(Count)) %>% 
        dplyr::mutate(weight_JA_calcofi = sum(weight)) %>% 
        dplyr::ungroup() %>% 
        group_by(line.station) %>% 
        dplyr::mutate(Lat = mean(Lat), 
                      Lon=mean(Lon)) %>%
        dplyr::mutate(num_years_calcofi= length(unique(year))) %>% 
        dplyr::ungroup() 
      data.calcofi
      
    })

  # Draw the leaflet map  
  output$mymap <- renderLeaflet({
    
    stations = unique(data.calcofi()[, c("num_years_calcofi","line.station","Lat","Lon")]) %>% 
      rename(latitude = Lat, longitude = Lon)
    
    qpal <- colorBin("YlOrRd", stations$num_years_calcofi, n = 5, pretty=TRUE)
    
    leaflet(data=stations,options = leafletOptions(preferCanvas = TRUE)) %>%
      # This is just the background map. There are several options - I like this because it shows boundaries, rivers, roads, etc.
      
      addProviderTiles(providers$CartoDB.Positron, 
                       options = providerTileOptions(
                         updateWhenZooming = FALSE,
                         updateWhenIdle = TRUE)) %>%
      # If I wanted to add a shapefile of streamlines for the basin I would use code like the following: addPolylines(data=zambezi_lines, weight=1,col = 'blue') %>% 
      # This code adds a shapefile outline of the basin (from the shared drive): 
      addCircleMarkers(data=stations, 
                  weight=2, 
                  color=~qpal(num_years_calcofi), 
                  fillOpacity = .6, 
                  radius = ~ sqrt(num_years_calcofi),
                  label=(paste("Station", stations$line.station,":" ,stations$num_years_calcofi,"years of data")), 
                    # Add label info when mouseover
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto"), 
                  layerId= stations$line.station) %>% 
      # This is the code that adds points for each of the discharge stations  
      leaflet::addLegend("bottomright", 
                         pal = qpal, 
                         values = ~num_years_calcofi,
                         title = "Years of data with at least one myctophid",
                         opacity = 0.8, 
                         position = "bottomleft"
      ) 
  })
  data_of_click <- reactiveValues(clickedMarker=NULL)
  
  # store the click
  observeEvent(input$mymap_marker_click,{
    data_of_click$clickedMarker <- input$mymap_marker_click
  })
  

  
  # This is the function that will get generate a plot based on what you click on for the map 
  output$plot <- renderPlot({
    #if(is.null(data_of_click$clickedMarker) == FALSE){site <- data_of_click$clickedMarker$id}
    site <- "90 35"
    if(is.null(data_of_click$clickedMarker) == FALSE){site <- data_of_click$clickedMarker$id}
    
    data.calcofi() %>% 
      dplyr::filter(line.station == site)  %>% 
      dplyr::select(year, line.station, weight_JA_calcofi, T_JA_calcofi) %>% 
      dplyr::rename("Total myctophids per site" = "T_JA_calcofi","Weight myctophids per site"="weight_JA_calcofi") %>% 
      tidyr::gather("Parameter","Value", -c(year:line.station)) %>% 
      ggplot(aes(x=year, y=Value)) + 
      #geom_hline(yintercept=(10), color="black", linetype="dashed") + 
      geom_point(pch=21, color="black", fill="gray", size=2) +
      xlab("Year")+ 
      xlim(1978, 2021)+
      facet_grid(Parameter~line.station) + 
      theme_bw()
  }) 
  
  output$seasonal.plot <- renderPlot({
    
    data.calcofi() %>% 
      group_by(year, month, TowType) %>% 
      dplyr::mutate(T_JA_calcofi = sum(Count)) %>% 
      dplyr::ungroup() %>% 
      dplyr::select(year, month, T_JA_calcofi, TowType) %>% 
      unique() %>% 
      ggplot(aes(x=as.factor(month), y=T_JA_calcofi, fill =TowType )) + 
      geom_boxplot(color="black") + 
      ylab("Annual myctophid catch across all sites") +
      xlab("Month")+
      labs(fill="Net Type")+ 
      theme_bw()
    
  }) 
  
  output$num.indiv <- renderTable({

    data.calcofi() %>% 
      dplyr::filter(Line > 76.7) %>%
      dplyr::select(year, line.station, weight, Count) %>% 
      dplyr::summarize(Total.weight.g = as.integer(sum(weight)), 
                       Total.number.individuals = sum(Count))
  })

  
  output$temp.salinity <- renderPlot({

    hydros %>%
      dplyr::filter(line.cruise %in% input$v1) %>% 
        #(line.station == input$v1 & cruise ==input$v2) |
        #              (line.station == input$v3 & cruise == input$v4)) %>%
      dplyr::select(line.cruise, salinity, temperature, standard_depth) %>% 
      dplyr::mutate(salinity = as.numeric(salinity), 
                    temperature = as.numeric(temperature), 
                    standard_depth = as.numeric(standard_depth)) %>% 
      dplyr::arrange(standard_depth) %>% 
      ggplot(aes(x=salinity, y=temperature, fill=line.cruise, color=line.cruise)) + 
      geom_point(pch=21, size=2.5) + 
      geom_line()+
      theme_bw() + 
      xlab("Salinity") + ylab("Temperature")+
      theme(legend.position = "top") + 
      theme(text = element_text(size = 20))   

  })
  
  #output$text <- renderText({ "test" })
  
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
