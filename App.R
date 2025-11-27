# ====================================================
# Author: Carter Vonderahe
# Date: 12/11/23
# Professor Donghyung Lee
# STA 404 Project 2 - New York City Airbnb Listings Explorer
# SHINY APP
# ====================================================


# set working directory - to be changed by whoever runs this code
# setwd("/Users/carter_vondy/Desktop/STA404/Projects/Project2")

# load libraries
library(tidyverse)
library(shiny)
library(shinyWidgets)
library(shinythemes)
library(leaflet)
library(RColorBrewer)
library(htmltools)
library(DT)

# read in data
load("Project2_ProcessedData.RData")


##################
# USER INTERFACE #
##################
ui <- fluidPage(
  
  # define theme and title for UI
  theme = shinytheme("simplex"),
  titlePanel("New York City Airbnb Listings Explorer"),
  
  # governs sidebar and main panels
  sidebarLayout(position = "right",
    
    conditionalPanel(condition = "input.tabs != 'Home' && input.tabs != 'References'",
      # sidebar panel - contains widgets
      sidebarPanel(
        
        # widgets in common between explore page and crime page
        conditionalPanel(
          condition = "input.tabs == 'Explore Listings' || input.tabs == 'Local Crime'",
          
          # borough inputs
          selectInput(inputId = "locationUI", label = "Location", 
                      multiple = TRUE, selectize = FALSE, size = 5,
                      choices = c("Bronx", "Brooklyn", "Manhattan", "Queens", "Staten Island"))
        ),
        
        # explore page widgets
        conditionalPanel(
          condition = "input.tabs == 'Explore Listings'",
          
          # room type input
          selectInput(inputId = "room_typeUI", label = "Listing Type",
                      choices = c("All Listings" = "All Listings",
                                  "Entire Home/Apartment" = "Entire home/apt",
                                  "Private Room" = "Private room",
                                  "Shared Room" = "Shared room",
                                  "Hotel Room" = "Hotel room"),
                      selected = "All Listings"),
          
          # price range input
          numericRangeInput(inputId = "priceUI", label = "Price Range (USD)",
                            min = 0, max = 30000, value = c(0, 1000), step = 5, width = '65%')
        ),
        
        # compare page widgets
        conditionalPanel(
          condition = "input.tabs == 'Compare Listings'",
          
          # listing id inputs
          textInput(inputId = "id1UI", label = "Enter the ID of Listing 1", value = "7097"),
          textInput(inputId = "id2UI", label = "Enter the ID of Listing 2", value = "5121"),
          
          # metric input
          selectInput(inputId = "varUI", label = "Comparison Metric",
                      choices = c("Price (USD)" = "price",
                                  "Rating (out of 5)" = "rating",
                                  "Number of Reviews" = "number_of_reviews",
                                  "Days Available per Year" = "availability_365",
                                  "Minimum Nights" = "minimum_nights"))
        ),
        
        # crime page widgets
        conditionalPanel(
          condition = "input.tabs == 'Local Crime'",
          
          # date range input
          dateRangeInput2(inputId = "dateUI", label = "Date Range", width = "65%",
                         start = "2023-01-01", end = "2023-09-01",
                         min = "2023-01-01", max = "2023-09-30",
                         minview = "months", maxview = "months",
                         startview = "year", format = "MM")
        )
        
      ) # end sidebarPanel
      
    ), # end conditionalPanel
    
    # main panel - contains plots
    mainPanel(
      
      tabsetPanel(id = "tabs",
                  
                  # home page
                  tabPanel(title = "Home",
                           p(h2(strong("Introduction"))),
                           p("Welcome to the New York City Airbnb Listings Explorer! Here you can explore listings 
                             across the city, compare metrics on specific listings, and view local crime rates to 
                             ensure a safe, comfortable stay."),
                           p(h4(strong("Explore Listings"))),
                           p("Visit the ", em("Explore Listings"), " tab to view a map of individual listings across 
                             NYC. Start by selecting the location(s), and then filter by listing type and a 
                             specific price range to suit the requirements of your stay. See the table below the map 
                             for more details about the listings. Click on the geo marker of a listing to see the 
                             listing's ID. This ID can be used to compare listings in the ", em("Compare Listings"), 
                             " tab."),
                           p(h4(strong("Compare Listings"))),
                           p("The ", em("Compare Listings"), " tab allows you to analyze the differences between 
                             two listings by various metrics, including price, rating out of 5 stars, number of 
                             reviews, days available out of the year, and the minimum number of nights required to 
                             book the listing. Enter the IDs of the listings you want to compare into the text 
                             fields and select your metric of comparison to get insights."),
                           p(h4(strong("Local Crime"))),
                           p("Safety is the most important part of any trip. Use the ", em("Local Crime"), " tab 
                             to view crime trends (arrests per 1,000 residents) in your preferred travel 
                             destination(s). Use the location filter to highlight and compare specific parts of the 
                             city, and supply a date range to zoom in on the timeline if desired."),
                           p(br("By: Carter Vonderahe", br("Last Updated: December 11, 2023")))),
                  
                  # explore listings page
                  tabPanel(title = "Explore Listings",
                           p(h2(strong("Explore Listings"))),
                           leafletOutput(outputId = "map"),
                           p(),
                           dataTableOutput(outputId = "table")),
                  
                  # compare listings page
                  tabPanel(title = "Compare Listings",
                           p(h2(strong("Compare Listings"))),
                           plotOutput(outputId = "bar"),
                           p(),
                           fluidRow(
                             splitLayout(cellWidths = c("50%", "50%"),
                                         cellArgs = list(style = "padding: 6px"),
                                         p(h4(strong("Listing 1 Map:"))),
                                         p(h4(strong("Listing 2 Map:")))),
                             splitLayout(cellWidths = c("50%", "50%"),
                                         cellArgs = list(style = "padding: 10px"),
                                         leafletOutput(outputId = "map1"),
                                         leafletOutput(outputId = "map2")))),
                  
                  # local crime page
                  tabPanel(title = "Local Crime",
                           p(h2(strong("Local Crime"))),
                           plotOutput(outputId = "line")),
                  
                  # reference page
                  tabPanel(title = "References",
                           p(h2(strong("References"))),
                           p(h4(strong("Data Sources:"))),
                           p("- Inside Airbnb. (2023, November 1). Inside Airbnb: Get the Data. http://insideairbnb.com/get-the-data"),
                           p("-	Pakzad, J. (2023, December 1). NYPD Arrests Dataset (2023). Kaggle. https://www.kaggle.com/datasets/justinpakzad/nypd-arrests-2023-dataset"),
                           p("- U.S. Census Bureau. (N.d.). MDAT. https://data.census.gov/mdat/?#/search?ds=CPSBASIC202309&amp;cv=PESEX&amp;rv=ucgid&amp;wt=PWSSWGT&amp;g=0500000US36005,36047,36061,36081,36085"),
                           p(h4(strong("R Packages:"))),
                           p("All analyses were performed using R Statistical Software (v4.1.1; R Core Team 2021)."),
                           p("-	Chang W, Cheng J, Allaire J, Sievert C, Schloerke B, Xie Y, Allen J, McPherson J, Dipert A, Borges B (2023). _shiny: Web Application Framework for R_. R package version 1.7.5.1, <URL: https://CRAN.R-project.org/package=shiny>."),
                           p("-	Chang W (2021). _shinythemes: Themes for Shiny_. R package version 1.2.0, <URL: https://CRAN.R-project.org/package=shinythemes>."),
                           p("-	Cheng J, Schloerke B, Karambelkar B, Xie Y (2023). _leaflet: Create Interactive Web Maps with the JavaScript 'Leaflet' Library_. R package version 2.2.1, <URL: https://CRAN.R-project.org/package=leaflet>."),
                           p("-	Cheng J, Sievert C, Schloerke B, Chang W, Xie Y, Allen J (2023). _htmltools: Tools for HTML_. R package version 0.5.5, <URL: https://CRAN.R-project.org/package=htmltools>."),
                           p("-	Grolemund G, Wickham H (2011). “Dates and Times Made Easy with lubridate.” _Journal of Statistical Software_, *40*(3), 1-25. <URL: https://www.jstatsoft.org/v40/i03/>."),
                           p("-	Neuwirth E (2022). _RColorBrewer: ColorBrewer Palettes_. R package version 1.1-3, <URL: https://CRAN.R-project.org/package=RColorBrewer>."),
                           p("-	Perrier V, Meyer F, Granjon D (2023). _shinyWidgets: Custom Inputs Widgets for Shiny_. R package version 0.8.0, <URL: https://CRAN.R-project.org/package=shinyWidgets>."),
                           p("-	Wickham H, Averick M, Bryan J, Chang W, McGowan LD, François R, Grolemund G, Hayes A, Henry L, Hester J, Kuhn M, Pedersen TL, Miller E, Bache SM, Müller K, Ooms J, Robinson D, Seidel DP, Spinu V, Takahashi K, Vaughan D, Wilke C, Woo K, Yutani H (2019). “Welcome to the tidyverse.” _Journal of Open Source Software_, *4*(43), 1686. doi: 10.21105/joss.01686 (URL: https://doi.org/10.21105/joss.01686)."),
                           p("- Yihui Xie, Joe Cheng and Xianying Tan (2023). DT: A Wrapper of the JavaScript Library 'DataTables'. R package version 0.30. https://CRAN.R-project.org/package=DT"),
                           p(h4(strong("Programming Functions:"))),
                           p("- David. (2019, April 25). Display only months in dateRangeInput or dateInput for a shiny app [R programming]. Stack Overflow. https://stackoverflow.com/questions/31152960/display-only-months-in-daterangeinput-or-dateinput-for-a-shiny-app-r-programmin"),
                           p("- Quentin. (2023, August 15). Add leaflet legend for polygon outlines. Stack Overflow. https://stackoverflow.com/questions/76871212/add-leaflet-legend-for-polygon-outlines")
                           )
                  
      ) # end tabsetPanel
      
    ) # end mainPanel
    
  ) # end sidebarLayout
  
) # end fliudPage


##########
# SERVER #
##########
server <- function(input, output) {
  
  ### MAP VISUAL ###
  output$map <- renderLeaflet({

    # if user selects at least one location
    if (length(input$locationUI) != 0) {
      
      # filter by user's selection of borough and price
      airbnb_data <- airbnb_data %>%
        filter(neighbourhood_group %in% input$locationUI) %>%
        filter(price >= input$priceUI[1] & price <= input$priceUI[2])
        
      # if user's selected listing type is anything but 'all listings', filter by listing type
      if (input$room_typeUI != "All Listings") {
        airbnb_data <- filter(airbnb_data, room_type == input$room_typeUI)
      }
      
      # specify colors for room_type on map
      pal <- colorFactor(c("red2", "green2", "navy", "yellow"),
                         domain = c("Entire home/apt", "Hotel room", "Private room", "Shared room"))
      
      # specify labels for geo markers
      labels <- ~paste0("<strong>", name,
                        "</strong><em><br>", room_type, " in ", neighbourhood, ", ", neighbourhood_group,
                        "</em><br>Price: $", price) %>%
        lapply(htmltools::HTML) # use the htmltools library
      
      # specify popup for geo markers
      popup <- ~paste0("<strong> Listing #: ", id) %>%
        lapply(htmltools::HTML) # use the htmltools library
      
      # draw map
      map <- leaflet(data = airbnb_data, options = leafletOptions(minZoom = 5, maxZoom = 20)) %>%
        setView(lng = -74.0060, lat = 40.7128, zoom = 10) %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(data = ny_neighborhoods,
                    stroke = TRUE, 
                    weight = 1,
                    smoothFactor = 0.3, 
                    fillOpacity = 0.1,
                    fillColor = "blue",
                    label = ~paste0(neighbourhood)) %>%
        addCircleMarkers(~longitude, ~latitude, radius = 1, color = ~pal(room_type),
                         label = labels, popup = popup) %>%
        addLegend(pal = pal, values = ~room_type, position = "bottomleft", title = "Listing Type") %>%
        addLegendCustom(position = "bottomleft", colors = "lightblue", labels = "Neighborhood",
                        sizes = 18, shapes = "square", borders = "blue")
      
      # else if user selects no locations, draw empty plot
    } else {
      
      map <- leaflet(options = leafletOptions(minZoom = 5, maxZoom = 20)) %>%
        setView(lng = -74.0060, lat = 40.7128, zoom = 10) %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(data = ny_neighborhoods, stroke = TRUE, weight = 1, smoothFactor = 0.3, 
                    fillOpacity = 0.1, fillColor = "blue", label = ~paste0(neighbourhood, " Neighborhood")) %>%
        addLegendCustom(position = "bottomleft", colors = "lightblue", labels = "Neighborhood",
                        sizes = 18, shapes = "square", borders = "blue")
      
    }
    
  }) # end renderLeaflet
  
  ### TABLE ###
  output$table <- renderDataTable({
    
    # if user selects at least one location
    if (length(input$locationUI) != 0) {
      
      # filter by user's selection of borough and price
      airbnb_data <- airbnb_data %>%
        filter(neighbourhood_group %in% input$locationUI) %>%
        filter(price >= input$priceUI[1] & price <= input$priceUI[2])
      
      # if user's selected listing type is anything but 'all listings', filter by listing type
      if (input$room_typeUI != "All Listings") {
        airbnb_data <- filter(airbnb_data, room_type == input$room_typeUI)
      }
      
      # remove scientific notation on listing id
      airbnb_data$id <- format(airbnb_data$id, scientific = FALSE)
      
      # create table
      temp_table <- airbnb_data %>%
        select(id, name, neighbourhood_group, room_type, price, minimum_nights) %>%
        rename(`Listing ID` = id,
               `Listing Name` = name,
               Location = neighbourhood_group,
               `Listing Type` = room_type,
               Price = price,
               `Minimum Nights` = minimum_nights)
      
      table <- datatable(temp_table, filter = "none", rownames = FALSE,
                         options = list(dom = 'ltip', iDisplayLength = 10))
      
    }
    
  }) # end renderDataTable
  
  ### BAR GRAPH ###
  output$bar <- renderPlot({
    
    # filter data for the two listings provided by user
    listing_compare_data <- airbnb_data %>%
      filter(id == input$id1UI | id == input$id2UI)
    
    # convert id to factor so bar graph works; specify levels so fill color is consistent
    listing_compare_data$id <- factor(listing_compare_data$id, levels = c(input$id1UI, input$id2UI))
    
    # specify dynamic labels for y-axis
    bar_labs <- c(price = "Price",
                    rating = "Rating",
                    minimum_nights = "Minimum Nights",
                    availability_365 = "Days Available per Year",
                    number_of_reviews = "Number of Reviews")
    
    # color palette for bar graph to ensure consistent fill coloring
    bar_pal <- c("royalblue4", "goldenrod2")
    names(bar_pal) <- c(input$id1UI, input$id2UI)
    
    # create bar graph
    ggplot(data = listing_compare_data) +
      geom_col(aes_string(x = "id", y = input$varUI, fill = "id")) +
      labs(title = paste0("Comparison of Listings by ", bar_labs[input$varUI]),
           x = "Listing #",
           y = bar_labs[input$varUI]) +
      scale_x_discrete(limits = rev) + # helps with consistent fill coloring
      scale_y_continuous(expand = c(0, 0)) +
      scale_fill_manual(values = bar_pal) +
      coord_flip() +
      theme_classic() +
      theme(
        # legend settings
        legend.position = "none",
        # axis settings
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 16),
        axis.title.y = element_text(angle = 0, vjust = .5),
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        # title settings
        plot.title = element_text(size = 20, hjust = .5),
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")
      )
      
  }) # end renderPlot
  
  ### LISTING 1 MAP ###
  output$map1 <- renderLeaflet({
    
    # filter data for the two listings provided by user
    listing_compare_data <- airbnb_data %>%
      filter(id == input$id1UI)
    
    # specify colors for room_type on map
    pal <- colorFactor(c("red2", "green2", "navy", "yellow"),
                       domain = c("Entire home/apt", "Hotel room", "Private room", "Shared room"))
    
    # specify labels for geo markers
    labels <- ~paste0("<strong>", name,
                      "</strong><em><br>", room_type, " in ", neighbourhood, ", ", neighbourhood_group,
                      "</em><br>Price: $", price) %>%
      lapply(htmltools::HTML) # use the htmltools library
    
    # specify popup for geo markers
    popup <- ~paste0("<strong> Listing #: ", id) %>%
      lapply(htmltools::HTML) # use the htmltools library
    
    # create map
    map1 <- leaflet(data = listing_compare_data, options = leafletOptions(minZoom = 5)) %>%
      setView(lng = listing_compare_data[1, "longitude"], lat = listing_compare_data[1, "latitude"], zoom = 15) %>%
      addProviderTiles(providers$OpenStreetMap.HOT) %>%
      addCircleMarkers(~longitude, ~latitude, radius = 15, color = ~pal(room_type),
                       label = labels, popup = popup, labelOptions = labelOptions(direction = "top")) %>%
      addLegend(pal = pal, values = ~room_type, position = "bottomleft", title = "Listing Type")
    
  }) # end renderLeaflet
  
  ### LISTING 2 MAP ###
  output$map2 <- renderLeaflet({
    
    # filter data for the two listings provided by user
    listing_compare_data <- airbnb_data %>%
      filter(id == input$id2UI)
    
    # specify colors for room_type on map
    pal <- colorFactor(c("red2", "green2", "navy", "yellow"),
                       domain = c("Entire home/apt", "Hotel room", "Private room", "Shared room"))
    
    # specify labels for geo markers
    labels <- ~paste0("<strong>", name,
                      "</strong><em><br>", room_type, " in ", neighbourhood, ", ", neighbourhood_group,
                      "</em><br>Price: $", price) %>%
      lapply(htmltools::HTML) # use the htmltools library
    
    # specify popup for geo markers
    popup <- ~paste0("<strong> Listing #: ", id) %>%
      lapply(htmltools::HTML) # use the htmltools library
    
    # create map
    map2 <- leaflet(data = listing_compare_data, options = leafletOptions(minZoom = 5)) %>%
      setView(lng = listing_compare_data[1, "longitude"], lat = listing_compare_data[1, "latitude"], zoom = 15) %>%
      addProviderTiles(providers$OpenStreetMap.HOT) %>%
      addCircleMarkers(~longitude, ~latitude, radius = 15, color = ~pal(room_type),
                       label = labels, popup = popup, labelOptions = labelOptions(direction = "top")) %>%
      addLegend(pal = pal, values = ~room_type, position = "bottomleft", title = "Listing Type")
    
  }) # end renderLeaflet
  
  ### LINE GRAPH ###
  output$line <- renderPlot({

    # filter data based on user's selection
    crime_pop_data <- filter(crime_pop_data,
                             Month >= min(ymd(input$dateUI)),
                             Month <= max(ymd(input$dateUI)))
    select_location <- filter(crime_pop_data, ARREST_BORO %in% input$locationUI)

    # keep colors fixed while user is filtering
    plotcolors <- brewer.pal(5, name = "Pastel1")
    names(plotcolors) <- sort(unique(crime_pop_data$ARREST_BORO))

    # create line graph
    ggplot() +
      geom_line(aes(x = Month, y = arrest_rate, group = ARREST_BORO),
                size = 1, data = crime_pop_data) +
      geom_line(aes(x = Month, y = arrest_rate, group = ARREST_BORO, color = ARREST_BORO),
                size = 2.5, data = select_location) +
      scale_x_date(date_breaks = "1 month", minor_breaks = NULL, date_labels = "%B") +
      scale_color_manual(values = plotcolors[input$locationUI]) +
      labs(title = "2023 NYC Crime Trends",
           subtitle = "Arrests per 1,000 Residents",
           x = NULL,
           y = NULL,
           color = "Location") +
      theme_classic() +
      theme(
        # legend settings
        legend.position = "bottom",
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 16),
        # axis settings
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 16),
        # title settings
        plot.title = element_text(size = 20, hjust = .5),
        plot.subtitle = element_text(size = 16, hjust = .5),
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")
      )

  }) # end renderPlot
  
} # end server


###################
# RUN APPLICATION #
###################
shinyApp(ui = ui, server = server)

