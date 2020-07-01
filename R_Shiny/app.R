library(shinydashboard)
library(shiny)
library(tidyverse)
library(shinyWidgets)
library(scales)
library(plotly)
library(leaflet)


data = read.csv("Data/AB_NYC_2019.csv")
data = data %>%
  select(-calculated_host_listings_count, -reviews_per_month,
         -availability_365, -last_review) 
boroughs = unique(data$neighbourhood_group)
neighbourhoods = unique(data$neighbourhood)
room_types = unique(data$room_type)

ui <- dashboardPage(skin = "blue",
    dashboardHeader(title = "New York Airbnb 2019"),
    dashboardSidebar(
        pickerInput(
            inputId = "borough",
            label = "Borough",
            choices = sort(boroughs),
            options = list(`actions-box` = TRUE),
            multiple = TRUE,
            selected = boroughs
            ),
        sidebarMenu(
            pickerInput(
                inputId = "neighbourhood",
                label = "Neighbourhood",
                choices = sort(neighbourhoods),
                options = list(`actions-box` = TRUE),
                multiple = TRUE,
                selected = neighbourhoods
                )
            ),
        sidebarMenu(
            pickerInput(
                inputId = "room_type",
                label = "Room Type",
                choices = sort(room_types),
                options = list(`actions-box` = TRUE),
                multiple = TRUE,
                selected = room_types
            )
        ),
        sidebarMenu(
            sliderInput(
                inputId = "price_range",
                label = "Price Per Night",
                min = 0,
                max = 1000,
                value = c(0, 1000)
                 )
            ),
        sidebarMenu(
            sliderInput(
                inputId = "min_stay",
                label = "Minimum Nights",
                min = 0,
                max = 100,
                value = c(0, 100)
                )
        ),
        sidebarMenu(
            sliderInput(
                inputId = "min_reviews",
                label = "Minimum Reviews",
                min = 0,
                max = 100,
                value = 0
                )
        )
      ),
    dashboardBody(
      tabsetPanel(
        id = "tabs",
        tabPanel(
          title = "Dashboard",
          value = "page1",
          
          fluidRow(
            br()
          ),
       
        fluidRow(
             valueBoxOutput("total_listings"),
             valueBoxOutput("average_price"),
             valueBoxOutput("median_price")
             ),
        fluidRow(),
        fluidRow(
            column(
                width = 6,
                plotlyOutput("hist",
                             height = 550)
            ),
            column(
                width = 6,
                leafletOutput("map",
                              height = 550)
                
            )
        )  
            ),
        tabPanel(
          title = "Raw Data",
          value = "page2",
          
          fluidRow(
            br(),
            column(1,
            downloadButton(
              "download_data",
              "Download Data")
            )
          ),
          fluidRow(
            br()
            ),
          
          fluidRow(
            column(dataTableOutput("raw_data"), width = 12
              
            )
          )
          
          
          
        )
        )
        
    )
)
server <- function(input, output, session) {
  
    # filter listings based on selection inputs
    base_listings = reactive({
        listings = data %>%
            filter(neighbourhood_group %in% input$borough,
                   neighbourhood %in% input$neighbourhood,
                   room_type %in% input$room_type,
                   between(price,input$price_range[1], input$price_range[2]),
                   between(minimum_nights, input$min_stay[1], input$min_stay[2]),
                   number_of_reviews >= input$min_reviews)
     
                
    })
    
    
    # update neighbourhood selection box to only include the neighbourhoods 
    # that are part of the selected boroughs.
    relevent_neighbourhoods = reactive({
      listings = data %>%
        filter(neighbourhood_group %in% input$borough)
      
      
    })
    
    observeEvent(input$borough, {
      rn = relevent_neighbourhoods()
      
      
      
      updatePickerInput(session = session, 
                        inputId = "neighbourhood",
                        choices = sort(unique(rn$neighbourhood)),
                        selected = unique(rn$neighbourhood))
      
    }, ignoreInit = TRUE, ignoreNULL = FALSE)

    
    
    # total listings box
    output$total_listings = renderValueBox({
        base_listings() %>%
            tally() %>%
            pull() %>%
            as.integer() %>%
            valueBox(subtitle = "Listings",
                     color = "light-blue")
    })
    
    # average price box
    output$average_price = renderValueBox({
        base_listings() %>%
            summarise(avg = mean(price)) %>%
            replace(is.na(.), 0) %>%
            pull() %>%
            round() %>%
            dollar() %>%
            valueBox(subtitle = "Average Price",
                     color = "light-blue")
    })
    
    # median price box
    output$median_price = renderValueBox({
        base_listings() %>%
            summarise(med = median(price)) %>%
            replace(is.na(.), 0) %>%
            pull() %>%
            round() %>%
            dollar() %>%
            valueBox(subtitle = "Median Price",
                     color = "light-blue")    
    })
    
    # interactive price distribution histogram 
    output$hist = renderPlotly({
        base_listings() %>%
            plot_ly(
                x = ~price,
                type = "histogram",
                nbinsx = 13,
                color = ~neighbourhood_group,
                hovertemplate = paste(
                   '%{x:$.2f}', "<br>", "Listings: ", "%{y}"
                )
        
            ) %>%
            layout(
                legend = list(orientation = "h",
                              xanchor = "center",
                              x = 0.5,
                              y = - 0.25),
                xaxis = list( title = "Price per Night ($)"),
                yaxis = list(title = "Number of Listings" ),
                paper_bgcolor = '#dcf3fb',
                plot_bgcolor = '#dcf3fb',
                title = list(text = "<b>Listing Prices<b>",
                             yref = "paper", 
                             xref = "paper",
                             x = 0,
                             y = 1, 
                             size = 24,
                             family = "Arial")
                
            )
    })
    
    # interactive map showing location of returned listings 
    output$map = renderLeaflet({
        results = base_listings()
        leaflet(data = results,
                options = leafletOptions(minZoom = 9)) %>%
            addProviderTiles(providers$Esri.WorldStreetMap) %>%
            addMarkers(
                clusterOptions = markerClusterOptions(),
                popup = paste(results$name, "<br/>", results$room_type, "<br/>",
                                str_c(results$neighbourhood, ", "),
                                results$neighbourhood_group, "<br/>",
                                str_c("$", results$price), " per night.")
            )
    })
    
    
    output$raw_data = renderDataTable({
      {base_listings()}
    }, options = list(scrollX = TRUE,
                      autoWidth = TRUE))
    
    output$download_data = downloadHandler(
      filename = function(){
        paste("airbnb", "csv", sep = ".")
      },
      content = function(file){
        write.csv(base_listings(), file)
      }
    )

}
shinyApp(ui, server)
