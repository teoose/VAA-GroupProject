
pacman::p_load(shiny, tidyverse, shinydashboard,
               leaflet, dplyr, readr, tidyverse,
               leaflet.extras, plotly, treemap,
               treemapify, colorspace)

# Reading the data file
gtd <- read_csv("data/data_filtered_analysis.csv")

gtd_top_targets <- gtd %>% 
  filter(targtype1_txt %in% c('Citizens', 'Military', 'Police', 
                              'Government', 'Business', 'Religious Institutions', 'Airports','Journalists'))



#Dashboard header carrying the title of the dashboard
header <- dashboardHeader(title = "Decoding Chaos: Visualing Global Terrorism", titleWidth = 450)  


#Sidebar content of the dashboard
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Global Impact", tabName = "GlobalImpact", icon = icon("globe")),
    menuItem("Global Trends", tabName = "GlobalTrends", icon = icon("arrow-trend-up")),
    menuItem("Country Statistics", tabName = "CountryStatistics", icon = icon("chart-simple")),
    menuItem("Distribution Analysis", tabName = "DistributionAnalysis", icon = icon("magnifying-glass-chart")),
    menuItem("Confirmatory Analysis", tabName = "ConfirmatoryAnalysis", icon = icon("microscope")),
    menuItem("Visit the GTD for Data", icon = icon("send",lib='glyphicon'), 
             href = "https://www.start.umd.edu/gtd/")))


GlobalImpactrow1 <- fluidRow(
  valueBoxOutput("value1")
  ,valueBoxOutput("value2")
  ,valueBoxOutput("value3")
  ,valueBoxOutput("value4")
  ,valueBoxOutput("value5")
  ,valueBoxOutput("value6"))

GlobalImpactrow2 <- fluidRow(
  box(title = "Visualisation of Attacks Globally since 1970"
      ,status = "primary"
      ,solidHeader = TRUE 
      ,collapsible = FALSE
      ,width = NULL  # Automatically adjust to full width
      ,align = "center"
      ,leafletOutput("mymap", height = "600px", width = "100%")  # Set map width to 100% of box
      ,sliderInput("slider", "Years:", 1970, 2017, 2000))
)


GlobalTrendsrow1 <- fluidRow(
  valueBoxOutput("value7")
  ,valueBoxOutput("value8")
  ,valueBoxOutput("value9")
  ,valueBoxOutput("value10")
  ,valueBoxOutput("value11")
  ,valueBoxOutput("value12"))

GlobalTrendsrow2 <- fluidRow( 
  box(
    title = "What kind of Attacks were carried out in each region?"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotOutput("Killbyattacktype", height = "300px"),
    selectInput("selector", h3("Select Region"), 
                choices = list("Australasia & Oceania", "Central America & Caribbean", 
                               "Central Asia", "East Asia", "Eastern Europe", 
                               "Middle East & North Africa", "North America",
                               "South America", "South Asia", "Southeast Asia", 
                               "Sub-Saharan Africa", "Western Europe"), selected = 1))
  ,box(
    title = "Terror Events Vs Casualties, since 1970"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE
    ,plotlyOutput("Events_Casualties", height = "300px")
    ,sliderInput("terror_event", h3("Select Year"), 
                 min = 1970, max = 2020, value = 1970, step = 1))
)



GlobalTrendsrow3 <- fluidRow( 
  box(
    title = "Who were the main targets in each region?"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE
    ,plotOutput("targetkilled", height = "300px")
    ,selectInput("selector3", h3("Select Region"), 
                 choices = list("Australasia & Oceania", "Central America & Caribbean", 
                                "Central Asia", "East Asia", "Eastern Europe", 
                                "Middle East & North Africa", "North America",
                                "South America", "South Asia", "Southeast Asia", 
                                "Sub-Saharan Africa", "Western Europe"), selected = 1))
  ,box(
    title = "Terror Victims Over time by Region"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE
    ,plotlyOutput("victimsbyregion", height = "300px")
    ,selectInput("selector2", h3("Select Region"), 
                 choices = list("Australasia & Oceania", "Central America & Caribbean", 
                                "Central Asia", "East Asia", "Eastern Europe", 
                                "Middle East & North Africa", "North America",
                                "South America", "South Asia", "Southeast Asia", 
                                "Sub-Saharan Africa", "Western Europe"), selected = 1)))




body <- dashboardBody(
  tabItems(
    # 1st tab content
    tabItem(tabName = "GlobalImpact",
            GlobalImpactrow1, GlobalImpactrow2
    ),
    # 2nd tab content
    tabItem(tabName = "GlobalTrends",
            GlobalTrendsrow1, GlobalTrendsrow2, GlobalTrendsrow3
    ),
    #3rd tab content
    tabItem(tabName = "CountryStatistics"
            # content
    ),
    #4th tab content
    tabItem(tabName = "DistributionAnalysis"
            #content
    ),
    #5th tab content
    tabItem(tabName = "ConfirmatoryAnalysis"
            #content)
    )
  )  
)

#completing the ui part with dashboardPage
ui <- dashboardPage(title = 'Global Terror Statistics Dashboard', header, sidebar, body, skin='red')    



# create the server functions for the dashboard  
server <- function(input, output) { 
  
  #some data manipulation to derive the values
  total_kills <- sum(gtd$nkill, na.rm  = TRUE)
  Total_wounded <- sum(gtd$nwound, na.rm = TRUE)
  countries <- gtd %>% group_by(country_txt) %>% summarise(value = sum(nkill, na.rm=TRUE)) %>% filter(value==max(value))
  weapon.type <- gtd %>% group_by(weaptype1_txt) %>% summarise(value = sum(nkill, na.rm = TRUE)) %>% filter(value==max(value))
  region_most <- gtd %>% group_by(region_txt) %>% summarise(value = sum(nkill, na.rm = TRUE)) %>% filter(value == max(value))
  attack.type <- gtd %>% group_by(attacktype1_txt) %>% summarise(value = sum(nkill, na.rm = TRUE)) %>% filter(value==max(value))
  target.type <- gtd %>% group_by(targtype1_txt) %>% summarise(value = sum(nkill, na.rm = TRUE)) %>% filter(value==max(value))
  year_most <- gtd %>% group_by(iyear) %>% summarise(value = sum(nkill, na.rm = TRUE)) %>% filter(value==max(value))
  
  attacker <- gtd %>%
    filter(gname != "Unknown") %>%  # Exclude rows with gname as "Unknown"
    group_by(gname) %>%
    summarise(value = sum(nkill, na.rm = TRUE)) %>%
    filter(value == max(value))
  
  city_most <- gtd %>%
    filter(city != "Unknown") %>%  # Exclude rows with city as "Unknown"
    group_by(city) %>%
    summarise(value = sum(nkill, na.rm = TRUE)) %>%
    filter(value == max(value))
  
  data_prepared <- gtd %>%
    group_by(iyear, country_txt) %>%
    summarise(Incidents = n(),
              Kills = sum(nkill, na.rm = TRUE),
              Wounded = sum(nwound, na.rm = TRUE)) %>%
    ungroup()
  
  data_prep1 <- gtd %>%
    group_by(iyear, region_txt) %>%
    summarise(Incidents = n(),
              Killed = sum(nkill, na.rm = TRUE),
              Wounded = sum(nwound, na.rm = TRUE)) %>%
    ungroup()
  
  GTD_summarised <- gtd %>%
    group_by(iyear, country_txt, region_txt, city, attacktype1_txt, targtype1_txt, motive, weaptype1_txt) %>%
    summarise(Total_events = n(),  # Correctly counting the number of events per group
              Total_Kills = sum(nkill, na.rm = TRUE),
              Total_Wounded = sum(nwound, na.rm = TRUE),
              .groups = 'drop')  # Drop the grouping
  
  
  aggregated_data_attack <- GTD_summarised %>%
    group_by(country_txt, region_txt, attacktype1_txt, targtype1_txt, motive, weaptype1_txt ) %>%
    summarise(
      TotalEvents = sum(Total_events),
      TotalKills = sum(Total_Kills),
      TotalWounded = sum(Total_Wounded)
    ) %>%
    ungroup()
  
  
  #creating the value BoxOutput content
  
  #Global Impact tab  
  
  output$value1 <- renderValueBox({
    total_attacks <- nrow(gtd)  
    valueBox(
      formatC(total_attacks, format = "d", big.mark = ','),
      "Total Attacks",
      icon = icon("triangle-exclamation", lib = "font-awesome"),  
      color = "red"  
    )
  })
  
  output$value2 <- renderValueBox({
    total_kills <- sum(gtd$nkill, na.rm = TRUE)
    valueBox(
      formatC(total_kills, format = "d", big.mark = ','),
      "Total Casualties",
      icon = icon("truck-medical", lib = "font-awesome"),  
      color = "red"
    )
  })
  
  
  output$value3 <- renderValueBox({
    Total_wounded <- sum(gtd$nwound, na.rm = TRUE)  
    valueBox(
      formatC(Total_wounded, format = "d", big.mark = ','),
      "Total Wounded",
      icon = icon("user-injured", lib = 'font-awesome'),  
      color = "red"  
    )
  })
  
  output$value4 <- renderValueBox({
    success_percentage <- sum(gtd$success == 1, na.rm = TRUE) / nrow(gtd) * 100
    valueBox(
      paste0(formatC(success_percentage, digits = 2), "%"),  
      "Attack Success Rate",
      icon = icon("bomb", lib = "font-awesome"),  
      color = "red"  
    )
  })
  
  
  output$value5 <- renderValueBox({
    valueBox(
      formatC(countries$value, format="d", big.mark=',')
      ,paste('Country with Most Casualties:',countries$country_txt)
      ,icon = icon("person-drowning",lib='font-awesome')
      ,color = "red")
    
    
  })
  
  output$value6 <- renderValueBox({
    valueBox(
      formatC(region_most$value, format="d", big.mark=',')
      ,paste('Region with Most Casualties:',region_most$region_txt)
      ,icon = icon("person-drowning",lib='font-awesome')
      ,color = "red")
    
  })
  
  
  #Global Trends Tab  
  
  output$value7 <- renderValueBox({
    
    valueBox(
      formatC(attacker$value, format="d", big.mark=',')
      ,paste('Top Terror Group:',attacker$gname)
      ,icon = icon("people-group",lib='font-awesome')
      ,color = "red")
    
  })
  
  output$value8 <- renderValueBox({
    
    valueBox(
      formatC(attack.type$value, format="d", big.mark=',')
      ,paste('Top Attack type:',attack.type$attacktype1_txt)
      ,icon = icon("people-robbery",lib='font-awesome')
      ,color = "red")
    
  })
  
  
  output$value9 <- renderValueBox({
    
    valueBox(
      formatC(weapon.type$value, format="d", big.mark=',')
      ,paste('Top Weapon Used:',weapon.type$weaptype1_txt)
      ,icon = icon("gun",lib='font-awesome')
      ,color = "red")
    
  })
  
  output$value10 <- renderValueBox({
    
    valueBox(
      formatC(target.type$value, format="d", big.mark=',')
      ,paste('Top Targets:',target.type$targtype1_txt)
      ,icon = icon("gun",lib='font-awesome')
      ,color = "red")
    
  })
  
  output$value11 <- renderValueBox({
    valueBox(
      formatC(city_most$value, format="d", big.mark=',')
      ,paste('Most Deadly City:',city_most$city)
      ,icon = icon("city",lib='font-awesome')
      ,color = "red")
    
  })
  
  output$value12 <- renderValueBox({
    valueBox(
      formatC(year_most$value, format="d", big.mark=',')
      ,paste('Most Deadly Year:',year_most$iyear)
      ,icon = icon("calendar-days",lib='font-awesome')
      ,color = "red")
    
  })
  
  
  #creating the plotOutput content
  
  #1 treemap 1 = What kinds of attacks were carried out?
  
  datasetregion <- reactive({
    
    filter(aggregated_data_attack, region_txt == input$selector)
  })
  
  output$Killbyattacktype <- renderPlot({
    data_region <- datasetregion()
    
    treemap(data_region,
            index = c("region_txt", "attacktype1_txt"),
            vSize = "TotalEvents",
            vColor = "TotalKills",
            type = "manual",
            algorithm = "pivotSize",
            sortID = "TotalKills",
            title = paste("What were the Attack Types in", input$selector, "?"),
            palette = "Reds",
            border.col = "white",
            border.lwds = 0.5)
    
  })
  
  
  #2
  #bubble plot: Terror events vs casualties since 1970
  
  output$Events_Casualties <- renderPlotly({
    data_filter <- data_prepared[data_prepared$iyear == input$terror_event, ]
    
    p <- ggplot(data_filter, aes(x = Incidents, y = Kills, size = Kills, color = country_txt)) +
      geom_point(alpha = 0.7) +
      scale_size(range = c(1, 20)) + 
      theme_minimal() +
      labs(title = paste("Terror Events Vs Casualties, Year = ", input$terror_event),
           x = "Number of Terror Events",
           y = "Number of Casualties") +
      theme(legend.position = "none") 
    
    ggplotly(p, tooltip = c("country_txt", "Incidents", "Kills"))
  })
  
  
  #3 -treemap: Who were the main targets in each region?
  datasettarget <- reactive({
    
    filter(aggregated_data_attack, region_txt == input$selector3)
  })
  
  output$targetkilled <- renderPlot({
    data_target <- datasettarget()
    
    treemap(data_target,
            index = c("region_txt", "targtype1_txt"),
            vSize = "TotalEvents",
            vColor = "TotalKills",
            type = "manual",
            algorithm = "pivotSize",
            sortID = "TotalKills",
            title = paste("Who were the Main Targets in", input$selector3, "?"),
            palette = "Reds",
            border.col = "white",
            border.lwds = 0.5)
    
  })
  
  #4 line plot: Terror victims over time by region  
  datasetInput <- reactive({
    
    filter(data_prep1, region_txt == input$selector2)
  })
  
  output$victimsbyregion <- renderPlotly({
    data_filter <- datasetInput()
    
    p1 <- ggplot(data_filter, aes(x = iyear)) +
      geom_line(aes(y = Incidents, color = "Incidents"), linetype = "solid", size = 0.5) +
      geom_line(aes(y = Killed, color = "Killed"), linetype = "solid", size = 0.5) +
      geom_line(aes(y = Wounded, color = "Wounded"), linetype = "solid", size = 0.5) +
      labs(title = paste("Terrorism Victims Over Time in", input$selector2),
           x = "Year",
           y = "Number",
           color = "Type") +
      theme_minimal() +
      scale_color_manual(values = c(Incidents = "blue", Killed = "black", Wounded = "red")) +
      theme(legend.position = "top")
    
    ggplotly(p1)
  })
  
  
  
  #global map    
  datasetyears <- reactive({
    
    filter(gtd, gtd$iyear %in% input$slider)
  })
  
  #create the map
  output$mymap <- renderLeaflet({
    data_years <- datasetyears()
    
    leaflet(data_years) %>% 
      setView(lng = 43.6793, lat = 33.2232, zoom = 2) %>% 
      addTiles() %>% 
      addCircles(data = data_years, lat = ~ latitude, lng = ~ longitude, weight = 1, 
                 radius = ~sqrt(nkill)*25000, popup = ~as.character(nkill), 
                 label = ~paste("Country:", country_txt,"Casualties:", nkill), fillOpacity = 0.5)
  })
  
}

# Run the app
shinyApp(ui = ui, server = server)
