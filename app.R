### Shiny Workspace-----------------------------------------------
suppressMessages(library(tidyverse))  
suppressMessages(library(ggplot2))
suppressMessages(library(ggthemes))
suppressMessages(library(shiny))
suppressMessages(library(lubridate))
suppressMessages(library(shinydashboard))
suppressMessages(library(plotly))
suppressMessages(library(leaflet))
suppressMessages(library(rsconnect))
suppressMessages(library(googlesheets4))

ss <- "https://docs.google.com/spreadsheets/d/1h2gnQeXHhrO-NhAVhXJK1lSdTpyvbdLbduRSIMAZjPQ/edit?usp=sharing"
obs_final <- read_sheet(ss) 
obs_final$date <- as.Date(obs_final$date)

ui <- dashboardPage(
  dashboardHeader(title = "Ski Conditions Dashboard"),
  dashboardSidebar(
    
    selectInput("select", 
                h3("Select Mountain"), 
                choices = list("Mt Hood, Oregon, USA" = 1, 
                               "Mt Baker, Washington, USA" = 2,
                               "Mt Bachelor, Oregon, USA" = 3, 
                               "White Pass, Washington, USA" = 4, 
                               "Schweitzer, Idaho, USA" = 5, 
                               "Tahoe, California, USA" = 6), 
                selected = 1),
    
    dateRangeInput("date", 
                   h3("Select a Date Range"), 
                   start = Sys.Date()-100, 
                   end = Sys.Date(),
                   format = "yyyy-mm-dd", 
                   startview = "month", 
                   weekstart = 0,
                   language = "en" )
  ),
  dashboardBody(
    
    # Frontpage - boxes - start -----------------------------------------------
    fluidRow(
      
      valueBoxOutput("date_out"),
      valueBoxOutput("depth_out"),
      valueBoxOutput("density")
    ),
    
    # Frontpage - boxes - end -------------------------------------------------
    
    # Box layout - Grid - Start #-------------------------------------------------
    fluidRow(
      box(leafletOutput("map", height = 400)),
      box(plotlyOutput("temp", height = 400)),
      box(plotlyOutput("snowdepth", height = 400)),
      box(plotlyOutput("prcp", height = 400))
      
    )
    #Box layout - Grid - end #------------------------------------------------- 
  )
)

server <- function(input, output) {
  
  data <- reactive({
    switch(input$select,
           "1" = obs_final %>% filter(mountain == "Mt Hood, Oregon, USA"),
           "3" = obs_final %>% filter(mountain == "Mt Bachelor, Oregon, USA"),
           "2" = obs_final %>% filter(mountain == "Mt Baker, Washington, USA"),
           "4" = obs_final %>% filter(mountain == "White Pass, Washington, USA"),
           "5" = obs_final %>% filter(mountain == "Schweitzer, Idaho, USA"),
           "6" = obs_final %>% filter(mountain == "Tahoe, California, USA"))
  })
  
  val_data <- reactive({
    data()%>% 
      filter(!is.na(date),!is.na(snwd))
  })
  
  ### Snow Depth Chart--------------------------------------------------------------
  output$snowdepth <- renderPlotly({
    
    p <- ggplot(data(),aes(date,snwd))+
      geom_area(aes(fill=name),size=1)+
      scale_x_date(limits = c(input$date[1], input$date[2]))+
      theme(legend.position="none",
            axis.title.x=element_blank(),
            axis.title.y=element_blank())+
      ggtitle("Snow Depth by Station (Inches)")+ 
      scale_fill_manual(values = c("#6794a7", "#76c0c1"))
    
    ggplotly(p)
  })
  
  ### Station Location Map-------------------------------------------------------------- 
  output$map <- renderLeaflet({
    
    data <- data()
    
    title <- "Weather Station Locations (click marker for info)" 
    
    content <- paste(sep = "<br/>",
                     paste("Ski Resort: ", data$mountain),                 
                     paste("Station: ",data$name),
                     paste("Elevation: ",data$elevationFt))
    
    leaflet() %>%
      addTiles() %>% 
      addMarkers(lng= data$longitude, lat= data$latitude, popup = content) %>% 
      addControl(title, position = "topright")
  })
  
  ### Temp Chart--------------------------------------------------------------
  
  output$temp <- renderPlotly({
    
    temp_final <- data() %>% 
      select(mountain, name, date,tmax, tmin) %>% 
      gather(key = temptype, value = temp, c(tmax,tmin))
    
    f <- ggplot(temp_final,aes(date,temp))+
      geom_line(aes(color=temptype))+
      scale_x_date(limits = c(input$date[1], input$date[2]))+
      theme(legend.position="none",
            axis.title.x=element_blank(),
            axis.title.y=element_blank())+
      geom_hline(yintercept=32, linetype="dashed", 
                 color = "blue", size=1)+
      ggtitle("High-Low Temperature by Station (Farenheit)")+
      scale_color_economist()
    ggplotly(f)
    
    
    
  })
  
  ### PRCP Chart--------------------------------------------------------------
  
  output$prcp <- renderPlotly({
    
    q <- ggplot(data(),aes(date,prcp))+
      geom_line(aes(color=name), size=1)+
      scale_x_date(limits = c(input$date[1], input$date[2]))+
      theme(legend.position="none",
            axis.title.x=element_blank(),
            axis.title.y=element_blank())+
      ggtitle("Precipitation by Station (Inches)") +   
      scale_color_economist()
    
    ggplotly(q)
    
  })
  
  ### Max Date Value Box--------------------------------------------------------------
  
  output$date_out <- renderValueBox({
    
    valueBox(
      val_data() %>% 
        filter(elevationFt == max(elevationFt)) %>% 
        filter(date == max(date)) %>% 
        select(date) %>% 
        mutate(date = paste(month(date, label = TRUE, abbr = FALSE),day(date),year(date))),
      "Last Snow Depth Reading",
      color = "blue",
      icon = icon("calendar-alt"),
      width = 2
    )
  })
  
  ### Depth Change Value Box--------------------------------------------------------------
  output$depth_out <- renderValueBox({
    
    depth_find <- function(df){
      x <- df %>% 
        filter(elevationFt == max(elevationFt)) %>% 
        filter(date == max(date)) %>% 
        select(snwd) 
      y <- df %>% 
        filter(elevationFt == max(elevationFt)) %>% 
        filter(date == min(date)) %>% 
        select(snwd)
      
      change <- x-y
      return(as.integer(change))
    }
    
    depth <- val_data() %>% 
      select(date,snwd,elevationFt) %>% 
      filter(date <= max(date),date >= max(date)-4, !is.na(snwd)) %>% 
      depth_find()
    
    valueBox(paste(depth," Inches"),
             "Snow Depth Change in last 4 days",
             color = "light-blue",
             icon = icon("snowflake"),
             width = 2
    )
  })
  
  ### Last Recorded Snow Density--------------------------------------------------------------
  output$density <- renderValueBox({
    
    dens_find <- function(df){
      x <- df %>% 
        filter(elevationFt == max(elevationFt)) %>% 
        filter(date == max(date)) %>%
        select(snw_dens)
      return(as.integer(x))
    }
    
    icon_func <- function(d)
    {
      ifelse(d > 30, return(icon("thumbs-down")),return(icon("thumbs-up")))
    }
    
    d <- val_data() %>% 
      select(date,snw_dens,elevationFt) %>% 
      dens_find()
    
    valueBox(paste(d,"%"),
             "Current Snow Density",
             color = "aqua",
             icon = icon_func(d),
             width = 3
             
             
    )
  })
  
}

shinyApp(ui, server)


