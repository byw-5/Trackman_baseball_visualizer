library(shinydashboard)
library(shiny)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(mgcv)
library(broom)
library(modelr)

data  <- read_csv("example_game_data.csv")
ump_data <- read_csv("example_ump_data.csv")

ui <- dashboardPage(
  dashboardHeader(title = "Trackman Visualizer"),
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("Pitch-by-pitch Grpahs", tabName="Point"),
      menuItem("Strike Zone Graphs", tabName="Zone"),
      menuItem("Spray Charts", tabName="Spray"),
      menuItem("Estimated Zone for Umpires", tabName="Ump")
    ),
    conditionalPanel(
      condition = "input.tabs == 'Point' | input.tabs == 'Zone'| input.tabs == 'Spray'" ,
      selectInput("pitcher_name", "Select a pitcher", choices = c("All pitchers",unique(data$Pitcher)), width = 225),
      selectInput("batter_name", "Select a batter", choices = c("All batters",unique(data$Batter)), width = 225),
      selectizeInput("pitch_type", "Pitch types:",choices = NULL,multiple = T, width = 225),
      checkboxGroupInput("throws", "Pitcher throws:",choices = c("Left","Right"),inline = TRUE),
      checkboxGroupInput("stand", "Batter stand:", choices = c("Left","Right"),inline = TRUE),
      checkboxGroupInput("strikes", "Strikes:",choices = c(0,1,2),inline = TRUE),
      checkboxGroupInput("balls", "Balls:",choices = c(0,1,2,3),inline = TRUE),
      actionButton(inputId   = "runReport", label = "Apply filter"),),
    conditionalPanel(
      style = "position: fixed; ",
      condition = "input.tabs == 'Ump'" ,
      selectInput("ump_name", "Select an umpire", choices = c(unique(ump_data$Umpire)), width = 225),)
    ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "Point", fluidRow(
          box(title = "All Pitches",plotOutput("plot")),
          box(title = "Pitch Type Percentage",plotOutput("plot2"))
        ),
        fluidRow(
          box(title = "All Swings",plotOutput("plot3")),
          box(title = "All Whiffs",plotOutput("plot4"))
        ),
        fluidRow(
          box(title = "Every Ball-in-play",
              height = 564,
              selectizeInput("result", "Ball-in-play outcomes:", choices = c("Non-hit","Single","Double","Triple","HomeRun"),multiple = T),
              plotOutput("plot5")),
          box(title = "Batted Ball Profile Filters",
              sliderInput("xwoba", label = "XWOBA >=",min = 0, max = max(data$xwoba),value = 0),
              sliderInput("speedl", label = "Exit velocity >=",min = 0, max = 120,value = 0),
              sliderInput("speedr", label = "Exit velocity <",min = 0, max = 120,value = 120),
              sliderInput("anglel", label = "Launch angle >=",min = -90, max = 90,value = -90),
              sliderInput("angler", label = "Launch angle <",min = -90, max = 90,,value = 90))
         )
      ),
      tabItem(
        tabName = "Zone",
        fluidRow(
          box(radioButtons("Options", "Graph type:",c("Pitch%" = "Pitch", "Swing%" = "Swing","Whiff%" = "Whiff"),inline = TRUE),
              plotOutput("plot6")),
          box(radioButtons("Options2", "Graph type:",c("Pitch%" = "Pitch", "Swing%" = "Swing","Whiff%" = "Whiff"),inline = TRUE, selected = "Swing"),
              plotOutput("plot7"))
        ),
        fluidRow(
          box(radioButtons("Options3", "Graph type:",c("AVG" = "AVG", "OPS" = "OPS","wOBA" = "wOBA","xwOBA" = "xwOBA" ),inline = TRUE),
              
              plotOutput("plot8")),
          box(radioButtons("Options4", "Graph type:",c("AVG" = "AVG", "OPS" = "OPS","wOBA" = "wOBA","xwOBA" = "xwOBA" ),inline = TRUE, selected = "wOBA"),
              
              plotOutput("plot9"))
        )
        ),
      tabItem(
        tabName = "Spray",
        fluidRow(
          box(selectizeInput("result2", "Ball-in-play outcomes:", choices = c("Non-hit","Single","Double","Triple","HomeRun"),multiple = T),
              checkboxGroupInput("plate", "Part of the plate:",choices = c("Inside","Outside"),inline = TRUE),
              plotOutput("plot10")),
          box(selectizeInput("result3", "Batted-ball types:", choices = c("GroundBall","LineDrive","FlyBall","Popup"),multiple = T),
              checkboxGroupInput("plate2", "Part of the plate:",choices = c("Inside","Outside"),inline = TRUE),
              plotOutput("plot11"))
        )
      ),
      tabItem(
        tabName = "Ump",
        fluidRow(
          box(title = "LH Batter, Fastball ",plotOutput("plot12")),
          box(title = "LH Batter, Non-fastball",plotOutput("plot13"))
        ),
        fluidRow(
          box(title = "RH Batter, Fastball",plotOutput("plot14")),
          box(title = "RH Batter, Non-fastball",plotOutput("plot15"))
        )
      )
      
      
      )
    
  )
)

server <- function(input, output, session) {
  
  matchup_data <- reactive({
    data %>% 
      filter(case_when(
        input$batter_name == "All batters" & input$pitcher_name != "All pitchers" ~ Pitcher == input$pitcher_name,
        input$batter_name != "All batters" & input$pitcher_name == "All pitchers" ~ Batter == input$batter_name,
        input$batter_name != "All batters" & input$pitcher_name != "All pitchers" ~ Pitcher == input$pitcher_name & Batter == input$batter_name,
        input$batter_name == "All batters" & input$pitcher_name == "All pitchers" ~ PitchNo > 0
      ))
  })
  
  observe({
    updateSelectizeInput(session, "pitch_type", choices = unique(matchup_data()$PitchType))
  })
  
  observe({

    if (is.null(input$result)) {
      updateSelectizeInput(session, "result",selected = c("Non-hit","Single","Double","Triple","HomeRun"))
    }
    if (is.null(input$result2)) {
      updateSelectizeInput(session, "result2",selected = c("Non-hit","Single","Double","Triple","HomeRun"))
    }
    if (is.null(input$result3)) {
      updateSelectizeInput(session, "result3",selected = c("GroundBall","LineDrive","FlyBall","Popup"))
    }
    if (is.null(input$pitch_type)) {
      updateSelectizeInput(session, "pitch_type",selected = unique(matchup_data()$PitchType))
    }
    if (is.null(input$pitch_type)) {
      updateSelectizeInput(session, "pitch_type",selected = unique(matchup_data()$PitchType))
    }
    if (is.null(input$throws)) {
      updateCheckboxGroupInput(session, "throws",selected = unique(data$PitcherThrows))
    }
    if (is.null(input$stand)) {
      updateCheckboxGroupInput(session, "stand",selected = unique(data$BatterSide))
    }
    if (is.null(input$balls)) {
      updateCheckboxGroupInput(session, "balls",selected = c(0,1,2,3))
    }
    if (is.null(input$strikes)) {
      updateCheckboxGroupInput(session, "strikes",selected = c(0,1,2))
    }
    if (is.null(input$plate)) {
      updateCheckboxGroupInput(session, "plate",selected = c("Inside","Outside"))
    }
    if (is.null(input$plate2)) {
      updateCheckboxGroupInput(session, "plate2",selected = c("Inside","Outside"))
    }
  })
  
  f <- eventReactive(input$runReport, {
    matchup_data() %>% 
      filter(PitchType %in% input$pitch_type,
             BatterSide %in% input$stand,
             PitcherThrows %in% input$throws,
             Balls %in% input$balls,
             Strikes %in% input$strikes)
  })
  
  g <- eventReactive(input$runReport, {
    matchup_data() %>% 
      filter(PitchType %in% input$pitch_type,
             BatterSide %in% input$stand,
             PitcherThrows %in% input$throws,
             Balls %in% input$balls,
             Strikes %in% input$strikes)%>% 
      group_by(PitchType, Strikes, Balls) %>% 
      summarise(N = n()) %>% 
      mutate(count = paste0(Balls, "-", Strikes)) %>%
      left_join(
        group_by(., count) %>% 
          summarise(N = sum(N)),
        by = "count"
      ) %>%
      mutate(usage = N.x / N.y) %>%
      select(count, PitchType, usage)
  })
  
  
  
  scale_fill_chris <- function(...){
    ggplot2:::manual_scale(
      'fill', 
      values = setNames(c("goldenrod","blueviolet","coral2","dodgerblue2","mediumaquamarine","gray34","black","lightsalmon4","white","white","white","white","white"), unique(data$PitchType)), 
      ...
    )
  }
  
  scale_fill_chris2 <- function(...){
    ggplot2:::manual_scale(
      'color', 
      values = setNames(c("goldenrod","blueviolet","coral2","dodgerblue2","mediumaquamarine","gray34","black","lightsalmon4","white","white","white","white","white"), unique(data$PitchType)), 
      ...
    )
  }
  
  color_plot<- ggplot() +
    annotate(geom = "rect", xmin = -0.947, xmax = -0.316,ymin = 1.5,ymax = 2.2,color = "black", alpha = 0,size = 1,fill = c("white")) +
    annotate(geom = "rect", xmin = -0.316, xmax = 0.316,ymin = 1.5,ymax = 2.2, color = "black", alpha = 0,size = 1,fill = c("white")) +
    annotate(geom = "rect", xmin =  0.316, xmax = 0.947,ymin = 1.5,ymax = 2.2, color = "black", alpha = 0,size = 1,fill = c("white")) +
    annotate(geom = "rect", xmin = -0.947, xmax = -0.316, ymin = 2.2, ymax = 2.9,  color = "black",  alpha = 0,size = 1,fill = c("white")) +
    annotate(geom = "rect", xmin = -0.316, xmax = 0.316,ymin = 2.2,ymax = 2.9,color = "black",alpha = 0,size = 1,fill = c("white")) +
    annotate(geom = "rect", xmin =  0.316, xmax = 0.947,ymin = 2.2,ymax = 2.9,color = "black",alpha = 0,size = 1,fill = c("white")) +
    annotate(geom = "rect", xmin = -0.947, xmax = -0.316,ymin = 2.9,ymax = 3.6,color = "black", alpha = 0,size = 1,fill = c("white")) +
    annotate(geom = "rect", xmin = -0.316, xmax = 0.316,ymin = 2.9,ymax = 3.6,  color = "black", alpha = 0,size = 1,fill = c("white")) +
    annotate(geom = "rect", xmin =  0.316, xmax = 0.947, ymin = 2.9, ymax = 3.6, color = "black", alpha = 0, size = 1, fill = c("white")) +
    annotate(geom = "rect", xmin =  0.947, xmax = 1.347, ymin = 2.067, ymax = 3.034, color = "black", alpha = 0, size = 1, fill = c("white")) +
    annotate(geom = "rect", xmin = -1.347, xmax = -0.947, ymin = 2.067, ymax = 3.034, color = "black", alpha = 0, size = 1, fill = c("white")) +
    annotate(geom = "rect", xmin = -0.449, xmax = 0.449, ymin = 3.6, ymax = 4, color = "black", alpha = 0, size = 1, fill = c("white")) +
    annotate(geom = "rect", xmin = -0.449, xmax = 0.449, ymin = 1.1, ymax = 1.5, color = "black", alpha = 0, size = 1, fill = c("white")) +
    annotate(geom = "polygon", x = c(-1.347, -0.449, -0.449, -0.947, -0.947, -1.347), y = c(4, 4, 3.6, 3.6, 3.034, 3.034), color = "black", alpha = 0, size = 1, fill = c("white")) +
    annotate(geom = "polygon", x = c(0.449, 1.347, 1.347, 0.947, 0.947, 0.449), y = c(4, 4, 3.034, 3.034, 3.6, 3.6), color = "black", alpha = 0, size = 1, fill = c("white")) +
    annotate(geom = "polygon", x = c(-1.347, -0.449, -0.449, -0.947, -0.947, -1.347), y = c(1.1, 1.1, 1.5, 1.5, 2.067, 2.067), color = "black", alpha = 0, size = 1, fill = c("white")) +
    annotate(geom = "polygon", x = c(0.449, 1.347, 1.347, 0.947, 0.947, 0.449), y = c(1.1, 1.1, 2.067, 2.067, 1.5, 1.5), color = "black", alpha = 0, size = 1, fill = c("white"))+
    coord_equal() + scale_x_continuous("",limits = c(-2, 2)) +scale_y_continuous("",limits = c(0.5, 4.6))
  
  
  output$plot <- renderPlot({
    p <- color_plot +
      geom_point(data = f(), size = 5,color = "black" ,shape = 21,aes(x = PlateLocSide, y = PlateLocHeight, fill = PitchType), show.legend = FALSE) +
      labs(title = "Pitcher's POV") + theme(plot.title = element_text(hjust = 0.5)) + scale_fill_chris()
    p
  })
  
  output$plot2 <- renderPlot({
    ggplot(g(), aes(x = count, y = usage, fill = PitchType)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = scales::percent(round(usage,2))), position = position_stack(vjust = 0.5), color = "white") +
      scale_fill_chris()+
      theme_minimal() +
      coord_flip()
  })
  
  output$plot3 <- renderPlot({
    
    p <- color_plot +
      geom_point(data = f() %>% filter(swing == 1), size = 5,color = "black" ,shape = 21,aes(x = PlateLocSide, y = PlateLocHeight, fill = PitchType), show.legend = FALSE) +
      labs(title = "Pitcher's POV") + theme(plot.title = element_text(hjust = 0.5)) + scale_fill_chris()
    
    p
  })
  
  output$plot4 <- renderPlot({
    
    p <- color_plot +
      geom_point(data = f() %>% filter(PitchCall == "StrikeSwinging"), size = 5,color = "black" ,shape = 21,aes(x = PlateLocSide, y = PlateLocHeight, fill = PitchType), show.legend = FALSE) +
      labs(title = "Pitcher's POV") + theme(plot.title = element_text(hjust = 0.5)) + scale_fill_chris()
    
    p
  })
  
  output$plot5 <- renderPlot({
    
    p <- color_plot +
      geom_point(data = matchup_data() %>% 
                   filter((is.na(ExitSpeed) != T | is.na(ExitSpeed) != T) & PitchCall == "InPlay") %>% 
                   filter(xwoba >= input$xwoba,
                          ExitSpeed >= input$speedl,
                          ExitSpeed < input$speedr,
                          Angle >= input$anglel,
                          Angle < input$angler) %>% 
                   filter(Result %in% input$result,
                          PitchType %in% input$pitch_type,
                          BatterSide %in% input$stand,
                          PitcherThrows %in% input$throws,
                          Balls %in% input$balls,
                          Strikes %in% input$strikes), size = 5,color = "black" ,shape = 21,aes(x = PlateLocSide, y = PlateLocHeight, fill = PitchType), show.legend = FALSE) +
      labs(title = "Pitcher's POV") + theme(plot.title = element_text(hjust = 0.5)) + scale_fill_chris()
    
    p
  })
  
  output$plot6 <- renderPlot({
    
    data <- f() %>% 
      group_by(zone) %>%
      summarise(N = n(),
                total = nrow(f()),
                swing = sum(as.numeric(swing), na.rm = T ),
                whiff = sum(as.numeric(whiff), na.rm = T )) %>% 
      mutate(swingper = swing/N) %>% 
      mutate(whiffper = whiff/swing) %>%
      mutate(pitchper = N/total) %>% 
      mutate(text = case_when(
        input$Options == "Swing" ~ ifelse(is.na(swingper)," ",paste0((round(swingper,digits = 2)*100),"%")),
        input$Options == "Whiff" ~ ifelse(is.na(whiffper)," ",paste0((round(whiffper,digits = 2)*100),"%")),
        input$Options == "Pitch" ~ ifelse(is.na(pitchper)," ",paste0((round(pitchper,digits = 2)*100),"%")))) %>% 
      mutate(color = case_when(
        input$Options == "Swing" ~ (ifelse(is.na(swingper),"gray",ifelse(swingper == 0.5, "white", ifelse(swingper > 0.5, "red","blue")))),
        input$Options == "Whiff" ~ (ifelse(is.na(whiffper),"gray",ifelse(whiffper == 0.5, "white", ifelse(whiffper > 0.5, "red","blue")))),
        input$Options == "Pitch" ~ (ifelse(is.na(pitchper),"gray",ifelse(pitchper == 0.075, "white", ifelse(pitchper > 0.075, "red","blue")))))) %>% 
      mutate(alpha = 0.4*case_when(
        input$Options == "Swing" ~ ifelse(is.na(swingper),0,ifelse(swingper == 0.5, 0.5, ifelse(swingper > 0.5, swingper, 1-swingper))),
        input$Options == "Whiff" ~ ifelse(is.na(whiffper),0,ifelse(whiffper == 0.5, 0.5, ifelse(whiffper > 0.5, whiffper, 1-whiffper))),
        input$Options == "Pitch" ~ ifelse(is.na(pitchper),0,ifelse(pitchper == 0.075, 0.5, ifelse(pitchper > 0.075, 10*pitchper, 10*pitchper))))) %>% 
      arrange(zone)
    
  
    
    if (nrow(data) != 0) {
      # If no data is available, display a custom error message
      #plot(0, 0, type = "n", xlim = c(0, 1), ylim = c(0, 1), xlab = "", ylab = "")
      #text(0.5, 0.5, "No data available for selected scenario.", cex = 1.2)
      all_zones <- data.frame(zone = c(1:9,11:14,16:19,21,23,27,29))
      
      existing_zones <- unique(data$zone)
      
      missing_zones <- setdiff(all_zones$zone, existing_zones)
      
      missing_rows <- data.frame(zone = missing_zones, 
                                 xwoba = rep(NA, length(missing_zones)), 
                                 alpha = rep(0, length(missing_zones)),
                                 color = rep("gray", length(missing_zones))
      )
      
      data <- bind_rows(data, missing_rows) %>% 
        arrange(zone)
    } 
    
    else{
      data <- data.frame (
        zone = c(1,2,3,4,5,6,7,8,9,11,12,13,14,16,17,18,19,21,23,27,29),
        xwoba = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
        color = c("gray","gray","gray","gray","gray","gray","gray","gray","gray","gray","gray","gray","gray","gray","gray","gray","gray","gray","gray","gray","gray"),
        alpha = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
      )
    }
    
    apple <- c(1,2,3)
    
    color_plot<- data %>%
      ggplot() +
      annotate(geom = "rect",
               xmin = -0.947,
               xmax = -0.316,
               ymin = 1.5,
               ymax = 2.2, 
               color = "black", 
               alpha = data$alpha[[7]],
               fill = c(data$color[[7]])) + geom_text(x = -0.6315,y = 1.85,label = (data$text[[7]][1]),color = "black", size = 5)+
      annotate(geom="rect",
               xmin = -0.316,
               xmax = 0.316,
               ymin = 1.5,
               ymax = 2.2, 
               color = "black", 
               alpha = data$alpha[[8]],
               fill = c(data$color[[8]])) + geom_text(x = 0,y = 1.85,label = data$text[[8]],color = "black", size = 5)+
      annotate(geom="rect",
               xmin = 0.316,
               xmax = 0.947,
               ymin = 1.5,
               ymax = 2.2, 
               color = "black", 
               alpha = data$alpha[[9]],
               fill = c(data$color[[9]])) + geom_text(x = 0.6315,y = 1.85,label = data$text[[9]],color = "black", size = 5)+
      annotate(geom="rect",
               xmin = -0.947,
               xmax = -0.316,
               ymin = 2.2,
               ymax = 2.9, 
               color = "black", 
               alpha = data$alpha[[4]],
               fill = c(data$color[[4]])) + geom_text(x = -0.6315,y = 2.55,label = data$text[[4]],color = "black", size = 5)+
      annotate(geom="rect",
               xmin = -0.316,
               xmax = 0.316,
               ymin = 2.2,
               ymax = 2.9,
               color = "black", 
               alpha = data$alpha[[5]],
               fill = c(data$color[[5]])) + geom_text(x = 0,y = 2.55,label = data$text[[5]],color = "black", size = 5)+
      annotate(geom="rect",
               xmin = 0.316,
               xmax = 0.947,
               ymin = 2.2,
               ymax = 2.9,
               color = "black", 
               alpha = data$alpha[[6]],
               fill = c(data$color[[6]])) + geom_text(x = 0.6315,y = 2.55,label = data$text[[6]],color = "black", size = 5)+
      annotate(geom="rect",
               xmin = -0.947,
               xmax = -0.316,
               ymin = 2.9,
               ymax = 3.6,
               color = "black", 
               alpha = data$alpha[[1]],
               fill = c(data$color[[1]])) +geom_text(x = -0.6315,y = 3.25,label = data$text[[1]],color = "black", size = 5)+
      annotate(geom="rect",
               xmin = -0.316,
               xmax = 0.316,
               ymin = 2.9,
               ymax = 3.6, 
               color = "black", 
               alpha = data$alpha[[2]],
               fill = c(data$color[[2]])) + geom_text(x = 0,y = 3.25,label = data$text[[2]],color = "black", size = 5)+
      annotate(geom="rect",
               xmin = 0.316,
               xmax = 0.947,
               ymin = 2.9,
               ymax = 3.6, 
               color = "black", 
               alpha = data$alpha[[3]],
               fill = c(data$color[[3]])) +geom_text(x = 0.6315,y = 3.25,label = data$text[[3]],color = "black", size = 5)+
      annotate(geom="rect",
               xmin = 0.947,
               xmax = 1.347,
               ymin = 2.067,
               ymax = 3.034,
               color = "black", 
               alpha = data$alpha[[14]],
               fill = c(data$color[[14]])) + geom_text(x = 1.147,y = 2.55,label = data$text[[14]],color = "black", size = 4.5)+
      annotate(geom="rect",
               xmin = -1.347,
               xmax = -0.947,
               ymin = 2.067,
               ymax = 3.034,
               color = "black", 
               alpha = data$alpha[[13]],
               fill = c(data$color[[13]])) + geom_text(x = -1.147,y = 2.55,label = data$text[[13]],color = "black", size = 4.5)+
      annotate(geom="rect",
               xmin = -0.449,
               xmax = 0.449,
               ymin = 3.6,
               ymax = 4, 
               color = "black", 
               alpha = data$alpha[[11]],
               fill = c(data$color[[11]])) + geom_text(x = 0 ,y = 3.8,label = data$text[[11]],color = "black", size = 4.5)+
      annotate(geom="rect",
               xmin = -0.449,
               xmax = 0.449,
               ymin = 1.1,
               ymax = 1.5, 
               color = "black", 
               alpha = data$alpha[[16]],
               fill = c(data$color[[16]])) + geom_text(x = 0,y = 1.3,label = data$text[[16]],color = "black", size = 4.5)+
      annotate(geom="polygon",
               x = c(-1.347, -0.449, -0.449, -0.947, -0.947, -1.347), 
               y = c(     4,      4,    3.6,    3.6,  3.034,  3.034),
               color = "black",
               alpha = data$alpha[[10]],
               fill = c(data$color[[10]])) + geom_text(x = -1.147,y = 3.8,label = data$text[[10]],color = "black", size = 4.5)+
      annotate(geom="polygon",
               x = c( 0.449,  1.347,  1.347,  0.947, 0.947,  0.449), 
               y = c(     4,      4,  3.034,  3.034,   3.6,  3.6),
               color = "black",
               alpha = data$alpha[[12]],
               fill = c(data$color[[12]])) +  geom_text(x = 1.147 ,y = 3.8,label = data$text[[12]],color = "black", size = 4.5)+
      annotate(geom="polygon",
               x = c(-1.347, -0.449, -0.449, -0.947, -0.947, -1.347), 
               y = c(   1.1,    1.1,    1.5,    1.5,  2.067,  2.067),
               color = "black",
               alpha = data$alpha[[15]],
               fill = c(data$color[[15]])) +  geom_text(x = -1.147,y = 1.3,label = data$text[[15]],color = "black", size = 4.5)+
      annotate(geom="polygon",
               x = c( 0.449,  1.347,  1.347,  0.947, 0.947,  0.449), 
               y = c(   1.1,    1.1,  2.067,  2.067,   1.5,    1.5),
               color = "black",
               alpha = data$alpha[[17]],
               fill = c(data$color[[17]])) + geom_text(x = 1.147,y = 1.3,label = data$text[[17]],color = "black", size = 4.5)+
      annotate(geom="polygon",
               x = c(-2, 0, 0, -1.347, -1.347, -2), 
               y = c(     4.6,      4.6,    4,    4,  2.5,  2.5),
               color = "black",
               alpha = data$alpha[[18]],
               fill = c(data$color[[18]])) + geom_text(x = -1.674,y = 4.3,label = data$text[[18]],color = "black", size = 4.5)+
      annotate(geom="polygon",
               x = c(2, 0, 0, 1.347, 1.347, 2), 
               y = c(     4.6,      4.6,    4,    4,  2.5,  2.5),
               color = "black",
               alpha = data$alpha[[19]],
               fill = c(data$color[[19]])) + geom_text(x = 1.674,y = 4.3,label = data$text[[19]],color = "black", size = 4.5)+
      annotate(geom="polygon",
               x = c(-2, -1.347, -1.347, 0, 0, -2), 
               y = c(   2.5,    2.5,    1.1,    1.1,  0.5,  0.5),
               color = "black",
               alpha = data$alpha[[20]],
               fill = c(data$color[[20]])) +  geom_text(x = -1.674,y = 0.8,label = data$text[[20]],color = "black", size = 4.5)+
      annotate(geom="polygon",
               x = c(2, 1.347, 1.347, 0, 0, 2), 
               y = c(   2.5,    2.5,    1.1,    1.1,  0.5,  0.5),
               color = "black",
               alpha = data$alpha[[21]],
               fill = c(data$color[[21]])) +  geom_text(x = 1.674,y = 0.8,label = data$text[[21]],color = "black", size = 5)+
      
      coord_equal() +
      scale_x_continuous("",
                         limits = c(-2, 2)) +
      scale_y_continuous("",
                         limits = c(0.5, 4.6))+ labs(title = "Pitcher's POV")+
      theme(plot.title = element_text(hjust = 0.5))
    
    color_plot 
    
  })
  
  output$plot7 <- renderPlot({
    
    data <- f() %>% 
      group_by(zone) %>%
      summarise(N = n(),
                total = nrow(f()),
                swing = sum(as.numeric(swing), na.rm = T ),
                whiff = sum(as.numeric(whiff), na.rm = T )) %>% 
      mutate(swingper = swing/N) %>% 
      mutate(whiffper = whiff/swing) %>%
      mutate(pitchper = N/total) %>% 
      mutate(text = case_when(
        input$Options2 == "Swing" ~ ifelse(is.na(swingper)," ",paste0((round(swingper,digits = 2)*100),"%")),
        input$Options2 == "Whiff" ~ ifelse(is.na(whiffper)," ",paste0((round(whiffper,digits = 2)*100),"%")),
        input$Options2 == "Pitch" ~ ifelse(is.na(pitchper)," ",paste0((round(pitchper,digits = 2)*100),"%")))) %>% 
      mutate(color = case_when(
        input$Options2 == "Swing" ~ (ifelse(is.na(swingper),"gray",ifelse(swingper == 0.5, "white", ifelse(swingper > 0.5, "red","blue")))),
        input$Options2 == "Whiff" ~ (ifelse(is.na(whiffper),"gray",ifelse(whiffper == 0.5, "white", ifelse(whiffper > 0.5, "red","blue")))),
        input$Options2 == "Pitch" ~ (ifelse(is.na(pitchper),"gray",ifelse(pitchper == 0.075, "white", ifelse(pitchper > 0.075, "red","blue")))))) %>% 
      mutate(alpha = 0.4*case_when(
        input$Options2 == "Swing" ~ ifelse(is.na(swingper),0,ifelse(swingper == 0.5, 0.5, ifelse(swingper > 0.5, swingper, 1-swingper))),
        input$Options2 == "Whiff" ~ ifelse(is.na(whiffper),0,ifelse(whiffper == 0.5, 0.5, ifelse(whiffper > 0.5, whiffper, 1-whiffper))),
        input$Options2 == "Pitch" ~ ifelse(is.na(pitchper),0,ifelse(pitchper == 0.075, 0.5, ifelse(pitchper > 0.075, 10*pitchper, 10*pitchper))))) %>% 
      arrange(zone)
    
    
    
    if (nrow(data) != 0) {
      # If no data is available, display a custom error message
      #plot(0, 0, type = "n", xlim = c(0, 1), ylim = c(0, 1), xlab = "", ylab = "")
      #text(0.5, 0.5, "No data available for selected scenario.", cex = 1.2)
      all_zones <- data.frame(zone = c(1:9,11:14,16:19,21,23,27,29))
      
      existing_zones <- unique(data$zone)
      
      missing_zones <- setdiff(all_zones$zone, existing_zones)
      
      missing_rows <- data.frame(zone = missing_zones, 
                                 xwoba = rep(NA, length(missing_zones)), 
                                 alpha = rep(0, length(missing_zones)),
                                 color = rep("gray", length(missing_zones))
      )
      
      data <- bind_rows(data, missing_rows) %>% 
        arrange(zone)
    } 
    
    else{
      data <- data.frame (
        zone = c(1,2,3,4,5,6,7,8,9,11,12,13,14,16,17,18,19,21,23,27,29),
        xwoba = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
        color = c("gray","gray","gray","gray","gray","gray","gray","gray","gray","gray","gray","gray","gray","gray","gray","gray","gray","gray","gray","gray","gray"),
        alpha = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
      )
    }
    
    apple <- c(1,2,3)
    
    color_plot<- data %>%
      ggplot() +
      annotate(geom = "rect",
               xmin = -0.947,
               xmax = -0.316,
               ymin = 1.5,
               ymax = 2.2, 
               color = "black", 
               alpha = data$alpha[[7]],
               fill = c(data$color[[7]])) + geom_text(x = -0.6315,y = 1.85,label = (data$text[[7]][1]),color = "black", size = 5)+
      annotate(geom="rect",
               xmin = -0.316,
               xmax = 0.316,
               ymin = 1.5,
               ymax = 2.2, 
               color = "black", 
               alpha = data$alpha[[8]],
               fill = c(data$color[[8]])) + geom_text(x = 0,y = 1.85,label = data$text[[8]],color = "black", size = 5)+
      annotate(geom="rect",
               xmin = 0.316,
               xmax = 0.947,
               ymin = 1.5,
               ymax = 2.2, 
               color = "black", 
               alpha = data$alpha[[9]],
               fill = c(data$color[[9]])) + geom_text(x = 0.6315,y = 1.85,label = data$text[[9]],color = "black", size = 5)+
      annotate(geom="rect",
               xmin = -0.947,
               xmax = -0.316,
               ymin = 2.2,
               ymax = 2.9, 
               color = "black", 
               alpha = data$alpha[[4]],
               fill = c(data$color[[4]])) + geom_text(x = -0.6315,y = 2.55,label = data$text[[4]],color = "black", size = 5)+
      annotate(geom="rect",
               xmin = -0.316,
               xmax = 0.316,
               ymin = 2.2,
               ymax = 2.9,
               color = "black", 
               alpha = data$alpha[[5]],
               fill = c(data$color[[5]])) + geom_text(x = 0,y = 2.55,label = data$text[[5]],color = "black", size = 5)+
      annotate(geom="rect",
               xmin = 0.316,
               xmax = 0.947,
               ymin = 2.2,
               ymax = 2.9,
               color = "black", 
               alpha = data$alpha[[6]],
               fill = c(data$color[[6]])) + geom_text(x = 0.6315,y = 2.55,label = data$text[[6]],color = "black", size = 5)+
      annotate(geom="rect",
               xmin = -0.947,
               xmax = -0.316,
               ymin = 2.9,
               ymax = 3.6,
               color = "black", 
               alpha = data$alpha[[1]],
               fill = c(data$color[[1]])) +geom_text(x = -0.6315,y = 3.25,label = data$text[[1]],color = "black", size = 5)+
      annotate(geom="rect",
               xmin = -0.316,
               xmax = 0.316,
               ymin = 2.9,
               ymax = 3.6, 
               color = "black", 
               alpha = data$alpha[[2]],
               fill = c(data$color[[2]])) + geom_text(x = 0,y = 3.25,label = data$text[[2]],color = "black", size = 5)+
      annotate(geom="rect",
               xmin = 0.316,
               xmax = 0.947,
               ymin = 2.9,
               ymax = 3.6, 
               color = "black", 
               alpha = data$alpha[[3]],
               fill = c(data$color[[3]])) +geom_text(x = 0.6315,y = 3.25,label = data$text[[3]],color = "black", size = 5)+
      annotate(geom="rect",
               xmin = 0.947,
               xmax = 1.347,
               ymin = 2.067,
               ymax = 3.034,
               color = "black", 
               alpha = data$alpha[[14]],
               fill = c(data$color[[14]])) + geom_text(x = 1.147,y = 2.55,label = data$text[[14]],color = "black", size = 4.5)+
      annotate(geom="rect",
               xmin = -1.347,
               xmax = -0.947,
               ymin = 2.067,
               ymax = 3.034,
               color = "black", 
               alpha = data$alpha[[13]],
               fill = c(data$color[[13]])) + geom_text(x = -1.147,y = 2.55,label = data$text[[13]],color = "black", size = 4.5)+
      annotate(geom="rect",
               xmin = -0.449,
               xmax = 0.449,
               ymin = 3.6,
               ymax = 4, 
               color = "black", 
               alpha = data$alpha[[11]],
               fill = c(data$color[[11]])) + geom_text(x = 0 ,y = 3.8,label = data$text[[11]],color = "black", size = 4.5)+
      annotate(geom="rect",
               xmin = -0.449,
               xmax = 0.449,
               ymin = 1.1,
               ymax = 1.5, 
               color = "black", 
               alpha = data$alpha[[16]],
               fill = c(data$color[[16]])) + geom_text(x = 0,y = 1.3,label = data$text[[16]],color = "black", size = 4.5)+
      annotate(geom="polygon",
               x = c(-1.347, -0.449, -0.449, -0.947, -0.947, -1.347), 
               y = c(     4,      4,    3.6,    3.6,  3.034,  3.034),
               color = "black",
               alpha = data$alpha[[10]],
               fill = c(data$color[[10]])) + geom_text(x = -1.147,y = 3.8,label = data$text[[10]],color = "black", size = 4.5)+
      annotate(geom="polygon",
               x = c( 0.449,  1.347,  1.347,  0.947, 0.947,  0.449), 
               y = c(     4,      4,  3.034,  3.034,   3.6,  3.6),
               color = "black",
               alpha = data$alpha[[12]],
               fill = c(data$color[[12]])) +  geom_text(x = 1.147 ,y = 3.8,label = data$text[[12]],color = "black", size = 4.5)+
      annotate(geom="polygon",
               x = c(-1.347, -0.449, -0.449, -0.947, -0.947, -1.347), 
               y = c(   1.1,    1.1,    1.5,    1.5,  2.067,  2.067),
               color = "black",
               alpha = data$alpha[[15]],
               fill = c(data$color[[15]])) +  geom_text(x = -1.147,y = 1.3,label = data$text[[15]],color = "black", size = 4.5)+
      annotate(geom="polygon",
               x = c( 0.449,  1.347,  1.347,  0.947, 0.947,  0.449), 
               y = c(   1.1,    1.1,  2.067,  2.067,   1.5,    1.5),
               color = "black",
               alpha = data$alpha[[17]],
               fill = c(data$color[[17]])) + geom_text(x = 1.147,y = 1.3,label = data$text[[17]],color = "black", size = 4.5)+
      annotate(geom="polygon",
               x = c(-2, 0, 0, -1.347, -1.347, -2), 
               y = c(     4.6,      4.6,    4,    4,  2.5,  2.5),
               color = "black",
               alpha = data$alpha[[18]],
               fill = c(data$color[[18]])) + geom_text(x = -1.674,y = 4.3,label = data$text[[18]],color = "black", size = 4.5)+
      annotate(geom="polygon",
               x = c(2, 0, 0, 1.347, 1.347, 2), 
               y = c(     4.6,      4.6,    4,    4,  2.5,  2.5),
               color = "black",
               alpha = data$alpha[[19]],
               fill = c(data$color[[19]])) + geom_text(x = 1.674,y = 4.3,label = data$text[[19]],color = "black", size = 4.5)+
      annotate(geom="polygon",
               x = c(-2, -1.347, -1.347, 0, 0, -2), 
               y = c(   2.5,    2.5,    1.1,    1.1,  0.5,  0.5),
               color = "black",
               alpha = data$alpha[[20]],
               fill = c(data$color[[20]])) +  geom_text(x = -1.674,y = 0.8,label = data$text[[20]],color = "black", size = 4.5)+
      annotate(geom="polygon",
               x = c(2, 1.347, 1.347, 0, 0, 2), 
               y = c(   2.5,    2.5,    1.1,    1.1,  0.5,  0.5),
               color = "black",
               alpha = data$alpha[[21]],
               fill = c(data$color[[21]])) +  geom_text(x = 1.674,y = 0.8,label = data$text[[21]],color = "black", size = 5)+
      
      coord_equal() +
      scale_x_continuous("",
                         limits = c(-2, 2)) +
      scale_y_continuous("",
                         limits = c(0.5, 4.6))+
      labs(title = "Pitcher's POV")+
      theme(plot.title = element_text(hjust = 0.5))
    
    color_plot 
    
  })
  
  output$plot8 <- renderPlot({
    
    data <- f() %>% 
      group_by(zone) %>%
      summarise(PA = sum(PA),
                AB = sum(AB),
                hit = sum(hit),
                OB = sum(hit)+sum(BB)+sum(HBP),
                TB = sum(Single)+sum(Double)*2+sum(Triple)*3+sum(HomeRun)*4,
                sumxwoba = sum(xwoba),
                sumwoba = sum(woba)) %>% 
      mutate(AVG = hit/AB) %>% 
      mutate(OBP = OB/PA) %>% 
      mutate(SLG = TB/AB) %>% 
      mutate(OBP = ifelse(is.na(OBP) & !is.na(SLG),0,OBP)) %>% 
      mutate(SLG = ifelse(is.na(SLG) & !is.na(OBP),0,SLG)) %>% 
      mutate(OPS = OBP+SLG) %>% 
      mutate(woba = sumwoba/PA) %>% 
      mutate(xwoba = sumxwoba/PA) %>% 
      mutate(text = case_when(
        input$Options3 == "AVG" ~ ifelse(is.na(AVG)," ",sub("^0", "", sprintf("%.3f", AVG))),
        input$Options3 == "OPS" ~ ifelse(is.na(OPS)," ",sub("^0", "", sprintf("%.3f", OPS))),
        input$Options3 == "wOBA" ~ ifelse(is.na(woba)," ",sub("^0", "", sprintf("%.3f", woba))),
        input$Options3 == "xwOBA" ~ ifelse(is.na(xwoba)," ",sub("^0", "", sprintf("%.3f", xwoba))))) %>%  
      mutate(color = case_when(
        input$Options3 == "AVG" ~ (ifelse(is.na(AVG),"gray",ifelse(AVG == 0.27, "white", ifelse(AVG > 0.27, "red","blue")))),
        input$Options3 == "OPS" ~ (ifelse(is.na(OPS),"gray",ifelse(OPS == 0.7, "white", ifelse(OPS > 0.7, "red","blue")))),
        input$Options3 == "wOBA" ~ (ifelse(is.na(woba),"gray",ifelse(woba == 0.3, "white", ifelse(woba > 0.3, "red","blue")))),
        input$Options3 == "xwOBA" ~ (ifelse(is.na(xwoba),"gray",ifelse(xwoba == 0.3, "white", ifelse(xwoba > 0.3, "red","blue")))))) %>% 
      mutate(alpha = 0.4*case_when(
        input$Options3 == "AVG" ~ (ifelse(is.na(AVG),0,ifelse(AVG == 0.27, 0.27, ifelse(AVG > 0.27, AVG,0.5-AVG)))),
        input$Options3 == "OPS" ~ (ifelse(is.na(OPS),0,ifelse(OPS == 0.7, 0.7, ifelse(OPS > 0.7, OPS,0.7-OPS)))),
        input$Options3 == "wOBA" ~ (ifelse(is.na(woba),0,ifelse(woba == 0.3, 0.3, ifelse(woba > 0.3, woba, 1-woba)))),
        input$Options3 == "xwOBA" ~ (ifelse(is.na(xwoba),0,ifelse(xwoba == 0.3, 0.3, ifelse(xwoba > 0.3, xwoba,1-xwoba)))))) %>% 
      arrange(zone)
    
    
    
    if (nrow(data) != 0) {
      # If no data is available, display a custom error message
      #plot(0, 0, type = "n", xlim = c(0, 1), ylim = c(0, 1), xlab = "", ylab = "")
      #text(0.5, 0.5, "No data available for selected scenario.", cex = 1.2)
      all_zones <- data.frame(zone = c(1:9,11:14,16:19,21,23,27,29))
      
      existing_zones <- unique(data$zone)
      
      missing_zones <- setdiff(all_zones$zone, existing_zones)
      
      missing_rows <- data.frame(zone = missing_zones,  
                                 alpha = rep(0, length(missing_zones)),
                                 color = rep("gray", length(missing_zones))
      )
      
      data <- bind_rows(data, missing_rows) %>% 
        arrange(zone)
    } 
    
    else{
      data <- data.frame (
        zone = c(1,2,3,4,5,6,7,8,9,11,12,13,14,16,17,18,19,21,23,27,29),
        xwoba = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
        color = c("gray","gray","gray","gray","gray","gray","gray","gray","gray","gray","gray","gray","gray","gray","gray","gray","gray","gray","gray","gray","gray"),
        alpha = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
      )
    }
    
    apple <- c(1,2,3)
    
    color_plot<- data %>%
      ggplot() +
      annotate(geom = "rect",
               xmin = -0.947,
               xmax = -0.316,
               ymin = 1.5,
               ymax = 2.2, 
               color = "black", 
               alpha = data$alpha[[7]],
               fill = c(data$color[[7]])) + geom_text(x = -0.6315,y = 1.85,label = (data$text[[7]][1]),color = "black", size = 5)+
      annotate(geom="rect",
               xmin = -0.316,
               xmax = 0.316,
               ymin = 1.5,
               ymax = 2.2, 
               color = "black", 
               alpha = data$alpha[[8]],
               fill = c(data$color[[8]])) + geom_text(x = 0,y = 1.85,label = data$text[[8]],color = "black", size = 5)+
      annotate(geom="rect",
               xmin = 0.316,
               xmax = 0.947,
               ymin = 1.5,
               ymax = 2.2, 
               color = "black", 
               alpha = data$alpha[[9]],
               fill = c(data$color[[9]])) + geom_text(x = 0.6315,y = 1.85,label = data$text[[9]],color = "black", size = 5)+
      annotate(geom="rect",
               xmin = -0.947,
               xmax = -0.316,
               ymin = 2.2,
               ymax = 2.9, 
               color = "black", 
               alpha = data$alpha[[4]],
               fill = c(data$color[[4]])) + geom_text(x = -0.6315,y = 2.55,label = data$text[[4]],color = "black", size = 5)+
      annotate(geom="rect",
               xmin = -0.316,
               xmax = 0.316,
               ymin = 2.2,
               ymax = 2.9,
               color = "black", 
               alpha = data$alpha[[5]],
               fill = c(data$color[[5]])) + geom_text(x = 0,y = 2.55,label = data$text[[5]],color = "black", size = 5)+
      annotate(geom="rect",
               xmin = 0.316,
               xmax = 0.947,
               ymin = 2.2,
               ymax = 2.9,
               color = "black", 
               alpha = data$alpha[[6]],
               fill = c(data$color[[6]])) + geom_text(x = 0.6315,y = 2.55,label = data$text[[6]],color = "black", size = 5)+
      annotate(geom="rect",
               xmin = -0.947,
               xmax = -0.316,
               ymin = 2.9,
               ymax = 3.6,
               color = "black", 
               alpha = data$alpha[[1]],
               fill = c(data$color[[1]])) +geom_text(x = -0.6315,y = 3.25,label = data$text[[1]],color = "black", size = 5)+
      annotate(geom="rect",
               xmin = -0.316,
               xmax = 0.316,
               ymin = 2.9,
               ymax = 3.6, 
               color = "black", 
               alpha = data$alpha[[2]],
               fill = c(data$color[[2]])) + geom_text(x = 0,y = 3.25,label = data$text[[2]],color = "black", size = 5)+
      annotate(geom="rect",
               xmin = 0.316,
               xmax = 0.947,
               ymin = 2.9,
               ymax = 3.6, 
               color = "black", 
               alpha = data$alpha[[3]],
               fill = c(data$color[[3]])) +geom_text(x = 0.6315,y = 3.25,label = data$text[[3]],color = "black", size = 5)+
      annotate(geom="rect",
               xmin = 0.947,
               xmax = 1.347,
               ymin = 2.067,
               ymax = 3.034,
               color = "black", 
               alpha = data$alpha[[14]],
               fill = c(data$color[[14]])) + geom_text(x = 1.147,y = 2.55,label = data$text[[14]],color = "black", size = 4.5)+
      annotate(geom="rect",
               xmin = -1.347,
               xmax = -0.947,
               ymin = 2.067,
               ymax = 3.034,
               color = "black", 
               alpha = data$alpha[[13]],
               fill = c(data$color[[13]])) + geom_text(x = -1.147,y = 2.55,label = data$text[[13]],color = "black", size = 4.5)+
      annotate(geom="rect",
               xmin = -0.449,
               xmax = 0.449,
               ymin = 3.6,
               ymax = 4, 
               color = "black", 
               alpha = data$alpha[[11]],
               fill = c(data$color[[11]])) + geom_text(x = 0 ,y = 3.8,label = data$text[[11]],color = "black", size = 4.5)+
      annotate(geom="rect",
               xmin = -0.449,
               xmax = 0.449,
               ymin = 1.1,
               ymax = 1.5, 
               color = "black", 
               alpha = data$alpha[[16]],
               fill = c(data$color[[16]])) + geom_text(x = 0,y = 1.3,label = data$text[[16]],color = "black", size = 4.5)+
      annotate(geom="polygon",
               x = c(-1.347, -0.449, -0.449, -0.947, -0.947, -1.347), 
               y = c(     4,      4,    3.6,    3.6,  3.034,  3.034),
               color = "black",
               alpha = data$alpha[[10]],
               fill = c(data$color[[10]])) + geom_text(x = -1.147,y = 3.8,label = data$text[[10]],color = "black", size = 4.5)+
      annotate(geom="polygon",
               x = c( 0.449,  1.347,  1.347,  0.947, 0.947,  0.449), 
               y = c(     4,      4,  3.034,  3.034,   3.6,  3.6),
               color = "black",
               alpha = data$alpha[[12]],
               fill = c(data$color[[12]])) +  geom_text(x = 1.147 ,y = 3.8,label = data$text[[12]],color = "black", size = 4.5)+
      annotate(geom="polygon",
               x = c(-1.347, -0.449, -0.449, -0.947, -0.947, -1.347), 
               y = c(   1.1,    1.1,    1.5,    1.5,  2.067,  2.067),
               color = "black",
               alpha = data$alpha[[15]],
               fill = c(data$color[[15]])) +  geom_text(x = -1.147,y = 1.3,label = data$text[[15]],color = "black", size = 4.5)+
      annotate(geom="polygon",
               x = c( 0.449,  1.347,  1.347,  0.947, 0.947,  0.449), 
               y = c(   1.1,    1.1,  2.067,  2.067,   1.5,    1.5),
               color = "black",
               alpha = data$alpha[[17]],
               fill = c(data$color[[17]])) + geom_text(x = 1.147,y = 1.3,label = data$text[[17]],color = "black", size = 4.5)+
      annotate(geom="polygon",
               x = c(-2, 0, 0, -1.347, -1.347, -2), 
               y = c(     4.6,      4.6,    4,    4,  2.5,  2.5),
               color = "black",
               alpha = data$alpha[[18]],
               fill = c(data$color[[18]])) + geom_text(x = -1.674,y = 4.3,label = data$text[[18]],color = "black", size = 4.5)+
      annotate(geom="polygon",
               x = c(2, 0, 0, 1.347, 1.347, 2), 
               y = c(     4.6,      4.6,    4,    4,  2.5,  2.5),
               color = "black",
               alpha = data$alpha[[19]],
               fill = c(data$color[[19]])) + geom_text(x = 1.674,y = 4.3,label = data$text[[19]],color = "black", size = 4.5)+
      annotate(geom="polygon",
               x = c(-2, -1.347, -1.347, 0, 0, -2), 
               y = c(   2.5,    2.5,    1.1,    1.1,  0.5,  0.5),
               color = "black",
               alpha = data$alpha[[20]],
               fill = c(data$color[[20]])) +  geom_text(x = -1.674,y = 0.8,label = data$text[[20]],color = "black", size = 4.5)+
      annotate(geom="polygon",
               x = c(2, 1.347, 1.347, 0, 0, 2), 
               y = c(   2.5,    2.5,    1.1,    1.1,  0.5,  0.5),
               color = "black",
               alpha = data$alpha[[21]],
               fill = c(data$color[[21]])) +  geom_text(x = 1.674,y = 0.8,label = data$text[[21]],color = "black", size = 5)+
      
      coord_equal() +
      scale_x_continuous("",
                         limits = c(-2, 2)) +
      scale_y_continuous("",
                         limits = c(0.5, 4.6))+
      labs(title = "Pitcher's POV")+
      theme(plot.title = element_text(hjust = 0.5))
    
    color_plot 
    
  })
  
  output$plot9 <- renderPlot({
    
    data <- f() %>% 
      group_by(zone) %>%
      summarise(PA = sum(PA),
                AB = sum(AB),
                hit = sum(hit),
                OB = sum(hit)+sum(BB)+sum(HBP),
                TB = sum(Single)+sum(Double)*2+sum(Triple)*3+sum(HomeRun)*4,
                sumxwoba = sum(xwoba),
                sumwoba = sum(woba)) %>% 
      mutate(AVG = hit/AB) %>% 
      mutate(OBP = OB/PA) %>% 
      mutate(SLG = TB/AB) %>% 
      mutate(OBP = ifelse(is.na(OBP) & !is.na(SLG),0,OBP)) %>% 
      mutate(SLG = ifelse(is.na(SLG) & !is.na(OBP),0,SLG)) %>% 
      mutate(OPS = OBP+SLG) %>% 
      mutate(woba = sumwoba/PA) %>% 
      mutate(xwoba = sumxwoba/PA) %>% 
      mutate(text = case_when(
        input$Options4 == "AVG" ~ ifelse(is.na(AVG)," ",sub("^0", "", sprintf("%.3f", AVG))),
        input$Options4 == "OPS" ~ ifelse(is.na(OPS)," ",sub("^0", "", sprintf("%.3f", OPS))),
        input$Options4 == "wOBA" ~ ifelse(is.na(woba)," ",sub("^0", "", sprintf("%.3f", woba))),
        input$Options4 == "xwOBA" ~ ifelse(is.na(xwoba)," ",sub("^0", "", sprintf("%.3f", xwoba))))) %>% 
      mutate(color = case_when(
        input$Options4 == "AVG" ~ (ifelse(is.na(AVG),"gray",ifelse(AVG == 0.27, "white", ifelse(AVG > 0.27, "red","blue")))),
        input$Options4 == "OPS" ~ (ifelse(is.na(OPS),"gray",ifelse(OPS == 0.7, "white", ifelse(OPS > 0.7, "red","blue")))),
        input$Options4 == "wOBA" ~ (ifelse(is.na(woba),"gray",ifelse(woba == 0.3, "white", ifelse(woba > 0.3, "red","blue")))),
        input$Options4 == "xwOBA" ~ (ifelse(is.na(xwoba),"gray",ifelse(xwoba == 0.3, "white", ifelse(xwoba > 0.3, "red","blue")))))) %>% 
      mutate(alpha = 0.4*case_when(
        input$Options4 == "AVG" ~ (ifelse(is.na(AVG),0,ifelse(AVG == 0.27, 0.27, ifelse(AVG > 0.27, AVG,0.5-AVG)))),
        input$Options4 == "OPS" ~ (ifelse(is.na(OPS),0,ifelse(OPS == 0.7, 0.7, ifelse(OPS > 0.7, OPS,0.7-OPS)))),
        input$Options4 == "wOBA" ~ (ifelse(is.na(woba),0,ifelse(woba == 0.3, 0.3, ifelse(woba > 0.3, woba, 1-woba)))),
        input$Options4 == "xwOBA" ~ (ifelse(is.na(xwoba),0,ifelse(xwoba == 0.3, 0.3, ifelse(xwoba > 0.3, xwoba,1-xwoba)))))) %>% 
      arrange(zone)
    
    
    
    if (nrow(data) != 0) {
      # If no data is available, display a custom error message
      #plot(0, 0, type = "n", xlim = c(0, 1), ylim = c(0, 1), xlab = "", ylab = "")
      #text(0.5, 0.5, "No data available for selected scenario.", cex = 1.2)
      all_zones <- data.frame(zone = c(1:9,11:14,16:19,21,23,27,29))
      
      existing_zones <- unique(data$zone)
      
      missing_zones <- setdiff(all_zones$zone, existing_zones)
      
      missing_rows <- data.frame(zone = missing_zones,  
                                 alpha = rep(0, length(missing_zones)),
                                 color = rep("gray", length(missing_zones))
      )
      
      data <- bind_rows(data, missing_rows) %>% 
        arrange(zone)
    } 
    
    else{
      data <- data.frame (
        zone = c(1,2,3,4,5,6,7,8,9,11,12,13,14,16,17,18,19,21,23,27,29),
        xwoba = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
        color = c("gray","gray","gray","gray","gray","gray","gray","gray","gray","gray","gray","gray","gray","gray","gray","gray","gray","gray","gray","gray","gray"),
        alpha = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
      )
    }
    
    apple <- c(1,2,3)
    
    color_plot<- data %>%
      ggplot() +
      annotate(geom = "rect",
               xmin = -0.947,
               xmax = -0.316,
               ymin = 1.5,
               ymax = 2.2, 
               color = "black", 
               alpha = data$alpha[[7]],
               fill = c(data$color[[7]])) + geom_text(x = -0.6315,y = 1.85,label = (data$text[[7]][1]),color = "black", size = 5)+
      annotate(geom="rect",
               xmin = -0.316,
               xmax = 0.316,
               ymin = 1.5,
               ymax = 2.2, 
               color = "black", 
               alpha = data$alpha[[8]],
               fill = c(data$color[[8]])) + geom_text(x = 0,y = 1.85,label = data$text[[8]],color = "black", size = 5)+
      annotate(geom="rect",
               xmin = 0.316,
               xmax = 0.947,
               ymin = 1.5,
               ymax = 2.2, 
               color = "black", 
               alpha = data$alpha[[9]],
               fill = c(data$color[[9]])) + geom_text(x = 0.6315,y = 1.85,label = data$text[[9]],color = "black", size = 5)+
      annotate(geom="rect",
               xmin = -0.947,
               xmax = -0.316,
               ymin = 2.2,
               ymax = 2.9, 
               color = "black", 
               alpha = data$alpha[[4]],
               fill = c(data$color[[4]])) + geom_text(x = -0.6315,y = 2.55,label = data$text[[4]],color = "black", size = 5)+
      annotate(geom="rect",
               xmin = -0.316,
               xmax = 0.316,
               ymin = 2.2,
               ymax = 2.9,
               color = "black", 
               alpha = data$alpha[[5]],
               fill = c(data$color[[5]])) + geom_text(x = 0,y = 2.55,label = data$text[[5]],color = "black", size = 5)+
      annotate(geom="rect",
               xmin = 0.316,
               xmax = 0.947,
               ymin = 2.2,
               ymax = 2.9,
               color = "black", 
               alpha = data$alpha[[6]],
               fill = c(data$color[[6]])) + geom_text(x = 0.6315,y = 2.55,label = data$text[[6]],color = "black", size = 5)+
      annotate(geom="rect",
               xmin = -0.947,
               xmax = -0.316,
               ymin = 2.9,
               ymax = 3.6,
               color = "black", 
               alpha = data$alpha[[1]],
               fill = c(data$color[[1]])) +geom_text(x = -0.6315,y = 3.25,label = data$text[[1]],color = "black", size = 5)+
      annotate(geom="rect",
               xmin = -0.316,
               xmax = 0.316,
               ymin = 2.9,
               ymax = 3.6, 
               color = "black", 
               alpha = data$alpha[[2]],
               fill = c(data$color[[2]])) + geom_text(x = 0,y = 3.25,label = data$text[[2]],color = "black", size = 5)+
      annotate(geom="rect",
               xmin = 0.316,
               xmax = 0.947,
               ymin = 2.9,
               ymax = 3.6, 
               color = "black", 
               alpha = data$alpha[[3]],
               fill = c(data$color[[3]])) +geom_text(x = 0.6315,y = 3.25,label = data$text[[3]],color = "black", size = 5)+
      annotate(geom="rect",
               xmin = 0.947,
               xmax = 1.347,
               ymin = 2.067,
               ymax = 3.034,
               color = "black", 
               alpha = data$alpha[[14]],
               fill = c(data$color[[14]])) + geom_text(x = 1.147,y = 2.55,label = data$text[[14]],color = "black", size = 4.5)+
      annotate(geom="rect",
               xmin = -1.347,
               xmax = -0.947,
               ymin = 2.067,
               ymax = 3.034,
               color = "black", 
               alpha = data$alpha[[13]],
               fill = c(data$color[[13]])) + geom_text(x = -1.147,y = 2.55,label = data$text[[13]],color = "black", size = 4.5)+
      annotate(geom="rect",
               xmin = -0.449,
               xmax = 0.449,
               ymin = 3.6,
               ymax = 4, 
               color = "black", 
               alpha = data$alpha[[11]],
               fill = c(data$color[[11]])) + geom_text(x = 0 ,y = 3.8,label = data$text[[11]],color = "black", size = 4.5)+
      annotate(geom="rect",
               xmin = -0.449,
               xmax = 0.449,
               ymin = 1.1,
               ymax = 1.5, 
               color = "black", 
               alpha = data$alpha[[16]],
               fill = c(data$color[[16]])) + geom_text(x = 0,y = 1.3,label = data$text[[16]],color = "black", size = 4.5)+
      annotate(geom="polygon",
               x = c(-1.347, -0.449, -0.449, -0.947, -0.947, -1.347), 
               y = c(     4,      4,    3.6,    3.6,  3.034,  3.034),
               color = "black",
               alpha = data$alpha[[10]],
               fill = c(data$color[[10]])) + geom_text(x = -1.147,y = 3.8,label = data$text[[10]],color = "black", size = 4.5)+
      annotate(geom="polygon",
               x = c( 0.449,  1.347,  1.347,  0.947, 0.947,  0.449), 
               y = c(     4,      4,  3.034,  3.034,   3.6,  3.6),
               color = "black",
               alpha = data$alpha[[12]],
               fill = c(data$color[[12]])) +  geom_text(x = 1.147 ,y = 3.8,label = data$text[[12]],color = "black", size = 4.5)+
      annotate(geom="polygon",
               x = c(-1.347, -0.449, -0.449, -0.947, -0.947, -1.347), 
               y = c(   1.1,    1.1,    1.5,    1.5,  2.067,  2.067),
               color = "black",
               alpha = data$alpha[[15]],
               fill = c(data$color[[15]])) +  geom_text(x = -1.147,y = 1.3,label = data$text[[15]],color = "black", size = 4.5)+
      annotate(geom="polygon",
               x = c( 0.449,  1.347,  1.347,  0.947, 0.947,  0.449), 
               y = c(   1.1,    1.1,  2.067,  2.067,   1.5,    1.5),
               color = "black",
               alpha = data$alpha[[17]],
               fill = c(data$color[[17]])) + geom_text(x = 1.147,y = 1.3,label = data$text[[17]],color = "black", size = 4.5)+
      annotate(geom="polygon",
               x = c(-2, 0, 0, -1.347, -1.347, -2), 
               y = c(     4.6,      4.6,    4,    4,  2.5,  2.5),
               color = "black",
               alpha = data$alpha[[18]],
               fill = c(data$color[[18]])) + geom_text(x = -1.674,y = 4.3,label = data$text[[18]],color = "black", size = 4.5)+
      annotate(geom="polygon",
               x = c(2, 0, 0, 1.347, 1.347, 2), 
               y = c(     4.6,      4.6,    4,    4,  2.5,  2.5),
               color = "black",
               alpha = data$alpha[[19]],
               fill = c(data$color[[19]])) + geom_text(x = 1.674,y = 4.3,label = data$text[[19]],color = "black", size = 4.5)+
      annotate(geom="polygon",
               x = c(-2, -1.347, -1.347, 0, 0, -2), 
               y = c(   2.5,    2.5,    1.1,    1.1,  0.5,  0.5),
               color = "black",
               alpha = data$alpha[[20]],
               fill = c(data$color[[20]])) +  geom_text(x = -1.674,y = 0.8,label = data$text[[20]],color = "black", size = 4.5)+
      annotate(geom="polygon",
               x = c(2, 1.347, 1.347, 0, 0, 2), 
               y = c(   2.5,    2.5,    1.1,    1.1,  0.5,  0.5),
               color = "black",
               alpha = data$alpha[[21]],
               fill = c(data$color[[21]])) +  geom_text(x = 1.674,y = 0.8,label = data$text[[21]],color = "black", size = 5)+
      
      coord_equal() +
      scale_x_continuous("",
                         limits = c(-2, 2)) +
      scale_y_continuous("",
                         limits = c(0.5, 4.6))+
      labs(title = "Pitcher's POV")+
      theme(plot.title = element_text(hjust = 0.5))
    
    color_plot 
    
  })
  
  output$plot10 <- renderPlot({
  
    data_plot <- read_csv("spray.csv")
    
    data <- f() %>%
      filter(is.na(landingX) == F & is.na(landingY) == F &PitchCall == "InPlay") 
    
    scale_fill_chris2 <- function(...){
      ggplot2:::manual_scale(
        'fill', 
        values = setNames(c("dodgerblue","lightsalmon4","white","lightsalmon"), c("FlyBall","GroundBall","LineDrive","Popup")), 
        ...
      )
    }
    
    ggplot(data_plot %>% filter(cut_poly == "a")) +
      geom_polygon(aes(x, y),fill = "white",color = "black", alpha =1)+
      geom_polygon(data = (data_plot %>% filter(cut_poly == "b")),aes(x, y),fill = "white",color = "black", alpha =1)+
      geom_polygon(data = (data_plot %>% filter(cut_poly == "c")),aes(x, y),fill = "white",color = "black", alpha =1)+
      geom_polygon(data = (data_plot %>% filter(cut_poly == "d")),aes(x, y),fill = "white",color = "black", alpha =1)+
      geom_polygon(data = (data_plot %>% filter(cut_poly == "e")),aes(x, y),fill = "white",color = "black", alpha =1)+
      coord_fixed()+ geom_point(data = data %>% filter(Result %in% input$result2) %>% filter(Plate %in% input$plate), aes(x = landingX, y = landingY,fill = AutoHitType),size = 3,color = "black", ,shape = 21)+ scale_fill_chris2()+
      labs(fill = "Batted-ball types")
    
    
    
  })
  
  output$plot11 <- renderPlot({
    
    data_plot <- read_csv("spray.csv")
    
    data <- f() %>%
      filter(is.na(landingX) == F & is.na(landingY) == F &PitchCall == "InPlay") %>% filter(AutoHitType %in% input$result3) %>% filter(Plate %in% input$plate2)
    
    color_data <- data %>% 
      group_by(landingZone) %>% 
      summarise(Percentage = n()/nrow(data)) %>% 
      mutate(color = ifelse(Percentage >= 0.2, "red","blue")) %>% 
      mutate(alpha = ifelse(Percentage >= 0.2, Percentage*1.5, 2*(0.2-Percentage))) %>% 
      mutate(text = paste0(round(Percentage, digits = 2)*100,"%"))
    
    ggplot(data_plot %>% filter(cut_poly == "a")) +
      geom_polygon(aes(x, y),fill = ifelse(length(color_data[color_data$landingZone == 1,]$Percentage) == 1, color_data[color_data$landingZone == 1,]$color, "white"),color = "black", alpha =(ifelse(length(color_data[color_data$landingZone == 1,]$Percentage) == 1, color_data[color_data$landingZone == 1,]$alpha, 0)))+
      geom_text(x = -150,y = 225,label = ifelse(length(color_data[color_data$landingZone == 1,]$Percentage) == 1, color_data[color_data$landingZone == 1,]$text, ""),color = "black", size = 8)+
      
      geom_polygon(data = (data_plot %>% filter(cut_poly == "b")),aes(x, y),fill = ifelse(length(color_data[color_data$landingZone == 2,]$Percentage) == 1, color_data[color_data$landingZone == 2,]$color, "white"),color = "black", alpha =(ifelse(length(color_data[color_data$landingZone == 2,]$Percentage) == 1, color_data[color_data$landingZone == 2,]$alpha, 0)))+
      geom_text(x = -90,y = 300,label = ifelse(length(color_data[color_data$landingZone == 2,]$Percentage) == 1, color_data[color_data$landingZone == 2,]$text, ""),color = "black", size = 8)+
      
      geom_polygon(data = (data_plot %>% filter(cut_poly == "c")),aes(x, y),fill = ifelse(length(color_data[color_data$landingZone == 3,]$Percentage) == 1, color_data[color_data$landingZone == 3,]$color, "white"),color = "black", alpha =(ifelse(length(color_data[color_data$landingZone == 3,]$Percentage) == 1, color_data[color_data$landingZone == 3,]$alpha, 0)))+
      geom_text(x = 0,y = 350,label = ifelse(length(color_data[color_data$landingZone == 3,]$Percentage) == 1, color_data[color_data$landingZone == 3,]$text, ""),color = "black", size = 8)+
      
      geom_polygon(data = (data_plot %>% filter(cut_poly == "d")),aes(x, y),fill = ifelse(length(color_data[color_data$landingZone == 4,]$Percentage) == 1, color_data[color_data$landingZone == 4,]$color, "white"),color = "black", alpha =(ifelse(length(color_data[color_data$landingZone == 4,]$Percentage) == 1, color_data[color_data$landingZone == 4,]$alpha, 0)))+
      geom_text(x = 90,y = 300,label = ifelse(length(color_data[color_data$landingZone == 4,]$Percentage) == 1, color_data[color_data$landingZone == 4,]$text, ""),color = "black", size = 8)+
      
      geom_polygon(data = (data_plot %>% filter(cut_poly == "e")),aes(x, y),fill = ifelse(length(color_data[color_data$landingZone == 5,]$Percentage) == 1, color_data[color_data$landingZone == 5,]$color, "white"),color = "black", alpha =(ifelse(length(color_data[color_data$landingZone == 5,]$Percentage) == 1, color_data[color_data$landingZone == 5,]$alpha, 0)))+
      geom_text(x = 150,y = 225,label = ifelse(length(color_data[color_data$landingZone == 5,]$Percentage) == 1, color_data[color_data$landingZone == 5,]$text, ""),color = "black", size = 8)+
      
      coord_fixed()
    
    
    
  })
  
  output$plot12 <- renderPlot({
    
    all <- ump_data %>% 
      filter(Umpire == input$ump_name)
    
    u_data <- all %>% 
      filter(PitchCall == "StrikeCalled" | PitchCall == "BallCalled") %>% 
      filter(BatterSide == "Left") %>% 
      filter(AutoPitchType == "Fastball" | AutoPitchType == "Cutter" | AutoPitchType == "TwoSeamFastBall") 
    
    strike_mod <- gam(PitchCall == "StrikeCalled" ~ s(PlateLocSide, PlateLocHeight),
                      family = binomial, data = u_data)
    
    hats <- strike_mod %>%
      augment(type.predict = "response")
    
    
    grid <- u_data %>%
      data_grid(PlateLocSide = seq_range(c(-2.347,2.347), n = 100),
                PlateLocHeight = seq_range(c(0.1,5), n = 100))
    
    grid_hats <- strike_mod %>%
      augment(type.predict = "response", newdata = grid)
    
    tile_plot1 <- color_plot %+% grid_hats +
      geom_tile(aes(x = PlateLocSide, y = PlateLocHeight,fill = .fitted, show.legend = FALSE), alpha = 0.8) +
      scale_fill_gradientn(colors = c("blue","green","yellow","orange","red"))+ theme(plot.title = element_text(hjust = 0.5))+
      labs(fill = "Strike probability",title = "Pitcher's POV")
    
    tile_plot1
    
  })
  
  output$plot13 <- renderPlot({
    
    all <- ump_data %>% 
      filter(Umpire == input$ump_name)
    
    u_data <- all %>% 
      filter(PitchCall == "StrikeCalled" | PitchCall == "BallCalled") %>% 
      filter(BatterSide == "Left") %>% 
      filter(AutoPitchType != "Fastball" & AutoPitchType != "Cutter" & AutoPitchType != "TwoSeamFastBall")
    
    strike_mod <- gam(PitchCall == "StrikeCalled" ~ s(PlateLocSide, PlateLocHeight),
                      family = binomial, data = u_data)
    
    hats <- strike_mod %>%
      augment(type.predict = "response")
    
    
    grid <- u_data %>%
      data_grid(PlateLocSide = seq_range(c(-2.347,2.347), n = 100),
                PlateLocHeight = seq_range(c(0.1,5), n = 100))
    
    grid_hats <- strike_mod %>%
      augment(type.predict = "response", newdata = grid)
    
    tile_plot1 <- color_plot %+% grid_hats +
      geom_tile(aes(x = PlateLocSide, y = PlateLocHeight,fill = .fitted, show.legend = FALSE), alpha = 0.8) +
      scale_fill_gradientn(colors = c("blue","green","yellow","orange","red"))+ theme(plot.title = element_text(hjust = 0.5))+
      labs(fill = "Strike probability",title = "Pitcher's POV")
      
    
    tile_plot1
    
  })
  
  output$plot14 <- renderPlot({
    
    all <- ump_data %>% 
      filter(Umpire == input$ump_name)
    
    u_data <- all %>% 
      filter(PitchCall == "StrikeCalled" | PitchCall == "BallCalled") %>% 
      filter(BatterSide != "Left") %>% 
      filter(AutoPitchType == "Fastball" | AutoPitchType == "Cutter" | AutoPitchType == "TwoSeamFastBall")
    
    strike_mod <- gam(PitchCall == "StrikeCalled" ~ s(PlateLocSide, PlateLocHeight),
                      family = binomial, data = u_data)
    
    hats <- strike_mod %>%
      augment(type.predict = "response")
    
    
    grid <- u_data %>%
      data_grid(PlateLocSide = seq_range(c(-2.347,2.347), n = 100),
                PlateLocHeight = seq_range(c(0.1,5), n = 100))
    
    grid_hats <- strike_mod %>%
      augment(type.predict = "response", newdata = grid)
    
    tile_plot1 <- color_plot %+% grid_hats +
      geom_tile(aes(x = PlateLocSide, y = PlateLocHeight,fill = .fitted, show.legend = FALSE), alpha = 0.8) +
      scale_fill_gradientn(colors = c("blue","green","yellow","orange","red"))+ theme(plot.title = element_text(hjust = 0.5))+
      labs(fill = "Strike probability",title = "Pitcher's POV")
    
    tile_plot1
    
  })
  
  output$plot15 <- renderPlot({
    
    all <- ump_data %>% 
      filter(Umpire == input$ump_name)
    
    u_data <- all %>% 
      filter(PitchCall == "StrikeCalled" | PitchCall == "BallCalled") %>% 
      filter(BatterSide != "Left") %>% 
      filter(AutoPitchType != "Fastball" & AutoPitchType != "Cutter" & AutoPitchType != "TwoSeamFastBall")
    
    strike_mod <- gam(PitchCall == "StrikeCalled" ~ s(PlateLocSide, PlateLocHeight),
                      family = binomial, data = u_data)
    
    hats <- strike_mod %>%
      augment(type.predict = "response")
    
    
    grid <- u_data %>%
      data_grid(PlateLocSide = seq_range(c(-2.347,2.347), n = 100),
                PlateLocHeight = seq_range(c(0.1,5), n = 100))
    
    grid_hats <- strike_mod %>%
      augment(type.predict = "response", newdata = grid)
    
    tile_plot1 <- color_plot %+% grid_hats +
      geom_tile(aes(x = PlateLocSide, y = PlateLocHeight,fill = .fitted, show.legend = FALSE), alpha = 0.8) +
      scale_fill_gradientn(colors = c("blue","green","yellow","orange","red"))+ theme(plot.title = element_text(hjust = 0.5))+
      labs(fill = "Strike probability",title = "Pitcher's POV")
  
    
    tile_plot1
    
  })
}





# Run the application 
shinyApp(ui = ui, server = server)
