### shinydashboard WHO CDCC Deaths 2020: main dashboard
# author: remy
# date: 13/01/2021


# load libraries
library(tidyverse) # data manipulation
library(shiny) # shiny apps, reactives, inputs, outputs
library(shinydashboardPlus) # fancy boxes
library(shinydashboard) # dashboard package
library(ggplot2) # data viz
library(plotly) # data viz, interactive plots
library(ggnewscale) # data viz
library(scales) # plot scales
library(DT) # interactive datatables
library(maptools) # wrld_simpl shapefile for the 
library(leaflet) # leaflet interactive map
library(RColorBrewer) # color palettes for the map
library(wesanderson) # color palettes for plots

# import objects & functions
source("who_objects.r")

# create color palette for death probability for leaflet map
cdcc_pal <- colorNumeric("RdYlGn", domain = wrld_simpl@data$percent, reverse = TRUE, na.color = "#939597")

# basic dashboard
################## header
header <- dashboardHeader(title = "WHO CDCC Deaths")

################# sidebar
sidebar <- dashboardSidebar(
  sidebarMenu(id = "tabs",
    # summary tab
    menuItem(
      text = "Summary",
      tabName = "summary",
      icon = icon("user-md")
    ),
    # input tab
    menuItem(
      text = "Inputs",
      tabName = "inputs",
      icon = icon("file-medical-alt")
    ),
    # plot tab
    menuItem(
      text = "Plots",
      tabName = "plots",
      icon = icon("chart-line")
    ),
    # maps tab
    menuItem(
      text = "About creator",
      tabName = "creator",
      icon = icon("user")
    ),
    # select country input
    selectInput(
      inputId =  "location",
      label = "Select Country",
      choices = unique(cdcc_deathprob$location),
      selected = "Switzerland"
    ),
    # compare countries checkbox
    checkboxInput("compare_countries", "Compare to another country?"),
    conditionalPanel(
      condition = "input.compare_countries == true",
      # select second country input
      selectInput(
        inputId =  "location_2",
        label = "Select Second Country",
        choices = unique(cdcc_deathprob$location),
        selected = "United States"
      )
    ),
    # select year input
    selectInput(
      inputId = "year", 
      label = "Select Year",
      choices = unique(cdcc_deathprob$year),
      selected = 2016
      ),
    # kaggle link output
    uiOutput("kaggle_link", align = "right"),
    # email
    h5("remy.data@gmx.ch", align = "right")
  )
)

################## body
body <- dashboardBody(
  tabItems(
    ## summary tab
    tabItem(
      tabName = "summary",
      # row 1
      # description and refs
      tabBox(
        width = 6,
        title = "Description",
        tabPanel("Info", htmlOutput("info_text")),
        tabPanel("Refs", uiOutput("refs"))
      ),
      fluidRow(
        column(
          width = 6,
          # dynamic mean probability infobox output shown for a selected year
          infoBoxOutput("mean_probability_year", width = 6),
          # static infobox showing the mean decrease in probability from 2000 compared to 2016
          infoBox(
            title = HTML(paste("From 2000 to 2016, on average,",
                               br(), "the probability to die from",
                               br(), "CDCC diseases has <u>decreased</u> by")),
            width = 6,
            value = label_percent()(mean_decrease/100),
            icon = icon("arrow-down"),
            fill = TRUE,
            color = "olive"
          )
        )
      ),
      # row 2
      fluidRow(
        column(
          width = 6,
          column(
            width = 6,
            # dynamic infobox showing deadliest country for males in a particular year
            infoBoxOutput("deadliest_country_male_year", width = NULL)
          ),
          column(
            width = 6,
            # dynamic infobox showing safest country for males in a particular year
            infoBoxOutput("safest_country_male_year", width = NULL)
          ),
          box(
            width = 12,
            # dynamic plot output showing the probability of death from cdcc over the years in a particular country
            # split up by sex
            # if "compare countries" is selected, then it shows both countries at once and the legend is labelled
            # according to the country name
            plotOutput("cdcc_plot_summary"),
            footer = HTML("The dashed line represents the average probability to die from CDCC<br>
                          diseases across all countries and sexes in a given year"),
            status = "danger"
          )
        ),
        column(
          width = 6,
          column(
            width = 6,
            # dynamic infobox showing deadliest country for females in a particular year
            infoBoxOutput("deadliest_country_female_year", width = NULL)
          ),
          column(
            width = 6,
            # dynamic infobox showing safest country for females in a particular year
            infoBoxOutput("safest_country_female_year", width = NULL)
          ),
          box(
            # dynamic map title output showing the selected year
            title = uiOutput("map_title"),
            # dynamic interactive map output showing the countries colored by cdcc death probability in a given year
            leafletOutput("cdcc_map"),
            status = "danger",
            width = 12,
            solidHeader = TRUE
          )
        )
      )
    ),
    ## inputs tab
    tabItem(
      tabName = "inputs",
      # row 1
      fluidRow(
        column(
          width = 6,
          box(
            width = NULL,
            title = "Probability Table",
            # static but interactive datatable output showing the cdcc input dataset
            dataTableOutput("input_table"),
            status = "danger"
          )
        ),
        column(
          width = 6,
          box(
            width = NULL,
            title = "Medical Personnel Table",
            # undeployed
            dataTableOutput("input_table_2"),
            status = "danger"
          )
        )
      )
    ),
    ## plots
    tabItem(
      tabName = "plots",
      # row 1
      fluidRow(
        box(
          width = 6,
          title = uiOutput("country1_title"),
          # see "summary" tab above
          plotlyOutput("cdcc_plot_plots_first_country"),
          status = "danger",
          solidHeader = TRUE
        ), 
        box(
          width = 6,
          title = uiOutput("country2_title"),
          # see summary tab above
          plotlyOutput("cdcc_plot_plots_second_country"),
          status = "danger",
          solidHeader = TRUE
        )
      ),
      fluidRow(
        box(
          width = 6,
          # dynamic plot output showing the difference in mean probability of cdcc death of a current year 
          # compared to the previous year in the dataset (or previous 5 year period)
          # if "compare countries" is selected, then it shows both countries at once and the legend is labelled
          # according to the country name
          plotOutput("cdcc_plot_difference"),
          status = "danger"
        )
      )
    ),
    ## creator
    tabItem(
      tabName = "creator",
      fluidRow(
        userBox(
          width = 12,
          title = userDescription(
            title = "Sascha Remy Brunner",
            subtitle = "Bioinformatician / Data Analyst",
            type = 1,
            image = "https://storage.googleapis.com/kaggle-avatars/images/5878132-kg.JPG",
            backgroundImage = "https://biology.mit.edu/wp-content/uploads/2017/12/MIT-Inhibitor-Model_Credit_MIT-News.jpeg"
          ),
          br(),
          descriptionBlock(
            header = "About",
            text = HTML("MSc Marine Biology with a focus on Big Data Analysis (Genomics).<br>
                        I like to tackle complex data problems using Unix/Linux and R.<br>
                        I use shinydashboard to convey stories through data.")
          ),
          background = "navy"
        )
      )
    )
  )
)

################## ui
ui <- dashboardPage(
    header = header, 
    sidebar = sidebar, 
    body = body,
    skin = "red",
    # some HTML for the email and kaggle link padding bc it looked a bit shitty
    tags$head(
      tags$style(
        HTML(
          "
          .h5, h5 {
          padding-right: 10px;
          }
          div#kaggle_link {
          padding-right: 10px;
          }
          "
        )
      )
    )
  )

################## server
server <- function(input, output, session) {
  
  ### reactives
  
  ## in summary tab
  # infobox mean probability, filters the cdcc_deathprob dataframe by input year, then pulls the mean prob from
  # this dataframe
  mean_probability_both_sexes <- reactive({
    
    cdcc_deathprob %>%
      filter(year == input$year,
             sex == "Both sexes") %>%
      summarise(mean_percent = mean(percent)) %>% pull()
    
  })
  # infobox deadliest male country in a selected year
  deadliest_country_male <- reactive({
    
    cdcc_deathprob %>%
      filter(year == input$year,
             sex == "Male") %>%
      arrange(desc(percent)) %>% head(1) %>% pull(location) %>% as.character()
    
  })
  # infobox deadliest female country in a selected year
  deadliest_country_female <- reactive({
    
    cdcc_deathprob %>%
      filter(year == input$year,
             sex == "Female") %>%
      arrange(desc(percent)) %>% head(1) %>% pull(location) %>% as.character()
    
  })
  # infobox safest male country in a selected year
  safest_country_male <- reactive({
    
    cdcc_deathprob %>%
      filter(year == input$year,
             sex == "Male") %>%
      arrange(percent) %>% head(1) %>% pull(location) %>% as.character()
    
  })
  # infobox safest female country in a selected year
  safest_country_female <- reactive({
    
    cdcc_deathprob %>%
      filter(year == input$year,
             sex == "Female") %>%
      arrange(percent) %>% head(1) %>% pull(location) %>% as.character()
    
  })
  
  
  ## in plots tab
  # plot reactive, filters the cdcc_deathprob dataframe by input country
  filter_plot_react <- reactive({
    
    cdcc_deathprob %>%
      filter(location == input$location)
    
  })
  # plot reactive, filters the cdcc_deathprob dataframe by second input country to compare both countries
  filter_plot_react_2 <- reactive({
    
    req(input$compare_countries)
    
    cdcc_deathprob %>%
      filter(location == input$location_2)
    
  })
  
  ## in maps tab
  # map reacitve, filters the cdcc_deathprob dataframe by a select input year for both sexes
  map_react <- reactive({
    cdcc_deathprob %>%
      filter(year == input$year,
             sex == "Both sexes")
  })
  
  ### outputs
 
 ## in summary tab
  # kaggle link output
  url <- a("SR Brunner", href="https://www.kaggle.com/reminho")
  output$kaggle_link <- renderUI({
    tagList("A work by", url)
  })
  
 # description text output
 output$info_text <- renderUI({
   
   HTML(paste("This app uses the most recent availble data from the World Health Organization (WHO) on", 
          "the probability to die from any cancer, diabetes, cardiovascular or chronic respiratory disease",
          "(here called CDCC diseases) across all the UN countries.", 
          br(),
          "The period of the data ranges from 2000 to 2016."))
   
 })
 # refs text output
 output$refs <- renderUI({
   
   url <- a("Kaggle", href="https://www.kaggle.com/utkarshxy/who-worldhealth-statistics-2020-complete")
     tagList("This dataset is freely availble from ", url)
     
 })
 
 # infobox mean probability output in a selected year
 output$mean_probability_year <- renderInfoBox({
   
   infoBox(
     title = HTML(paste0("Mean probability to die from",
                        br(), "CDCC diseases globally (", input$year, ")")),
     width = 3,
     value = label_percent()(mean_probability_both_sexes()/100),
     icon = icon("skull-crossbones"),
     fill = TRUE,
     color = "navy"
   )
   
 })

 # infobox deadliest country for males in a selected year
 output$deadliest_country_male_year <- renderInfoBox({
   
   infoBox(
     title =  HTML(paste0("Deadliest country for males",br(), "(", input$year, ")")),
     value = deadliest_country_male(),
     width = NULL,
     icon = icon("flag"),
     fill = FALSE,
     color = "navy"
   )
   
 })
 # infobox deadliest country for females in a selected year
 output$deadliest_country_female_year <- renderInfoBox({
   
   infoBox(
     title = HTML(paste0("Deadliest country for females",br(), "(", input$year, ")")),
     value = deadliest_country_female(),
     width = NULL,
     icon = icon("flag"),
     fill = FALSE,
     color = "navy"
   )
   
 })
 # infobox safest country for males in a selected year
 output$safest_country_male_year <- renderInfoBox({
   
   infoBox(
     title = HTML(paste0("Safest country for males",br(), "(", input$year, ")")),
     width = NULL,
     value = safest_country_male(),
     icon = icon("flag-o"),
     fill = FALSE,
     color = "olive"
   )
   
 })
 # infobox safest country for females in a selected year
 output$safest_country_female_year <- renderInfoBox({
   
   infoBox(
     title = HTML(paste0("Safest country for females",br(), "(", input$year, ")")),
     width = NULL,
     value = safest_country_female(),
     icon = icon("flag-o"),
     fill = FALSE,
     color = "olive"
   )
   
 })
 
 # leaflet base map output thats rendered upon visiting the application
 output$cdcc_map <- renderLeaflet({
   
   leaflet(options = leafletOptions(minZoom = 1)) %>%
          addTiles() %>%
          setMaxBounds(lng1 = max(wrld_simpl@data$LON) + 10,
                       lat1 = max(wrld_simpl@data$LAT) + 10,
                       lng2 = min(wrld_simpl@data$LON) - 10,
                       lat2 = min(wrld_simpl@data$LAT) - 10)
   
 })
 
 # comparison plot
 output$cdcc_plot_summary <- renderPlot({
   
   # conditional reactives triggering based on "compare countries" checkbox input
   if (input$compare_countries == FALSE) {
     # plot filter reactive & plot title reactive, country 1
     cdcc_deathprob <- filter_plot_react()
   } else {
     # country 1
     cdcc_deathprob <- filter_plot_react()
     # country 2
     cdcc_deathprob_country_2 <- filter_plot_react_2()
   }
   
   # actual base plot
   p <- cdcc_deathprob %>%
     ggplot(aes(x = year, y = percent, color = sex)) +
     geom_line(lwd = 1) +
     geom_point(size = 4) +
     geom_line(aes(y = avg_prob), lty = "dashed", lwd = 1) +
     scale_color_manual(values = wes_palette("Moonrise2")[c(1,2,4)]) +
     scale_x_continuous(breaks = c(2000, 2005, 2010, 2015, 2016)) +
     theme_light() +
     theme_bigfont
   
   # conditional addition of layers to the plot based on "compare countries" checkbox input
   if (input$compare_countries == FALSE) {
     p +
       labs(title = paste0("Probability of dying from CDCC diseases between age 30 - 70 in ", input$location),
            x = "Year",
            y = "Probability to die from CDCC diseases (%)",
            color = "Sex")
   } else {
     p + 
       labs(color = paste0(input$location)) +
       # second country
       new_scale_color() +
       geom_line(data = cdcc_deathprob_country_2, aes(x = year, y = percent, color = sex), lwd = 1) +
       geom_point(data = cdcc_deathprob_country_2, aes(x = year, y = percent, color = sex), 
                  size = 4,
                  shape = 17) +
       scale_color_manual(values = wes_palette("FantasticFox1")[c(3,4,5)]) +
       labs(title = paste0("Probability of dying from CDCC diseases between age 30 - 70 in ", 
                           input$location, " vs. ", input$location_2),
            x = "Year",
            y = "Probability to die from CDCC diseases (%)",
            color = input$location_2)
   }
 })

 ## observers
 # tip notification shown upon visiting the application for 10sec
 observe({
   showNotification(
     "Tip: If the plot tiles don't show, collapse the sidebar.",
     duration = 10
   )
 })
 
 # map react observer which triggers the map to add the colored chloropleths with addPolygons()
 observeEvent(input$year, {
   
   # reactive response (filter dataset by year)
   map_year <- map_react()

   # left join the filtered cdcc_deathprob dataframe into wrld_simpl@data slot
   map_cdcc_data <- wrld_simpl@data %>%
     left_join(map_year, by = c("NAME" = "location"))

   # add this new table to the shapefile
   wrld_simpl@data <- map_cdcc_data
   
   # generate the labels for each country in a given year
   map_labels <- lapply(seq(nrow(wrld_simpl@data)), function(i) {
     paste0("Country: ", wrld_simpl@data[i, "NAME"], "</br>",
            "Probability: ", wrld_simpl@data[i, "percent"], "%")
   })

   # add chloropleths to the base map showing the filtered data
   leafletProxy("cdcc_map", session) %>%
   clearGroup("cdcc") %>%
   addPolygons(data = wrld_simpl,
               weight = 1,
               group = "cdcc",
               color = ~cdcc_pal(percent),
               # add labels that display probability in percent
               label = lapply(map_labels, shiny::HTML),
               # highlight polygons on hover
               highlight = highlightOptions(weight = 5, color = "white",
                                            bringToFront = TRUE))
   
 })
 # dynamic map title output
 output$map_title <- renderPrint({
   
   HTML(cat("Map of CDCC disease death probability in", input$year, "(both sexes)"))
   
 })
 
 ### in inputs
 # cdcc_deathprob datatable output
 output$input_table <- DT::renderDT(
   
   datatable(cdcc_deathprob,
             rownames = FALSE,
             colnames = c('Country', 'Year', 'Indicator', 'Sex', 'Probability (%)', 'Avg. Prob per year (%)'), 
             options = list(pageLength = 5))
   
 )
 
 # not deployed
 output$input_table_2 <- DT::renderDT(
   
   datatable(medical_personnel,
             rownames = FALSE,
             colnames = c('Country', 'Year', 'Nurses (per 10k people)', 'Doctors (per 10k people)', 'Pharmacists (per 10k people)'), 
             options = list(pageLength = 15))
   
 )
  
  ### in plots
 
 ## time series plot
  output$cdcc_plot_plots_first_country <- renderPlotly({
    
    # plot filter reactive & plot title reactive, country 1
    cdcc_deathprob <- filter_plot_react()
    
    # actual base plot
    p <- cdcc_deathprob %>%
      ggplot(aes(x = year, y = percent, color = sex)) +
      geom_line(lwd = 0.5) +
      geom_line(aes(y = avg_prob), lty = "dashed", lwd = 0.5) +
      geom_point(size = 2) +
      annotate("segment", x = 2010, xend = 2011, y = 20.25, yend = 23.25,
               colour = "black") +
      annotate("text", x = 2008, y = 23.5, 
               label = "Avg. probability across all countries (both sexes)") +
      scale_color_manual(values = wes_palette("Moonrise2")[c(1,2,4)]) +
      scale_x_continuous(breaks = c(2000, 2005, 2010, 2015, 2016)) +
      labs(x = "Year",
           y = "Probability to die from CDCC diseases (%)",
           color = "Sex") +
      theme_light() +
      theme_plotlyfont
    
    ggplotly(p)
  })
  
  # dynamic plot title output
  output$country1_title <- renderPrint({
    
    HTML(cat("Probability to die from CDCC diseases in", input$location))
    
  })
  
  # second country
  output$cdcc_plot_plots_second_country <- renderPlotly({
    
    # plot filter reactive & plot title reactive, country 2
    cdcc_deathprob_country_2 <- filter_plot_react_2()
    
    # actual base plot
    p <- cdcc_deathprob_country_2 %>%
      ggplot(aes(x = year, y = percent, color = sex)) +
      geom_line(lwd = 0.5) +
      geom_line(aes(y = avg_prob), lty = "dashed", lwd = 0.5) +
      geom_point(size = 2) +
      annotate("segment", x = 2010, xend = 2011, y = 20.25, yend = 23.25,
               colour = "black") +
      annotate("text", x = 2008, y = 23.5, 
               label = "Avg. probability across all countries (both sexes)") +
      scale_color_manual(values = wes_palette("Moonrise2")[c(1,2,4)]) +
      scale_x_continuous(breaks = c(2000, 2005, 2010, 2015, 2016)) +
      labs(x = "Year",
           y = "Probability to die from CDCC diseases (%)",
           color = "Sex") +
      theme_light() +
      theme_plotlyfont
    
    ggplotly(p)
  })
  
  # dynamic plot title output
  output$country2_title <- renderPrint({
    
    HTML(cat("Probability to die from CDCC diseases in", input$location_2))
    
  })
  
  ## time series difference plot
  output$cdcc_plot_difference <- renderPlot({
    
    # conditional reactives triggering based on "compare countries" checkbox input
    if (input$compare_countries == FALSE) {
      # plot filter reactive & plot title reactive, country 1
      cdcc_deathprob <- filter_plot_react()
      
      # calc the difference between the current year and the previous one per country, by sex and plot
      cdcc_diffprob <- cdcc_deathprob %>%
        arrange(year) %>%
        group_by(location, sex) %>%
        mutate(diff_year = percent - dplyr::lag(percent))
    } else {
      # country 1
      cdcc_deathprob <- filter_plot_react()
      # country 2
      cdcc_deathprob_country_2 <- filter_plot_react_2()
      
      # calc the difference between the current year and the previous one per country, by sex and plot
      cdcc_diffprob <- cdcc_deathprob %>%
        arrange(year) %>%
        group_by(location, sex) %>%
        mutate(diff_year = percent - dplyr::lag(percent))
      
      # country 2
      # calc the difference between the current year and the previous one per country, by sex and plot
      cdcc_diffprob_country_2 <- cdcc_deathprob_country_2 %>%
        arrange(year) %>%
        group_by(location, sex) %>%
        mutate(diff_year = percent - dplyr::lag(percent))
    }
    
    # actual plot
    p2 <- cdcc_diffprob %>%
      ggplot(aes(x = year, y = diff_year, color = sex)) +
      geom_line(lwd = 1) +
      geom_point(size = 3) +
      geom_hline(yintercept = 0, lwd = 0.5, linetype = "longdash", color = "black") +
      scale_x_continuous(limits = c(2005, 2016) ,breaks = c(2005, 2010, 2015, 2016)) +
      scale_y_continuous(
        # limits = c(-10, 2), 
        breaks = seq(-10, 2, by = 1)) +
      scale_color_manual(values = wes_palette("Moonrise2")[c(1,2,4)]) +
      theme_light() +
      theme_bigfont
    
    # conditional addition of layers to the plot based on "compare countries" checkbox input
    if (input$compare_countries == FALSE) {
      p2 +
        labs(title = paste0("Difference in probability between the current and the previous year in ", input$location),
             x = "Year",
             y = "Difference in probability to the previous year (%)",
             color = "Sex")
    } else {
      p2 + 
        labs(color = paste0(input$location)) +
        # second country
        new_scale_color() +
        geom_line(data = cdcc_diffprob_country_2, aes(x = year, y = diff_year, color = sex), lwd = 1) +
        geom_point(data = cdcc_diffprob_country_2, aes(x = year, y = diff_year, color = sex), 
                   size = 4,
                   shape = 17) +
        scale_color_manual(values = wes_palette("FantasticFox1")[c(3,4,5)]) +
        labs(title = paste0("Difference in probability between the current and the previous year in ", 
                            input$location, " vs. ", input$location_2),
             x = "Year",
             y = "Difference in probability to the previous year (%)",
             color = input$location_2)
    }
  })
}
# run app
shinyApp(ui, server)