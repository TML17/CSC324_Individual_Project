# Load Required Packages
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(rsconnect)) install.packages("rsconnect", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(leaflet)) install.packages("leaflet", repos = "http://cran.us.r-project.org")
if(!require(plotly)) install.packages("plotly", repos = "http://cran.us.r-project.org")
if(!require(geojsonio)) install.packages("geojsonio", repos = "http://cran.us.r-project.org")
if(!require(shiny)) install.packages("shiny", repos = "http://cran.us.r-project.org")
if(!require(shinyWidgets)) install.packages("shinyWidgets", repos = "http://cran.us.r-project.org")
if(!require(shinydashboard)) install.packages("shinydashboard", repos = "http://cran.us.r-project.org")
if(!require(shinythemes)) install.packages("shinythemes", repos = "http://cran.us.r-project.org")
if(!require(DataExplorer)) install.packages("DataExplorer", repos = "http://cran.us.r-project.org")
if(!require(igraph)) install.packages("igraph", repos = "http://cran.us.r-project.org")
if(!require(modeldata)) install.packages("modeldata", repos = "http://cran.us.r-project.org")
if(!require(networkD3)) install.packages("networkD3", repos = "http://cran.us.r-project.org")


# ------------------------------------------------------------------------------
# DATA PREPARATION AND TRANSFORMATION
# ------------------------------------------------------------------------------

# Read in the primary dataset that will be used for visualization
rawDF <- read_csv("data/rawDF.csv")

# Select only relevant columns from the raw dataset for further analysis
selectDF <- rawDF[, c("country", "year", "co2", "co2_per_capita", "co2_per_gdp",
                      "total_ghg", "ghg_per_capita", "ghg_per_gdp",
                      "methane", "methane_per_capita", "methane_per_gdp",
                      "nitrous_oxide", 'nitrous_oxide_per_capita', "nitrous_oxide_per_gdp")]

# Initialize a list to store categorical breakdowns of the dataset
catOfDF <- vector(mode="list", length=4)
names(catOfDF) <- c("total_ghg", "co2", "methane", "nitrous_oxide")

# Break down the selected data by total greenhouse gas emissions and its respective metrics
catOfDF[[1]] <- selectDF %>%
  select("country", "year", "total_ghg", "ghg_per_capita", "ghg_per_gdp") %>%
  rename( "total" = "total_ghg",
          "per_capita" = "ghg_per_capita",
          "per_gdp" = "ghg_per_gdp")

# Break down the selected data by CO2 emissions and its respective metrics
catOfDF[[2]] <- selectDF %>%
  select("country", "year", "co2", "co2_per_capita", "co2_per_gdp") %>%
  rename( "total" = "co2",
          "per_capita" = "co2_per_capita",
          "per_gdp" = "co2_per_gdp")

# Break down the selected data by methane emissions and its respective metrics
catOfDF[[3]] <- selectDF %>%
  select("country", "year", "methane", "methane_per_capita", "methane_per_gdp") %>%
  rename( "total" = "methane",
          "per_capita" = "methane_per_capita",
          "per_gdp" = "methane_per_gdp")

# Break down the selected data by nitrous oxide emissions and its respective metrics
catOfDF[[4]] <- selectDF %>%
  select("country", "year", "nitrous_oxide", "nitrous_oxide_per_capita", "nitrous_oxide_per_gdp") %>%
  rename( "total" = "nitrous_oxide",
          "per_capita" = "nitrous_oxide_per_capita",
          "per_gdp" = "nitrous_oxide_per_gdp")

# The catOfDF list now contains four data frames, each representing a different category of emissions


# ------------------------------------------------------------------------------
# LINE PLOT FUNCTIONS
# ------------------------------------------------------------------------------

# CO2 EMISSIONS BY YEAR
# This function plots CO2 emissions over a specified range of years for a given country.
co2_year_plot<-function(input, coun, year1, year2) {
  # Filter data based on the specified years, non-zero CO2 values, and country
  plot_df = subset(input, year>=year1 & year<=year2 & co2!=0 & country == coun)
  
  # Create the plot
  g1 = ggplot(plot_df, aes(x = year, y = co2, color = country)) + 
    geom_line() + 
    geom_point(size = 1, alpha = 0.8) +
    ylab("CO2 emission") + 
    xlab("Year") + 
    theme_bw() + 
    theme(legend.title = element_blank(), legend.position = "", plot.title = element_text(size=10), plot.margin = margin(5, 12, 5, 5))
  
  return(g1)
}

# CO2 EMISSIONS PER CAPITA
# This function plots CO2 emissions per capita over a specified range of years for a given country.
co2_per_capita_plot<-function(input, coun, year1, year2) {
  # Filter data based on the specified years, non-zero CO2 per capita values, and country
  plot_df = subset(input, year>=year1 & year<=year2 & co2_per_capita!=0 & country == coun)
  
  # Create the plot
  g1 = ggplot(plot_df, aes(x = year, y = co2_per_capita, color = country)) + 
    geom_line() + 
    geom_point(size = 1, alpha = 0.8) +
    ylab("CO2 emission per capita") + 
    xlab("Year") + 
    theme_bw() + 
    theme(legend.title = element_blank(), legend.position = "", plot.title = element_text(size=10), plot.margin = margin(5, 12, 5, 5))
  
  return(g1)
}
# CO2 EMISSIONS PER GDP
# This function plots CO2 emissions per GDP over a specified range of years for a given country.
co2_per_gdp_plot<-function(input, coun, year1, year2) {
  # Filter data based on the specified years, non-zero CO2 per GDP values, and country
  plot_df = subset(input, year>=year1 & year<=year2 & co2_per_gdp!=0 & country == coun)
  
  # Create the plot
  g1 = ggplot(plot_df, aes(x = year, y = co2_per_gdp, color = country)) + 
    geom_line() + 
    geom_point(size = 1, alpha = 0.8) +
    ylab("CO2 emission per GDP") + 
    xlab("Year") + 
    theme_bw() + 
    theme(legend.title = element_blank(), legend.position = "", plot.title = element_text(size=10), plot.margin = margin(5, 12, 5, 5))
  
  return(g1)
}

# METHANE EMISSIONS BY YEAR
# This function plots methane emissions over a specified range of years for a given country.
ch4_year_plot<-function(input, coun, year1, year2) {
  # Filter data based on the specified years, non-zero methane values, and country
  plot_df = subset(input, year>=year1 & year<=year2 & methane!=0 & country == coun)
  
  # Create the plot
  g1 = ggplot(plot_df, aes(x = year, y = methane, color = country)) + 
    geom_line() + 
    geom_point(size = 1, alpha = 0.8) +
    ylab("Methane emission") + 
    xlab("Year") + 
    theme_bw() + 
    theme(legend.title = element_blank(), legend.position = "", plot.title = element_text(size=10), plot.margin = margin(5, 12, 5, 5))
  
  return(g1)
}

# METHANE EMISSIONS PER CAPITA
# This function plots methane emissions per capita over a specified range of years for a given country.
ch4_per_capita_plot<-function(input, coun, year1, year2) {
  # Filter data based on the specified years, non-zero methane per capita values, and country
  plot_df = subset(input, year>=year1 & year<=year2 & methane_per_capita!=0 & country == coun)
  
  # Create the plot
  g1 = ggplot(plot_df, aes(x = year, y = methane_per_capita, color = country)) + 
    geom_line() + 
    geom_point(size = 1, alpha = 0.8) +
    ylab("Methane emission per capita") + 
    xlab("Year") + 
    theme_bw() + 
    theme(legend.title = element_blank(), legend.position = "", plot.title = element_text(size=10), plot.margin = margin(5, 12, 5, 5))
  
  return(g1)
}

# METHANE EMISSIONS PER GDP
# This function plots methane emissions per GDP over a specified range of years for a given country.
ch4_per_gdp_plot<-function(input, coun, year1, year2) {
  # Filter data based on the specified years, non-zero methane per GDP values, and country
  plot_df = subset(input, year>=year1 & year<=year2 & methane_per_gdp!=0 & country == coun)
  
  # Create the plot
  g1 = ggplot(plot_df, aes(x = year, y = methane_per_gdp, color = country)) + 
    geom_line() + 
    geom_point(size = 1, alpha = 0.8) +
    ylab("Methane emission per GDP") + 
    xlab("Year") + 
    theme_bw() + 
    theme(legend.title = element_blank(), legend.position = "", plot.title = element_text(size=10), plot.margin = margin(5, 12, 5, 5))
  
  return(g1)
}

# NITROUS OXIDE EMISSIONS BY YEAR
# This function plots nitrous oxide emissions over a specified range of years for a given country.
no2_year_plot<-function(input, coun, year1, year2) {
  # Filter data based on the specified years, non-zero nitrous oxide values, and country
  plot_df = subset(input, year>=year1 & year<=year2 & nitrous_oxide!=0 & country == coun)
  
  # Create the plot
  g1 = ggplot(plot_df, aes(x = year, y = nitrous_oxide, color = country)) + 
    geom_line() + 
    geom_point(size = 1, alpha = 0.8) +
    ylab("NO2 emission") + 
    xlab("Year") + 
    theme_bw() + 
    theme(legend.title = element_blank(), legend.position = "", plot.title = element_text(size=10), plot.margin = margin(5, 12, 5, 5))
  
  return(g1)
}

# NITROUS OXIDE EMISSIONS PER CAPITA
# This function plots nitrous oxide emissions per capita over a specified range of years for a given country.
no2_per_capita_plot<-function(input, coun, year1, year2) {
  # Filter data based on the specified years, non-zero nitrous oxide per capita values, and country
  plot_df = subset(input, year>=year1 & year<=year2 & nitrous_oxide_per_capita!=0 & country == coun)
  
  # Create the plot
  g1 = ggplot(plot_df, aes(x = year, y = nitrous_oxide_per_capita, color = country)) + 
    geom_line() + 
    geom_point(size = 1, alpha = 0.8) +
    ylab("NO2 emission per capita") + 
    xlab("Year") + 
    theme_bw() + 
    theme(legend.title = element_blank(), legend.position = "", plot.title = element_text(size=10), plot.margin = margin(5, 12, 5, 5))
  
  return(g1)
}

# NITROUS OXIDE EMISSIONS PER GDP
# This function plots nitrous oxide emissions per GDP over a specified range of years for a given country.
no2_per_gdp_plot<-function(input, coun, year1, year2) {
  # Filter data based on the specified years, non-zero nitrous oxide per GDP values, and country
  plot_df = subset(input, year>=year1 & year<=year2 & nitrous_oxide_per_gdp!=0 & country == coun)
  
  # Create the plot
  g1 = ggplot(plot_df, aes(x = year, y = nitrous_oxide_per_gdp, color = country)) + 
    geom_line() + 
    geom_point(size = 1, alpha = 0.8) +
    ylab("NO2 emission per GDP") + 
    xlab("Year") + 
    theme_bw() + 
    theme(legend.title = element_blank(), legend.position = "", plot.title = element_text(size=10), plot.margin = margin(5, 12, 5, 5))
  
  return(g1)
}

# TOTAL GREENHOUSE GAS EMISSIONS BY YEAR
# This function plots total greenhouse gas emissions over a specified range of years for a given country.
total_ghg_plot<-function(input, coun, year1, year2) {
  # Filter data based on the specified years, non-zero total greenhouse gas values, and country
  plot_df = subset(input, year>=year1 & year<=year2 & total_ghg!=0 & country == coun)
  
  # Create the plot
  g1 = ggplot(plot_df, aes(x = year, y = total_ghg, color = country)) + 
    geom_line() + 
    geom_point(size = 1, alpha = 0.8) +
    ylab("Total greenhouse gas emission") + 
    xlab("Year") + 
    theme_bw() + 
    theme(legend.title = element_blank(), legend.position = "", plot.title = element_text(size=10), plot.margin = margin(5, 12, 5, 5))
  
  return(g1)
}

# TOTAL GREENHOUSE GAS EMISSIONS PER CAPITA BY YEAR
# This function plots total greenhouse gas emissions per capita over a specified range of years for a given country.
total_ghg_per_capita<-function(input, coun, year1, year2) {
  # Filter data based on the specified years, non-zero total greenhouse gas per capita values, and country
  plot_df = subset(input, year>=year1 & year<=year2 & ghg_per_capita!=0 & country == coun)
  
  # Create the plot
  g1 = ggplot(plot_df, aes(x = year, y = ghg_per_capita, color = country)) + 
    geom_line() + 
    geom_point(size = 1, alpha = 0.8) +
    ylab("Total greenhouse gas emission per capita") + 
    xlab("Year") + 
    theme_bw() + 
    theme(legend.title = element_blank(), legend.position = "", plot.title = element_text(size=10), plot.margin = margin(5, 12, 5, 5))
  
  return(g1)
}

# TOTAL GREENHOUSE GAS EMISSIONS PER GDP BY YEAR
# This function plots total greenhouse gas emissions per GDP over a specified range of years for a given country.
total_ghg_per_gdp<-function(input, coun, year1, year2) {
  # Filter data based on the specified years, non-zero total greenhouse gas per GDP values, and country
  plot_df = subset(input, year>=year1 & year<=year2 & ghg_per_gdp!=0 & country == coun)
  
  # Create the plot
  g1 = ggplot(plot_df, aes(x = year, y = ghg_per_gdp, color = country)) + 
    geom_line() + 
    geom_point(size = 1, alpha = 0.8) +
    ylab("Total greenhouse gas emission per GDP") + 
    xlab("Year") + 
    theme_bw() + 
    theme(legend.title = element_blank(), legend.position = "", plot.title = element_text(size=10), plot.margin = margin(5, 12, 5, 5))
  
  return(g1)
}

# Note: The structure of these functions is consistent. Each function:
# 1. Filters the data based on a given metric, date range, and country.
# 2. Constructs a line plot for the specific metric.
# 3. Applies consistent theming and labeling to the plot.
# 4. Returns the generated plot.


# COMPARISON FUNCTION FOR EMISSION DATA
# This function aggregates the emission data for a specified year and produces a bar plot comparing 
# the total emissions for the selected emission types.

# Parameters:
# which_year: Integer. The specific year for which the emission data should be aggregated.
# checker: List. A list of emission types to be compared. Valid entries include "total_ghg", "methane", and "nitrous_oxide".
comparison <- function(which_year, checker = list("total_ghg", "methane", "nitrous_oxide")){
  df <- data.frame(type = 0,
                   emission = 0)
  df <- df[-1,]
  
  if("ghg" %in% checker && !("methane" %in% checker) && !("nitrous_oxide" %in% checker)){
    df <- rbind(df, c(type = "ghg", emission = sum(filter(selectDF, year == which_year)$total_ghg, na.rm = TRUE)))
    colnames(df) <- c('type','emission')
    df$emission <- as.double(df$emission)
  }
  else if(!("ghg" %in% checker) && "methane" %in% checker && !("nitrous_oxide" %in% checker)){
    df <- rbind(df, c(type = "methane", emission = sum(filter(selectDF, year == which_year)$methane, na.rm = TRUE)))
    colnames(df) <- c('type','emission')
    df$emission <- as.double(df$emission)
  }
  else if(!("ghg" %in% checker) && !("methane" %in% checker) && "nitrous_oxide" %in% checker){
    df <- rbind(df, c(type = "nitrous_oxide", emission = sum(filter(selectDF, year == which_year)$nitrous_oxide, na.rm = TRUE)))
    colnames(df) <- c('type','emission')
    df$emission <- as.double(df$emission)
  }
  
  else if("ghg" %in% checker && "methane" %in% checker && !("nitrous_oxide" %in% checker)){
    df <- rbind(df, c(type = "ghg", emission = sum(filter(selectDF, year == which_year)$total_ghg, na.rm = TRUE)))
    df <- rbind(df, c(type = "methane", emission = sum(filter(selectDF, year == which_year)$methane, na.rm = TRUE)))
    colnames(df) <- c('type','emission')
    df$emission <- as.double(df$emission)
  }
  else if(!("ghg" %in% checker) && "methane" %in% checker && "nitrous_oxide" %in% checker){
    df <- rbind(df, c(type = "methane", emission = sum(filter(selectDF, year == which_year)$methane, na.rm = TRUE)))
    df <- rbind(df, c(type = "nitrous_oxide", emission = sum(filter(selectDF, year == which_year)$nitrous_oxide, na.rm = TRUE)))
    colnames(df) <- c('type','emission')
    df$emission <- as.double(df$emission)
  }
  else if("ghg" %in% checker && !("methane" %in% checker) && "nitrous_oxide" %in% checker){
    df <- rbind(df, c(type = "ghg", emission = sum(filter(selectDF, year == which_year)$total_ghg, na.rm = TRUE)))
    df <- rbind(df, c(type = "nitrous_oxide", emission = sum(filter(selectDF, year == which_year)$nitrous_oxide, na.rm = TRUE)))
    colnames(df) <- c('type','emission')
    df$emission <- as.double(df$emission)
  }
  else if("ghg" %in% checker && "methane" %in% checker && "nitrous_oxide" %in% checker){
    df <- rbind(df, c(type = "ghg", emission = sum(filter(selectDF, year == which_year)$total_ghg, na.rm = TRUE)))
    df <- rbind(df, c(type = "methane", emission = sum(filter(selectDF, year == which_year)$methane, na.rm = TRUE)))
    df <- rbind(df, c(type = "nitrous_oxide", emission = sum(filter(selectDF, year == which_year)$nitrous_oxide, na.rm = TRUE)))
    colnames(df) <- c('type','emission')
    df$emission <- as.double(df$emission)
  }
  graph = ggplot(data = df,
                 mapping = aes(x = emission, y = type)) +
    geom_col() +
    labs(title = "The Bar Plot Comparison",
         y = "Amount of Emission",
         x = "Type")
  
  graph
}

# Define UI for app that draws a histogram ----
ui <- bootstrapPage(
  tags$head(includeHTML("gtag.html")),
  navbarPage(
    title = "Interactive GHG Traker",
    theme = bslib::bs_theme(version = 4, bootswatch = "minty"),
    tabPanel("Total GHG Map",
      div(class="outer", tags$head(includeCSS("styles.css")),
        title = "Map",
        leafletOutput("mymap", width="100%", height="100%"),
        absolutePanel(id = "controls", class = "panel panel-default",
                      top = 150, left = 55, width = 250, fixed=TRUE, height = "auto", draggable = TRUE,
                      textInput("year", "Enter Year (1851-2021):", value="1990"), # Range: 1851-2021
                      selectInput("species", "Select a type of GHG:",
                        c("Total GHG" = "total_ghg",
                        "CO2" = "co2",
                        "Methane" = 'methane',
                        "Nitrous Oxide" = 'nitrous_oxide'), selected = "co2"),
                      selectInput("kind", "Select a Map:",
                                  c("Total Emission" = "Total Emission",
                                    "Per Capita" = "Per Capita",
                                    "Per GDP" = 'Per GDP'))
          )
        )
    ),
    tabPanel("Line Plots",
             
             sidebarLayout(
               sidebarPanel(
                 
                 span(tags$i(h3("Line Plot for GHG Emission"))),
                 
                 pickerInput("country_select", "Country/Region:",
                             choices = distinct(selectDF[order(-selectDF$co2),],country),
                             selected = as.character(selectDF[order(-selectDF$co2),]$country)[0],
                             multiple = FALSE),

               selectInput("type_selected", "Type of GHG:",
                            c("Total GHG" = "total_ghg",
                               "CO2" = "co2",
                               "Methane" = 'methane',
                               "Nitrous Oxide" = 'nitrous_oxide'), selected = "co2"),
               textInput("start_year", "Enter Start Year (1851-2021):", value="1980"), # Range: 1851-2021
               textInput("end_year", "Enter End Year (1851-2021):", value="2000"), # Range: 1851-2021
                  "If the line plot is not displaying properly, it means that we don't have enough data from the selected choices to render a reasonable plot."
               ),

               mainPanel(
                 
                 tabsetPanel(
                   tabPanel("Total Emission", plotlyOutput("lineplot_total")),
                   tabPanel("Emission Per Capita", plotlyOutput("lineplot_perCap")),
                   tabPanel("Emission Per GDP", plotlyOutput("lineplot_perGDP"))
                 )
               )
      )
    ),
    
    tabPanel("Comparison",
             sidebarLayout(
               sidebarPanel(
                 textInput("year_comparison", "Enter Year (1851-2021):", value="1990"), # Range: 1851-2021
                 checkboxGroupInput("checkGroup", 
                                    h3("Choose Emission Types to Compare"), 
                                    choices = list("Nitrous Oxide" = "nitrous_oxide",
                                                   "Methane" = "methane", 
                                                   "GHG" = "ghg"),
                                    selected = list("ghg", "methane", "nitrous_oxide")),
               ),
               mainPanel(
                 plotOutput("comparison")
               )
             )
    ),
    
    tabPanel("About This Site",
             tags$div(
               tags$h3("About"), 
               "This is a visual illustration of green house gas emission data. Various visualization including maps, line plots, and bar plots are organized in different tabs. ",
               
               tags$br(),tags$br(),tags$h3("Code"),
               "Code and input data used to generate this Shiny App is on ",tags$a(href="https://github.com/TML17/CSC324_Individual_Project", "Github."),
               tags$br(),tags$br(),tags$h3("Author"),
               tags$a(href="https://github.com/TML17", "Chengxin Liu"), tags$br(),
               
               tags$br(),tags$h3("Reference"),
               
               downloadButton("downloadCsv", "Download as CSV"),tags$br(),tags$br(),
               "HTML and CSS Style files are adapted from ", tags$a(href="https://github.com/eparker12/nCoV_tracker",
                          "COVID-19 interactive mapping tool."), tags$br(), tags$br(),
               "Data set is from CO2 and Greenhouse Gas Emissions by", tags$a(href="https://github.com/owid/co2-data",
                                                                                  "Our World in Data."), "which contains data from these sources. ",tags$br(),tags$br(),
              "CO2 emissions: this data is sourced from the", tags$a(href="http://www.globalcarbonproject.org/carbonbudget", "Global Carbon Project"), "The Global Carbon Project typically releases a new update of CO2 emissions annually.", tags$br(),
              "Greenhouse gas emissions (including methane, and nitrous oxide) this data is sourced from the CAIT Climate Data Explorer, and downloaded from the", tags$a(href="https://www.climatewatchdata.org/data-explorer/historical-emissionshttps://www.climatewatchdata.org/data-explorer/historical-emissions", "Climate Watch Portal"), tags$br(),
              "Energy (primary energy, energy mix and energy intensity) this data is sourced from a combination of two sources. The", tags$a(href="https://www.bp.com/en/global/corporate/energy-economics/statistical-review-of-world-energy.html", "BP Statistical Review of World Energy"), "is published annually, but it does not provide data on primary energy consumption for all countries.", tags$br(),tags$br(),tags$br() 
             )
             
    )
  )
)
# branch

# Define server logic required  ----
server <- function(input, output) {
  shapeurl <- "https://raw.githubusercontent.com/johan/world.geo.json/master/countries.geo.json"
  WorldCountry <- geojson_read(shapeurl, what = "sp")

  yearDF <- reactive({
    return(left_join(data.frame(Name = WorldCountry$name), catOfDF[[input$species]] %>%
                       filter(year == input$year), by = c("Name" ="country")))
  })
  # plot total emission per year in Map
  pal_PerYear <- reactive({
    colorBin("Oranges", domain = yearDF()$total, bins = c(0, 10, 50, 100, 500, 1000, 2000, Inf))})
  mapPerYear <- renderLeaflet(
    leaflet(WorldCountry) %>%
      addTiles() %>%
      addPolygons(
        fillColor = ~(pal_PerYear()(yearDF()$total)),
        weight = 2,
        opacity = 1,
        color = "white",
        fillOpacity = 0.7,
        highlight = highlightOptions(
          weight = 3,
          color = "grey",
          fillOpacity = 0.7,
          bringToFront = TRUE),
        label = lapply(paste("<strong>", yearDF()$Name, "</strong>", "<br/>",
                             input$species, yearDF()$total)
                       , HTML)) %>%
      fitBounds(~-100,-60,~60,70) %>%
      addLegend(pal = pal_PerYear(), values = yearDF()$total,
                title = paste(input$species, input$kind, sep=" "), position = "bottomright")
  )

  # plot emission per year and per capita
  pal_PerCap <- reactive({
    colorBin("Oranges", domain = yearDF()$per_capita, bins = c(0, 1, 2, 5, 10, 15, 20, Inf))})
  mapPerCap <- renderLeaflet(
    leaflet(WorldCountry) %>%
      addTiles() %>%
      addPolygons(
        fillColor = ~(pal_PerCap()(yearDF()$per_capita)),
        weight = 2,
        opacity = 1,
        color = "white",
        fillOpacity = 0.7,
        highlight = highlightOptions(
          weight = 3,
          color = "grey",
          fillOpacity = 0.7,
          bringToFront = TRUE),
        label = lapply(paste("<strong>", yearDF()$Name, "</strong>", "<br/>",
                             input$species, yearDF()$per_capita)
                       , HTML)) %>%
      addLegend(pal = pal_PerCap(), values = yearDF()$per_capita,
                title = paste(input$species, input$kind, sep=" "), position = "bottomright")
  )

  # plot emission per year and per gdp
  pal_PerGDP <- reactive({
    colorBin("Oranges", domain = yearDF()$per_gdp, bins = c(0, 0.2, 0.5, 1, 1.5, 2, Inf))})
  mapPerGDP <- renderLeaflet(
    leaflet(WorldCountry) %>%
      addTiles() %>%
      addPolygons(
        fillColor = ~(pal_PerGDP()(yearDF()$per_gdp)),
        weight = 2,
        opacity = 1,
        color = "white",
        fillOpacity = 0.7,
        highlight = highlightOptions(
          weight = 3,
          color = "grey",
          fillOpacity = 0.7,
          bringToFront = TRUE),
        label = lapply(paste("<strong>", yearDF()$Name, "</strong>", "<br/>",
                             input$species, yearDF()$per_gdp)
                       , HTML)) %>%
      addLegend(pal = pal_PerGDP(), values = yearDF()$per_gdp,
                title = paste(input$species, input$kind, sep=" "), position = "bottomright")
  )
  
  # Render map based on kind selected
  observeEvent(input$kind, {
  if (input$kind == "Total Emission") {output$mymap <- mapPerYear}
  if (input$kind == "Per Capita") {output$mymap <-  mapPerCap}
  if (input$kind == "Per GDP") {output$mymap <-  mapPerGDP}
  })

  # Line plots
  observeEvent(input$type_selected, {
    if (input$type_selected == "total_ghg") {output$lineplot_total <- renderPlotly({
        total_ghg_plot(selectDF, input$country_select, input$start_year, input$end_year)
      })
    }
    if (input$type_selected == "co2") {output$lineplot_total <-  renderPlotly({
        co2_year_plot(selectDF, input$country_select, input$start_year, input$end_year)
      })
    }
    if (input$type_selected == "methane") {output$lineplot_total <-  renderPlotly({
        ch4_year_plot(selectDF, input$country_select, input$start_year, input$end_year)
      })
    }
    if (input$type_selected == "nitrous_oxide") {output$lineplot_total <-  renderPlotly({
        no2_year_plot(selectDF, input$country_select, input$start_year, input$end_year)
      })
    }
  })
  
  observeEvent(input$type_selected, {
    if (input$type_selected == "total_ghg") {output$lineplot_perCap <- renderPlotly({
      total_ghg_per_capita(selectDF, input$country_select, input$start_year, input$end_year)
    })
    }
    if (input$type_selected == "co2") {output$lineplot_perCap <-  renderPlotly({
      co2_per_capita_plot(selectDF, input$country_select, input$start_year, input$end_year)
    })
    }
    if (input$type_selected == "methane") {output$lineplot_perCap <-  renderPlotly({
      ch4_per_capita_plot(selectDF, input$country_select, input$start_year, input$end_year)
    })
    }
    if (input$type_selected == "nitrous_oxide") {output$lineplot_perCap <-  renderPlotly({
      no2_per_capita_plot(selectDF, input$country_select, input$start_year, input$end_year)
    })
    }
  })
  
  observeEvent(input$type_selected, {
    if (input$type_selected == "total_ghg") {output$lineplot_perGDP <- renderPlotly({
      total_ghg_per_gdp(selectDF, input$country_select, input$start_year, input$end_year)
    })
    }
    if (input$type_selected == "co2") {output$lineplot_perGDP <-  renderPlotly({
      co2_per_gdp_plot(selectDF, input$country_select, input$start_year, input$end_year)
    })
    }
    if (input$type_selected == "methane") {output$lineplot_perGDP <-  renderPlotly({
      ch4_per_gdp_plot(selectDF, input$country_select, input$start_year, input$end_year)
    })
    }
    if (input$type_selected == "nitrous_oxide") {output$lineplot_perGDP <-  renderPlotly({
      no2_per_gdp_plot(selectDF, input$country_select, input$start_year, input$end_year)
    })
    }
  })
  
  # Plot comparison bar plot
  output$comparison <- renderPlot({
    comparison(input$year_comparison, input$checkGroup)
  })
  
  # output to download data
  output$downloadCsv <- downloadHandler(
    filename = function() {
      paste("owid-co2-data.csv")
    },
    content = function(file) {
      write.csv(rawDF, file)
    }
  )
  
  output$rawtable <- renderPrint({
    printDF = rawDF %>% select(c("country", "year", "co2", "co2_per_capita", "co2_per_gdp",
                               "total_ghg", "ghg_per_capita", "ghg_per_gdp",
                               "methane", "methane_per_capita", "methane_per_gdp",
                               "nitrous_oxide", 'nitrous_oxide_per_capita', "nitrous_oxide_per_gdp"))
    orig <- options(width = 1000)
    print(tail(printDF, input$maxrows), row.names = FALSE)
    options(orig)
  })
}

shinyApp(ui = ui, server = server)
