# title: "CUNY MSDS DATA 608: Final Project"
# author: "Samantha Deokinanan"
# date: "November 30, 2020"

# Required Library
library(tidyverse)
library(shiny)
library(shinythemes)
library(plotly)
library(rgeos)
library(maptools)
library(rjson)

Sys.setlocale("LC_ALL", "C")

# Loading Data Sets
urlRemote = "https://raw.githubusercontent.com/"
pathGithub = "greeneyefirefly/DATA608-Visual_Analytics/master/Final_Project/"
airquality = "OutdoorAirAndHealth-wide.csv"
nyc_districts = "Neighborhood.geojson"
zipcodes = "zipcodes.csv"

airquality = read.csv(paste0(urlRemote, pathGithub, airquality))
nyc_districts = fromJSON(file = paste0(urlRemote, pathGithub, nyc_districts))
zipcodes = read.csv(paste0(urlRemote, pathGithub, zipcodes))

# Air Quality Data Transformation
airquality = Filter(function(x) ! all(is.na(x)), airquality)

# Annual Average
air_toxin_picklist = c("Benzene (C6H6)", "Formaldehyde (CH2O)", "Fine Particulate Matter (PM2.5)", 
                       "Nitrogen Dioxide (NO2)", "Ozone (O3)", "Sulfur Dioxide (SO2)")
air_toxin_header = c("geo_area_id", "geo_area_name", 
                     "c6h6.2005", "c6h6.2011", 
                     "ch2o.2005", "ch2o.2011",
                     "pm2.5.2009","pm2.5.2010","pm2.5.2011","pm2.5.2012",
                     "pm2.5.2013","pm2.5.2014","pm2.5.2015","pm2.5.2016",
                     "no2.2009","no2.2010","no2.2011","no2.2012",
                     "no2.2013","no2.2014","no2.2015","no2.2016",
                     "o3.sum.2009","o3.sum.2010","o3.sum.2011",
                     "o3.sum.2012","o3.sum.2013","o3.sum.2014","o3.sum.2015",
                     "o3.sum.2016", "so2.2009","so2.2010","so2.2011","
                     so2.2012","so2.2013","so2.2014","so2.2015","so2.2016") 
air_toxin = airquality[,c(2,3,5:8,15:22,39:46,79:86,102:109)]
names(air_toxin) = air_toxin_header

# Fine Particulate Matter (PM2.5)
pm2.5 = airquality[,c(2,3,15:22,87:101)]

pm2.5_header = c("geo_area_id", "geo_area_name", 
                 "pm2.5.2009","pm2.5.2010","pm2.5.2011","pm2.5.2012",
                 "pm2.5.2013","pm2.5.2014","pm2.5.2015","pm2.5.2016",
                 "er.adult.05.07","er.adult.09.11","er.adult.12.14",
                 "er.kids.05.07","er.kids.09.11","er.kids.12.14",
                 "cardio.adult.05.07","cardio.adult.09.11","cardio.adult.12.14",
                 "death.adult.05.07","death.adult.09.11","death.adult.12.14",
                 "resp.adult.05.07","resp.adult.09.11","resp.adult.12.14") 
names(pm2.5) = pm2.5_header

# Nitrogen Dioxide (NO2)
no2 = airquality[,c(2,3,39:62)]

no2_header = c("geo_area_id", "geo_area_name", 
               "no2.2009","no2.2010","no2.2011","no2.2012",
               "no2.2013","no2.2014","no2.2015","no2.2016",
               "no2.w.2009","no2.w.2010","no2.w.2011","no2.w.2012",
               "no2.w.2013","no2.w.2014","no2.w.2015","no2.w.2016",
               "no2.su.2009","no2.su.2010","no2.su.2011","no2.su.2012",
               "no2.su.2013","no2.su.2014","no2.su.2015","no2.su.2016") 
names(no2) = no2_header

# Sulfur Dioxide (SO2)
so2 = airquality[,c(2,3,102:109)]

so2_header = c("geo_area_id", "geo_area_name", 
               "so2.2009","so2.2010","so2.2011","so2.2012",
               "so2.2013","so2.2014","so2.2015","so2.2016") 
names(so2) = so2_header

# Ozone (O3)
o3 = airquality[,c(2,3,63:77,79:86)]

o3_header = c("geo_area_id", "geo_area_name", 
              "er.adult.05.07","er.adult.09.11","er.adult.12.14",
              "er.kids.05.07","er.kids.09.11","er.kids.12.14",
              "asthma.adult.05.07","asthma.adult.09.11","asthma.adult.12.14",
              "asthma.kid.05.07","asthma.kid.09.11","asthma.kid.12.14",
              "death.adult.05.07","death.adult.09.11","death.adult.12.14",
              "o3.2009","o3.2010","o3.2011","o3.2012",
              "o3.2013","o3.2014","o3.2015","o3.2016") 
names(o3) = o3_header

# -----------------------------------------
# Define UI for application
ui = fluidPage(
  tabsetPanel(
    tabPanel("What's in my air?",
             titlePanel("The Effects of Outdoor Air Quality on Health in New York City"),
             p("Clean air is vital as it provides oxygen and other gases that sustain the delicate balance of life on Earth. However, the quality of the air can be affected by air pollution. Air pollution occurs when certain gases and particles build up in the atmosphere to such levels that they can cause disease, death to humans, damage to other living organisms such as food crops, or damage to the natural or man-made environment. These substances, known as pollutants, can be solid particles, liquid droplets, or gases, and are classified as primary or secondary pollutants."),
             p("The primary pollutant tends to come from man-made sources, including the burning of fossil fuels such as coal, oil, petrol or diesel, but can also come from natural sources such as volcanic eruptions and forest fires. Unlike primary pollutants, secondary pollutants are not emitted directly. Rather, they form in the air when primary pollutants as a result of chemical reactions. The federal Clean Air Act authorized the Environmental Protection Agency (EPA) to set National Ambient Air Quality Standards (NAAQS) for pollutants that threaten human health and public welfare throughout the country, (Clean Air Act, EPA). EPA established NAAQS for six most common pollutants called criteria air pollutants: ground-level ozone, fine particulate matter, carbon monoxide, nitrogen dioxide, sulfur dioxide, and volatile organic compounds (eg Benzene and Formaldehyde), among which ground-level ozone, fine particulate, and nitrogen dioxide are the most widespread health threats."),
             br(),
             h4(p(strong("NYC Air Pollution Data"))),
             p("Inspired by NYC Environmental Health Air Quality Explorer, this project aims to interactively present data on primary and secondary air pollutants index, namely for PM2.5, nitrogen dioxide (NO2), volatile organic compounds, and ozone, to depict their health risks on New York City. More specifically, it aims to geographically map the user's neighborhood air quality and compare it to New York City and EPA's annual standards. The data set is from NYCCAS Air Pollution Rasters on NYC OpenData. It is a citywide raster file of annual average predicted surface for nitrogen dioxide (NO2), fine particulate matter (PM2.5), black carbon (BC), and nitrogen oxides (NOx); summer average for ozone (O3) and winter average for sulfur dioxide (SO2). There are surface predictions generated from Land Use Regression modeling of December 2008- December 2015 (years 1-7) New York Community Air Survey monitoring data. As these are estimated annual average levels produced by a statistical model, they are not comparable to short term localized monitoring or monitoring done for regulatory purposes."),
             br(),
             h3(p(strong("Your Neighborhood Air Quality", 
                         style = "color:black")), align = "center"),
             p("The map below is of New York City. The air quality monitoring network data allows a comparison of the concentration of the most harmful air toxins among neighborhoods. Use the map to compare your neighborhood's levels of harmful air toxin with that of New York City", align = "center"),
             br(),
             sidebarPanel(
               textInput('user_zip', "Enter your zipcode", value = "11208"),
               selectInput('user_air_toxin', "Select Air Toxin", sort(air_toxin_picklist))),
             mainPanel(plotlyOutput("MyNYC"))
    ),
    
    tabPanel("PM2.5",
             titlePanel("Particulate Matter Effect on Health"),
             p("Air pollution has a serious toxicological impact on human health and the environment. The primary pollutant PMs are tiny particles of organic chemicals, minerals, and dust, emitted from the combustion of fossil fuel. High concentrations of PM2.5 (particulate matter measuring 2.5 micrometers across) in urban air pose a great health risk to urban residents (Burnett, et al., 2018). Fine PM can effortlessly penetrate the human respiratory system, increasing the risk for and intensifying respiratory and cardiovascular illnesses. A study conducted by WHO in 2005 projected that by 2050, fine PM in the atmosphere could reasonably be the cause of death for over 6.2 million people per year."),
             br(),
             h4(p(strong("Your Neighborhood PM2.5 Concentration", style = "color:black")), 
                align = "center"),
             p("In NYC, fine particulate matter is measured in units of micrograms per cubic meter of air. For PM2.5, NYC meets the EPA's annual average standard, but short-term concentrations sometimes exceed this threshold. Use the graph to estimate a neighborhood's health risk as it pertains to PM2.5. Along with the annual averages, they also include the estimated number of all-cause deaths associated with the difference in PM2.5 for indicator years relative to modeled natural background; estimates based on the number of emergency department (ED) visits for asthma; estimates based on the number of hospitalizations for respiratory causes; and estimates based on the number of hospitalizations for cardiovascular causes (modeled under no man-made air pollution emissions)."),
             sidebarPanel(
               textInput('user_zip_pm25', 'Enter your zipcode', value = "11208")),
             mainPanel(
               tabsetPanel(
                 tabPanel("PM2.5", plotlyOutput("pm2.5")),
                 tabPanel("Asthma Emergency Department Visits", plotlyOutput("er.pm2.5")),
                 tabPanel("Cardiovascular Hospitalizations", plotlyOutput("cardio.pm2.5")),
                 tabPanel("Respiratory Hospitalizations", plotlyOutput("resp.pm2.5")),
                 tabPanel("Death Rates", plotlyOutput("death.pm2.5"))
               )
             ),
    ),
    
    tabPanel("NO2 & SO2",
             titlePanel("Nitrogen and Sulfur Dioxide Effects on Health"), 
             p("Moreover, in significant concentrations, primary pollutant nitrogen dioxide is highly toxic. It can cause serious lung damage with a delayed effect. It also plays a major role in the atmospheric reactions that produce ground-level ozone or smog. Whereas sulfur dioxide pollution is known to cause heart disease and bronchitis. Pollutants have even more adverse effects when in moderate concentrations as it can lead to a fall in lung function in asthmatics. Sulfur dioxide pollution is considered more harmful when particulate and other pollution concentrations are high. This is known as the cocktail effect. Increasing concentration of these gases in the atmosphere also causes global warming and climate change as dinitrogen monoxide has 310 times more global warming potentiality than carbon dioxide (Sen et al, 2017)."),
             br(),
             h4(p(strong("Your Neighborhood NO2 & SO2 Concentration", style = "color:black")), 
                align = "center"),
             p("For NO2 and SO2, the gaseous pollutants in NYC are measured in units of parts per billion (ppb). NYC meets the EPA's annual average standard, but short-term concentrations sometimes exceed this threshold. Along with the annual averages, they also include the estimated number for the Summer and Winter seasons (where available). Use the graph to estimate a neighborhood's health risk as it pertains to their concentrations."),
             sidebarPanel(
               textInput('user_zip_no2_so2', 'Enter your zipcode', value = "11208")),
             mainPanel(
               tabsetPanel(
                 tabPanel("Nitrogen Dioxide", plotlyOutput("no2")),
                 tabPanel("Seasonal Nitrogen Dioxide", 
                          plotlyOutput("sea.avg.no2"),
                          plotlyOutput("sea.winter.no2"),
                          plotlyOutput("sea.summer.no2")),
                 tabPanel("Sulfur Dioxide", plotlyOutput("so2"))
               )
             ),
    ),
    
    tabPanel("Ozone",
             titlePanel("Ozone Effect on Health"), 
             p("Ground level ozone is a prominent example of a secondary pollutant, formed by the action of sunlight on volatile organic compounds such as Benzene in the presence of nitrogen dioxide. There are no direct man-made emissions of ozone to the atmosphere. Ozone can cause irritation to the respiratory tract and eyes, causing chest tightness, coughing and wheezing, especially amongst those with respiratory and heart problems (Criteria Air Pollution, EPA). Ground level ozone can also have detrimental effects on plants and ecosystems, including damage to plants, reductions of crop yield, and an increase of vegetation vulnerability to disease."),
             br(),
             h4(p(strong("Your Neighborhood Ozone Concentration", style = "color:black")), 
                align = "center"),
             p("In NYC, ozone is measured in units of parts per billion (ppb). For Ozone, NYC meets the EPA's annual average standard, but short-term concentrations sometimes exceed this threshold. Along with the annual averages, they also include estimates based on the number of cardiovascular and respiratory mortalities associated with ozone for indicator years relative to modeled natural background; estimates based on the number of emergency department (ED) visits for asthma; and estimates based on the number of hospitalizations for urgent asthma. Use the graph to estimate a neighborhood's health risk as it pertains to ozone levels."),
             sidebarPanel(
               textInput('user_zip_o3', 'Enter your zipcode', value = "11208")),
             mainPanel(
               tabsetPanel(
                 tabPanel("Ozone", plotlyOutput("o3")),
                 tabPanel("Emergency Department Visits", plotlyOutput("er.o3")),
                 tabPanel("Asthma Hospitalizations", plotlyOutput("asthma.o3")),
                 tabPanel("Death Rates", plotlyOutput("death.o3"))
               )
             ),
    ),
    
    tabPanel("Works Cited",
             titlePanel("Works Cited"),
             p("1. Burnett, R., Chen, H., Szyszkowicz, M., Fann, N., Hubbell, B., Pope, C. A., 3rd, Apte, J. S., Brauer, M., Cohen, A., Weichenthal, S., Coggins, J., Di, Q., Brunekreef, B., Frostad, J., Lim, S. S., Kan, H., Walker, K. D., Thurston, G. D., Hayes, R. B., Lim, C. C., … Spadaro, J. V. (2018).", em(" Global Estimates of Mortality Associated with Long-Term Exposure To Outdoor Fine Particulate Matter.")," Proceedings of the National Academy of Sciences of the United States of America, 115(38), 9592–9597. https://doi.org/10.1073/pnas.1803222115"),
             p("2. EPA.", em(" Criteria Air Pollutants")," (https://www.epa.gov/criteria-air-pollutants). U. S. Environmental Protection Agency. Accessed 28 October 2020."),
             p("3. EPA.", em(" Summary of the Clean Air Act")," (https://www.epa.gov/laws-regulations/summary-clean-air-act). U.S.C. §7401 et seq. (1970). The official text of the CAA is available in the United States Code on FDSys, from the US Government Printing Office. Accessed 28 October 2020."),
             p("4. NYC OpenData.", em(" NYCCAS Air Pollution Rasters.")," Department of Health and Mental Hygiene (DOHMH). Updated March 3, 2020. Accessed 28 October 2020."),
             p("5. WHO, (2005).", em(" WHO Air Quality Guidelines for Particulate Matter, Ozone, Nitrogen Dioxide, and Sulfur Dioxide.")," World Health Organization: Geneva."),
             p("6. Sen, Abhishek & Khan, Indrani & Kundu, Debajyoti & Das, Kousik & Datta, Jayanta. (2017). ", em("Ecophysiological evaluation of tree species for biomonitoring of air quality and identification of air pollution-tolerant species.")," Environmental Monitoring and Assessment. 189. 1-15. 10.1007/s10661-017-5955-x."),
    )
  ),
  hr(),
  p(em("Developed by"), 
    br("Samantha Deokinanan"), 
    style = "text-align:center; font-family: times"),
)

# ----------------------------------------
# Define Server Logic
server = function(input, output, session) {
  output$MyNYC = renderPlotly({
    # Data based on User Inputs
    user_location = zipcodes %>% filter(str_detect(Zipcode, as.character(input$user_zip)))
    user_toxin = air_toxin[air_toxin$geo_area_id == user_location$UHF.Code, 
                           grepl(gsub("[\\(\\)]", "", 
                                      regmatches(tolower(input$user_air_toxin), 
                                                 gregexpr("\\(.*?\\)", 
                                                          tolower(input$user_air_toxin)))[[1]]), 
                                 colnames(air_toxin))]
    user_toxin$user_avg = rowMeans(user_toxin)
    
    # NYC data
    nyc_toxin = cbind(district = air_toxin$geo_area_id,
                      district_name = air_toxin$geo_area_name,
                      air_toxin[,grepl(gsub("[\\(\\)]", "", 
                                            regmatches(tolower(input$user_air_toxin), 
                                                       gregexpr("\\(.*?\\)", 
                                                                tolower(input$user_air_toxin)))[[1]]), 
                                       colnames(air_toxin))])
    nyc_toxin$nyc_avg = rowMeans(nyc_toxin[,-c(1,2)])
    
    title = paste0("Average ", input$user_air_toxin, " levels in NYC.")
    bar_title = paste0(gsub("[\\(\\)]", "", 
                            regmatches(input$user_air_toxin,
                                       gregexpr("\\(.*?\\)", 
                                                input$user_air_toxin))[[1]]), 
                       " levels")
    plot_ly(text = nyc_toxin$district_name) %>% 
      add_trace(type = "choropleth", 
                geojson = nyc_districts,
                locations = nyc_toxin$district,
                z = nyc_toxin$nyc_avg,
                colorscale = "Viridis",
                featureidkey = "properties.uhfcode") %>% 
      layout(geo = list(fitbounds = "locations", visible = FALSE), 
             title = title,
             autosize = TRUE,
             annotations = list(
               list(x = 0 , y = 1, 
                    text = sprintf("%s Average = %1.2f", 
                                   user_location$Neighborhood, user_toxin$user_avg), 
                    showarrow = FALSE, xref = 'paper', 
                    yref = 'paper', font = list(size = 13)))) %>% 
      colorbar(title = bar_title)
  })
  
  output$pm2.5 = renderPlotly({
    # Data based on User Inputs
    user_location = zipcodes %>% filter(str_detect(Zipcode, as.character(input$user_zip_pm25)))
    user_pm2.5 = pm2.5[pm2.5$geo_area_id == user_location$UHF.Code,]
    user_pm2.5$user_avg_pm2.5 = rowMeans(user_pm2.5[,c(3:10)])
    
    # NYC PM2.5 data
    pm2.5$nyc_avg = rowMeans(pm2.5[, c(3:10)])
    pm2.5_nyc_avg = mean(pm2.5$nyc_avg)
    
    ax = list(title = "Neighborhood", zeroline = FALSE, showline = FALSE, 
              showticklabels = FALSE, showgrid = FALSE)
    
    plot_ly(pm2.5, 
            x = reorder(pm2.5$nyc_avg, pm2.5$geo_area_name), 
            y = ~ nyc_avg, 
            type = 'bar',
            text = pm2.5$geo_area_name,
            hovertemplate = paste("<b>%{text}</b><br>",
                                  "%{yaxis.title.text}: %{y:1.2f}<br>",
                                  "<extra></extra>"),
            marker = list(color = 'rgb(49,130,189)')) %>%
      layout(xaxis = ax,
             yaxis = list(title = "PM2.5"),
             barmode = 'group',
             annotations = list(
               list(x = 0, y = 1,
                    text = sprintf("EPA standard: 12 ug/m^3"),
                    showarrow = FALSE, 
                    xref = 'paper', 
                    yref = 'paper'),
               list(x = 0, y = 0.95, 
                    text = sprintf("NYC average: %1.2f ug/m^3", 
                                   pm2.5_nyc_avg),
                    showarrow = FALSE, 
                    xref = 'paper', 
                    yref = 'paper'),
               list(x = 0, y = 0.90, 
                    text = sprintf("%s average: %1.2f ug/m^3", 
                                   user_location$Neighborhood, user_pm2.5$user_avg_pm2.5),
                    showarrow = FALSE, 
                    xref = 'paper', 
                    yref = 'paper')))
  })
  
  output$er.pm2.5 = renderPlotly({
    # Data based on User Inputs
    user_location = zipcodes %>% filter(str_detect(Zipcode, as.character(input$user_zip_pm25)))
    user_pm2.5 = pm2.5[pm2.5$geo_area_id == user_location$UHF.Code,]
    user_pm2.5$user_avg_er_adult = rowMeans(user_pm2.5[,c(11:13)])
    user_pm2.5$user_avg_er_kid = rowMeans(user_pm2.5[,c(14:16)])
    
    # NYC PM2.5 data
    pm2.5$nyc_avg_adult = rowMeans(pm2.5[, c(11:13)])
    pm2.5_nyc_avg_adult = mean(pm2.5$nyc_avg_adult)
    pm2.5$nyc_avg_kid = rowMeans(pm2.5[, c(14:16)])
    pm2.5_nyc_avg_kid = mean(pm2.5$nyc_avg_kid)
    
    ax = list(title = "Neighborhood", zeroline = FALSE, showline = FALSE, 
              showticklabels = FALSE, showgrid = FALSE)
    
    plot_ly(pm2.5, 
            x = reorder(pm2.5$nyc_avg_adult, pm2.5$geo_area_name), 
            y = ~ nyc_avg_adult, 
            type = 'bar', 
            text = pm2.5$geo_area_name,
            hovertemplate = paste("<b>%{text}</b><br>",
                                  "Adults - 18 yrs & older: %{y:1.2f}<br>",
                                  "<extra></extra>"),
            marker = list(color = 'rgb(49,130,189)')) %>% 
      add_trace(y = ~nyc_avg_kid,  
                text = pm2.5$geo_area_name,
                hovertemplate = paste("<b>%{text}</b><br>",
                                      "Children - 0 to 17 yrs: %{y:1.2f}<br>",
                                      "<extra></extra>"),
                marker = list(color = 'rgb(204,204,204)')) %>%
      layout(xaxis = ax,
             yaxis = list(title = "Asthma ER Visits per 100K adults/children"),
             barmode = 'group',
             showlegend = FALSE,
             annotations = list(
               list(x = 0, y = 1, 
                    text = sprintf("NYC average: %1.2f per 100K adults & %1.2f per 100K children", 
                                   pm2.5_nyc_avg_adult, pm2.5_nyc_avg_kid),
                    showarrow = FALSE, 
                    xref = 'paper', 
                    yref = 'paper'),
               list(x = 0, y = 0.95, 
                    text = sprintf("%s average: %1.2f per 100K adults &  %1.2f per 100K children", 
                                   user_location$Neighborhood,
                                   user_pm2.5$user_avg_er_adult, 
                                   user_pm2.5$user_avg_er_kid),
                    showarrow = FALSE, 
                    xref = 'paper', 
                    yref = 'paper')))
  })
  
  output$cardio.pm2.5 = renderPlotly({
    # Data based on User Inputs
    user_location = zipcodes %>% filter(str_detect(Zipcode, as.character(input$user_zip_pm25)))
    user_pm2.5 = pm2.5[pm2.5$geo_area_id == user_location$UHF.Code,]
    user_pm2.5$user_avg_adult = rowMeans(user_pm2.5[,c(17:19)])
    
    # NYC PM2.5 data
    pm2.5$nyc_avg_adult = rowMeans(pm2.5[, c(17:19)])
    pm2.5_nyc_avg_adult = mean(pm2.5$nyc_avg_adult)
    
    ax = list(title = "Neighborhood", zeroline = FALSE, showline = FALSE, 
              showticklabels = FALSE, showgrid = FALSE)
    
    plot_ly(pm2.5, 
            x = reorder(pm2.5$nyc_avg_adult, pm2.5$geo_area_name), 
            y = ~ nyc_avg_adult, 
            type = 'bar', 
            text = pm2.5$geo_area_name,
            hovertemplate = paste("<b>%{text}</b><br>",
                                  "Adults - 40 yrs & older: %{y:1.2f}<br>",
                                  "<extra></extra>"),
            marker = list(color = 'rgb(49,130,189)')) %>%
      layout(xaxis = ax,
             yaxis = list(title = "Cardiovascular Hospitalizations per 100K adults"),
             barmode = 'group',
             showlegend = FALSE,
             annotations = list(
               list(x = 0, y = 1, 
                    text = sprintf("NYC average: %1.2f per 100K adults", 
                                   pm2.5_nyc_avg_adult),
                    showarrow = FALSE, 
                    xref = 'paper', 
                    yref = 'paper'),
               list(x = 0, y = 0.95, 
                    text = sprintf("%s average: %1.2f per 100K adults", 
                                   user_location$Neighborhood,
                                   user_pm2.5$user_avg_adult),
                    showarrow = FALSE, 
                    xref = 'paper', 
                    yref = 'paper')))
  })
  
  output$resp.pm2.5 = renderPlotly({
    # Data based on User Inputs
    user_location = zipcodes %>% filter(str_detect(Zipcode, as.character(input$user_zip_pm25)))
    user_pm2.5 = pm2.5[pm2.5$geo_area_id == user_location$UHF.Code,]
    user_pm2.5$user_avg_adult = rowMeans(user_pm2.5[,c(23:25)])
    
    # NYC PM2.5 data
    pm2.5$nyc_avg_adult = rowMeans(pm2.5[, c(23:25)])
    pm2.5_nyc_avg_adult = mean(pm2.5$nyc_avg_adult)
    
    ax = list(title = "Neighborhood", zeroline = FALSE, showline = FALSE, 
              showticklabels = FALSE, showgrid = FALSE)
    
    plot_ly(pm2.5, 
            x = reorder(pm2.5$nyc_avg_adult, pm2.5$geo_area_name), 
            y = ~ nyc_avg_adult, 
            type = 'bar', 
            text = pm2.5$geo_area_name,
            hovertemplate = paste("<b>%{text}</b><br>",
                                  "Adults - 20 yrs & older: %{y:1.2f}<br>",
                                  "<extra></extra>"),
            marker = list(color = 'rgb(49,130,189)')) %>%
      layout(xaxis = ax,
             yaxis = list(title = "Respiratory Hospitalizations per 100K adults"),
             barmode = 'group',
             showlegend = FALSE,
             annotations = list(
               list(x = 0, y = 1, 
                    text = sprintf("NYC average: %1.2f per 100K adults", 
                                   pm2.5_nyc_avg_adult),
                    showarrow = FALSE, 
                    xref = 'paper', 
                    yref = 'paper'),
               list(x = 0, y = 0.95, 
                    text = sprintf("%s average: %1.2f per 100K adults", 
                                   user_location$Neighborhood,
                                   user_pm2.5$user_avg_adult),
                    showarrow = FALSE, 
                    xref = 'paper', 
                    yref = 'paper')))
  })
  
  output$death.pm2.5 = renderPlotly({
    # Data based on User Inputs
    user_location = zipcodes %>% filter(str_detect(Zipcode, as.character(input$user_zip_pm25)))
    user_pm2.5 = pm2.5[pm2.5$geo_area_id == user_location$UHF.Code,]
    user_pm2.5$user_avg_adult = rowMeans(user_pm2.5[,c(20:22)])
    
    # NYC PM2.5 data
    pm2.5$nyc_avg_adult = rowMeans(pm2.5[, c(20:22)])
    pm2.5_nyc_avg_adult = mean(pm2.5$nyc_avg_adult)
    
    ax = list(title = "Neighborhood", zeroline = FALSE, showline = FALSE, 
              showticklabels = FALSE, showgrid = FALSE)
    
    plot_ly(pm2.5, 
            x = reorder(pm2.5$nyc_avg_adult, pm2.5$geo_area_name), 
            y = ~ nyc_avg_adult, 
            type = 'bar', 
            text = pm2.5$geo_area_name,
            hovertemplate = paste("<b>%{text}</b><br>",
                                  "Adults - 30 yrs & older: %{y:1.2f}<br>",
                                  "<extra></extra>"),
            marker = list(color = 'rgb(49,130,189)')) %>%
      layout(xaxis = ax,
             yaxis = list(title = "Death Rate per 100K adults"),
             barmode = 'group',
             showlegend = FALSE,
             annotations = list(
               list(x = 0, y = 1, 
                    text = sprintf("NYC average: %1.2f per 100K adults", 
                                   pm2.5_nyc_avg_adult),
                    showarrow = FALSE, 
                    xref = 'paper', 
                    yref = 'paper'),
               list(x = 0, y = 0.95, 
                    text = sprintf("%s average: %1.2f per 100K adults", 
                                   user_location$Neighborhood,
                                   user_pm2.5$user_avg_adult),
                    showarrow = FALSE, 
                    xref = 'paper', 
                    yref = 'paper')))
  })
  
  output$no2 = renderPlotly({
    # Data based on User Inputs
    user_location = zipcodes %>% filter(str_detect(Zipcode, as.character(input$user_zip_no2_so2)))
    user_no2 = no2[no2$geo_area_id == user_location$UHF.Code,]
    user_no2$user_avg = rowMeans(user_no2[,c(3:10)])
    user_no2$user_avg_w = rowMeans(user_no2[,c(11:18)])
    user_no2$user_avg_su = rowMeans(user_no2[,c(19:26)])
    
    # NYC no2 data
    no2$nyc_avg = rowMeans(no2[, c(3:10)])
    no2_nyc_avg = mean(no2$nyc_avg)
    no2$nyc_avg_w = rowMeans(no2[, c(11:18)])
    no2_nyc_avg_w = mean(no2$nyc_avg_w)
    no2$nyc_avg_su = rowMeans(no2[, c(19:26)])
    no2_nyc_avg_su = mean(no2$nyc_avg_su)
    
    ax = list(title = "Neighborhood", zeroline = FALSE, showline = FALSE, 
              showticklabels = FALSE, showgrid = FALSE)
    
    plot_ly(no2, 
            x = reorder(no2$nyc_avg, no2$geo_area_name), 
            y = ~ nyc_avg, 
            type = 'bar', 
            text = no2$geo_area_name,
            hovertemplate = paste("<b>%{text}</b><br>",
                                  "Average: %{y:1.2f}<br>",
                                  "<extra></extra>"),
            marker = list(color = 'rgb(163, 163, 163)')) %>% 
      add_trace(y = ~nyc_avg_w,  
                text = no2$geo_area_name,
                hovertemplate = paste("<b>%{text}</b><br>",
                                      "Winter: %{y:1.2f}<br>",
                                      "<extra></extra>"),
                marker = list(color = 'rgb(81, 73, 216)')) %>% 
      add_trace(y = ~nyc_avg_su,  
                text = no2$geo_area_name,
                hovertemplate = paste("<b>%{text}</b><br>",
                                      "Summer: %{y:1.2f}<br>",
                                      "<extra></extra>"),
                marker = list(color = 'rgb(208, 216, 73)')) %>%
      layout(xaxis = ax,
             yaxis = list(title = "NO2 Concentration, in ppb"),
             barmode = 'group',
             showlegend = FALSE,
             annotations = list(
               list(x = 0, y = 1, 
                    text = "EPA standard: 53 ppb",
                    showarrow = FALSE, 
                    xref = 'paper', 
                    yref = 'paper'),
               list(x = 0, y = 0.95, 
                    text = sprintf("NYC average: %1.2f ppb, in winter: %1.2f ppb, in summer: %1.2f ppb", 
                                   no2_nyc_avg, no2_nyc_avg_w, no2_nyc_avg_su),
                    showarrow = FALSE, 
                    xref = 'paper', 
                    yref = 'paper'),
               list(x = 0, y = 0.90, 
                    text = sprintf("%s average: %1.2f ppb, in winter: %1.2f ppb, in summer: %1.2f ppb", 
                                   user_location$Neighborhood,
                                   user_no2$user_avg, 
                                   user_no2$user_avg_w,
                                   user_no2$user_avg_su),
                    showarrow = FALSE, 
                    xref = 'paper', 
                    yref = 'paper')))
  })
  
  output$sea.avg.no2 = renderPlotly({
    # Data based on User Inputs
    user_location = zipcodes %>% filter(str_detect(Zipcode, as.character(input$user_zip_no2_so2)))
    user_no2 = no2[no2$geo_area_id == user_location$UHF.Code,]
    user_no2$user_avg = rowMeans(user_no2[,c(3:10)])

    # NYC no2 data
    no2$nyc_avg = rowMeans(no2[, c(3:10)])
    no2_nyc_avg = mean(no2$nyc_avg)

    plot_ly(text = no2$geo_area_name) %>% 
      add_trace(type = "choropleth", 
                geojson = nyc_districts,
                locations = no2$geo_area_id,
                z = no2$nyc_avg,
                colorscale = "Viridis",
                featureidkey = "properties.uhfcode") %>% 
      layout(geo = list(fitbounds = "locations", visible = FALSE), 
             title = "Average NO2",
             autosize = TRUE,
             annotations = list(
               list(x = 0 , y = 1, 
                    text = sprintf("%s Average = %1.2f", 
                                   user_location$Neighborhood, user_no2$user_avg), 
                    showarrow = FALSE, xref = 'paper', 
                    yref = 'paper', font = list(size = 13)))) %>% 
      colorbar(title = "NO2 Levels, in ppb", limits = c(5, 40))
  })
  
  output$sea.winter.no2 = renderPlotly({
    # Data based on User Inputs
    user_location = zipcodes %>% filter(str_detect(Zipcode, as.character(input$user_zip_no2_so2)))
    user_no2 = no2[no2$geo_area_id == user_location$UHF.Code,]
    user_no2$user_avg_w = rowMeans(user_no2[,c(11:18)])

    # NYC no2 data
    no2$nyc_avg_w = rowMeans(no2[, c(11:18)])
    no2_nyc_avg_w = mean(no2$nyc_avg_w)

    plot_ly(text = no2$geo_area_name) %>% 
      add_trace(type = "choropleth", 
                geojson = nyc_districts,
                locations = no2$geo_area_id,
                z = no2$nyc_avg_w,
                colorscale = "Viridis",
                featureidkey = "properties.uhfcode") %>% 
      layout(geo = list(fitbounds = "locations", visible = FALSE), 
             title = "Average NO2 Levels in Winter",
             autosize = TRUE,
             annotations = list(
               list(x = 0 , y = 1, 
                    text = sprintf("%s Winter Levels = %1.2f", 
                                   user_location$Neighborhood, user_no2$user_avg_w), 
                    showarrow = FALSE, xref = 'paper', 
                    yref = 'paper', font = list(size = 13)))) %>% 
      colorbar(title = "NO2 Levels, in ppb", limits = c(5, 40))
    
  })
  
  output$sea.summer.no2 = renderPlotly({
    # Data based on User Inputs
    user_location = zipcodes %>% filter(str_detect(Zipcode, as.character(input$user_zip_no2_so2)))
    user_no2 = no2[no2$geo_area_id == user_location$UHF.Code,]
    user_no2$user_avg_su = rowMeans(user_no2[,c(19:26)])
    
    # NYC no2 data
    no2$nyc_avg_su = rowMeans(no2[, c(19:26)])
    no2_nyc_avg_su = mean(no2$nyc_avg_su)
    
   plot_ly(text = no2$geo_area_name) %>% 
      add_trace(type = "choropleth", 
                geojson = nyc_districts,
                locations = no2$geo_area_id,
                z = no2$nyc_avg_su,
                colorscale = "Viridis",
                featureidkey = "properties.uhfcode") %>% 
      layout(geo = list(fitbounds = "locations", visible = FALSE), 
             title = "Average NO2 Levels in Summer",
             autosize = TRUE,
             annotations = list(
               list(x = 0 , y = 1, 
                    text = sprintf("%s Summer Levels = %1.2f", 
                                   user_location$Neighborhood, user_no2$user_avg_su), 
                    showarrow = FALSE, xref = 'paper', 
                    yref = 'paper', font = list(size = 13)))) %>% 
      colorbar(title = "NO2 Levels, in ppb", limits = c(5, 40))
    
  })
  
  output$so2 = renderPlotly({
    # Data based on User Inputs
    user_location = zipcodes %>% filter(str_detect(Zipcode, as.character(input$user_zip_no2_so2)))
    user_so2 = so2[so2$geo_area_id == user_location$UHF.Code,]
    user_so2$user_avg = rowMeans(user_so2[,c(3:10)])
    
    # NYC so2 data
    so2$nyc_avg = rowMeans(so2[, c(3:10)])
    so2_nyc_avg = mean(so2$nyc_avg)
    
    ax = list(title = "Neighborhood", zeroline = FALSE, showline = FALSE, 
              showticklabels = FALSE, showgrid = FALSE)
    
    plot_ly(so2, 
            x = reorder(so2$nyc_avg, so2$geo_area_name), 
            y = ~ nyc_avg, 
            type = 'bar', 
            text = so2$geo_area_name,
            hovertemplate = paste("<b>%{text}</b><br>",
                                  "Average: %{y:1.2f}<br>",
                                  "<extra></extra>"),
            marker = list(color = 'rgb(49,130,189)')) %>%
      layout(xaxis = ax,
             yaxis = list(title = "SO2 Concentration, in ppb"),
             barmode = 'group',
             showlegend = FALSE,
             annotations = list(
               list(x = 0, y = 1, 
                    text = "EPA standard: 30 ppb",
                    showarrow = FALSE, 
                    xref = 'paper', 
                    yref = 'paper'),
               list(x = 0, y = 0.95, 
                    text = sprintf("NYC average: %1.2f ppb", 
                                   so2_nyc_avg),
                    showarrow = FALSE, 
                    xref = 'paper', 
                    yref = 'paper'),
               list(x = 0, y = 0.90, 
                    text = sprintf("%s average: %1.2f ppb", 
                                   user_location$Neighborhood,
                                   user_so2$user_avg),
                    showarrow = FALSE, 
                    xref = 'paper', 
                    yref = 'paper')))
  })
  
  output$o3 = renderPlotly({
    # Data based on User Inputs
    user_location = zipcodes %>% filter(str_detect(Zipcode, as.character(input$user_zip_o3)))
    user_o3 = o3[o3$geo_area_id == user_location$UHF.Code,]
    user_o3$user_avg_o3 = rowMeans(user_o3[,c(18:25)])
    
    # NYC o3 data
    o3$nyc_avg = rowMeans(o3[, c(18:25)])
    o3_nyc_avg = mean(o3$nyc_avg)
    
    ax = list(title = "Neighborhood", zeroline = FALSE, showline = FALSE, 
              showticklabels = FALSE, showgrid = FALSE)
    
    plot_ly(o3, 
            x = reorder(o3$nyc_avg, o3$geo_area_name), 
            y = ~ nyc_avg, 
            type = 'bar',
            text = o3$geo_area_name,
            hovertemplate = paste("<b>%{text}</b><br>",
                                  "%{yaxis.title.text}: %{y:1.2f}<br>",
                                  "<extra></extra>"),
            marker = list(color = 'rgb(49,130,189)')) %>%
      layout(xaxis = ax,
             yaxis = list(title = "Ozone Concentration, in ppb"),
             barmode = 'group',
             annotations = list(
               list(x = 0, y = 1,
                    text = sprintf("EPA standard: 70 ppb"),
                    showarrow = FALSE, 
                    xref = 'paper', 
                    yref = 'paper'),
               list(x = 0, y = 0.95, 
                    text = sprintf("NYC average: %1.2f ppb", 
                                   o3_nyc_avg),
                    showarrow = FALSE, 
                    xref = 'paper', 
                    yref = 'paper'),
               list(x = 0, y = 0.90, 
                    text = sprintf("%s average: %1.2f ppb", 
                                   user_location$Neighborhood, user_o3$user_avg_o3),
                    showarrow = FALSE, 
                    xref = 'paper', 
                    yref = 'paper')))
  })
  
  output$er.o3 = renderPlotly({
    # Data based on User Inputs
    user_location = zipcodes %>% filter(str_detect(Zipcode, as.character(input$user_zip_o3)))
    user_o3 = o3[o3$geo_area_id == user_location$UHF.Code,]
    user_o3$user_avg_er_adult = rowMeans(user_o3[,c(3:5)])
    user_o3$user_avg_er_kid = rowMeans(user_o3[,c(6:8)])
    
    # NYC o3 data
    o3$nyc_avg_adult = rowMeans(o3[, c(3:5)])
    o3_nyc_avg_adult = mean(o3$nyc_avg_adult)
    o3$nyc_avg_kid = rowMeans(o3[, c(6:8)])
    o3_nyc_avg_kid = mean(o3$nyc_avg_kid)
    
    ax = list(title = "Neighborhood", zeroline = FALSE, showline = FALSE, 
              showticklabels = FALSE, showgrid = FALSE)
    
    plot_ly(o3, 
            x = reorder(o3$nyc_avg_adult, o3$geo_area_name), 
            y = ~ nyc_avg_adult, 
            type = 'bar', 
            text = o3$geo_area_name,
            hovertemplate = paste("<b>%{text}</b><br>",
                                  "Adults - 18 yrs & older: %{y:1.2f}<br>",
                                  "<extra></extra>"),
            marker = list(color = 'rgb(49,130,189)')) %>% 
      add_trace(y = ~nyc_avg_kid,  
                text = o3$geo_area_name,
                hovertemplate = paste("<b>%{text}</b><br>",
                                      "Children - 0 to 17 yrs: %{y:1.2f}<br>",
                                      "<extra></extra>"),
                marker = list(color = 'rgb(204,204,204)')) %>%
      layout(xaxis = ax,
             yaxis = list(title = "Asthma ER Visits per 100K adults/children"),
             barmode = 'group',
             showlegend = FALSE,
             annotations = list(
               list(x = 0, y = 1, 
                    text = sprintf("NYC average: %1.2f per 100K adults & %1.2f per 100K children", 
                                   o3_nyc_avg_adult, o3_nyc_avg_kid),
                    showarrow = FALSE, 
                    xref = 'paper', 
                    yref = 'paper'),
               list(x = 0, y = 0.95, 
                    text = sprintf("%s average: %1.2f per 100K adults &  %1.2f per 100K children", 
                                   user_location$Neighborhood,
                                   user_o3$user_avg_er_adult, 
                                   user_o3$user_avg_er_kid),
                    showarrow = FALSE, 
                    xref = 'paper', 
                    yref = 'paper')))
  })
  
  output$asthma.o3 = renderPlotly({
    # Data based on User Inputs
    user_location = zipcodes %>% filter(str_detect(Zipcode, as.character(input$user_zip_o3)))
    user_o3 = o3[o3$geo_area_id == user_location$UHF.Code,]
    user_o3$user_avg_er_adult = rowMeans(user_o3[,c(9:11)])
    user_o3$user_avg_er_kid = rowMeans(user_o3[,c(12:14)])
    
    # NYC o3 data
    o3$nyc_avg_adult = rowMeans(o3[, c(9:11)])
    o3_nyc_avg_adult = mean(o3$nyc_avg_adult)
    o3$nyc_avg_kid = rowMeans(o3[, c(12:14)])
    o3_nyc_avg_kid = mean(o3$nyc_avg_kid)
    
    ax = list(title = "Neighborhood", zeroline = FALSE, showline = FALSE, 
              showticklabels = FALSE, showgrid = FALSE)
    
    plot_ly(o3, 
            x = reorder(o3$nyc_avg_adult, o3$geo_area_name), 
            y = ~ nyc_avg_adult, 
            type = 'bar', 
            text = o3$geo_area_name,
            hovertemplate = paste("<b>%{text}</b><br>",
                                  "Adults - 18 yrs & older: %{y:1.2f}<br>",
                                  "<extra></extra>"),
            marker = list(color = 'rgb(49,130,189)')) %>% 
      add_trace(y = ~nyc_avg_kid,  
                text = o3$geo_area_name,
                hovertemplate = paste("<b>%{text}</b><br>",
                                      "Children - 0 to 17 yrs: %{y:1.2f}<br>",
                                      "<extra></extra>"),
                marker = list(color = 'rgb(204,204,204)')) %>%
      layout(xaxis = ax,
             yaxis = list(title = "Asthma Hospitalization per 100K adults/children"),
             barmode = 'group',
             showlegend = FALSE,
             annotations = list(
               list(x = 0, y = 1, 
                    text = sprintf("NYC average: %1.2f per 100K adults & %1.2f per 100K children", 
                                   o3_nyc_avg_adult, o3_nyc_avg_kid),
                    showarrow = FALSE, 
                    xref = 'paper', 
                    yref = 'paper'),
               list(x = 0, y = 0.95, 
                    text = sprintf("%s average: %1.2f per 100K adults &  %1.2f per 100K children", 
                                   user_location$Neighborhood,
                                   user_o3$user_avg_er_adult, 
                                   user_o3$user_avg_er_kid),
                    showarrow = FALSE, 
                    xref = 'paper', 
                    yref = 'paper')))
  })
  
  output$death.o3 = renderPlotly({
    # Data based on User Inputs
    user_location = zipcodes %>% filter(str_detect(Zipcode, as.character(input$user_zip_o3)))
    user_o3 = o3[o3$geo_area_id == user_location$UHF.Code,]
    user_o3$user_avg_adult = rowMeans(user_o3[,c(15:17)])
    
    # NYC o3 data
    o3$nyc_avg_adult = rowMeans(o3[, c(15:17)])
    o3_nyc_avg_adult = mean(o3$nyc_avg_adult)
    
    ax = list(title = "Neighborhood", zeroline = FALSE, showline = FALSE, 
              showticklabels = FALSE, showgrid = FALSE)
    
    plot_ly(o3, 
            x = reorder(o3$nyc_avg_adult, o3$geo_area_name), 
            y = ~ nyc_avg_adult, 
            type = 'bar', 
            text = o3$geo_area_name,
            hovertemplate = paste("<b>%{text}</b><br>",
                                  "Adults - 30 yrs & older: %{y:1.2f}<br>",
                                  "<extra></extra>"),
            marker = list(color = 'rgb(49,130,189)')) %>%
      layout(xaxis = ax,
             yaxis = list(title = "Death Rate per 100K adults"),
             barmode = 'group',
             showlegend = FALSE,
             annotations = list(
               list(x = 0, y = 1, 
                    text = sprintf("NYC average: %1.2f per 100K adults", 
                                   o3_nyc_avg_adult),
                    showarrow = FALSE, 
                    xref = 'paper', 
                    yref = 'paper'),
               list(x = 0, y = 0.95, 
                    text = sprintf("%s average: %1.2f per 100K adults", 
                                   user_location$Neighborhood,
                                   user_o3$user_avg_adult),
                    showarrow = FALSE, 
                    xref = 'paper', 
                    yref = 'paper')))
  })
}


# ---------------------------------------------------------------
# Run the application
shinyApp(ui = ui, server = server)
