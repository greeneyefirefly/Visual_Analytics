# title: "CUNY MSDS DATA 608: Module 3"
# author: "Samantha Deokinanan"
# date: "September 24, 2020"

# ShinyApp found at https://greeneyefirefly.shinyapps.io/DATA608-HW3-Deokinanan/

# Question 1
# As a researcher, you frequently compare mortality rates from particular causes across
# different States. You need a visualization that will let you see (for 2010 only) the crude
# mortality rate, across all States, from one cause (for example, Neoplasms, which are
# effectively cancers). Create a visualization that allows you to rank States by crude mortality
# for each cause of death.

# Question 2:
# Often you are asked whether particular States are improving their mortality rates (per cause)
# faster than, or slower than, the national average. Create a visualization that lets your clients
# see this for themselves for one cause of death at the time. Keep in mind that the national
# average should be weighted by the national population.

# Required Library
library(tidyverse)
library(shiny)
library(plotly)

# Loading Data Set
urlRemote  = "https://raw.githubusercontent.com/"
pathGithub = "charleyferrari/CUNY_DATA_608/master/module3/data/"
fileTrain   = "cleaned-cdc-mortality-1999-2010-2.csv"
data = read.csv(paste0(urlRemote, pathGithub, fileTrain))

# Q1 Data
# Subset for 2010 only
df2010 = subset(data, Year == 2010)
cause2010 = unique(df2010$ICD.Chapter)

# Q2 Data
causes = unique(data$ICD.Chapter)
causes = causes[-c(7,18)]
states = unique(data$State)

# Design User-Interface
ui = fluidPage(
  tabsetPanel(
    tabPanel("Q1: Mortality Rates by States",
             titlePanel("CDC Mortality Rates in 2010, by States"),
             sidebarPanel(
               selectInput("cause", "Cause of Death:", sort(cause2010))),
             mainPanel(plotlyOutput("PlotMortalityRates", height = 800))),
    tabPanel("Q2: National Average",
             titlePanel("Mortality Rates Over Time, by States"),
             sidebarPanel(
               selectInput('causes', 'Cause of Death', sort(causes)),
               selectInput('states', 'State', sort(states))),
             mainPanel(plotlyOutput("PlotNatAvg")))
  )
)

# Create Server
server = function(input, output, session){
  output$PlotMortalityRates = renderPlotly({
    
    mrates = df2010 %>% filter(ICD.Chapter == input$cause)
    
    plot_ly(mrates) %>%
      add_bars(x = mrates$Crude.Rate, y = reorder(mrates$State, -mrates$Crude.Rate),
               color = I("#69b3a2"), orientation = 'h', width = 0.1, name = ' ') %>%
      add_markers(x = mrates$Crude.Rate, y = reorder(mrates$State, -mrates$Crude.Rate), name = ' ',
                  marker = list(size = 10, color = "#69b3a2",
                                line = list(color = "#69b3a2", width = 2))) %>%
      layout(title = sprintf("Morality Rates for %s", str_to_title(input$cause)), 
             xaxis = list(title = "CDC Mortality Rate"),
             yaxis = list(title = "State", tickfont = list(size = 10)), showlegend = FALSE)
  })
  
  output$PlotNatAvg = renderPlotly({

    selected_state = data %>% filter(State == input$states, ICD.Chapter == input$causes)
    
    if(nrow(selected_state) <= 1){
      selected_state = selected_state[, c("Year", "Crude.Rate")]
      selected_state = rbind(selected_state, data.frame(Year = setdiff(1999:2010, selected_state$Year), 
                                Crude.Rate = rep(NA, times = length(1999:2010))))
      selected_state = selected_state[order(selected_state$Year),]
    }
    
    natavg = data %>% filter(ICD.Chapter == input$causes, State != input$states)
    natavg = data.frame(Year = 1999:2010, 
                        Crude.Rate = aggregate(Deaths ~ Year, FUN = sum, data = natavg)$Deaths / 
                          (aggregate(Population ~ Year, FUN = sum, data = natavg)$Population / 100000))

    plot_ly(selected_state) %>%
      add_lines(x = selected_state$Year, y = selected_state$Crude.Rate, color = I("#cd7eaf"), 
                name = 'State Rate') %>%
      add_lines(x = natavg$Year, y = round(natavg$Crude.Rate,3), color = I("#1a9263"), 
                name = 'National Average Rate') %>%
      layout(title = sprintf("Morality Rates for the State of %s vs the National Average", input$states),
             xaxis = list(title = "Year"),
             yaxis = list(title = "Morality Rates"))
  })
}

shinyApp(ui = ui, server = server)
