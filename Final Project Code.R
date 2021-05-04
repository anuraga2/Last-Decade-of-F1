library(dplyr)
library(plotly)
library(readr)
library(sqldf)


# Setting working directory to the project folder
setwd("D:/Spring 21/STAT 440/Final Project")

# listing all the csv files in the data directory
file_nm = list.files("archive/")



# Reading all the files in the archive folder:
for(i in seq(1:length(file_nm))){
  str = paste0(file_nm[i],"=","read_csv(",'"','archive/',file_nm[i],'"',")")
  eval(parse(text = str))
}

# The first question we might ask is, why do we care about ferrari ?
# We need to use the constructor files for this analysis

# Checking out the structure of the constructor files
str(constructor_standings.csv)
str(constructors.csv)
str(races.csv)

# Finding out the race id for the last race of each season
# the race id will be used for determining the championship position of each constructor at the end of the season
last_race = races.csv %>% 
  group_by(year) %>% 
  filter(round == max(round)) %>% 
  select(year, raceId) %>% 
  arrange(desc(year), raceId) %>% filter(year < 2021)

# joining the last race data set with the constructor_standings.csv file to get the constructor position at 
# the end of each season. Then filtering for position 1 and aggregating the resultant data set at a constructor
# level to get the number of championships won by them since the beginning of the sport

constructor_champ_count = constructor_standings.csv %>% 
  inner_join(last_race, by = "raceId") %>% 
  filter(position == 1) %>% 
  group_by(constructorId) %>% 
  summarise(championship_wins = n()) %>% 
  arrange(desc(championship_wins)) %>% 
  inner_join(constructors.csv, by = "constructorId") %>%
  select(name, championship_wins) %>% arrange(desc(championship_wins))




constructor_champ_count$name = factor(constructor_champ_count$name, 
                                      levels = unique(constructor_champ_count$name)[order(constructor_champ_count$championship_wins, 
                                                                                          decreasing = TRUE)])

# Making plotly bar chart
fig = plot_ly(constructor_champ_count, 
              x = ~name, 
              y = ~championship_wins,
              text = constructor_champ_count$championship_wins,
              textposition = 'auto',
              type = "bar", 
              name = 'Champ Counts')

# changing axis labels and chart titles
fig = fig %>% layout(yaxis = list(title = 'Championship Count'), 
                     xaxis = list(title = 'Constructor'),
                     title = "Championship Count for each constructor (1950 - 2020)")

# displaying the figure
fig

## plot shows that Ferrari has been the most consistent team in history when it comes to winning championships
champion_constructor = function(start_year, end_year){
  last_race_y = races.csv %>% 
    group_by(year) %>% 
    filter(round == max(round)) %>% 
    select(year, raceId) %>% 
    arrange(desc(year), raceId) %>% filter(year <= end_year & year >= start_year)
  
  constructor_standings.csv %>% 
    inner_join(last_race_y, by = "raceId") %>% 
    filter(position == 1) %>% 
    inner_join(constructors.csv, by = "constructorId") %>% 
    select(year, name) %>% rename(`Constructor Champion` = name) %>% arrange(year)
}

champion_constructor(1998, 2010)



## Ferrari's position in the last decade

last_race_info = races.csv %>% 
                  group_by(year) %>% 
                    filter(round == max(round)) %>% 
                      select(year,raceId)

ferrari_standing_ld = constructor_standings.csv %>% 
                        inner_join(last_race_info, by = "raceId") %>% 
                        filter(constructorId == 6 & year >= 2010 & year <= 2020)


fig = plot_ly(
  x = as.factor(ferrari_standing_ld$year),
  y = ferrari_standing_ld$position,
  name = "Ferrari Standings",
  type = "bar"
)


fig = fig %>% layout(title = "Ferrari's Constructor Standings",
                     xaxis = list(title = "Championship Year"),
                     yaxis = list(title = "Championship Position"))

fig


## Points deficit calculation
champion_points = constructor_standings.csv %>% 
      inner_join(last_race_info, by = "raceId") %>% 
        filter(year >= 2010 & year <= 2020 & position == 1) %>% select(year, points)

colnames(champion_points) = c("Year", "Champion Team Points")

Ferrari_points = constructor_standings.csv %>% 
  inner_join(last_race_info, by = "raceId") %>% 
  filter(year >= 2010 & year <= 2020 & constructorId == 6) %>% select(year, points)

colnames(Ferrari_points) = c("Year", "Ferrari Points")

deficit  = champion_points %>% 
              inner_join(Ferrari_points, by = "Year") %>% 
                mutate(`Points Deficit` = ((`Champion Team Points` - `Ferrari Points`)/`Champion Team Points`)*100)


fig = plot_ly(
  x = as.factor(deficit$Year),
  y = deficit$`Points Deficit`,
  name = "Points Deficit",
  type = "scatter",
  mode = 'lines'
)


fig = fig %>% layout(title = "YoY Points Deficit Of Ferrari",
                     xaxis = list(title = "Championship Year"),
                     yaxis = list(title = "Points Deficit (%)"))

fig


## Race by Race points comparison for each year 

constructor_standings.csv
races.csv

ferr_vs_champ = 
constructor_standings.csv %>% 
  inner_join(races.csv, by = "raceId") %>% 
  select(constructorId, year, round, points) %>% 
  arrange(year, round, constructorId, points) %>% filter(year >= 2010 & 
                                                           year <= 2020 &
                                                           (constructorId  == 6 | 
                                                              constructorId  == 9 | 
                                                              constructorId  == 131))

cham_2010_2013 = ferr_vs_champ %>% 
  filter(year >= 2010 & year <= 2013 & (constructorId  == 6 | constructorId  == 9)) %>% 
  pivot_wider(names_from = constructorId, values_from = points)

colnames(cham_2010_2013) = c("year","round","Ferrari","Champion Team")
  
cham_2014_2020 = ferr_vs_champ %>% 
  filter(year >= 2014 & year <= 2020 & (constructorId  == 131 | constructorId  == 9)) %>% 
  pivot_wider(names_from = constructorId, values_from = points)


colnames(cham_2014_2020) = c("year","round","Ferrari","Champion Team")

champ_vs_ferr = rbind(cham_2010_2013, cham_2014_2020)




fig = plot_ly(champ_vs_ferr %>% filter(year == 2010), 
              x = ~round, 
              y = ~Ferrari,
              name = 'Ferrari', 
              type = 'scatter', 
              mode = 'lines',
              line = list(color = "#eb4034"))

fig = fig %>% add_trace(y = ~`Champion Team`, 
                        name = 'Red Bull',
                        line = list(color = "#5f4db8"))

fig = fig %>% layout(title = 'Ferrari vs Champion Team 2010',
                     xaxis = list(title = 'Race Number'),
                     yaxis = list(title = 'Points'),
                     showlegend = FALSE)

fig1 = plot_ly(champ_vs_ferr %>% filter(year == 2011), x = ~round, y = ~Ferrari,name = "Ferrari", type = 'scatter', mode = 'lines')
fig1 = fig1 %>% add_trace(y = ~`Champion Team`, name = "Red Bull")
fig1 = fig1 %>% layout(title = "Ferrari vs Champion Team",
                     xaxis = list(title = "Race Number"),
                     yaxis = list(title = "Points"),
                     showlegend = FALSE)

points_plot = function(year_n){
  
  if(year_n %in% c(2010, 2011, 2012, 2013)){
    
    fig = plot_ly(champ_vs_ferr %>% filter(year == year_n), 
                  x = ~round, 
                  y = ~Ferrari,
                  name = 'Ferrari', 
                  type = 'scatter', 
                  mode = 'lines',
                  line = list(color = "#eb4034"))
    
    fig = fig %>% add_trace(y = ~`Champion Team`, 
                            name = 'Red Bull',
                            line = list(color = "#5f4db8"))
    
  }else{
    
    fig = plot_ly(champ_vs_ferr %>% filter(year == year_n), 
                  x = ~round, 
                  y = ~Ferrari,
                  name = 'Ferrari', 
                  type = 'scatter', 
                  mode = 'lines',
                  line = list(color = "#eb4034"))
    
    fig = fig %>% add_trace(y = ~`Champion Team`, 
                            name = 'Mercedes',
                            line = list(color = "#36b1b5"))
    
  }
  
  chart_title = paste("Ferrari vs Champion Team", as.character(year_n))
  fig = fig %>% layout(xaxis = list(title = 'Race Number'),
                       yaxis = list(title = 'Points'),
                       annotations = list(text = chart_title, 
                                          showarrow = F,
                                          x = 10 , 
                                          y = 800),
                       showlegend = FALSE)
  
  return(fig)
}

year = seq(from = 2010, to = 2020)

plot_list = list()

for(year in year){
  fig = points_plot(year)
  plot_list = append(plot_list, fig)
}



fig_f = subplot(list(points_plot(2010), 
                     points_plot(2011),
                     points_plot(2012),
                     points_plot(2013),
                     points_plot(2014),
                     points_plot(2015),
                     points_plot(2016),
                     points_plot(2017),
                     points_plot(2018),
                     points_plot(2019),
                     points_plot(2020)), nrows = 5)


fig_f


## Podium Gains and No Podium Start
races = races.csv
constructor_standings = constructor_standings.csv
constructors=constructors.csv
results = results.csv


season_winner = champion_constructor(2010, 2020)
colnames(season_winner) = c("Year","Constructor_Champion")

podium_gain = sqldf("
                    select races.year,
                           constructors.name,
                           sum(case when results.grid > 3 and results.position < 4 then 1 else 0 end) as PodiumGain,
                           sum(case when results.grid > 3 then 1 else 0 end) as NoPodiumStart
                    from results
                    
                    join constructors
                    on constructors.constructorId = results.constructorId
                    
                    join races
                    on races.raceId = results.raceId
                    
                    join season_winner
                    on season_winner.year = races.year
                    
                    where (races.year between 2010 and 2020) and constructors.name in ('Ferrari', season_winner.Constructor_Champion)
                    
                    group by races.year, constructors.name
      
                  ")

podium_gain = podium_gain %>% mutate(name = case_when(
                                     name == "Ferrari" ~ "Ferrari",
                                     name == "Red Bull" ~ "Champion Team",
                                     name == "Mercedes" ~ "Champion Team"
))

podium_gain$percent_podium = podium_gain$PodiumGain / podium_gain$NoPodiumStart

perc_pod_df =  podium_gain %>% 
              select(year, name, percent_podium) %>% 
              pivot_wider(names_from = name, values_from = percent_podium)


no_pod_df = podium_gain %>% 
  select(year, name, NoPodiumStart) %>% 
  pivot_wider(names_from = name, values_from = NoPodiumStart)


fig = plot_ly(perc_pod_df, 
              x = ~year, 
              y = ~Ferrari,
              name = 'Ferrari', 
              type = 'scatter', 
              mode = 'lines',
              line = list(color = "#eb4034"))

fig = fig %>% add_trace(y = ~`Champion Team`,
                        name = 'Champion Team',
                        line = list(color = "#36b1b5"))

fig = fig %>% layout(title = "% Podium Gain (Ferrari vs Champion Team)",
                     xaxis = list(title = "Year"),
                     yaxis = list(title = "% Podium Gain"))
fig



fig = plot_ly(no_pod_df, 
              x = ~year, 
              y = ~Ferrari,
              name = 'Ferrari', 
              type = 'scatter', 
              mode = 'lines',
              line = list(color = "#eb4034"))

fig = fig %>% add_trace(y = ~`Champion Team`,
                        name = 'Champion Team',
                        line = list(color = "#36b1b5"))

fig = fig %>% layout(title = "No Podium Start (Ferrari vs Champion Team)",
                     xaxis = list(title = "Year"),
                     yaxis = list(title = "No Podium Start"))
fig



library(gapminder)
gapminder

## Season Points Vettel and Hamilton

  races = races.csv
  results = results.csv
  status = status.csv
  
  # Filtering data for Hamilton and Vettel
  drivers = drivers.csv %>% filter(code == "HAM" | code == "VET")
  results = results.csv %>% filter((driverId == 1 | driverId == 20) & position == "1") 
  constructors = constructors.csv
  
  # Creating a data set for year wise cumulative victories
  df_victories = results.csv %>% 
                  filter((driverId == 1 | driverId == 20) & position == "1") %>% 
                  inner_join(races.csv, by = "raceId") %>% 
                  inner_join(drivers.csv, by = "driverId")%>% 
                  select(driverRef, year, raceId, position) %>% 
                  arrange(driverRef, year) %>% 
                  group_by(driverRef) %>% 
                  mutate(Victories = cumsum(position)) %>% 
                  group_by(driverRef, year)  
  
  
  # Looking at the Victories volume for Both Vettel and Hamilton
  fig = df_victories %>% 
    plot_ly(x = ~driverRef ,
            y = ~victories,
            text = ~driverRef,
            type = 'bar') %>% 
    layout(xaxis = list(title = "Driver"),
           title = "Hamilton vs Vettel",
           yaxis = list(title = "Victories"))
  
  
  

# Poles
drivers.csv


vet_poles = results.csv %>% 
  filter(driverId == 20) %>% 
  inner_join(races.csv, by = 'raceId') %>% 
  select(driverId,constructorId, date, grid) %>% 
  arrange(driverId, date) %>% 
  filter(grid == 1) %>% 
  group_by(driverId) %>% 
  mutate(poles = cumsum(grid)) %>% 
  inner_join(constructors.csv, by = "constructorId") %>% 
  select(driverId, date, poles , name) %>% inner_join(drivers.csv, by = "driverId") %>% 
  select(driverId, date,poles, name, surname)

ham_poles = results.csv %>% 
  filter(driverId == 1) %>% 
  inner_join(races.csv, by = 'raceId') %>% 
  select(driverId,constructorId, date, grid) %>% 
  arrange(driverId, date) %>% 
  filter(grid == 1) %>% 
  group_by(driverId) %>% 
  mutate(poles = cumsum(grid)) %>% 
  inner_join(constructors.csv, by = "constructorId") %>% 
  select(driverId, date, poles , name) %>% inner_join(drivers.csv, by = "driverId") %>% 
  select(driverId, date,poles, name, surname)


poles_plot = function(df, constructor1 = 'McLaren', constructor2 = 'Mercedes'){
  
  
  const1 = df %>% filter(name == constructor1)
  const2 = df %>% filter(name == constructor2)
  
  fig_poles = plot_ly(x = ~const1$date, 
                      y = ~const1$poles, 
                      type = 'scatter', 
                      mode = 'lines', 
                      fill = 'tozeroy',
                      name = constructor1)
  
  fig_poles = fig_poles %>% add_trace(x = ~const2$date, 
                                      y = ~const2$poles, 
                                      name = constructor2, 
                                      fill = 'tozeroy')
  if(constructor1 == 'McLaren'){
    chart_title = 'Career Poles (Hamilton)'
  }else{
    chart_title = 'Career Poles (Vettel)'
  }
  
  fig_poles = fig_poles %>% layout(xaxis = list(title = 'Date'),
                                   yaxis = list(title = 'Poles'),
                                   title = chart_title)
  
  fig_poles
}

poles_plot(ham_poles, 'McLaren','Mercedes')
poles_plot(vet_poles, 'Red Bull','Ferrari')



# Victories

ham_wins =   results.csv %>% 
  filter(driverId == 1 & position == "1") %>% 
  inner_join(races.csv, by = "raceId") %>% 
  inner_join(drivers.csv, by = "driverId") %>% 
  select(driverId,constructorId, date, position) %>% 
  mutate(position = as.numeric(position)) %>% arrange(driverId, date) %>% 
  filter(position == 1) %>% group_by(driverId) %>% mutate(Wins = cumsum(position)) %>% 
  inner_join(constructors.csv, by = "constructorId") %>% 
  select(driverId, date, Wins , name) %>% inner_join(drivers.csv, by = "driverId") %>% 
  select(driverId, date,Wins, name, surname)

vettel_wins = results.csv %>% 
  filter(driverId == 20 & position == "1") %>% 
  inner_join(races.csv, by = "raceId") %>% 
  inner_join(drivers.csv, by = "driverId") %>% 
  select(driverId,constructorId, date, position) %>% 
  mutate(position = as.numeric(position)) %>% arrange(driverId, date) %>% 
  filter(position == 1) %>% group_by(driverId) %>% mutate(Wins = cumsum(position)) %>% 
  inner_join(constructors.csv, by = "constructorId") %>% 
  select(driverId, date, Wins , name) %>% inner_join(drivers.csv, by = "driverId") %>% 
  select(driverId, date,Wins, name, surname)


vict_plot = function(df, constructor1 = 'McLaren', constructor2 = 'Mercedes'){
  
  const1 = df %>% filter(name == constructor1)
  const2 = df %>% filter(name == constructor2)
  
  fig_poles = plot_ly(x = ~const1$date, 
                      y = ~const1$Wins, 
                      type = 'scatter', 
                      mode = 'lines', 
                      fill = 'tozeroy',
                      name = constructor1)
  
  fig_poles = fig_poles %>% add_trace(x = ~const2$date, 
                                      y = ~const2$Wins, 
                                      name = constructor2, 
                                      fill = 'tozeroy')
  if(constructor1 == 'Mclaren'){
    chart_title = 'Career Wins (Hamilton)'
  }else{
    chart_title = 'Career Wins (Vettel)'
  }
  
  fig_poles = fig_poles %>% layout(xaxis = list(title = 'Date'),
                                   yaxis = list(title = 'Victories'),
                                   title = chart_title)
  return(fig_poles)
}

vict_plot(ham_wins)
vict_plot(vettel_wins, 'Red Bull', 'Ferrari')


# Constructor races won
# constructor id 6 - Ferrari,9 - Red Bull, 131 - Mercedes
races.csv
constructors.csv


races = results.csv %>% 
        filter(constructorId == 6 | constructorId == 9 | constructorId == 131) %>% 
        inner_join(races.csv, by = "raceId") %>%
        filter(year >= 2014 & year <= 2020) %>%
        filter(position == "1") %>% 
        group_by(constructorId) %>% summarise(n = n()) %>% 
        inner_join(constructors.csv, by = "constructorId")
  
fig = plot_ly(
  x = races$name,
  y = races$n,
  type = "bar"
)

fig = fig %>% 
  layout(title = "Total Wins(2014-2020)",
         xaxis = list(title = "Constructor"),
         yaxis = list(title = "Win Count"))

fig

poles = 
results.csv %>% 
  filter(constructorId == 6 | constructorId == 9 | constructorId == 131) %>% 
  inner_join(races.csv, by = "raceId") %>%
  filter(year >= 2014 & year <= 2020 & grid == 1) %>% 
  select(constructorId, raceId, date, grid) %>% 
  group_by(constructorId) %>% mutate(poles = cumsum(grid)) %>%
  ungroup() %>% 
  inner_join(constructors.csv, by = "constructorId") %>% 
  select(date, name, poles) %>% 
  pivot_wider(names_from = name, values_from = poles)


poles = poles %>% fill(Mercedes, .direction = "down")
poles = poles %>% fill(Ferrari, .direction = "down")
poles = poles %>% fill(`Red Bull`, .direction = "down")
poles = poles %>% replace_na(list(Ferrari = 0, `Red Bull` = 0))

poles


fig = plot_ly(poles, 
               x = ~date, 
               y = ~Mercedes, 
               name = 'Mercedes', 
               type = 'scatter', 
               mode = 'none', 
               stackgroup = 'one', 
               fillcolor = '#32b8c9')

fig = fig %>% add_trace(y = ~Ferrari, name = 'Ferrari', fillcolor = '#c93237')
fig = fig %>% add_trace(y = ~`Red Bull`, name = 'Red Bull', fillcolor = '#4432c9')
fig = fig %>% layout(title = "Pole Positions (2014 - 2020)",
                     xaxis = list(title = "Year"),
                     yaxis = list(title = "Poles Count"))

fig