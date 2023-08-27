# import the necessary packages
library(leaflet)
library(RPostgres)
library(shiny)
library(shinyalert)
library(shinyWidgets)
library(shinythemes)
library(tidyverse)
library(slickR)
library(tidyr)
library(dplyr)
library(DT)
library(ggplot2)
library(shinyjs)

# connection to Vancouver database
con = dbConnect(
  drv = dbDriver('Postgres'), 
  dbname = 'projNBA_van',
  host = 'db-postgresql-nyc1-44203-do-user-8018943-0.b.db.ondigitalocean.com', 
  port = 25060,
  user = 'nba_van', 
  password = 'AVNS_kftmfvgVH_B09dvCDSl'
)

# coordinates of Rogers arena----
ra <- c(49.2778, -123.1088)

# players----
plyrs <- dbGetQuery(
  con, 
  'SELECT * FROM players JOIN player_stats using (player_id) ORDER BY player_id'
)

# coach----
coach <- dbGetQuery(
  con,
  "SELECT * FROM staff_members WHERE first_name = 'Gregg' AND last_name = 'Popovich'"
)

# staff_members----
staff <- dbGetQuery(
  con,
  "SELECT * FROM Staff_members OFFSET 1"
)

# community image----
image_list <- paste0("community", 1:5, ".jpg")

nbaTeams = c(
  'Los Angeles Lakers',
  'Portland Trail Blazers',
  'Golden State Warriors',
  'Utah Jazz',
  'Boston Celtics',
  'Brooklyn Nets',
  'New York Knicks',
  'Philadelphia 76ers',
  'Toronto Raptors',
  'Chicago Bulls',
  'Cleveland Cavaliers',
  'Detroit Pistons',
  'Indiana Pacers',
  'Milwaukee Bucks',
  'Atlanta Hawks',
  'Charlotte Hornets',
  'Miami Heat',
  'Orlando Magic',
  'Washington Wizards',
  'Denver Nuggets',
  'Minnesota Timberwolves',
  'Oklahoma City Thunder',
  'Los Angeles Clippers',
  'Phoenix Suns',
  'Sacramento Kings',
  'Dallas Mavericks',
  'Houston Rockets',
  'Memphis Grizzlies',
  'New Orleans Pelicans',
  'San Antonio Spurs'
)
