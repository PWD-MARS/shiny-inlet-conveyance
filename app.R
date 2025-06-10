#4/7/2021 Nick Manna, ARKF, Inc. 
#This file, app.R, works with the sourced inlet_conveyance.R file to create the PWD GSI MARS Inlet Conveyance Tests app
#See Fieldwork App R script for more details 

# SET UP
#0.0: load libraries -------------
#shiny
library(shiny)
#pool for database connections
library(pool)
#odbc for database connections
library(odbc)
#tidyverse for data manipulations
library(tidyverse)
#shinythemes for colors
library(shinythemes)
#lubridate to work with dates
library(lubridate)
#shinyjs() to use easy java script functions
library(shinyjs)
#DT for datatables
library(DT)
#reactable for reactable tables
library(reactable)
# driver
library(RPostgres)

#0.1: database connection and global options --------

#set default page length for datatables
options(DT.options = list(pageLength = 15))

#set db connection
#using a pool connection so separate connnections are unified
#gets environmental variables saved in local or pwdrstudio environment
#poolConn <- dbConnect(odbc::odbc(), dsn = "mars14_datav2", uid = Sys.getenv("shiny_uid"), pwd = Sys.getenv("shiny_pwd"))

# DB connections & functions
poolConn <- dbPool(RPostgres::Postgres(),
                      host = "PWDMARSDBS1.pwd.phila.local",
                      port = 5434,
                      dbname = "mars_prod",
                      user = Sys.getenv("shiny_uid"),
                      password = Sys.getenv("shiny_pwd")
)

#js warning about leaving page
jscode <- 'window.onbeforeunload = function() { return "Please use the button on the webpage"; };'

#0.2 source scripts.  ----
#each script contains a module, which includes UI and server code
source("inlet_conveyance.R")


#1: UI FUNCTION -----
#initialize variables for UI and call all UI functions
#call all the UI functions

ui <- function(req){
  
  #1.1: load required variables -----
  #define global variables that will be required each time the UI runs
  
  #query site names (non SMP)
  site_name_query <- "select * from fieldwork.tbl_site_name_lookup"
  site_names <- odbc::dbGetQuery(poolConn, site_name_query) %>% 
    dplyr::arrange(site_name) %>% 
    dplyr::pull()
  
  #construction phase types
  con_phase <- dbGetQuery(poolConn, "select * from fieldwork.tbl_con_phase_lookup")
  
  #this function adds a little red star to indicate that a field is required. It uses HTML, hence "html_req"
  html_req <- function(label){
    HTML(paste(label, tags$span(style="color:red", tags$sup("*"))))
  }
  
  #this function adds a blue dagger to indicate that a field is required for future tests. It uses HTML. it is slightly Christian
  future_req <- function(label){
    HTML(paste(label, tags$span(style="color:blue", tags$sup("†"))))
  }
  
  #field test priority
  priority <- dbGetQuery(poolConn, "select * from fieldwork.tbl_field_test_priority_lookup")
  
  #project work numbers
  work_number <- dbGetQuery(poolConn, "select distinct worknumber from external.tbl_projectbdv") %>% pull()
  
  # 1.2: actual UI------------------------
  
  #use tagList so tags and shinyjs can be called without being inside of the navbarPage. When they're inside navbarpage, they create small invisible fake tabs that take up space and act weird when clicked on
  tagList(
    #call jscode to warn when leaving page
    tags$head(tags$script(jscode)),
    #must call useShinyjs() for shinyjs() functionality to work in app
    useShinyjs(),
    #navbarPage("Fieldwork", theme = shinytheme("cerulean"), id = "inTabset",
    #Inlet Conveyance (Add/Edit Inlet Conveyance Test, View Inlet Conveyance Tests, View Future Inlet Conveyance Tests)
    inlet_conveyanceUI("inlet_conveyance", work_number = work_number, html_req = html_req,
                       con_phase = con_phase, priority = priority, site_names = site_names, future_req = future_req),
  )
  # )
  
}

# 2: server function ----
#call modules, referencing the UI names above. These are functions, so any data originating outside the function needs to be named as an argument, whether it is lookup data, or from another tab. Modules need to be assigned to variables so they can be used in other module functions. 
server <- function(input, output, session) {
  
  # 2.1: required variables -----
  #define global variables that will be defined each time server runs
  
  #con phase
  con_phase <- dbGetQuery(poolConn, "select * from fieldwork.tbl_con_phase_lookup")
  
  #all system ids
  sys_id <- odbc::dbGetQuery(poolConn, paste0("select distinct system_id from external.mat_assets")) %>% 
    dplyr::arrange(system_id) %>% 
    dplyr::pull()
  
  #replace special characters with friendlier characters
  special_char_replace <- function(note){
    
    note_fix <- note %>% 
      str_replace_all(c("•" = "-", "ï‚§" = "-", "“" = '"', '”' = '"'))
    
    return(note_fix)
    
  }
  
  # 2.2: Server Module functions ---------------------------
  #Inlet Conveyance
  inlet_conveyance <- inlet_conveyanceServer("inlet_conveyance", parent_session = session, poolConn = poolConn, con_phase = con_phase,
                                             sys_id =sys_id, special_char_replace = special_char_replace)
  
}

#Run this function to run the app!
shinyApp(ui, server)