#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(leaflet)
library(plotly)


#
fluidPage(theme="bootstrap_solar.css",
  

  # Application title
  headerPanel("SF Restaurants Inspection"),
  
  # Sidebar with a select inputs for month, year and risk category
  sidebarLayout(
    sidebarPanel(
      
      div(style="display: inline-block;vertical-align:top; width: 120px;",
          selectInput("year", label = "Select Year:", 
                  choices = list("2014" = '2014', "2015" = '2015', "2016" = '2016'),
                  selected= "2014")),
      
      div(style="display: inline-block;vertical-align:top; width: 120px;",
          selectInput("month", label = "Select Month:", 
                  choices = list("Jan"='01',
                                 "Feb"='02',
                                 "Mar"='03',
                                 "Apr"='04',
                                 "May"='05',
                                 "Jun"='06',
                                 "Jul"='07',
                                 "Aug"='08',
                                 "Sep"='09',
                                 "Oct"='10',
                                 "Nov"='11',
                                 "Dec"='12',
                                 "None"='None'), selected= "Jan")),
      
       div(style="display: inline-block;vertical-align:top; width: 120px;",
          selectInput("day", label = "Select Day:",choices=unique_days)), #choices=list(unique_days)
      
      
      selectInput("risk_cat",
                   "Risk Category:",
                   choices = sort(factor(unique(SF_rest$risk_cat), levels=c("No Risk","Low Risk", "Moderate Risk", "High Risk")))
                  ),
                   
    
      checkboxInput("checkbox", label = ("Display High Risk Violations Heatmap by Postcode "), value= FALSE) 
           
                  
                         
       
       
       ),
    
    # Show SF map with location markers for restaurants
    mainPanel(
      tabsetPanel(
        tabPanel("SF Restaurant Location Map",leafletOutput("map", width = "100%", height = 400)),
        tabPanel("Time Series: Number of Risk Violations", dygraphOutput("countrisk")),
        tabPanel("Bar Chart: Risk Violations by SF Postcodes  ", plotlyOutput("postcode", width="100%", height=400)),
        tabPanel("Bar Chart: Top 20 Restaurants Postcode 94133", plotlyOutput("name_postcode", width="100%", height=400))
        
        
      )
      
      
    )
    
  )
)
