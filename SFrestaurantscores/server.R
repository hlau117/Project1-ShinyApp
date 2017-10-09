#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(plotly)
library(ggplot2)
library(tidyr)
library(leaflet)
library(dplyr)




function(input, output, session) {
  
  
  
  
  output$map <- renderLeaflet({
    #generate map
    
    leaflet(SF_rest) %>% 
      addTiles(urlTemplate = "https://api.mapbox.com/styles/v1/mapbox/streets-v10/tiles/256/{z}/{x}/{y}?access_token=pk.eyJ1IjoiaGxhdTExNyIsImEiOiJjajhlcjE4a3kxNzkzMzNuOHN2d3hiajkyIn0.U9VY6-Um2f99cfPj9n46Ow", 
               attribution ='<a href="https://www.mapbox.com/about/maps/">Mapbox</a> <a href="http://www.openstreetmap.org/copyright">OpenStreetMap</a> <strong><a href="https://www.mapbox.com/map-feedback/" target="_blank">Improve this map</a></strong>') %>%
      fitBounds(~min(long),~min(lat),~max(long), ~max(lat))
  })
  
 
  
  # add top 20 restaurant markers to map based on frequency for input risk level and date
  SF_top20_rest<- reactive({
    
    #condition 1 ( no input month and day value)
    if(input$month=="None" & input$day=="None" ){ 
      merge_mydate= input$year
      
      con_1= SF_rest %>%
        filter(risk_cat == input$risk_cat & year == merge_mydate) %>%
        group_by(name, long, lat) %>%
        mutate(count=n())%>%
        arrange(desc(count))%>%
        distinct(name, address, postcode, descr,long, lat, count)
      
      if(empty(con_1)!=TRUE){
        con_1=aggregate(descr~name + address + postcode + long + lat + count,
                      con_1, paste, collapse=", ")
        
        con_1= con_1 %>% 
          arrange(desc(count)) %>%
          head(20)
      }
      
    #condition 2 ( no input day value)
    }else if(input$day=="None" & input$month!="None"){  
      merge_mydate= paste(input$year, input$month,sep='-')
      
      con_2= SF_rest %>%
        filter(risk_cat == input$risk_cat & mmyydate == merge_mydate) %>%
        group_by(name, long, lat) %>%
        mutate(count=n())%>%
        distinct(name, address, postcode, descr,long, lat, count)
        
        #check if data frame is empty
        if(empty(con_2)!=TRUE){
          con_2=aggregate(descr~name + address + postcode + long + lat + count, 
                          con_2, paste, collapse=", ")
          
          con_2= con_2 %>%
            arrange(desc(count)) %>%
            head(20)
        }else{
            con_2
          }
    
    #condition 3( input all 3: year, month and day )
    }else{
      merge_mydate= paste(input$year, input$month, input$day, sep='-')
      
      con_3=SF_rest %>%
        filter(risk_cat == input$risk_cat & fulldate == merge_mydate) %>%
        group_by(name, address, long, lat) %>%
        mutate(count=n())%>%
        distinct(name, address, postcode, descr,long, lat, count)
      
      #check if data frame is not empty
      if(empty(con_3) != TRUE){
        con_3=aggregate(descr~name + address + postcode + long + lat + count,
                        con_3, paste, collapse=", ")
        con_3= con_3 %>%
          arrange(desc(count)) %>%
          head(20)
      
      
      }else{
        
        con_3
      }
        
        
        
    }
    
    
  })
  
  

  #add markers to map based on input risk category and date
  observeEvent(SF_top20_rest(),{
    top20markers <- SF_top20_rest()
    
    
   #check if there is data for date input
    
    if(nrow(top20markers)<1){
      leafletProxy("map", data = top20markers)%>%
      clearMarkers()
    }
      
    
    if(nrow(top20markers)>0){
    leafletProxy("map", data = top20markers) %>%
      clearMarkers()%>%
      addAwesomeMarkers(lng=~long,
                 lat=~lat, 
                 popup= paste("Address:", paste(top20markers$address, top20markers$postcode, sep=','), "<br>",
                              paste(paste("#",input$risk_cat,"Violations", sep=" "), top20markers$count, sep=":"), "<br>",
                              "Violation Description:", top20markers$descr, "<br>"
                              ),
                 label= ~name
                              
      )
    }
      
  })
  
  
  
  observeEvent(input$checkbox,{
    #plot chloropleth on number of high risk violations by postcodes
    if(input$checkbox==TRUE){
      pal = colorNumeric(
      palette = "YlOrRd",
      domain = postcode_bounds@data$postcode_count
      )
      
      casecountpopup = paste0("<strong>", postcode_bounds@data$postcode, "</strong>", "<br>", 
                              "Number of Cases: ", postcode_bounds@data$postcode_count, "<br>", 
                              "Violations Ratio: ", format(postcode_bounds@data$postcode_ratio, digits=4))
      
      
      title=paste0("<strong>","SF High Risk Counts by Top 10 Postcodes","</strong>","<br>",
                   "(Contributes 65% of HR Violations in SF)", "<br>")
      
      leafletProxy("map", data = postcode_bounds)%>%
        clearMarkers()%>%
        clearControls() %>%
        addPolygons(fillColor = ~pal(postcode_count),
                  fillOpacity = 0.85,
                  color="#BDBDC3",
                  weight=1,
                  popup= casecountpopup)%>%
        addLegend(position = "bottomleft", pal = pal, values = ~postcode_count, title = title)
      
    }else if(input$checkbox==FALSE){
        
        leafletProxy("map", data=postcode_bounds)%>%
          clearControls()%>%
          clearShapes()
          
        
      }
    
    
    
    
    
    
  })
  
  
  
  
  
  
  
  output$postcode <- renderPlotly({
   #plot risk category restaurants by postcode
     
     
     plot_total_postcode=total_postcode%>%
       ggplot(aes(x=postcode, y=postcode_count)) + 
       geom_bar(aes(fill= risk_cat), stat='identity', position='dodge') +
       ggtitle("Number of Different Risk Category Violations by SF Postcodes")+
       labs(y='count') + 
       theme(axis.text.x = element_text(angle = 20, hjust = 1))
       
      ggplotly(plot_total_postcode)
       
     
     
  })
  
  output$name_postcode <- renderPlotly({
    #plot high risk category restaurants by postcode 94133
    
    
    name_postcode_94133=SF_rest%>%
      filter(postcode==94133 & risk_cat=="High Risk")%>%
      group_by(name)%>%
      mutate(highriskcount=n())%>%
      distinct(name, postcode,highriskcount)%>%
      arrange(desc(highriskcount))%>%
      head(20)%>%
      ggplot(aes(x= postcode, y=highriskcount)) + 
      geom_bar(aes(fill= name), stat='identity', position='dodge') +
      ggtitle("Top 20 Restaurants with High Risk Category Violations in Postcode 94133")+
      labs(y='count') + 
      theme(axis.text.x = element_text(angle = 20, hjust = 1))
    
    ggplotly(name_postcode_94133)
    
    
    
  })
   
  
  output$countrisk <- renderDygraph({
     #plot  risk cat over time
     
     LR=SF_rest %>% 
       filter(risk_cat=="Low Risk") %>% 
       group_by(fulldate)%>%
       summarise(risk_count= n())
     
     LR_tseries= xts(LR$risk_count, order.by=LR$fulldate)
     
     
     MR=SF_rest %>% 
       filter(risk_cat=="Moderate Risk") %>% 
       group_by(fulldate)%>%
       summarise(risk_count= n())
     
     MR_tseries= xts(MR$risk_count, order.by=MR$fulldate)
     
     
     HR=SF_rest %>%
       filter(risk_cat=="High Risk") %>% 
       group_by(fulldate)%>%
       summarise(risk_count= n())
     
     HR_tseries= xts(HR$risk_count, order.by=HR$fulldate)
     
     NR=SF_rest %>%
       filter(risk_cat=="No Risk") %>% 
       group_by(fulldate)%>%
       summarise(risk_count= n())
     
     NR_tseries= xts(NR$risk_count, order.by=NR$fulldate)
     
     
     SF_tseries= cbind(NR=NR_tseries, LR=LR_tseries, MR=MR_tseries, HR=HR_tseries)
     
     
     dygraph(SF_tseries, main=" Frequency of Various Risk Category Violations of SF Restaurants 2014-2016") %>%
       dyAxis("y", label = "Risk Category Count", valueRange = c(0,80),axisLabelColor="white")%>%
       dyAxis("x", label = "Date", axisLabelColor="white")%>%
       dySeries("LR", label = "Low Risk", color='green') %>%
       dySeries("MR", label = "Moderate Risk", color='orange')%>%
       dySeries("HR", label = "High Risk", color= 'red')%>%
       dySeries("NR", label = "No Risk", color='blue')%>%
       dyLegend(show = "always", hideOnMouseOut = FALSE)%>%
       dyOptions(stackedGraph = FALSE) %>%
       dyRangeSelector(height = 20)
       
       
     
        })
  
}
