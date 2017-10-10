
library(rgdal)
library(plyr)
library(dplyr)


load("Clean_SF_rest.rda")

# Create Dataframe of Risk Violations By Postcodes

  hr_postcode=SF_rest %>%
    filter(risk_cat=="High Risk")%>%
    group_by(postcode) %>%
    mutate(num_rest=n_distinct(name),
           postcode_count= n())%>%
    distinct(postcode,risk_cat, postcode_count, num_rest)%>%
    mutate(postcode_ratio= postcode_count/3601)%>%
    arrange(desc(postcode_count))
    
  
  lr_postcode=SF_rest %>%
    filter(risk_cat=="Low Risk")%>%
    group_by(postcode) %>%
    mutate(postcode_count=n()) %>%
    distinct(postcode,risk_cat, postcode_count)%>%
    arrange(desc(postcode_count))
    
  
  mr_postcode=SF_rest %>%
    filter(risk_cat=="Moderate Risk")%>%
    group_by(postcode) %>%
    mutate(postcode_count=n()) %>%
    distinct(postcode,risk_cat, postcode_count)%>%
    arrange(desc(postcode_count))
    
  
  nr_postcode=SF_rest %>%
    filter(risk_cat=="No Risk")%>%
    group_by(postcode) %>%
    mutate(postcode_count=n()) %>%
    distinct(postcode,risk_cat, postcode_count)%>%
    arrange(desc(postcode_count))
    
  
  
  total_postcode= rbind(nr_postcode,lr_postcode,mr_postcode,hr_postcode)
  
  
  #computation for creating High Risk Violations Heatmap
  
  postcode_bounds= readOGR(dsn = "C:/Users/hans/OneDrive/Documents/Data_Science/NYCDSA/Project1-ShinyApp/SFrestaurantscores",
                           layer = "cb_2016_us_zcta510_500k")
  
  
  
  #if (input$checkbox== 'High Risk')
  hr_density=total_postcode %>%
    filter(risk_cat=="High Risk") %>%
    select(postcode, postcode_count, postcode_ratio, num_rest)
  
  hr_density$postcode= as.factor(hr_density$postcode)
  
  names(postcode_bounds@data)[names(postcode_bounds@data)== "ZCTA5CE10"] = "postcode"
  
  postcode_bounds= postcode_bounds[postcode_bounds@data$postcode %in% levels(hr_density$postcode),]
  
  postcode_bounds@data= postcode_bounds@data %>%
    filter(postcode_bounds@data$postcode %in% levels(hr_density$postcode)) %>% 
    droplevels()
  
  
  
  postcode_bounds@data= postcode_bounds@data %>% inner_join(hr_density, by='postcode')
  
  
  #save two rda file
  save(postcode_bounds, file="Spatial_Zipcodes.rda")
  save(total_postcode, file= "total_postcode.rda")
  