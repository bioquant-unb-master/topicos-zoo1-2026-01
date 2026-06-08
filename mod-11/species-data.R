library(rebird)
library(tidyverse)
# Look at eBird taxonomy
rebird:::tax
species_code('Neothraupis fasciata')
# Find all eBird reportings of red kite in Spain in the last 30 days: 
# Find all eBird reportings of red kite in Spain in the last 30 days: 
ebirdregion(loc = 'ES', species = 'redkit1', back=30, key='71m25u8fb27k')
# Download eBird detection in Spain for the months April-June 2022:

ebd_Esp_2022 <- bind_rows(
    # Apply the download function to each day in the provided date sequence and collect output in a list
    lapply(seq(as.Date('2022/04/01'),as.Date('2022/06/30'),'days'), FUN=function(x){
      ebirdhistorical(loc = 'ES', date=x, fieldSet='full', key='71m25u8fb27k')
    }))

days2=seq(from=as.Date('2022/04/01'),to=as.Date('2022/06/30'),by=1)
FUN2=function(x){
  ebirdhistorical(loc = 'ES', date=x, fieldSet='full', key='71m25u8fb27k')}        
ebd_Esp_2022 <- bind_rows(
  # Apply the download function to each day in the provided date sequence and collect output in a list
  lapply(days2,FUN2))
saveRDS(ebd_Esp_2022, file = "spainebird.rds")
