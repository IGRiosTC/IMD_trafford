library(tidyverse) 

lsoa_lookup <- read_csv("https://www.traffordDataLab.io/spatial_data/lookups/statistical_lookup.csv") %>%
  filter(lad11nm=="Trafford")%>%
  select(area_code = lsoa11cd) %>% 
  unique()

df <- read_csv("https://www.traffordDataLab.io/open_data/imd_2015/IMD_2015_long.csv") %>%
  filter(measure=="Decile", 
         lsoa11cd %in% lsoa_lookup$area_code) %>%
  filter(!index_domain %in% c("Income Deprivation Affecting Children Index (IDACI)","Income Deprivation Affecting Older People Index (IDAOPI)")) %>% 
  mutate(index_domain=gsub("([A-Za-z]+).*", "\\1", df$index_domain)) %>%
  mutate(index_domain = factor(df$index_domain, levels = c("Index", "Income", "Employment", "Health", "Education", "Barriers", "Living", "Crime")))
                              
imd <-  df %>% 
  group_by(lsoa11cd) %>%
  spread(index_domain,value) 
  



write_csv(imd, "IMD_trafford.csv")

histo<- df %>%
  select(-c(lsoa11cd,measure))%>%
  group_by(index_domain,value)%>%
  summarise(freq=n()) %>%
  spread(value,freq)
  
write_csv(histo, "imd_freq.csv", na = "0")
