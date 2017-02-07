# create data for plotting on a map
library(ggmap)
library(zipcode)
library(dplyr)
library(lubridate)

data(zipcode)

cust_info <- read.csv('cust_info.csv')
cust_info$Birthday <- as.Date(cust_info$Birthday)
cust_trans <- read.csv('cust_trans.csv')
cust_trans$Date <- as.Date(cust_trans$Date)

cust_trans %>% 
  group_by(Cust_ID) %>% 
  summarise(Date = min(Date),
            Reward = max(Reward_A,na.rm = T))%>% 
  mutate(year = year(Date),
         year.month = paste(year(Date), month(Date), sep = '.')) -> first_trans

cust_trans %>%  
  group_by(Cust_ID) %>% 
  summarise(max_date = max(Date)[1],
            censored = ifelse('CL' %in% Transaction,0,1)) %>% 
  mutate(Cov_ID = paste0(Cust_ID,'A'))-> surv_cust_trans

cust_info %>% 
  select(Birthday, Cust_ID) -> birthday

surv_cust_trans <- merge(surv_cust_trans, birthday, by = 'Cust_ID')
surv_cust_trans%>% 
  mutate(age = as.numeric(max_date - Birthday)/365.25) %>% 
  select(-c(Birthday, max_date, Cov_ID)) -> age_cust_trans

names(cust_info)[8] <- 'zip'
cust_trans <- merge(first_trans, age_cust_trans, by = 'Cust_ID')
geocode <- merge(cust_info, zipcode, by = 'zip')
geocode <- merge(geocode, cust_trans, by = 'Cust_ID')

geocode %>% 
  group_by(zip) %>% 
  summarise(lat = latitude[1],
            lon = longitude[1],
            pct_male = (sum(Gender == 'male')/n()),
            race_w = sum(Race == 'W')/n(),
            race_A = sum(Race == 'A')/n(),
            race_B = sum(Race == 'B')/n(),
            race_H = sum(Race == 'H')/n(),
            race_O = sum(Race == 'O')/n(),
            year = min(year, na.rm=T),
            freq = n(),
            avg_reward = as.integer(mean(Reward, na.rm = T)),
            pct_claim = sum(!is.na(Reward))/n(),
            avg_age = mean(age, na.rm = T),
            pct_current_cust = sum(censored == 1)/n(),
            state = state[1]) -> geocode2




library(plotly)


######################
# marker styling
geocode2$hover <- with(geocode2, paste('Zipcode',zip,'<br>',
                                     "Percent Male", round(pct_male,2),'<br>', "Average Reward ",
                                     round(avg_reward,2), "<br>", "Percent Claiming Reward", round(pct_claim,2), 
                                     '<br>', "Average Age", round(avg_age,2), 
                                     "<br>", "Percent Current Customers", round(pct_current_cust,2)))


m <- list(
  colorbar = list(title = "Average Reward Amount"),
  size = 8, opacity = 0.8, symbol = 'circle'
)

# geo styling
g <- list(
  scope = 'usa',
  projection = list(
    type = 'conic conformal',
    rotation = list(
      lon = -100
    )
  ),
  showland = TRUE,
  landcolor = toRGB("gray95"),
  subunitcolor = toRGB("gray85"),
  countrycolor = toRGB("gray85"),
  countrywidth = 0.5,
  subunitwidth = 0.5
)


plot_ly(geocode2, lat = lat, lon = lon, text = hover ,size = freq, color = avg_reward,
        type = 'scattergeo', mode = 'markers',
        marker = m) %>%
  layout(title = 'IAA Insurance', geo = g)


