Sys.setenv(TZ = 'GMT')
library(tidyverse)
library(plotly)
library(tidymodels)
library(ggridges)
library(syuzhet)
library(factoextra)

## read in data
#hosts <- readxl::read_xlsx('data/Hosts.xlsx',sheet = 'Sheet1') 
listing_raw <- read_csv('data/listings_out.csv') 

# get a summary of the data
listing_raw %>% skimr::skim()

## Data Cleaning ------------------------------------------------------------------
#Approach
# Remove rows with missing host_is_superhost 
# check prop of host category with missing information and impute using the median for the groups
# replace missing host_response_rate with "missing"
# drop square_feet,first and last review
# derive host_profile_age from host_since

# helper functions
# compute median after removing NA's
new_median <- function(x){
  median(x,na.rm = TRUE)
}

replace_with_median <- function(x ){
  if_else(is.na(listing_raw[[x]]),
          if_else(listing_raw$host_is_superhost == FALSE,
                  col_medians[[x]][1],
                  col_medians[[x]][2]),listing_raw[[x]])
}

df <- listing_raw %>% filter(!is.na(host_is_superhost)) %>% 
  select(host_is_superhost,where(is.numeric),-host_id,-id)

col_medians <- aggregate(df,by = df[,1],FUN = new_median)


# clean listing data
listing <- listing_raw %>% 
  select(-first_review,-last_review,-square_feet,-amenity_kitchen,-amenity_tv,-host_listings_count) %>% 
  mutate(
    host_response_time = if_else(is.na(host_response_time),"missing",host_response_time),
    host_response_time = factor(host_response_time,
                                levels = c("within an hour", "within a few hours",
                                           "within a day", "a few days or more", "missing")),
    bedrooms = if_else(is.na(bedrooms),
                       if_else(host_is_superhost == FALSE,col_medians$beds[1],col_medians$beds[2]),bedrooms),
    host_response_rate = replace_with_median('host_response_rate'),
    host_total_listings_count = replace_with_median('host_total_listings_count'),
    beds = replace_with_median('beds'),
    bedrooms = replace_with_median('bedrooms'),
    bathrooms = replace_with_median('bathrooms'),
    security_deposit_flag = if_else(is.na(security_deposit),0,1),
    security_deposit = replace_with_median('security_deposit'),
    cleaning_fee_flag = if_else(is.na(cleaning_fee),0,1),
    cleaning_fee = replace_with_median('cleaning_fee'),
    review_scores_rating = replace_with_median('review_scores_rating')/100,
    review_scores_accuracy = replace_with_median('review_scores_accuracy')/10,
    review_scores_cleanliness = replace_with_median('review_scores_cleanliness')/10,
    review_scores_checkin = replace_with_median('review_scores_checkin')/10,
    review_scores_communication = replace_with_median('review_scores_communication')/10,
    review_scores_location = replace_with_median('review_scores_location')/10,
    review_scores_value = replace_with_median('review_scores_value')/10,
    host_profile_age = lubridate::interval(start = host_since,end = lubridate::today())%/%lubridate::years(1)
) %>% 
filter(!is.na(host_is_superhost)) 


# summary of cleaned data
listing %>% skimr::skim()

# save cleaned listing data to file
#write_csv(listing,'data/listing_main.csv')


# Extract host information
hosts <- listing %>% group_by(host_id) %>% 
  summarise(host_response_rate = mean(host_response_rate),
            host_total_listings_count = mean(host_total_listings_count),
            accommodates = mean(accommodates),
            beds = round(mean(beds),1),
            bedrooms = round(mean(bedrooms),1),
            bathrooms = round(mean(bathrooms)),
            price = mean(price),
            security_deposit = mean(security_deposit),
            cleaning_fee = mean(cleaning_fee),
            guests_included = round(mean(guests_included),1),
            extra_people = round(mean(extra_people),1),
            minimum_nights = round(mean(minimum_nights),1),
            maximum_nights = round(mean(maximum_nights),1),
            availability_30 = round(mean(availability_30),1),
            availability_60 = round(mean(availability_60),1),
            availability_90 = round(mean(availability_90),1),
            availability_365 = round(mean(availability_365),1),
            number_of_reviews = round(mean(number_of_reviews),1),
            review_scores_rating = mean(review_scores_rating),
            review_scores_accuracy = mean(review_scores_accuracy),
            review_scores_cleanliness = mean(review_scores_cleanliness),
            review_scores_checkin = mean(review_scores_checkin),
            review_scores_communication = mean(review_scores_communication),
            review_scores_location = mean(review_scores_location),
            review_scores_value = mean(review_scores_value),
            days_since_calendar_updated = round(mean(days_since_calendar_updated),1),
            host_profile_age = round(mean(host_profile_age),1)
            ) %>% 
  left_join(y = listing %>% select(host_id,host_is_superhost,host_response_time,
                                   host_has_profile_pic,
                                   host_identity_verified) %>% unique(),
            by = c('host_id'= 'host_id'))

hosts %>% skimr::skim()


# save cleaned data to file
# write_csv(hosts,'data/Hosts_main.csv')


# Get review summaries
review_means <- hosts %>% group_by(host_is_superhost) %>% 
  summarise(
            rating = mean(review_scores_rating),
            accuracy = mean(review_scores_accuracy),
            cleanliness = mean(review_scores_cleanliness),
            checkin = mean(review_scores_checkin),
            communication = mean(review_scores_communication),
            location = mean(review_scores_location),
            value = mean(review_scores_value)
            )

# pivot
review_means_pvt <- pivot_longer(review_means,cols = c('rating','accuracy','cleanliness','checkin',
                                                       'communication','location','value'),
                                 names_to = "review_areas",values_to = "review_Scores")

# write to file
# write_csv(review_means_pvt,'data/review_means.csv')


## Visuals-----------------------------------
# Boxplot of review rating by host category
review_box_plot <- hosts %>% 
  select(host_is_superhost,review_scores_rating) %>%
  ggplot(aes(x =host_is_superhost, y = review_scores_rating, fill = host_is_superhost)) + 
  geom_boxplot() 

ggplotly(review_box_plot)


# Boxplot of price by host category and room_type
price_room_type_boxplot <- listing %>% 
  select(host_is_superhost,room_type,price) %>% 
  ggplot(aes(x = room_type, y = price,fill=host_is_superhost)) + 
  geom_boxplot() +
  theme_minimal()

price_room_type_boxplot


# boxplot of ratings against host_verification_status
rating_verified_boxplot <- listing %>% 
  select(host_is_superhost,host_identity_verified,review_scores_rating) %>% 
  ggplot(aes(x = host_is_superhost, y = review_scores_rating,fill = host_identity_verified)
  ) + 
  geom_boxplot() +
  theme_minimal()

rating_verified_boxplot


# ridgeplot of price against superhost, neighbourhood
neighbourhoods <- listing %>% 
  select(host_is_superhost,neighbourhood_cleansed,price) %>% 
  group_by(host_is_superhost,neighbourhood_cleansed) %>% 
  summarise(count = n()) %>%
  arrange(desc(count)) %>% ungroup()

selected_neighbourhoods <- unique(neighbourhoods$neighbourhood_cleansed)[c(1:5)] # ,17:22

#Distribution of prices in top neighbourhoods
prices_top_neighbourhoods_plot <- listing %>% 
  filter(neighbourhood_cleansed %in% selected_neighbourhoods) %>% 
  mutate(neighbourhood_cleansed = factor(neighbourhood_cleansed, levels = c(selected_neighbourhoods))) %>% 
  select(neighbourhood_cleansed,price) %>% arrange(desc(price)) %>% 
  ggplot(aes(x = price, y = fct_rev(neighbourhood_cleansed),
             fill = neighbourhood_cleansed)) + xlim(c(0,500)) +
  geom_density_ridges(stat = "binline",bins = 20) + ylab(label = "Neighbourhoods")

prices_top_neighbourhoods_plot




## Test some hypothesis --------------------------------
# 1. Do superhosts have significantly higher ratings
hosts %>% select(host_is_superhost,review_scores_rating) %>% 
  mutate( host_is_superhost = fct_rev(as.factor(host_is_superhost))) %>% 
  t.test(review_scores_rating ~ host_is_superhost,data = .,
         alternative = "greater",mu = 0 )

# Yes, superhosts have significantly higher ratings


#2. Do superhosts charge significantly higher prices
hosts %>% select(host_is_superhost,price) %>% 
  mutate( host_is_superhost = fct_rev(as.factor(host_is_superhost))) %>% 
  t.test(price ~ host_is_superhost,data = .,
         alternative = "greater",mu = 0 )

# No, they don't. There's no significant difference between prices between 
# listings by super and regular hosts


#3. Do prices of hosts differ based on response_time?
price_response_time <- listing %>% 
  select(host_is_superhost,price,host_response_time) %>% 
  aov(formula = price ~ host_is_superhost * host_response_time)

summary(price_response_time)  

# Effects of different response times on price is significant
# When coupled with host status, not so much


#4. Do Prices of hosts vary based on room type
price_room_type <- listing %>% 
  select(host_is_superhost,price,room_type) %>% 
  aov(formula = price ~ host_is_superhost * room_type)

summary(price_room_type)  

# Significant difference in prices based on room_type
# no significant effect when combined with host classification


#5. Do prices of listings differ based on host type and identity verification status?
price_host_identity_verified <- listing %>% 
  select(host_is_superhost,price,host_identity_verified) %>% 
  aov(formula = price ~ host_is_superhost * host_identity_verified)

summary(price_host_identity_verified)  

# No significant differences in prices between as a result of identity verification status


#6. Do rating of hosts differ based on identity verification status?
rating_verified <- hosts %>% 
  select(review_scores_rating,host_identity_verified) %>% 
  t.test(formula = review_scores_rating ~ fct_rev(as.factor(host_identity_verified)),
         data = ., alternative = "greater",mu = 0 )

rating_verified


#7. Do prices of listings vary based on neighbourhood?
price_neighbourhood <- listing %>% 
  select(host_is_superhost,price,neighbourhood_cleansed) %>% 
  aov(formula = price ~ host_is_superhost * neighbourhood_cleansed)

summary(price_neighbourhood)

# Variations in neighbourhoods significantly affect price



## Non-super agents that were close! --------------------------------------------------
## Criteria:
# response_rate within a day >= 90%
# no_bookings >= 10  _ will use avg no_reviews as proxy
# cancellation_rate - NA
# review score >= 96%


# Normal hosts that have superhost features
next_superhosts <- hosts %>% filter(
  host_is_superhost == FALSE &
    host_response_time == names(summary(hosts$host_response_time))[3] &
    host_response_rate >= 0.9 &
    number_of_reviews >= 10 &
    review_scores_rating >= 0.96
)


paste('Approximately',
      scales::percent(dim(next_superhosts)[1]/dim(hosts %>%
                                                    filter(host_is_superhost == FALSE))[1],accuracy = 0.1),
      'of normal hosts qualify for superhost status') 



## Extracting Review sentiments------------------------------------------------------------------------------------
reviews <- read.csv(file = 'data/reviews_details.csv')

reviews$sentiments <- get_sentiment(reviews$comments)

min(scale(reviews$sentiments)) 
max(scale(reviews$sentiments))


# review data with superhosts
reviews_new <- reviews %>% 
  left_join(listing %>% 
              select(id,host_id,host_is_superhost,review_scores_rating),
            by = c("listing_id" = "id"))

reviews_new %>% skimr::skim()


# average sentiment of superhosts
host_sentiments <- reviews_new %>% 
  group_by(host_id,host_is_superhost) %>% 
  summarise(sentiment = mean(sentiments), count = n()) %>% 
  arrange(desc(sentiment)) %>%  ungroup()

#write_csv(host_sentiments,'data/review_sentiments.csv')


# hosts with the worst sentiments of superhosts
# no superhosts in the bottom 20 based on sentiments
# superhosts have positive sentiments on average
reviews_new %>% 
  group_by(host_id,host_is_superhost) %>% 
  filter(host_is_superhost == 'FALSE') %>% 
  summarise(sentiment = mean(sentiments), count = n()) %>%
  arrange(desc(sentiment)) %>% head(20)

# hypothesis test for sentiments between superhosts and regulars - significant
sentiment_test <- reviews_new %>% group_by(host_id,host_is_superhost) %>%
  summarise(sentiment = mean(sentiments)) %>% 
  t.test(data = .,sentiment ~ fct_rev(factor(host_is_superhost)),alternative = "greater",mu = 0)

sentiment_test
# There's a significant difference in mean sentiments between normal and superhosts



# plot distribution of sentiments
# The palette 
cbbPalette <- c("#56acc2","#872450")

ggplotly(reviews_new %>% group_by(host_id,host_is_superhost) %>%  
  summarise(sentiment = mean(sentiments)) %>% ggplot(aes(x = sentiment,fill = host_is_superhost )) + 
  geom_density(alpha = 0.5) + 
    # geom_vline(xintercept = sentiment_test$estimate[1],linetype = 3,alpha = 0.8,color = '#56acc2') +
    # geom_vline(xintercept = sentiment_test$estimate[2],linetype = 3,alpha = 0.8,color = '#872450') +
    scale_fill_manual(values = cbbPalette) + 
    theme_minimal()) 


# hosts with multiple reviews but negative sentiments on average
reviews_new %>% 
  group_by(host_id,host_is_superhost) %>% 
  summarise(sentiment = mean(sentiments), count = n()) %>% 
  filter(sentiment < 0 & count > 1) %>% head(20)


# hosts with the highest sentiments on average and sample comments
reviews_new %>% 
  group_by(host_id,host_is_superhost) %>% 
  summarise(sentiment = mean(sentiments), count = n()) %>% 
  arrange(desc(sentiment)) %>%
  head(20) %>% 
  left_join(listing %>% select(id,host_id),
            by = c("host_id" = "host_id")) %>% 
  left_join(reviews_new %>% select(listing_id,comments),by = c("id" = "listing_id")) %>% 
  select(host_id,host_is_superhost,sentiment,count,comments)





## Cluster analysis?? ------------------------------------------------------------

# Prepare data for clustering
hosts <-  hosts %>% mutate(
  host_is_superhost = as.factor(host_is_superhost),
  host_has_profile_pic = as.factor(host_has_profile_pic),
  host_identity_verified = as.factor(host_identity_verified)
)



hosts_recipe <- 
  recipe(host_is_superhost ~ ., data = hosts) %>% 
  update_role(host_id, new_role = "ID") %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_zv(all_numeric_predictors()) %>% 
  step_center(all_numeric_predictors()) %>% 
  step_scale(all_numeric_predictors()) %>% 
  step_normalize(all_numeric_predictors())

preped_rec <- prep(hosts_recipe)

prepped_host <- juice(preped_rec) 

prepped_host %>% skimr::skim()


# Dimensionality reduction
library(umap)

umap_hosts <- umap::umap(prepped_host %>%  select(where(is.numeric),
                n_components = 2, random_state = 15))

#iris.umap = umap(iris.data,) 
layout <- umap_hosts[["layout"]] 
layout <- tibble(layout[,1],layout[,2]) 
hosts_umap <- tibble(cbind(layout, hosts$host_is_superhost)) %>% 
  rename(x1 = `layout[, 1]`,
         x2 = `layout[, 2]`,
         superhost = `hosts$host_is_superhost`)

#names(cbind(layout, hosts$host_is_superhost))

#write_csv(hosts_umap,'data/hosts_umap_layout.csv')

fig <- plot_ly(hosts_umap, x = ~x1, y = ~x2, color = ~superhost,
               colors = cbbPalette,
               type = 'scatter', mode = 'markers')%>%  
  layout(
    plot_bgcolor = "#e5ecf6",title = 'Data dimensionality reduction',
    legend=list(title=list(text='Superhost')), 
    xaxis = list( 
      title = "Component 1"),  
    yaxis = list( 
      title = "Component 2")) 

fig

# Save plotly image
#htmlwidgets::saveWidget(fig,file = 'images/umap_layout_plot_hosts_2.html')


# The plot of the data in a lower dimension does not reveal any clear clusters or
# subgroups within the dataset.
# Another interesting thing to note is that the superhost classification does not
# form a cluster and is randomly/evenly dispersed through out the plot. This 
# further confirms the earlier hypothesis that there is not significant difference
# between super and normal hosts besides the label.

# -------------------------

