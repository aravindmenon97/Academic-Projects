install.packages("OpenStreetMap")
library(tidyverse)
library(magrittr)
library(Metrics)
library(plotly)
library(OpenStreetMap)
install.packages("maps")
library(maps)
install.packages("zeallot")
library(zeallot)
install.packages("lubridate")
library(lubridate)
install.packages("cowplot")
library(cowplot)

oil <- read.csv("oil.csv")
hol_events = read_csv("holidays_events.csv")
stores = read_csv("/Users/shreya/Downloads/store-sales-time-series-forecasting/stores.csv")
train = read_csv("/Users/shreya/Downloads/store-sales-time-series-forecasting/train.csv")
test = read_csv("/Users/shreya/Downloads/store-sales-time-series-forecasting/test.csv")
transactions = read_csv("/Users/shreya/Downloads/store-sales-time-series-forecasting/transactions.csv")


# Fix dates
train$day <- train$date %>% day()
train$month <- train$date %>% month()
train$month_lab <- train$date %>% month(label = TRUE)
train$year <- train$date %>% year()
train$week <- train$date %>% week()
train$week_day <- train$date %>% wday(week_start = getOption("lubridate.week.start", 1), label = TRUE)

train$family %>% table() %>% as.data.frame()

a <- train %>% group_by(family) %>% summarise(mean_sales = round(mean(sales, na.rm=TRUE),2),
                                              median_sales = round(median(sales, na.rm = TRUE),2),
                                              IQR_sales = IQR(sales),
                                              max_sales = max(sales),
                                              total_sales = sum(sales))
a


b <- ggplot(a)+
  geom_col(aes(x = family, y = mean_sales, fill = str_to_title(family)))+
  scale_fill_viridis_d()+
  theme(axis.text.x=element_blank(),axis.ticks.x=element_blank())+
  labs(title = 'Proportion of sales by product family (rollover to see labels)',
       x = 'Product family',
       y = 'Average daily sales',
       legend = 'Family')

ggplotly(b, width = 800, height = 450)

train %>% group_by(store_nbr) %>% summarise(avg_sales = mean(sales),
                                            total_sales = sum(sales),
                                            median_sales = median(sales),
                                            IQR_sals = IQR(sales),
                                            IQR_to_avg_sales = IQR(sales)/mean(sales))

plt <- train %>% 
  filter(store_nbr <= 30 & sales > 0) %>%  
  group_by(date, store_nbr) %>% 
  summarise(daily_sales = sum(sales), .groups = "keep") %>% 
  filter(daily_sales <= 45000) %>% 
  ggplot()+
  geom_density(aes(x = daily_sales, fill = as_factor(store_nbr)), alpha = 0.5)+
  #scale_x_log10()+
  labs(title = "Density plot of stores 1 to 30 ",
       subtitle = "Click legend values to show/hide stores",
       fill = "Store number",
       x = "Total daily sales")
#xlim(0,45000) 
ggplotly(plt)


stores$type %>% 
  table() %>% 
  as.data.frame() %>% 
  ggplot()+
  geom_col(aes(y = Freq, x = ., fill = as_factor(.)), colour='black', size=1)+
  scale_fill_viridis_d()+
  labs(title = 'Distribution of store types',
       fill = 'Type:',
       x = 'Store type')

left_join(stores, transactions, by = 'store_nbr') %>% group_by(type) %>% summarise(avg_trans = mean(transactions),
                                                                                   total_trans = sum(transactions),
                                                                                   sd_trans = sd(transactions))
left_join(stores, train, by = 'store_nbr') %>% 
  group_by(type) %>% 
  summarise(avg_sales = mean(sales),
            total_sales = sum(sales),
            sd_sales = sd(sales),
            .groups = 'keep')

left_join(stores, train, by = 'store_nbr') %>% 
  group_by(type, year) %>% 
  summarise(avg_sales = mean(sales),
            total_sales = sum(sales),
            sd_sales = sd(sales),
            .groups = 'keep') %>% 
  ggplot()+geom_line(aes(x = year, y = avg_sales, colour = type))+
  labs(title = "Average daily sales per year by store type",
       y = "Average daily sales")

oil %>% ggplot(aes(x = date, y = dcoilwtico, colour = dcoilwtico))+geom_line(na.rm = TRUE)+
  labs(y = 'Oil price',
       colour = 'Oil price',
       title = 'Local price of oil during the training period')

p1 <- hol_events %>% ggplot()+geom_bar(aes(x = locale, fill = locale), colour = 'black', size = 1)+
  scale_fill_viridis_d()+
  labs(title = "Holiday locale")+
  guides(fill="none")
p1

p2 <- hol_events %>% ggplot()+geom_bar(aes(x = type, fill = type), colour = 'black', size = 1)+
  scale_fill_viridis_d()+
  labs(title = "Holiday type")+
  guides(fill="none")
p2

plot_grid(plotlist = list(p1,p2), nrow = 1)


hol_events$day <- hol_events$date %>% day()
hol_events$month <- hol_events$date %>% month()
hol_events$month_lab <- hol_events$date %>% month(label = TRUE)
hol_events$year <- hol_events$date %>% year()
hol_events$week <- hol_events$date %>% week()
hol_events$week_day <- hol_events$date %>% wday(week_start = getOption("lubridate.week.start", 1), label = TRUE)

hol_events %>% 
  filter(locale == "National" & transferred == "FALSE") %>% 
  group_by(year, month_lab) %>% 
  summarise(national_hols_per_month = n_distinct(description), .groups = "keep")

require(randomForest)

set.seed(100)
library(randomForest)
head(train)
#rf <- randomForest(train$sales + train$day + train$month, data=train, proximity=TRUE)
#print(rf)

# Now that we have installed the randomforest library, let’s build the random forest model

rf <- randomForest(sales ~ ., data = train, mtry = 3,
                         importance = TRUE, na.action = na.omit) 
pred = predict(rf, newdata=test)


model <- randomForest(sales ~ ., data = train, ntree = 1000, mtry = 5)

model

# The next step is to validate our model using the test data

prediction <- predict(model, newdata = test)

table(prediction, test$sales)

prediction



#Saving 

write.csv("/Users/shreya/Downloads/submission_aravind_final.csv", row.names = FALSE)
