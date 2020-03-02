#Packages and Libraries
install.packages("ggplot2") #For data visualisation
install.packages("prophet") #to predict demand
library("ggplot2")
library("prophet")
library("utils")
library("DT")
library("dplyr")
library("lattice")

str(hotel) #Checking data types of each feature in data

#Reading in and understanding data types
hotel
str(hotel)
head(hotel)

#Monthly Demand
table(hotel$arrival_date_year) %>% barplot(col = "steelblue", main = "Annual Booking")
table(hotel$arrival_date_year)
hotel_2015 <- hotel[hotel$arrival_date_year == "2015", ]
hotel_2016 <- hotel[hotel$arrival_date_year == "2016", ]
hotel_2017 <- hotel[hotel$arrival_date_year == "2017", ]

par(mfrow = c(3,1), mar = c(0.5, 0.5, 0.5, 0.5))
table(hotel_2015$Month) %>% barplot(col = "steelblue", main = "Monthly Booking 2015", ylim = 50000)
table(hotel_2016$Month) %>% barplot(col = "steelblue", main = "Monthly Booking 2016")
table(hotel_2017$Month) %>% barplot(col = "steelblue", main = "Monthly Booking 2017")

#Conclusion: 
##Data is from July 2015 to August 2017 so excludes various month

##Observing Booking vs Cancellation Trends:
##Manipulating data to get Cancel vs Noncancel trends in data frame form
##2015
#Total Bookings
par(mfrow = c(1,1))
dplyr::count()
m <- count(hotel_2015, hotel_2015$Month, hotel_2015$arrival_date_year)

#Cancelled bookings
dplyr::count()
n <- count(hotel_2015, hotel_2015$is_canceled, hotel_2015$Month)

#Converting to dataframes
m <- as.data.frame(m)
n <- as.data.frame(n)

#Giving names to Cancelled, Not Cancelled
n$`hotel_2015$is_canceled`[n$`hotel_2015$is_canceled` == "1"] <- "Not Cancelled" 
n$`hotel_2015$is_canceled`[n$`hotel_2015$is_canceled` == "2"] <- "Cancelled" 

#Combining the total bookings and cancellation data
r <- cbind(m,n)

#Renaming headers of this new data frame
names(r) <- c("Month", "Year", "Total Booking", "Cancelled Status", 
              "Month2", "Cancelled Quantity")

#Plotting!
g <- ggplot(data = r, aes(fill = r$`Cancelled Status`, y = r$`Total Booking`,
                          x = r$Month))
g + geom_bar(position= "stack", stat = "identity" ) + 
  ggtitle("2015 Monthwise Booking Cancelation trend") + xlab("Months")
  + ylab("")

#Country-wise monthly demand
head(hotel)
x <- as.data.frame(table(hotel$country))
x <- x[x$Freq > 100, ]
x <- x[x$Freq < 1000, ]

qplot(x$Var1, x$Freq, data = x, xlab = "Country of Origin of Customers",
      main = "100 - 1000 Bookings from Customers by Country of Origin")

y <- as.data.frame(table(hotel$country))
y <- y[y$Freq > 1000, ]

qplot(y$Var1, y$Freq, data = y, xlab = "Country of Origin of Customers",
      main = "More than 1000 Bookings from Customers by Country of Origin")

#Pie chart percentage wise.
library(RColorBrewer)
myPalette <- brewer.pal(5, "Set2") 
pie(y$Freq, labels = y$Var1, col = myPalette)

#Portugal is the highest visiting country

market_segment <- as.data.frame(table(hotel$market_segment))
market_segment
g3 <- ggplot(data = market_segment, aes(x = market_segment$Var1, 
                                 y = market_segment$Freq),
             labs(x = "Market Segments"), labs(y = "Bookings"),
             labs(title = "Market Segment wise Bookings"))


g3 + geom_point(shape = 21, color = "black", fill = "#69b3a2", size = 6,)



#Travel Agents and Offline Tourist Advisor are main source of customers
#Group 3rd largest
#Direct 4th largest

#Exploring the performance of bookings for each hotel
hotel_cancel <- as.data.frame(table(hotel$hotel, hotel$is_canceled))
names(hotel_cancel) <- c("Hotel Names", "Booking Status", "Frequency of Bookings")
par(mfrow = c(1,1))

qplot(hotel_cancel$`Hotel Names`, hotel_cancel$`Frequency of Bookings`,
      color = hotel_cancel$`Booking Status`, geom = "point", xlab = "Hotels",
      ylab = "Bookings", main = "Hotel-wise cancellation trends")

##We can see there are high number of cancellations for City Hotel
hotel_cancel
City_hotel_confirmedbookings <- 46228 - 33102
City_hotel_confirmedbookings
Resort_hotel_confirmedbookings <- 28938 - 11122
Resort_hotel_confirmedbookings
#Overall Resort hotel has higher bookings

#Booking lead time correlates with cancellation
install.packages("hrbrthemes")
library(hrbrthemes)
hotel$is_canceled <- as.factor(hotel$is_canceled)
plot(hotel$is_canceled, hotel$lead_time)
plot(hotel$hotel, hotel$lead_time)

##Add a date column in hotel data
library("lubridate")
hotel <- read.csv("hotel_bookings.csv")
str(hotel$DATE)
hotel$DATE <- as.character(hotel$DATE)
hotel$DATE <- dmy(hotel$DATE)
hotel$bookings <- "1"
hotel$bookings <- as.numeric(hotel$bookings)
bookings <- tapply(hotel$bookings, hotel$DATE, sum)
bookings <- as.data.frame(bookings)
bookings <- tibble::rownames_to_column(bookings)
names(bookings) <- c("DATE", "BOOKINGS")
bookings$DATE <- as.Date(bookings$DATE, "%Y-%m-%d")


##Lets plot a time series graph
ggplot(bookings, aes(x = bookings$DATE, y = bookings$BOOKINGS)) + geom_line()

##Predict Future Demand!
names(bookings) <- c("ds", "y")
demand <- prophet(bookings, seasonality.mode = "additive",
                  daily.seasonality = TRUE)

demand
future <- make_future_dataframe(demand, periods = 365)
tail(future)
forecast <- predict(demand, future)
tail(forecast[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])
plot(demand, forecast)

dyplot.prophet(demand, forecast)
prophet_plot_components(demand, forecast)  


##Predicting Cancellations 
cancellations <- tapply(hotel$is_canceled, hotel$DATE, sum)
cancellations <- as.data.frame(cancellations)
cancellations <- tibble::rownames_to_column(cancellations, var = "DATE")
cancellations$DATE <- as.character(cancellations$DATE)
cancellations$DATE <- as.Date(cancellations$DATE, "%Y-%m-%d")
cancellations
#time series graph of cancellations
ggplot(cancellations, aes(x = DATE, y = cancellations)) +
  geom_line()

names(cancellations) <- c("ds", "y")
cancellations_predict <- prophet(cancellations, daily.seasonality = TRUE)
future_cancellations <- make_future_dataframe(cancellations_predict, periods = 365)
forecast_cancellations <- predict(cancellations_predict, future_cancellations)
plot(cancellations_predict, forecast_cancellations)

