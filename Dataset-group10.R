###GROUP PROJECT - GROUP 10 FILE R ##################

install.packages("stringr")
library(stringr)

#DEFINING THE VARIABLES: 
log_price=AirBB_Dataset$log_price
score = AirBB_Dataset$review_scores_rating
reviews = AirBB_Dataset$number_of_reviews

Property= AirBB_Dataset$property_type
room= AirBB_Dataset$room_type
AirBB_Dataset$num_amenities=(num_commas <- str_count(AirBB_Dataset$amenities, ","))
accomodates= AirBB_Dataset$accommodates
cancellation_policy=AirBB_Dataset$cancellation_policy
cleaning_fee=AirBB_Dataset$cleaning_fee
city = AirBB_Dataset$city
AirBB_Dataset$description_words=num_commas <- str_count(AirBB_Dataset$description, " ")
ReviewRating=AirBB_Dataset$review_scores_rating
id_verified = AirBB_Dataset$host_identity_verified
resp_rate = AirBB_Dataset$host_response_rate
host_since = AirBB_Dataset$host_since
instant_bookable = AirBB_Dataset$instant_bookable
latitude = AirBB_Dataset$latitude
longitude = AirBB_Dataset$longitude
neighbourhood = AirBB_Dataset$neighbourhood
AirBB_Dataset$name_length=(num_commas <- str_count(AirBB_Dataset$name, " "))+1
number_of_reviews=AirBB_Dataset$number_of_reviews
first_review = AirBB_Dataset$first_review
bedrooms=AirBB_Dataset$bedrooms
beds=AirBB_Dataset$beds
bathrooms= AirBB_Dataset$bathrooms

AirBB_Dataset$description_words=num_commas <- str_count(AirBB_Dataset$description, " ")

AirBB_Dataset$name_length=(num_commas <- str_count(AirBB_Dataset$name, " "))+1

AirBB_Dataset$num_amenities=(num_commas <- str_count(AirBB_Dataset$amenities, ","))

AirBB_Dataset$price=(exp(AirBB_Dataset$log_price))

AirBB_Dataset$price_per_accommodate <- log(exp(AirBB_Dataset$log_price)/(AirBB_Dataset$accommodates),base = exp(1))

#PRICE 
log_price=AirBB_Dataset$log_price
table(log_price)
summary(log_price) #mean is 4.782

score=AirBB_Dataset$review_scores_rating
reviews=AirBB_Dataset$number_of_reviews

AirBB_Dataset$price=(exp(AirBB_Dataset$log_price))
AirBB_Dataset$price_per_accommodate <- log(exp(AirBB_Dataset$log_price)/(AirBB_Dataset$accommodates),base = exp(1))

summary(AirBB_Dataset$log_price)
summary(AirBB_Dataset$price)
summary(AirBB_Dataset$price_per_accommodate)

sd(AirBB_Dataset$price)
sd(AirBB_Dataset$log_price)
sd(AirBB_Dataset$price_per_accommodate)

hist(AirBB_Dataset$log_price)

boxplot(AirBB_Dataset$log_price)


# CLEANING THE DATA SET 
AirBB_Dataset <- subset(AirBB_Dataset, select = c(log_price, property_type, room_type, amenities, accommodates,
                                                  bathrooms, cancellation_policy, cleaning_fee, city, description,
                                                 host_identity_verified, host_response_rate,instant_bookable, host_since,                                                name, neighbourhood, number_of_reviews, review_scores_rating, 
                                                bedrooms, beds, latitude, longitude))
score = AirBB_Dataset$review_scores_rating
reviews = AirBB_Dataset$number_of_reviews

#PROPERTY TYPE 
#AirBB_Dataset <- AirBB_Dataset[!grepl("Cave", AirBB_Dataset$property_type), ]
#AirBB_Dataset <- AirBB_Dataset[!grepl("Chalet", AirBB_Dataset$property_type), ]
#AirBB_Dataset <- AirBB_Dataset[!grepl("Casa particular", AirBB_Dataset$property_type), ]
#AirBB_Dataset <- AirBB_Dataset[!grepl("Yurt", AirBB_Dataset$property_type), ]
#AirBB_Dataset <- AirBB_Dataset[!grepl("Treehouse", AirBB_Dataset$property_type), ]
#AirBB_Dataset <- AirBB_Dataset[!grepl("Tent", AirBB_Dataset$property_type), ]
#AirBB_Dataset <- AirBB_Dataset[!grepl("Tipi", AirBB_Dataset$property_type), ]
#AirBB_Dataset <- AirBB_Dataset[!grepl("Train", AirBB_Dataset$property_type), ]
#AirBB_Dataset <- AirBB_Dataset[!grepl("Vacation home", AirBB_Dataset$property_type), ]
#AirBB_Dataset <- AirBB_Dataset[!grepl("hut", AirBB_Dataset$property_type), ]
#AirBB_Dataset <- AirBB_Dataset[!grepl("Lighthouse", AirBB_Dataset$property_type), ]
#AirBB_Dataset <- AirBB_Dataset[!grepl("Serviced apartment", AirBB_Dataset$property_type), ]
#AirBB_Dataset <- AirBB_Dataset[!grepl("Earth House", AirBB_Dataset$property_type), ]
#AirBB_Dataset <- AirBB_Dataset[!grepl("Hut", AirBB_Dataset$property_type), ]
#AirBB_Dataset <- AirBB_Dataset[!grepl("Parking Space", AirBB_Dataset$property_type), ]
#AirBB_Dataset <- AirBB_Dataset[!grepl("Island", AirBB_Dataset$property_type), ]

 
table(Property)
prop.table(table(Property))
barplot(table(Property), las = 2)
boxplot(log_price~AirBB_Dataset$property_type, cex.names=0.5, las= 2, lax = 2) #???
tapply(log_price,Property,summary)
#conditional and joint probabilities
#1.out of the  places with price under the mean, how many are apartments?
25576/38725 #66%
#confounder: the majority of property is apartments:
49024/74111 #66.1%

quantile(log_price, 0.99)
#2 in the places with a price in the top 1%, how many are apartments?
228/742 #30% still confounder 
#and villas?
26/742 #3.5% however of the villas this is 26/179=14% --> not so high too

#3 What type of properties have the 1% highest price?And the 1% lowest price?
quantile(log_price, 0.99)
table(Property,log_price>6.894467 )
#in 1% highest price: 227 apartment,36 conodminium,367 houses, and few of others

quantile(log_price, 0.01)
table(Property,log_price<3.367296)
#1%lowest price: 365 apartments, 340 houses, 18 condominium and a few others

tapply(AirBB_Dataset$accommodates,AirBB_Dataset$room_type,summary)
tapply(AirBB_Dataset$log_price,AirBB_Dataset$room_type,summary)
tapply(AirBB_Dataset$number_of_reviews,AirBB_Dataset$room_type,summary)
tapply(AirBB_Dataset$review_scores_rating,AirBB_Dataset$room_type,summary)
prop.table(table(AirBB_Dataset$room_type, AirBB_Dataset$city),2)
table(AirBB_Dataset$property_type, AirBB_Dataset$city)

# ROOM TYPE

room= AirBB_Dataset$room_type
table(room)
prop.table(table(room)) #55.7% entire home, 41.3% private room and 0.3% shared room
barplot(table(room))
boxplot(log_price~room)
tapply(log_price,room,summary)

number_of_reviews = AirBB_Dataset$number_of_reviews
room_type= AirBB_Dataset$room_type
boxplot(number_of_reviews~room_type)



a=boxplot(log_price)
a$stats 

#joint and conditionals
Home1=(room == "Entire home/apt")&(log_price<2.995732)
table(Home1) 
20/74111 #0.027%
20/41310 #0.048%
Private1= (room == "Private room")&(log_price<2.995732)
table(Private1)
40/74111 #0.053%
40/30638 #0.131%
Shared1= (room == "Shared room")&(log_price<2.995732)
table(Shared1)
100/74111 #0.135%
100/2163 #4.623%

Home2=(room == "Entire home/apt")&(log_price>=2.995732)&(log_price<4.317488)
table(Home2) 
1561/74111 #2.106%
1561/41310 #3.778%
Private2=(room == "Private room")&(log_price>=2.995732)&(log_price<4.317488)
table(Private2)
15159/74111 #20.454%
15159/30638 #49.477%
Shared2= (room == "Shared room")&(log_price>=2.995732)&(log_price<4.317488)
table(Shared2)
1592/74111 #2.148%
1592/2163 #73.601%


Home3=(room == "Entire home/apt")&(log_price>=4.317488)&(log_price<4.709530)
table(Home3)
8002/74111 #10.797%
8002/41310 #19.371%
Private3=(room == "Private room")&(log_price>=4.317488)&(log_price<4.709530)
table(Private3)
10252/74111 #13.833 %
10252/30638 #33.462%
Shared3= (room == "Shared room")&(log_price>=4.317488)&(log_price<4.709530)
table(Shared3)
299/74111 #0.403%
299/2163 #13.823%

Home4=(room == "Entire home/apt")&(log_price>=4.709530)&(log_price<5.220356 )
table(Home4) 
14912/74111 #20.121%
14912/41310 #36.098%
Private4=(room == "Private room")&(log_price>=4.709530)&(log_price<5.220356)
table(Private4)
3811/74111 #5.142%
3811/30638 #12.439
Shared4= (room == "Shared room")&(log_price>=4.709530)&(log_price<5.220356)
table(Shared4)
97/74111 #0.131%
97/2163 #4.484%

Home5=(room == "Entire home/apt")&(log_price>=5.220356 )&(log_price<6.572283)
table(Home5) 
15546/74111 #20.976%
15546/41310 #37.632%
Private5=(room == "Private room")&(log_price>=5.220356)&(log_price<6.572283)
table(Private5)
1283/74111 #1.731%
1283/30638 #4.187%
Shared5= (room == "Shared room")&(log_price>=5.220356)&(log_price<6.572283)
table(Shared5)
65/74111 #0.087%
65/2163 #3%

Home6=(room == "Entire home/apt")&(log_price>=6.572283)
table(Home6) 
1269/74111 #1.712%
1269/41310 #3.072%
Private6=(room == "Private room")&(log_price>=6.572283)
table(Private6)
93/74111 #0.125%
93/30638 #0.303%
Shared6= (room == "Shared room")&(log_price>=6.572283)
table(Shared6)
10/74111 #0.013%
10/2163 #0.462%


#AMENITIES
AirBB_Dataset$num_amenities=(num_commas <- str_count(AirBB_Dataset$amenities, ","))
typeof(AirBB_Dataset$amenities)

summary(AirBB_Dataset$num_amenities)
sd(AirBB_Dataset$num_amenities,na.rm = TRUE)

pct_over_50 <- 100 * mean(AirBB_Dataset$num_amenities > 50,na.rm = TRUE)
pct_over_50

AirBB_Dataset$num_amenities = ifelse(AirBB_Dataset$num_amenities > 50, NA, AirBB_Dataset$num_amenities)

boxplot(AirBB_Dataset$log_price ~ AirBB_Dataset$num_amenities)
boxplot(AirBB_Dataset$number_of_reviews ~ AirBB_Dataset$num_amenities)                   
boxplot(AirBB_Dataset$review_scores_rating ~ AirBB_Dataset$num_amenities)

cor(AirBB_Dataset$number_of_reviews,AirBB_Dataset$num_amenities,use="complete.obs")
cor(AirBB_Dataset$log_price,AirBB_Dataset$num_amenities,use="complete.obs")
cor(AirBB_Dataset$log_price,AirBB_Dataset$num_amenities,use="complete.obs")

tapply(AirBB_Dataset$number_of_reviews, AirBB_Dataset$num_amenities,mean) 

tapply(AirBB_Dataset$num_amenities, AirBB_Dataset$room_type, summary)
tapply(AirBB_Dataset$num_amenities,AirBB_Dataset$city,summary,use="complete.obs")

summary(log_price) #interquantile range: from 4.317 to 5.220

#what basic amenities do they have under the first quartile?
10001 /18472#54.4% has a tv or cable tv
17615/18472 #95.3%has wireless interent
12924/18472 #70% air condotioning
15182/18472 #82.2% has heating
10708/18472 #58% has carbon monoxide detector

#ones in the interquantile range
27314/36974 #73.87% has a tv or cable tv
35640/36974 #96.4 %has wireless interent
28034/36974 #75.8% air condotioning
33748/36974 #91.3% has heating
23765/36974 #64.3% has carbon monoxide detector

#what about the ones over the third quartile:

16338/18665 #87.5 has a tv or cable tv
18125/18665 #97.1% has wireless interent
14252/18665 #76.3% air condotioning
17513/18665 #93.8% has heating
12717/18665 #68.1% has carbon monoxide detector

#more refined amenities

#under first quartile
767/18472 #4.1%"Private entrance
2508/18472 #13.58% Safety card
3215/18472 #17.4% "24-hour check-in"
1141/18472 #6.17% Pool
2254/18472 #12.2% Breakfast


#ones in the interquantile range
4049/36974 #10.95% private entrance
5641/36974 #15.26 % safety card
10001/36974 #27.05% 24-hour check in
3081/36974 #8.33% pool
4076/36974 #11.02% breakfast

#over third quartile
2454/18665#13.15%"Private entrance
3364/18665# 18% Safety card
5799/18665# 31% "24-hour check-in"
2061/18665#11.04% Pool
1976/18665#10.58%Breakfast

#ACCOMODATES

accomodates= AirBB_Dataset$accommodates #from 1 to 16
table(accomodates)
prop.table(table(accomodates))
hist(accomodates)
summary(AirBB_Dataset$accommodates)
sd(AirBB_Dataset$accommodates)
plot(accomodates,log_price)
scatter.smooth(accomodates,log_price)
tapply(log_price,accomodates,summary)
boxplot(log_price~accomodates)
cor(accomodates,log_price) # 0.5675742 

boxplot(AirBB_Dataset$accommodates~AirBB_Dataset$city) # accomodates per city

boxplot(AirBB_Dataset$log_price~AirBB_Dataset$accommodates)
cor(AirBB_Dataset$accommodates,AirBB_Dataset$log_price)#price and accomodates 

boxplot(AirBB_Dataset$price_per_accommodate~AirBB_Dataset$accommodates)#price per accomodate and accomodates 
cor(AirBB_Dataset$accommodates,AirBB_Dataset$price_per_accommodate)

prop.table(table(AirBB_Dataset$room_type,AirBB_Dataset$accommodates),1)
tapply(AirBB_Dataset$accommodates,AirBB_Dataset$city,summary)
tapply(AirBB_Dataset$accommodates,AirBB_Dataset$room_type,summary)


#outliers - lowest1%, how many accomodates?
quantile(log_price, 0.01)
table(accomodates, log_price<3.367296)
quantile(log_price, 0.99)
table(accomodates,log_price>6.894467) #mostly are 6,8 guests' houses 
#consider there are fewer houses for eleven accomodates then for two for example

#analysis of correlation between rooms and guests:
table(room,accomodates)
prop.table(table(room,accomodates),2) 
#as the number of guests increase, it's more likely they have an entire home/apt. not single rooms (more probable with 1  and 2 guests)

#CANCELLATION POLICY 

cancellation_policy=AirBB_Dataset$cancellation_policy
tab = table(cancellation_policy)
tab2 = prop.table(tab)# only 0,0017% of the observations are superstrict_30 
#or superstrict_60, 
prop.table(table(AirBB_Dataset$cancellation_policy))
boxplot(AirBB_Dataset$review_scores_rating~AirBB_Dataset$cancellation_policy)
tapply(AirBB_Dataset$number_of_reviews,AirBB_Dataset$cancellation_policy,summary)

# 30,42% flex, 25,72% mod, 43,68% strict 
tab2
boxplot(log_price~cancellation_policy)
cancellation_policy[is.na(cancellation_policy)] <- 0
# I create another set of data (numerical) in order to analyse the data with plots and formulas

policy1<-data.frame(cancellation_policy)
policy1$cancellation_policy<-c(flexible=1,moderate=2,strict=3,super_strict_30=4,super_strict_60=5)[policy1$cancellation_policy]
policy2 <-unlist(policy1)
summary(policy2)
sd(policy2)
tab= table(log_price,policy2)
barplot(tab)

table(AirBB_Dataset$cancellation_policy)
prop.table(table(AirBB_Dataset$cancellation_policy))
boxplot(AirBB_Dataset$review_scores_rating~AirBB_Dataset$cancellation_policy)
tapply(AirBB_Dataset$number_of_reviews,AirBB_Dataset$cancellation_policy,summary)

#We analyse what happen when the cost is very high and when is very low
quantile(log_price, 0.99)
table(policy2,log_price>6.894467 )
prop.table(table(policy2,log_price>6.894467 ),2)

quantile(log_price, 0.01)
table(policy2,log_price<3.367296)
prop.table(table(policy2,log_price<3.367296),2)

tapply(ReviewRating, cancellation_policy, summary)
tapply(number_of_reviews, cancellation_policy, summary)

# city and cancellation policy 
# NYC 
cancellationpercity= AirBB_Dataset[city == "NYC",]$cancellation_policy
tab = table(cancellationpercity)
prop.table(tab)
# 30,9 % flex, 24,27% mod,44,7% strict,


#LA
cancellationpercity= AirBB_Dataset[city == "LA",]$cancellation_policy
tab = table(cancellationpercity)
prop.table(tab)
# 30,23%flex 25,65%mod, 44%strict 

#SF 
cancellationpercity= AirBB_Dataset[city == "SF",]$cancellation_policy
tab = table(cancellationpercity)
prop.table(tab)
#29% flex, 29,8% mod, 40,9% strict 

#Boston 
cancellationpercity= AirBB_Dataset[city == "Boston",]$cancellation_policy
tab = table(cancellationpercity)
prop.table(tab)
#24,3% flex, 23,5% mod, 51,1% strict 

#Chicago 
cancellationpercity= AirBB_Dataset[city == "Chicago",]$cancellation_policy
tab = table(cancellationpercity)
prop.table(tab)
#25,7% flex, 30,8% mod, 43,2% strict 

#DC 
cancellationpercity= AirBB_Dataset[city == "DC",]$cancellation_policy
tab = table(cancellationpercity)
prop.table(tab)
#36% flex, 27% mod, 35%  #FLEX

# CLEANING FEE
cleaning_fee=AirBB_Dataset$cleaning_fee
table(cleaning_fee)
boxplot(log_price~cleaning_fee)
prop.table(table(AirBB_Dataset$cleaning_fee))
tapply(log_price, cleaning_fee, summary)

tapply(AirBB_Dataset$number_of_reviews,AirBB_Dataset$cleaning_fee,summary)
tapply(AirBB_Dataset$review_scores_rating,AirBB_Dataset$cleaning_fee,summary)
tapply(AirBB_Dataset$log_price,AirBB_Dataset$cleaning_fee,summary)

priceexp = exp(log_price)
boxplot(priceexp~cleaning_fee)

prop.table(table(cleaning_fee))
tapply(ReviewRating, cleaning_fee, summary)
tapply(number_of_reviews, cleaning_fee, summary)
boxplot(score ~ cleaning_fee)
boxplot(reviews ~ cleaning_fee)

#CITY 
city = AirBB_Dataset$city
boxplot(log_price~city)
tab1=table(city)
table(AirBB_Dataset$city)
prop.table(table(AirBB_Dataset$city))
boxplot(AirBB_Dataset$log_price~AirBB_Dataset$city)
boxplot(AirBB_Dataset$review_scores_rating~AirBB_Dataset$city)
boxplot(AirBB_Dataset$number_of_reviews~AirBB_Dataset$city)

tapply(AirBB_Dataset$review_scores_rating,AirBB_Dataset$city,mean,na.rm = TRUE)
tapply(AirBB_Dataset$number_of_reviews,AirBB_Dataset$city,mean,na.rm = TRUE)
tapply(AirBB_Dataset$log_price,AirBB_Dataset$city,mean,na.rm = TRUE)
tapply(AirBB_Dataset$accommodates,AirBB_Dataset$city,summary)


barplot1=barplot(tab1, main = "barplot of absolute frequencies") # BARPLOT1: absolute frequencies of accomodations per city 
barplot1
q75=quantile(log_price, 0.75)# third quantile 
topprice= AirBB_Dataset[log_price>=q75,]$city 
tab2 = table(topprice)
barplot2=barplot(tab2, main = "barplot of high prices" ) #BARPLOT2
barplot2

q25 = quantile(log_price, 0.25) #first quantile 
lowprice=AirBB_Dataset[log_price<=q25,]$city
tab3=table(lowprice)
barplot3 = barplot(tab3, main = "bar plot of low prices") #BARPLOT 3
barplot3

boxplot(number_of_reviews~city)

#City and GDP per capita: we have tried to investigate how prices vary depending
#on the average GDP per capita in each city

short_data<-AirBB_Dataset[,c(1:4,6:10,12:18,20:22,24,26)]  #????
summary(short_data)
head(short_data)
Y<-AirBB_Dataset[,c(1,10)]

length(Y[,2]) #length of the matrix 
mean_gdp<-rep(0,74111)

Y<-data.frame(Y,mean_gdp)
table(Y[,2])

#we now introduce the values of the mean GDP per capita:
Y[,3][Y[,2]=="Boston"]=108506
Y[,3][Y[,2]=="Chicago"]=80398
Y[,3][Y[,2]=="DC"]=95593
Y[,3][Y[,2]=="LA"]=86532
Y[,3][Y[,2]=="NYC"]=100806
Y[,3][Y[,2]=="SF"]=144633

head(Y)
AirBB_Dataset<-data.frame(AirBB_Dataset,Y[,3])

colnames(AirBB_Dataset)[27]="mean_gdp"

head(AirBB_Dataset)

#info about MEAN 

summary(Y) #to have info about min,max,first quart.a nd so on..
price=AirBB_Dataset$log_price


GDP= AirBB_Dataset[,27]
GDP
summary(GDP)

cor(price,GDP) #we found a low correlation between price and GDP per capita 

#DESCRIPTION
AirBB_Dataset$description_words=num_commas <- str_count(AirBB_Dataset$description, " ")
plot(AirBB_Dataset$log_price~AirBB_Dataset$description_words)
boxplot(AirBB_Dataset$number_of_reviews~AirBB_Dataset$description_words)
boxplot(AirBB_Dataset$review_scores_rating~AirBB_Dataset$description_words)
boxplot(AirBB_Dataset$description_words~AirBB_Dataset$room_type)
boxplot(AirBB_Dataset$description_words~AirBB_Dataset$city)
boxplot(AirBB_Dataset$log_price~AirBB_Dataset$description_words)
cor(AirBB_Dataset$description_words,AirBB_Dataset$log_price)

#REVIEW SCORE RATING 
ReviewRating=AirBB_Dataset$review_scores_rating
summary(ReviewRating)
ReviewRating[is.na(ReviewRating)] <- 0  #I will know that when there will be 0, it will mean that it's NA

sd(ReviewRating)
tab= table(log_price,ReviewRating)
barplot(tab)
boxplot(log_price~ReviewRating)
plot(ReviewRating,log_price,cex=0.1)
scatter.smooth(ReviewRating,log_price,cex=0.2)

#HOST ID VERIFICATION
id_verified = AirBB_Dataset$host_identity_verified

tapply(AirBB_Dataset$review_scores_rating,AirBB_Dataset$host_identity_verified,sd,na.rm = TRUE)
tapply(AirBB_Dataset$number_of_reviews,AirBB_Dataset$host_identity_verified,summary)
tapply(AirBB_Dataset$log_price,AirBB_Dataset$host_identity_verified,summary)

boxplot(AirBB_Dataset$log_price~AirBB_Dataset$host_identity_verified)#no correlation, slightly higher mean and median for true
boxplot(AirBB_Dataset$review_scores_rating~AirBB_Dataset$host_identity_verified)
boxplot(AirBB_Dataset$number_of_reviews~AirBB_Dataset$host_identity_verified)

logprice1= AirBB_Dataset[(complete.cases(id_verified)),]$log_price #filter for complete cases
id_verified_complete = AirBB_Dataset[(complete.cases(id_verified)),]$host_identity_verified
prop.table(table(id_verified_complete))
tapply(logprice1, id_verified_complete, summary)
tapply(logprice1, id_verified_complete, sd)
cor(logprice1, id_verified_complete) #low cor (0.0241697)
plot(id_verified_complete, logprice1) #it isn't useful
tapply(score, id_verified, summary)
tapply(reviews, id_verified, summary)

#HOST RESPONSE RATE
resp_rate = AirBB_Dataset$host_response_rate

logprice2 = AirBB_Dataset[complete.cases(resp_rate),]$log_price #filter NA (55812 data)
resp_rate_complete = AirBB_Dataset[complete.cases(resp_rate),]$host_response_rate
boxplot(logprice2 ~ resp_rate_complete, cex.axis = 0.5, las = 2) #prices differ for different categories
tapply(logprice2, resp_rate_complete, summary) #it isn't useful because there are too many categories
tapply(logprice2, resp_rate_complete, sd)
 

#To plot data, convert them
AirBB_Dataset222[((complete.cases(resp_rate))&(resp_rate == "100%")), 14] = "100"
#repeat for all the others categories 

logprice_converted = AirBB_Dataset222[complete.cases(resp_rate),]$log_price 
resp_rate_converted = AirBB_Dataset222[complete.cases(resp_rate),]$host_response_rate
plot(logprice_converted, resp_rate_converted) #no correlation, datas are concentrated at the top
#most frequent data
table(resp_rate_converted)
# 100%: 77.5%
# 90%: 4.1%
# 80%: 2%
# 0%:  1.6%

#Analyse frequencies in different price ranges

b = boxplot(logprice2 ~ resp_rate2, cex.axis = 0.5, las = 2)
b$stats #find 6 price ranges (lower outliers, below Q1, between Q1 and Q2, between Q2 and Q3, above Q3, upper outliers)

price1 = AirBB_Dataset222[(logprice < 2.944439),]$log_price
resp1 = AirBB_Dataset222[(logprice < 2.944439),]$host_response_rate
table(resp1) #lower: except for one value (0%), resp rate is always above 50%, 70% are above 99%
scatter.smooth(price1, resp1)

price2 = AirBB_Dataset222[((logprice >= 2.944439)&(logprice < 4.290459)),]$log_price
resp2 = AirBB_Dataset222[((logprice >= 2.944439)&(logprice < 4.290459)),]$host_response_rate
table(resp2) #58% got 100%, 3.1% got 90%, 1.5% got 80%, 1.3% got 0%
scatter.smooth(price2, resp2)

price3 = AirBB_Dataset222[((logprice < 4.700480)&(logprice >= 4.290459)),]$log_price
resp3 = AirBB_Dataset222[((logprice < 4.700480)&(logprice >= 4.290459)),]$host_response_rate
table(resp3) #59% got 100%, 3.1% got 90%, 1.4% got 80%, 1.3% got 0%
scatter.smooth(price3, resp3)

price4 = AirBB_Dataset222[((logprice < 5.192957)&(logprice >=4.700480)),]$log_price
resp4 = AirBB_Dataset222[((logprice < 5.192957)&(logprice >= 4.700480)),]$host_response_rate
table(resp4) #60% got 100%, 3.2% got 90% 1.6% got 80%, 1% got 0%
scatter.smooth(price4, resp4)

price5 = AirBB_Dataset222[((logprice < 6.543912)&(logprice >= 5.192957)),]$log_price
resp5 = AirBB_Dataset222[((logprice < 6.543912)&(logprice >= 5.192957)),]$host_response_rate
table(resp5) #57.1% got 100%, 3.0% got 90%, 1.6% got 80%, 1.2% got 0%
scatter.smooth(price5, resp5)


price6 = AirBB_Dataset222[((logprice > 6.543912)),]$log_price
resp6 = AirBB_Dataset222[((logprice > 6.543912)),]$host_response_rate
table(resp6) #40% got 100%, 1.9% got 90%, 1.3% got 80%, 2% got 0%
scatter.smooth(price6, resp6)

#Analyse prices in 5 categories:
#response rate below 25%
price25 = AirBB_Dataset222[((resp_rate_converted == 0)|(resp_rate_converted == 10)|((resp_rate_converted >= 11)&(resp_rate_converted <25))|(resp_rate_converted == 6)),]$log_price
summary(price25)
#response rate below 50%
price50 = AirBB_Dataset222[((resp_rate_converted >= 25)&(resp_rate_converted<50)),]$log_price
summary(price50)
#response rate below 70%
price70 = AirBB_Dataset222[((resp_rate_converted >= 50)&(resp_rate_converted<70)),]$log_price
summary(price70)
#response rate below 80%
price80 = AirBB_Dataset222[((resp_rate_converted >= 70)&(resp_rate_converted<80)),]$log_price
summary(price80)
#response rate below 90%
price90 = AirBB_Dataset222[((resp_rate_converted >= 80)&(resp_rate_converted<90)),]$log_price
summary(price90)
#response rate below 100%
price100 = AirBB_Dataset222[((resp_rate_converted >= 90)|(resp_rate_converted ==100)),]$log_price
summary(price100)

boxplot(price25, price50, price70, price80, price90, price100)

#HOST SINCE 
host_since = AirBB_Dataset$host_since
logprice3 = AirBB_Dataset[complete.cases(host_since),]$log_price
host_since_complete = AirBB_Dataset[complete.cases(host_since),]$host_since
summary(host_since_complete)
sd(host_since_complete)
hist(host_since_complete, breaks = "quarters", cex.axis = 0.5, las = 2)
b = boxplot(logprice3)
b$stats
since1 = AirBB_Dataset222[complete.cases(host_since)&(log_price < 2.995732),]$host_since
since2 = AirBB_Dataset222[complete.cases(host_since)&((log_price >= 2.995732)&(log_price < 4.317488)),]$host_since
since3 = AirBB_Dataset222[complete.cases(host_since)&((log_price >= 4.317488)&(log_price < 4.709530)),]$host_since
since4 = AirBB_Dataset222[complete.cases(host_since)&((log_price >= 4.709530)&(log_price < 5.220356)),]$host_since
since5 = AirBB_Dataset222[complete.cases(host_since)&((log_price >= 5.220356)&(log_price < 6.572283)),]$host_since
since6 = AirBB_Dataset222[complete.cases(host_since)&(log_price >= 6.572283),]$host_since
boxplot(since1, since2, since3, since4, since5, since6)
summary(since1)
summary(since2)
summary(since3)
summary(since4)
summary(since5)
summary(since6)
sd(since1)
sd(since2)
sd(since3)
sd(since4)
sd(since5)
sd(since6)

#INSTANT BOOKABLE
instant_bookable = AirBB_Dataset$instant_bookable
logprice4 = AirBB_Dataset[complete.cases(instant_bookable),]$log_price
instant_bookable_complete = AirBB_Dataset[complete.cases(instant_bookable),]$instant_bookable
prop.table(table(instant_bookable_complete))
tapply(logprice4, instant_bookable_complete, summary)
boxplot(logprice4 ~ instant_bookable_complete) #no correlation, slightly higher for false (also for true min = 0)
cor(logprice4, instant_bookable_complete) #-0.04427128
tapply(score, instant_bookable, summary)
tapply(reviews, instant_bookable, summary)
boxplot(score ~ instant_bookable)
boxplot(reviews ~ instant_bookable)

#LATITUDE
latitude = AirBB_Dataset$latitude
summary(latitude)
scatter.smooth(log_price, latitude)
cor(log_price, latitude) #very low cor (-0.002193364)
h = hist(latitude, breaks = 30)
h$counts
h$breaks

logprice_NYC = AirBB_Dataset[(city == "NYC"),]$log_price
latitude_NYC = AirBB_Dataset[(city == "NYC"),]$latitude
cor(logprice_NYC, latitude_NYC) #higher 0.07477868
scatter.smooth(logprice_NYC, latitude_NYC)

logprice_LA = AirBB_Dataset[(city == "LA"),]$log_price
latitude_LA = AirBB_Dataset[(city == "LA"),]$latitude
cor(logprice_LA, latitude_LA) #low -0.03008398
scatter.smooth(logprice_LA, latitude_LA)

logprice_SF = AirBB_Dataset[(city == "SF"),]$log_price
latitude_SF = AirBB_Dataset[(city == "SF"),]$latitude
cor(logprice_SF, latitude_SF) #0.2051497
scatter.smooth(logprice_SF, latitude_SF)

logprice_DC = AirBB_Dataset[(city == "DC"),]$log_price
latitude_DC = AirBB_Dataset[(city == "DC"),]$latitude
cor(logprice_DC, latitude_DC) #-0.1362901
scatter.smooth(logprice_DC, latitude_DC)

logprice_Chicago = AirBB_Dataset[(city == "Chicago"),]$log_price
latitude_Chicago = AirBB_Dataset[(city == "Chicago"),]$latitude
cor(logprice_Chicago, latitude_Chicago) ##0.07389882
scatter.smooth(logprice_Chicago, latitude_Chicago)

logprice_Boston = AirBB_Dataset[(city == "Boston"),]$log_price
latitude_Boston = AirBB_Dataset[(city == "Boston"),]$latitude
cor(logprice_Boston, latitude_Boston) #0.2911388
scatter.smooth(logprice_Boston, latitude_Boston)

city = AirBB_Dataset$city
city == "NYC"

price_NYC1 = AirBB_Dataset[((city == "NYC")&(latitude < 40.6)),]$log_price
latitude_NYC1 = AirBB_Dataset[((city == "NYC")&(latitude < 40.6)),]$latitude
summary(price_NYC1)
sd(price_NYC1)

price_NYC2 = AirBB_Dataset[((city == "NYC")&(latitude >= 40.6)&(latitude < 40.7)),]$log_price
latitude_NYC2 = AirBB_Dataset[((city == "NYC")&(latitude >= 40.6)&(latitude < 40.7)),]$latitude
summary(price_NYC2)
sd(price_NYC2)

price_NYC3 = AirBB_Dataset[((city == "NYC")&(latitude >= 40.7)&(latitude < 40.8)),]$log_price
latitude_NYC3 = AirBB_Dataset[((city == "NYC")&(latitude >= 40.7)&(latitude < 40.8)),]$latitude
summary(price_NYC3)
sd(price_NYC3)

price_NYC4 = AirBB_Dataset[((city == "NYC")&(latitude >= 40.8)&(latitude < 50)&(log_price > 0)),]$log_price
latitude_NYC4 = AirBB_Dataset[((city == "NYC")&(latitude >= 40.8)&(latitude < 50)&(log_price > 0)),]$latitude
summary(price_NYC4)
sd(price_NYC4)
boxplot(price_NYC1, price_NYC2, price_NYC3, price_NYC4)

#LONGITUDE
longitude = AirBB_Dataset$longitude
summary(longitude)
scatter.smooth(log_price, longitude)
cor(log_price, longitude) #very low cor (-0.002193364)
h = hist(longitude, breaks = 50)
h$counts
h$breaks

logprice_NYC = AirBB_Dataset[(city == "NYC"),]$log_price
longitude_NYC = AirBB_Dataset[(city == "NYC"),]$longitude
cor(logprice_NYC, longitude_NYC) #higher 0.07477868
scatter.smooth(logprice_NYC, longitude_NYC)

logprice_LA = AirBB_Dataset[(city == "LA"),]$log_price
longitude_LA = AirBB_Dataset[(city == "LA"),]$longitude
cor(logprice_LA, longitude_LA) #low -0.03008398
scatter.smooth(logprice_LA, longitude_LA)

logprice_SF = AirBB_Dataset[(city == "SF"),]$log_price
longitude_SF = AirBB_Dataset[(city == "SF"),]$longitude
cor(logprice_SF, longitude_SF) #0.2051497
scatter.smooth(logprice_SF, longitude_SF)

logprice_DC = AirBB_Dataset[(city == "DC"),]$log_price
longitude_DC = AirBB_Dataset[(city == "DC"),]$longitude
cor(logprice_DC, longitude_DC) #-0.1362901
scatter.smooth(logprice_DC, longitude_DC)

logprice_Chicago = AirBB_Dataset[(city == "Chicago"),]$log_price
longitude_Chicago = AirBB_Dataset[(city == "Chicago"),]$longitude
cor(logprice_Chicago, longitude_Chicago) ##0.07389882
scatter.smooth(logprice_Chicago, longitude_Chicago)

logprice_Boston = AirBB_Dataset[(city == "Boston"),]$log_price
longitude_Boston = AirBB_Dataset[(city == "Boston"),]$longitude
cor(logprice_Boston, longitude_Boston) #0.2911388
scatter.smooth(logprice_Boston, longitude_Boston)

#DISTRICTS

#5 districts New York

priceQueens = AirBB_Dataset[((city == "NYC")&(latitude >= 40.54)&(latitude < 40.79)&(longitude >= -73.96)&(longitude < -73.69)),]$log_price
priceManh = AirBB_Dataset[((city == "NYC")&(latitude >= 40.70)&(latitude < 40.87)&(longitude >= -74.01)&(longitude < -73.90)),]$log_price
priceBrook = AirBB_Dataset[((city == "NYC")&(latitude >= 40.57)&(latitude < 40.73)&(longitude >= -74.04)&(longitude < -73.85)),]$log_price
priceBronx = AirBB_Dataset[((city == "NYC")&(latitude >= 40.78)&(latitude < 40.91)&(longitude >= -73.93)&(longitude < -73.77)),]$log_price
priceStatenI = AirBB_Dataset[((city == "NYC")&(latitude >= 40.49)&(latitude < 40.63)&(longitude >= -74.25)&(longitude < -74.05)),]$log_price
boxplot(priceQueens, priceManh, priceBrook, priceBronx, priceStatenI)

reviewsQueens = AirBB_Dataset[((city == "NYC")&(latitude >= 40.54)&(latitude < 40.79)&(longitude >= -73.96)&(longitude < -73.69)),]$number_of_reviews
reviewsManh = AirBB_Dataset[((city == "NYC")&(latitude >= 40.70)&(latitude < 40.87)&(longitude >= -74.01)&(longitude < -73.90)),]$number_of_reviews
reviewsBrook = AirBB_Dataset[((city == "NYC")&(latitude >= 40.57)&(latitude < 40.73)&(longitude >= -74.04)&(longitude < -73.85)),]$number_of_reviews
reviewsBronx = AirBB_Dataset[((city == "NYC")&(latitude >= 40.78)&(latitude < 40.91)&(longitude >= -73.93)&(longitude < -73.77)),]$number_of_reviews
reviewsStatenI = AirBB_Dataset[((city == "NYC")&(latitude >= 40.49)&(latitude < 40.63)&(longitude >= -74.25)&(longitude < -74.05)),]$number_of_reviews
boxplot(reviewsQueens, reviewsManh, reviewsBrook, reviewsBronx, reviewsStatenI)
summary(reviewsBronx)
summary(reviewsBrook)
summary(reviewsManh)
summary(reviewsQueens)
summary(reviewsStatenI) #highest average Manhattan, lowest bronx

scoreQueens = AirBB_Dataset[((city == "NYC")&(latitude >= 40.54)&(latitude < 40.79)&(longitude >= -73.96)&(longitude < -73.69)),]$review_scores_rating
scoreManh = AirBB_Dataset[((city == "NYC")&(latitude >= 40.70)&(latitude < 40.87)&(longitude >= -74.01)&(longitude < -73.90)),]$review_scores_rating
scoreBrook = AirBB_Dataset[((city == "NYC")&(latitude >= 40.57)&(latitude < 40.73)&(longitude >= -74.04)&(longitude < -73.85)),]$review_scores_rating
scoreBronx = AirBB_Dataset[((city == "NYC")&(latitude >= 40.78)&(latitude < 40.91)&(longitude >= -73.93)&(longitude < -73.77)),]$review_scores_rating
scoreStatenI = AirBB_Dataset[((city == "NYC")&(latitude >= 40.49)&(latitude < 40.63)&(longitude >= -74.25)&(longitude < -74.05)),]$review_scores_rating
boxplot(scoreQueens, scoreManh, scoreBrook, scoreBronx, scoreStatenI)
summary(scoreBronx)
summary(scoreBrook)
summary(scoreManh)
summary(scoreQueens)
summary(scoreStatenI) #highest average brooklyn, lowest bronx

#NEIGHBOURHOODS
neighbourhood = AirBB_Dataset$neighbourhood
barplot(table(neighbourhood))
which.max(table(neighbourhood))
which.min(table(neighbourhood))
606/74111
14/74111

neighbourhood_NYC = AirBB_Dataset[(city == "NYC"),]$neighbourhood
boxplot(logprice_NYC ~ neighbourhood_NYC, cex.axis= 0.5, las = 2) #too many categories


neighbourhood_Boston = AirBB_Dataset[(city == "Boston"),]$neighbourhood
boxplot(logprice_Boston ~ neighbourhood_Boston, cex.axis= 0.5, las = 2)
table(neighbourhood_Boston)
which.max(table(neighbourhood_Boston))
which.min(table(neighbourhood_Boston))
neighbourhood_Boston
414/3468
1/3468

neighbourhood_SF = AirBB_Dataset[(city == "SF"),]$neighbourhood
boxplot(logprice_SF ~ neighbourhood_SF, cex.axis= 0.5, las = 2) 

neighbourhood_LA = AirBB_Dataset[(city == "LA"),]$neighbourhood
boxplot(logprice_LA ~ neighbourhood_LA, cex.axis= 0.5, las = 2) #too many categories

neighbourhood_Chicago = AirBB_Dataset[(city == "Chicago"),]$neighbourhood
boxplot(logprice_Chicago ~ neighbourhood_Chicago, cex.axis= 0.5, las = 2) #too many categories

neighbourhood_DC = AirBB_Dataset[(city == "DC"),]$neighbourhood
boxplot(logprice_DC ~ neighbourhood_DC, cex.axis= 0.5, las = 2) #too many categories

#NAMES 
AirBB_Dataset$name_length=(num_commas <- str_count(AirBB_Dataset$name, " "))+1
summary(AirBB_Dataset$name_length)
plot(AirBB_Dataset$name_length,AirBB_Dataset$review_scores_rating)
boxplot(AirBB_Dataset$log_price~AirBB_Dataset$name_length)#???
boxplot(AirBB_Dataset$number_of_reviews~AirBB_Dataset$name_length)
boxplot(AirBB_Dataset$review_scores_rating~AirBB_Dataset$name_length)
cor(AirBB_Dataset$name_length,AirBB_Dataset$log_price)


#NUMBER OF REVIEWS 
hist(AirBB_Dataset$number_of_reviews)
boxplot(AirBB_Dataset$number_of_reviews)
summary(AirBB_Dataset$number_of_reviews)
sd(AirBB_Dataset$number_of_reviews, na.rm = TRUE)
tapply(AirBB_Dataset$number_of_reviews, AirBB_Dataset$city,summary)
boxplot(AirBB_Dataset$review_scores_rating~AirBB_Dataset$number_of_reviews)
boxplot(AirBB_Dataset$log_price~AirBB_Dataset$number_of_reviews)

#REVIEW SCORE RATING 
hist(AirBB_Dataset$review_scores_rating)
summary(AirBB_Dataset$review_scores_rating)
sd(AirBB_Dataset$review_scores_rating, na.rm = TRUE)
boxplot(AirBB_Dataset$log_price~AirBB_Dataset$review_scores_rating)
boxplot(AirBB_Dataset$price_per_accommodate~AirBB_Dataset$review_scores_rating)
cor(AirBB_Dataset$log_price,AirBB_Dataset$review_scores_rating,use = "complete.obs")
tapply(AirBB_Dataset$review_scores_rating, AirBB_Dataset$city,summary)

#FIRST REVIEW
first_review = AirBB_Dataset$first_review
b = boxplot(log_price)
b$stats
#Airbbdataset222
first1 = AirBB_Dataset222[complete.cases(first_review)&(log_price < 2.995732),]$first_review
first2 = AirBB_Dataset222[complete.cases(first_review)&((log_price >= 2.995732)&(log_price < 4.317488)),]$first_review
first3 = AirBB_Dataset222[complete.cases(first_review)&((log_price >= 4.317488)&(log_price < 4.709530)),]$first_review
first4 = AirBB_Dataset222[complete.cases(first_review)&((log_price >= 4.709530)&(log_price < 5.220356)),]$first_review
first5 = AirBB_Dataset222[complete.cases(first_review)&((log_price >= 5.220356)&(log_price < 6.572283)),]$first_review
first6 = AirBB_Dataset222[complete.cases(first_review)&(log_price >= 6.572283),]$first_review
boxplot(first1, first2, first3, first4, first5, first6)

#BEDROOMS
AirBB_Dataset$price_per_accommodate <- log(exp(AirBB_Dataset$log_price)/(AirBB_Dataset$accommodates),base = exp(1))

bedrooms = AirBB_Dataset$bedrooms
summary(AirBB_Dataset$bedrooms)
sd(AirBB_Dataset$bedrooms)
boxplot(AirBB_Dataset$price_per_accommodate~AirBB_Dataset$bedrooms)
cor(AirBB_Dataset$bedrooms,AirBB_Dataset$price_per_accommodate, use="complete.obs")

boxplot(log_price~bedrooms)
cor(AirBB_Dataset$bedrooms,AirBB_Dataset$log_price, use="complete.obs")

boxplot(AirBB_Dataset$review_scores_rating~AirBB_Dataset$bedrooms)
boxplot(AirBB_Dataset$number_of_reviews~AirBB_Dataset$bedrooms)
cor(AirBB_Dataset$bedrooms,AirBB_Dataset$number_of_reviews, use="complete.obs")
cor(AirBB_Dataset$bedrooms,AirBB_Dataset$review_scores_rating, use="complete.obs")

u=table(AirBB_Dataset$bedrooms,AirBB_Dataset$room_type)
u
prop.table(u)

g=table(AirBB_Dataset$city, AirBB_Dataset$bedrooms)
prop.table(g,1)

#BEDS

beds = AirBB_Dataset$beds
summary(AirBB_Dataset$beds)
boxplot(AirBB_Dataset$beds)
sd(AirBB_Dataset$beds)
boxplot(AirBB_Dataset$price_per_accommodate~AirBB_Dataset$beds)
cor(AirBB_Dataset$beds,AirBB_Dataset$price_per_accommodate, use="complete.obs")
boxplot(log_price~beds)

cor(AirBB_Dataset$beds,AirBB_Dataset$log_price, use="complete.obs")

boxplot(AirBB_Dataset$review_scores_rating~AirBB_Dataset$beds)
boxplot(AirBB_Dataset$number_of_reviews~AirBB_Dataset$beds)
cor(AirBB_Dataset$beds,AirBB_Dataset$number_of_reviews, use="complete.obs")
cor(AirBB_Dataset$beds,AirBB_Dataset$review_scores_rating, use="complete.obs")

#BATHROOMS 
bathrooms= AirBB_Dataset$bathrooms
bathrooms[is.na(bathrooms)] <- 0
summary(bathrooms)
sd(bathrooms)

tab = table(log_price,bathrooms)
barplot(tab)
scatter.smooth(bathrooms,log_price)

boxplot(AirBB_Dataset$price_per_accommodate~AirBB_Dataset$bathrooms)
cor(AirBB_Dataset$bathrooms,AirBB_Dataset$price_per_accommodate, use="complete.obs")
boxplot(log_price~bathrooms)
cor(AirBB_Dataset$bathrooms,AirBB_Dataset$log_price, use="complete.obs")
boxplot(AirBB_Dataset$review_scores_rating~AirBB_Dataset$bathrooms)
boxplot(AirBB_Dataset$number_of_reviews~AirBB_Dataset$bathrooms)
prop.table(table(AirBB_Dataset$city,AirBB_Dataset$beds))

