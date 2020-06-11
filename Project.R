-############################################PREPARING DATA############################################

#Import necessary libraries
library(plyr)
library(readxl)
getwd()
#Read the raw data
raw_data <- read_excel("Billionaires_1996_to_2014.xlsx", sheet = "updated data 2-17")

#Clean the data
general_df <- na.omit(data.frame(Name = raw_data$name, Year = raw_data$year, Rank = raw_data$rank, Gender = raw_data$gender, Region = raw_data$region, IsAdvancedEconomy = raw_data$north, Sector = raw_data$sector, NetWorth = raw_data$networthusbillion, SelfMade = raw_data$selfmade))

#Create filters to return indexes by region
region_north_america <- match(general_df[general_df$Region=="North America",]$Name,general_df$Name)
region_europe <- match(general_df[general_df$Region=="Europe",]$Name,general_df$Name)
region_east_asia <- match(general_df[general_df$Region=="East Asia",]$Name,general_df$Name)
region_latin_america <- match(general_df[general_df$Region=="Latin America",]$Name,general_df$Name)
region_middle_east <- match(general_df[general_df$Region=="Middle East/North Africa",]$Name,general_df$Name)
region_south_asia <- match(general_df[general_df$Region=="South Asia",]$Name,general_df$Name)
region_sub_saharan_africa <- match(general_df[general_df$Region=="Sub-Saharan Africa",]$Name,general_df$Name)

#Create filters to return indexes by year
date_1996 <- match(general_df[general_df$Year=="1996",]$Name,general_df$Name)
date_2014 <- match(general_df[general_df$Year=="2014",]$Name,general_df$Name)

#Create filters to return indexes by self-made
self_made <- match(general_df[general_df$SelfMade=="self-made",]$Name,general_df$Name)

#Create filters to return indexes by sectors
realestate <- match(general_df[general_df$Sector=="real estate",]$Name,general_df$Name)
media <- match(general_df[general_df$Sector=="media",]$Name,general_df$Name)
hedge_funds <- match(general_df[general_df$Sector=="hedge funds",]$Name,general_df$Name)
construction <- match(general_df[general_df$Sector=="construction",]$Name,general_df$Name)
retail <- match(general_df[general_df$Sector=="retail",]$Name,general_df$Name)
software <- match(general_df[general_df$Sector=="software",]$Name,general_df$Name)
technology <- match(general_df[general_df$Sector=="technology",]$Name,general_df$Name)
pharmacy <- match(general_df[general_df$Sector=="pharmaceuticals",]$Name,general_df$Name)
oil <- match(general_df[general_df$Sector=="oil",]$Name,general_df$Name)
banking <- match(general_df[general_df$Sector=="banking",]$Name,general_df$Name)

#An Ex. usage of intersect of two or more filter to fetch related data
sum(general_df$NetWorth[Reduce(intersect, list(region_north_america, date_1996))])



############################################SELF-MADE BILLIONAIRES TABLE############################################

#Growth rate of #of billionaries by regions calculation
growth_of_billionaires <- c(
  length(general_df$Name[Reduce(intersect, list(region_north_america, date_2014))])*100/length(general_df$Name[Reduce(intersect, list(region_north_america, date_1996))]),
  length(general_df$Name[Reduce(intersect, list(region_europe, date_2014))])*100/length(general_df$Name[Reduce(intersect, list(region_europe, date_1996))]),
  length(general_df$Name[Reduce(intersect, list(region_east_asia, date_2014))])*100/length(general_df$Name[Reduce(intersect, list(region_east_asia, date_1996))]),
  length(general_df$Name[Reduce(intersect, list(region_latin_america, date_2014))])*100/length(general_df$Name[Reduce(intersect, list(region_latin_america, date_1996))]),
  length(general_df$Name[Reduce(intersect, list(region_middle_east, date_2014))])*100/length(general_df$Name[Reduce(intersect, list(region_middle_east, date_1996))]),
  length(general_df$Name[Reduce(intersect, list(region_south_asia, date_2014))])*100/length(general_df$Name[Reduce(intersect, list(region_south_asia, date_1996))]),
  length(general_df$Name[Reduce(intersect, list(region_sub_saharan_africa, date_2014))])*100/length(general_df$Name[Reduce(intersect, list(region_sub_saharan_africa, date_1996))]))


#Frequency distribution and percentage of self-made billionaires calculation

#North America
north_america_freq_1996 <- data.frame(count(general_df$SelfMade[Reduce(intersect, list(region_north_america, date_1996))]))
north_america_freq_2014 <- data.frame(count(general_df$SelfMade[Reduce(intersect, list(region_north_america, date_2014))]))

north_america_selfmade_per_1996 <- north_america_freq_1996$freq[2]*100 / sum(north_america_freq_1996$freq)
north_america_selfmade_per_2014 <- north_america_freq_2014$freq[2]*100 / sum(north_america_freq_2014$freq)

north_america_total_selfmade_1996 <- sum(general_df$NetWorth[Reduce(intersect, list(region_north_america, date_1996, self_made))])
north_america_total_selfmade_2014 <- sum(general_df$NetWorth[Reduce(intersect, list(region_north_america, date_2014, self_made))])


#Europe
europe_freq_1996 <- data.frame(count(general_df$SelfMade[Reduce(intersect, list(region_europe, date_1996))]))
europe_freq_2014 <- data.frame(count(general_df$SelfMade[Reduce(intersect, list(region_europe, date_2014))]))

europe_selfmade_per_1996 <- europe_freq_1996$freq[2]*100 / sum(europe_freq_1996$freq)
europe_selfmade_per_2014 <- europe_freq_2014$freq[2]*100 / sum(europe_freq_2014$freq)

europe_total_selfmade_1996 <- sum(general_df$NetWorth[Reduce(intersect, list(region_europe, date_1996, self_made))])
europe_total_selfmade_2014 <- sum(general_df$NetWorth[Reduce(intersect, list(region_europe, date_2014, self_made))])


#East Asia
east_asia_freq_1996 <- data.frame(count(general_df$SelfMade[Reduce(intersect, list(region_east_asia, date_1996))]))
east_asia_freq_2014 <- data.frame(count(general_df$SelfMade[Reduce(intersect, list(region_east_asia, date_2014))]))

east_asia_selfmade_per_1996 <- east_asia_freq_1996$freq[2]*100 / sum(east_asia_freq_1996$freq)
east_asia_selfmade_per_2014 <- east_asia_freq_2014$freq[2]*100 / sum(east_asia_freq_2014$freq)

east_asia_total_selfmade_1996 <- sum(general_df$NetWorth[Reduce(intersect, list(region_east_asia, date_1996, self_made))])
east_asia_total_selfmade_2014 <- sum(general_df$NetWorth[Reduce(intersect, list(region_east_asia, date_2014, self_made))])


#Latin America
latin_america_freq_1996 <- data.frame(count(general_df$SelfMade[Reduce(intersect, list(region_latin_america, date_1996))]))
latin_america_freq_2014 <- data.frame(count(general_df$SelfMade[Reduce(intersect, list(region_latin_america, date_2014))]))

latin_america_selfmade_per_1996 <- latin_america_freq_1996$freq[2]*100 / sum(latin_america_freq_1996$freq)
latin_america_selfmade_per_2014 <- latin_america_freq_2014$freq[2]*100 / sum(latin_america_freq_2014$freq)

latin_america_total_selfmade_1996 <- sum(general_df$NetWorth[Reduce(intersect, list(region_latin_america, date_1996, self_made))])
latin_america_total_selfmade_2014 <- sum(general_df$NetWorth[Reduce(intersect, list(region_latin_america, date_2014, self_made))])


#Middle East/North Africa
middle_east_freq_1996 <- data.frame(count(general_df$SelfMade[Reduce(intersect, list(region_middle_east, date_1996))]))
middle_east_freq_2014 <- data.frame(count(general_df$SelfMade[Reduce(intersect, list(region_middle_east, date_2014))]))

middle_east_selfmade_per_1996 <- middle_east_freq_1996$freq[2]*100 / sum(middle_east_freq_1996$freq)
middle_east_selfmade_per_2014 <- middle_east_freq_2014$freq[2]*100 / sum(middle_east_freq_2014$freq)

middle_east_total_selfmade_1996 <- sum(general_df$NetWorth[Reduce(intersect, list(region_middle_east, date_1996, self_made))])
middle_east_total_selfmade_2014 <- sum(general_df$NetWorth[Reduce(intersect, list(region_middle_east, date_2014, self_made))])


#South Asia
south_asia_freq_1996 <- data.frame(count(general_df$SelfMade[Reduce(intersect, list(region_south_asia, date_1996))]))
south_asia_freq_2014 <- data.frame(count(general_df$SelfMade[Reduce(intersect, list(region_south_asia, date_2014))]))

south_asia_selfmade_per_1996 <- south_asia_freq_1996$freq[2]*100 / sum(south_asia_freq_1996$freq)
south_asia_selfmade_per_2014 <- south_asia_freq_2014$freq[2]*100 / sum(south_asia_freq_2014$freq)

south_asia_total_selfmade_1996 <- sum(general_df$NetWorth[Reduce(intersect, list(region_south_asia, date_1996, self_made))])
south_asia_total_selfmade_2014 <- sum(general_df$NetWorth[Reduce(intersect, list(region_south_asia, date_2014, self_made))])


#Sub-Saharan Africa
sub_saharan_africa_freq_1996 <- data.frame(count(general_df$SelfMade[Reduce(intersect, list(region_sub_saharan_africa, date_1996))]))
sub_saharan_africa_freq_2014 <- data.frame(count(general_df$SelfMade[Reduce(intersect, list(region_sub_saharan_africa, date_2014))]))

sub_saharan_africa_selfmade_per_1996 <- sub_saharan_africa_freq_1996$freq[2]*100 / sum(sub_saharan_africa_freq_1996$freq)
sub_saharan_africa_selfmade_per_2014 <- sub_saharan_africa_freq_2014$freq[2]*100 / sum(sub_saharan_africa_freq_2014$freq)

sub_saharan_africa_total_selfmade_1996 <- sum(general_df$NetWorth[Reduce(intersect, list(region_sub_saharan_africa, date_1996, self_made))])
sub_saharan_africa_total_selfmade_2014 <- sum(general_df$NetWorth[Reduce(intersect, list(region_sub_saharan_africa, date_2014, self_made))])

#Create self-made billionaires table
billionaires_table <- data.frame(rbind(
  "North America" = list(length(general_df$Name[Reduce(intersect, list(region_north_america, date_1996))]),
                         length(general_df$Name[Reduce(intersect, list(region_north_america, date_2014))]),
                         "Advanced",
                         round(growth_of_billionaires[1],1),
                         round(north_america_selfmade_per_1996,1),
                         round(north_america_selfmade_per_2014,1),
                         round(north_america_total_selfmade_1996,1),
                         round(north_america_total_selfmade_2014)),
  "Europe" = list(length(general_df$Name[Reduce(intersect, list(region_europe, date_1996))]),
                  length(general_df$Name[Reduce(intersect, list(region_europe, date_2014))]),
                  "Advanced",
                  round(growth_of_billionaires[2],1),
                  round(europe_selfmade_per_1996,1),
                  round(europe_selfmade_per_2014,1),
                  round(europe_total_selfmade_1996,1),
                  round(europe_total_selfmade_2014)),
  "East Asia" = list(length(general_df$Name[Reduce(intersect, list(region_east_asia, date_1996))]),
                     length(general_df$Name[Reduce(intersect, list(region_east_asia, date_2014))]),
                     "Emerging",
                     round(growth_of_billionaires[3],1),
                     round(east_asia_selfmade_per_1996,1),
                     round(east_asia_selfmade_per_2014,1),
                     round(east_asia_total_selfmade_1996,1),
                     round(east_asia_total_selfmade_2014)),
  "Latin America" = list(length(general_df$Name[Reduce(intersect, list(region_latin_america, date_1996))]),
                         length(general_df$Name[Reduce(intersect, list(region_latin_america, date_2014))]),
                         "Emerging",
                         round(growth_of_billionaires[4],1),
                         round(latin_america_selfmade_per_1996,1),
                         round(latin_america_selfmade_per_2014,1),
                         round(latin_america_total_selfmade_1996,1),
                         round(latin_america_total_selfmade_2014)),
  "Middle East/North Africa" = list(length(general_df$Name[Reduce(intersect, list(region_middle_east, date_1996))]),
                                    length(general_df$Name[Reduce(intersect, list(region_middle_east, date_2014))]),
                                    "Emerging",
                                    round(growth_of_billionaires[5],1),
                                    round(middle_east_selfmade_per_1996,1),
                                    round(middle_east_selfmade_per_2014,1),
                                    round(middle_east_total_selfmade_1996,1),
                                    round(middle_east_total_selfmade_2014)),
  "South Asia" = list(length(general_df$Name[Reduce(intersect, list(region_south_asia, date_1996))]),
                      length(general_df$Name[Reduce(intersect, list(region_south_asia, date_2014))]),
                      "Emerging",
                      round(growth_of_billionaires[6],1),
                      round(south_asia_selfmade_per_1996,1),
                      round(south_asia_selfmade_per_2014,1),
                      round(south_asia_total_selfmade_1996,1),
                      round(south_asia_total_selfmade_2014)),
  "Sub-Saharan Africa" = list(length(general_df$Name[Reduce(intersect, list(region_sub_saharan_africa, date_1996))]),
                              length(general_df$Name[Reduce(intersect, list(region_sub_saharan_africa, date_2014))]),
                              "Emerging",
                              round(growth_of_billionaires[7],1),
                              round(sub_saharan_africa_selfmade_per_1996,1),
                              round(sub_saharan_africa_selfmade_per_2014,1),
                              round(sub_saharan_africa_total_selfmade_1996,1),
                              round(sub_saharan_africa_total_selfmade_2014))))

#Add the column names
colnames(billionaires_table) <- c("Total # of Billionaires in 1996", "Total # of Billionaires in 2014", "Region's Development Level", "Growth rate of #of Billionaires between 1996 and 2014 (%)", "The Percentage of Self-made wealth in 1996 (%)", "The Percentage of Self-made wealth in 2014 (%)", "The Total #of Self-made wealth in 1996 (b$)", "The Total #of Self-made wealth in 2014 (b$)")



############################################NETWORTH GROWTH RATE OF BILLIONAIRES TABLE############################################

#Growth rate of networth of billionaires by regions calculation
growth_of_networth <- c(
  sum(general_df$NetWorth[Reduce(intersect, list(region_north_america, date_2014))])*100/sum(general_df$NetWorth[Reduce(intersect, list(region_north_america, date_1996))]),
  sum(general_df$NetWorth[Reduce(intersect, list(region_europe, date_2014))])*100/sum(general_df$NetWorth[Reduce(intersect, list(region_europe, date_1996))]),
  sum(general_df$NetWorth[Reduce(intersect, list(region_east_asia, date_2014))])*100/sum(general_df$NetWorth[Reduce(intersect, list(region_east_asia, date_1996))]),
  sum(general_df$NetWorth[Reduce(intersect, list(region_latin_america, date_2014))])*100/sum(general_df$NetWorth[Reduce(intersect, list(region_latin_america, date_1996))]),
  sum(general_df$NetWorth[Reduce(intersect, list(region_middle_east, date_2014))])*100/sum(general_df$NetWorth[Reduce(intersect, list(region_middle_east, date_1996))]),
  sum(general_df$NetWorth[Reduce(intersect, list(region_south_asia, date_2014))])*100/sum(general_df$NetWorth[Reduce(intersect, list(region_south_asia, date_1996))]),
  sum(general_df$NetWorth[Reduce(intersect, list(region_sub_saharan_africa, date_2014))])*100/sum(general_df$NetWorth[Reduce(intersect, list(region_sub_saharan_africa, date_1996))]))

#Create networth growth rate of billionaires table
region_table <- data.frame(rbind(
  "North America" = list(sum(general_df$NetWorth[Reduce(intersect, list(region_north_america, date_1996))]),
                      sum(general_df$NetWorth[Reduce(intersect, list(region_north_america, date_2014))]),
                      "Advanced",
                      length(general_df$Name[Reduce(intersect, list(region_north_america, date_2014))]),
                      round(growth_of_networth[1],1),
                      min(general_df$NetWorth[Reduce(intersect, list(region_north_america, date_2014))]),
                      max(general_df$NetWorth[Reduce(intersect, list(region_north_america, date_2014))]),
                      round(mean(general_df$NetWorth[Reduce(intersect, list(region_north_america, date_2014))]),1),
                      round(sd(general_df$NetWorth[Reduce(intersect, list(region_north_america, date_2014))]),1)),
  "Europe" = list(sum(general_df$NetWorth[Reduce(intersect, list(region_europe, date_1996))]),
               sum(general_df$NetWorth[Reduce(intersect, list(region_europe, date_2014))]),
               "Advanced",
               length(general_df$Name[Reduce(intersect, list(region_europe, date_2014))]),
               round(growth_of_networth[2],1),
               min(general_df$NetWorth[Reduce(intersect, list(region_europe, date_2014))]),
               max(general_df$NetWorth[Reduce(intersect, list(region_europe, date_2014))]),
               round(mean(general_df$NetWorth[Reduce(intersect, list(region_europe, date_2014))]),1),
               round(sd(general_df$NetWorth[Reduce(intersect, list(region_europe, date_2014))]),1)),
  "East Asia" = list(sum(general_df$NetWorth[Reduce(intersect, list(region_east_asia, date_1996))]),
                  sum(general_df$NetWorth[Reduce(intersect, list(region_east_asia, date_2014))]),
                  "Emerging",
                  length(general_df$Name[Reduce(intersect, list(region_east_asia, date_2014))]),
                  round(growth_of_networth[3],1),
                  min(general_df$NetWorth[Reduce(intersect, list(region_east_asia, date_2014))]),
                  max(general_df$NetWorth[Reduce(intersect, list(region_east_asia, date_2014))]),
                  round(mean(general_df$NetWorth[Reduce(intersect, list(region_east_asia, date_2014))]),1),
                  round(sd(general_df$NetWorth[Reduce(intersect, list(region_east_asia, date_2014))]),1)),
  "Latin America" = list(sum(general_df$NetWorth[Reduce(intersect, list(region_latin_america, date_1996))]),
                      sum(general_df$NetWorth[Reduce(intersect, list(region_latin_america, date_2014))]),
                      "Emerging",
                      length(general_df$Name[Reduce(intersect, list(region_latin_america, date_2014))]),
                      round(growth_of_networth[4],1),
                      min(general_df$NetWorth[Reduce(intersect, list(region_latin_america, date_2014))]),
                      max(general_df$NetWorth[Reduce(intersect, list(region_latin_america, date_2014))]),
                      round(mean(general_df$NetWorth[Reduce(intersect, list(region_latin_america, date_2014))]),1),
                      round(sd(general_df$NetWorth[Reduce(intersect, list(region_latin_america, date_2014))]),1)),
  "Middle East/North Africa" = list(sum(general_df$NetWorth[Reduce(intersect, list(region_middle_east, date_1996))]),
                                 sum(general_df$NetWorth[Reduce(intersect, list(region_middle_east, date_2014))]),
                                 "Emerging",
                                 length(general_df$Name[Reduce(intersect, list(region_middle_east, date_2014))]),
                                 round(growth_of_networth[5],1),
                                 min(general_df$NetWorth[Reduce(intersect, list(region_middle_east, date_2014))]),
                                 max(general_df$NetWorth[Reduce(intersect, list(region_middle_east, date_2014))]),
                                 round(mean(general_df$NetWorth[Reduce(intersect, list(region_middle_east, date_2014))]),1),
                                 round(sd(general_df$NetWorth[Reduce(intersect, list(region_middle_east, date_2014))]),1)),
  "South Asia" = list(sum(general_df$NetWorth[Reduce(intersect, list(region_south_asia, date_1996))]),
                   sum(general_df$NetWorth[Reduce(intersect, list(region_south_asia, date_2014))]),
                   "Emerging",
                   length(general_df$Name[Reduce(intersect, list(region_south_asia, date_2014))]),
                   round(growth_of_networth[6],1),
                   min(general_df$NetWorth[Reduce(intersect, list(region_south_asia, date_2014))]),
                   max(general_df$NetWorth[Reduce(intersect, list(region_south_asia, date_2014))]),
                   round(mean(general_df$NetWorth[Reduce(intersect, list(region_south_asia, date_2014))]),1),
                   round(sd(general_df$NetWorth[Reduce(intersect, list(region_south_asia, date_2014))]),1)),
  "Sub-Saharan Africa" = list(sum(general_df$NetWorth[Reduce(intersect, list(region_sub_saharan_africa, date_1996))]),
                           sum(general_df$NetWorth[Reduce(intersect, list(region_sub_saharan_africa, date_2014))]),
                           "Emerging",
                           length(general_df$Name[Reduce(intersect, list(region_sub_saharan_africa, date_2014))]),
                           round(growth_of_networth[7],1),
                           min(general_df$NetWorth[Reduce(intersect, list(region_sub_saharan_africa, date_2014))]),
                           max(general_df$NetWorth[Reduce(intersect, list(region_sub_saharan_africa, date_2014))]),
                           round(mean(general_df$NetWorth[Reduce(intersect, list(region_sub_saharan_africa, date_2014))]),1),
                           round(sd(general_df$NetWorth[Reduce(intersect, list(region_sub_saharan_africa, date_2014))]),1))))

#Add the column names
colnames(region_table) <- c("Total Networth of Billionaires in 1996 (b$)", "Total Networth of Billionaires in 2014 (b$)", "Region's Development Level", "Total # of Billionaires", "Growth rate between 1996 and 2014 (%)", "The Min Networth of Billionaires in 2014 (b$)", "The Max Networth of Billionaires in 2014 (b$)", "The Mean of Networth of Billionaires in 2014 (b$)", "The Std of Networth of Billionaires in 2014")

#Calculate and add the total informations
advanced_regions_ratio <- round((growth_of_networth[1]+growth_of_networth[2])/2)
emerging_regions_ratio <- round((growth_of_networth[3]+growth_of_networth[4]+growth_of_networth[5]+growth_of_networth[6]+growth_of_networth[7])/5)
total_networth_growth_ratio <- paste("Emer/Adv = ", round(emerging_regions_ratio / round(advanced_regions_ratio), 1))

region_table <- rbind(region_table, "TOTAL" = list(
  sum(unlist(region_table$`Total Networth of Billionaires in 1996 (b$)`, use.names = FALSE)),
  sum(unlist(region_table$`Total Networth of Billionaires in 2014 (b$)`, use.names = FALSE)),
  "N/A",
  sum(unlist(region_table$`Total # of Billionaires`, use.names = FALSE)),
  total_networth_growth_ratio,
  "N/A",
  "N/A",
  "N/A",
  "N/A"))



############################################CREATE FREQUENCY DISTRUBITION TABLE OF NETWORTH GROWTH RATE OF BILLIONAIRES############################################

#Set the break points
breaks <- seq(min(general_df$NetWorth[Reduce(intersect, list(region_north_america, date_2014))]), max(general_df$NetWorth[Reduce(intersect, list(region_north_america, date_2014))]), by=5)

#Calculation frequency distrubition per region
north_america_freq_table <- count(general_df$NetWorth[Reduce(intersect, list(region_north_america, date_2014))])
north_america_freq_table.cut <- cut(north_america_freq_table$x, breaks, right=FALSE)
north_america_freq_table.freq <- data.frame(table(north_america_freq_table.cut))

region_europe_freq_table <- count(general_df$NetWorth[Reduce(intersect, list(region_europe, date_2014))])
region_europe_freq_table.cut <- cut(region_europe_freq_table$x, breaks, right=FALSE)
region_europe_freq_table.freq <- data.frame(table(region_europe_freq_table.cut))

region_east_asia_freq_table <- count(general_df$NetWorth[Reduce(intersect, list(region_east_asia, date_2014))])
region_east_asia_freq_table.cut <- cut(region_east_asia_freq_table$x, breaks, right=FALSE)
region_east_asia_freq_table.freq <- data.frame(table(region_east_asia_freq_table.cut))

region_latin_america_freq_table <- count(general_df$NetWorth[Reduce(intersect, list(region_latin_america, date_2014))])
region_latin_america_freq_table.cut <- cut(region_latin_america_freq_table$x, breaks, right=FALSE)
region_latin_america_freq_table.freq <- data.frame(table(region_latin_america_freq_table.cut))

region_middle_east_freq_table <- count(general_df$NetWorth[Reduce(intersect, list(region_middle_east, date_2014))])
region_middle_east_freq_table.cut <- cut(region_middle_east_freq_table$x, breaks, right=FALSE)
region_middle_east_freq_table.freq <- data.frame(table(region_middle_east_freq_table.cut))

region_south_asia_freq_table <- count(general_df$NetWorth[Reduce(intersect, list(region_south_asia, date_2014))])
region_south_asia_freq_table.cut <- cut(region_south_asia_freq_table$x, breaks, right=FALSE)
region_south_asia_freq_table.freq <- data.frame(table(region_south_asia_freq_table.cut))

region_sub_saharan_africa_freq_table <- count(general_df$NetWorth[Reduce(intersect, list(region_sub_saharan_africa, date_2014))])
region_sub_saharan_africa_freq_table.cut <- cut(region_sub_saharan_africa_freq_table$x, breaks, right=FALSE)
region_sub_saharan_africa_freq_table.freq <- data.frame(table(region_sub_saharan_africa_freq_table.cut))


#Create the frequency distrubition table of networth growth rate of billionaires
region_freq_table <- data.frame(rbind(
  "North America" = north_america_freq_table.freq$Freq,
  "Europe" = region_europe_freq_table.freq$Freq,
  "East Asia" = region_east_asia_freq_table.freq$Freq,
  "Latin America" = region_latin_america_freq_table.freq$Freq,
  "Middle East/North Africa" = region_middle_east_freq_table.freq$Freq,
  "South Asia" = region_south_asia_freq_table.freq$Freq,
  "Sub-Saharan Africa" = region_sub_saharan_africa_freq_table.freq$Freq
))

#Add the column names
colnames(region_freq_table) <- c("1 to 6 b$", "6 to 11 b$", "11 to 16 b$", "16 to 21 b$", "21 to 26 b$", "26 to 31 b$", "31 to 36 b$")



############################################NETWORTH GROWTH RATE OF BILLIONAIRES TABLE############################################

#Growth rate of networth of billionaires by regions calculation
sector_table <- data.frame(rbind(
  "Real Estate" = list(
    tp96 <- length(general_df$Name[Reduce(intersect, list(date_1996, realestate))]),
    tp14 <- length(general_df$Name[Reduce(intersect, list(date_2014, realestate))]),
    tm96 <- sum(general_df$NetWorth[Reduce(intersect, list(date_1996, realestate,region_east_asia))]),
    tm14 <-sum(general_df$NetWorth[Reduce(intersect, list(date_2014, realestate,region_north_america))]),
    p <- round(tp14*100/tp96,1),
    m <- round(tm14*100/tm96,1),
    round(m/p*100,1),
    round((1-m/p)*-1*100,1)
  ),
  "Retail" = list(
    tp96 <- length(general_df$Name[Reduce(intersect, list(date_1996, retail))]),
    tp14 <- length(general_df$Name[Reduce(intersect, list(date_2014, retail))]),
    tm96 <- sum(general_df$NetWorth[Reduce(intersect, list(date_1996, retail))]),
    tm14 <-sum(general_df$NetWorth[Reduce(intersect, list(date_2014, retail))]),
    p <- round(tp14*100/tp96,1),
    m <- round(tm14*100/tm96,1),
    round(m/p*100,1),
    round((1-m/p)*-1*100,1)
  ),
  "Media" = list(
    tp96 <- length(general_df$Name[Reduce(intersect, list(date_1996, media))]),
    tp14 <- length(general_df$Name[Reduce(intersect, list(date_2014, media))]),
    tm96 <- sum(general_df$NetWorth[Reduce(intersect, list(date_1996, media))]),
    tm14 <-sum(general_df$NetWorth[Reduce(intersect, list(date_2014, media))]),
    p <- round(tp14*100/tp96,1),
    m <- round(tm14*100/tm96,1),
    round(m/p*100,1),
    round((1-m/p)*-1*100,1)
  ),
  "Construction" = list(
    tp96 <- length(general_df$Name[Reduce(intersect, list(date_1996, construction))]),
    tp14 <- length(general_df$Name[Reduce(intersect, list(date_2014, construction))]),
    tm96 <- sum(general_df$NetWorth[Reduce(intersect, list(date_1996, construction))]),
    tm14 <-sum(general_df$NetWorth[Reduce(intersect, list(date_2014, construction))]),
    p <- round(tp14*100/tp96,1),
    m <- round(tm14*100/tm96,1),
    round(m/p*100,1),
    round((1-m/p)*-1*100,1)
  ),
  "Banking" = list(
    tp96 <- length(general_df$Name[Reduce(intersect, list(date_1996, banking))]),
    tp14 <- length(general_df$Name[Reduce(intersect, list(date_2014, banking))]),
    tm96 <- sum(general_df$NetWorth[Reduce(intersect, list(date_1996, banking))]),
    tm14 <-sum(general_df$NetWorth[Reduce(intersect, list(date_2014, banking))]),
    p <- round(tp14*100/tp96,1),
    m <- round(tm14*100/tm96,1),
    round(m/p*100,1),
    round((1-m/p)*-1*100,1)
  ),
  "Oil" = list(
    tp96 <- length(general_df$Name[Reduce(intersect, list(date_1996, oil))]),
    tp14 <- length(general_df$Name[Reduce(intersect, list(date_2014, oil))]),
    tm96 <- sum(general_df$NetWorth[Reduce(intersect, list(date_1996, oil))]),
    tm14 <-sum(general_df$NetWorth[Reduce(intersect, list(date_2014, oil))]),
    p <- round(tp14*100/tp96,1),
    m <- round(tm14*100/tm96,1),
    round(m/p*100,1),
    round((1-m/p)*-1*100,1)
  ),
  "Pharmacy" = list(
    tp96 <- length(general_df$Name[Reduce(intersect, list(date_1996, pharmacy))]),
    tp14 <- length(general_df$Name[Reduce(intersect, list(date_2014, pharmacy))]),
    tm96 <- sum(general_df$NetWorth[Reduce(intersect, list(date_1996, pharmacy))]),
    tm14 <-sum(general_df$NetWorth[Reduce(intersect, list(date_2014, pharmacy))]),
    p <- round(tp14*100/tp96,1),
    m <- round(tm14*100/tm96,1),
    round(m/p*100,1),
    round((1-m/p)*-1*100,1)
  ),
  "Software" = list(
    tp96 <- length(general_df$Name[Reduce(intersect, list(date_1996, software))]),
    tp14 <- length(general_df$Name[Reduce(intersect, list(date_2014, software))]),
    tm96 <- sum(general_df$NetWorth[Reduce(intersect, list(date_1996, software))]),
    tm14 <-sum(general_df$NetWorth[Reduce(intersect, list(date_2014, software))]),
    p <- round(tp14*100/tp96,1),
    m <- round(tm14*100/tm96,1),
    round(m/p*100,1),
    round((1-m/p)*-1*100,1)
  ),
  "Hedge Funds" = list(
    tp96 <- length(general_df$Name[Reduce(intersect, list(date_1996, hedge_funds))]),
    tp14 <- length(general_df$Name[Reduce(intersect, list(date_2014, hedge_funds))]),
    tm96 <- sum(general_df$NetWorth[Reduce(intersect, list(date_1996, hedge_funds))]),
    tm14 <-sum(general_df$NetWorth[Reduce(intersect, list(date_2014, hedge_funds))]),
    p <- round(tp14*100/tp96,1),
    m <- round(tm14*100/tm96,1),
    round(m/p*100,1),
    round((1-m/p)*-1*100,1)
  ),
  "Technology" = list(
    tp96 <- length(general_df$Name[Reduce(intersect, list(date_1996, technology))]),
    tp14 <- length(general_df$Name[Reduce(intersect, list(date_2014, technology))]),
    tm96 <- sum(general_df$NetWorth[Reduce(intersect, list(date_1996, technology))]),
    tm14 <-sum(general_df$NetWorth[Reduce(intersect, list(date_2014, technology))]),
    p <- round(tp14*100/tp96,1),
    m <- round(tm14*100/tm96,1),
    round(m/p*100,1),
    round((1-m/p)*-1*100,1)
  )
))

#### Region sector networth filter #####

#east asia technology
sum(general_df$NetWorth[Reduce(intersect, list(date_1996, technology,region_east_asia))])
sum(general_df$NetWorth[Reduce(intersect, list(date_2014, technology,region_east_asia))])

#east asia realestate
sum(general_df$NetWorth[Reduce(intersect, list(date_1996, realestate,region_east_asia))])
sum(general_df$NetWorth[Reduce(intersect, list(date_2014,realestate ,region_east_asia))])

#east asia construction
sum(general_df$NetWorth[Reduce(intersect, list(date_1996, construction,region_east_asia))])
sum(general_df$NetWorth[Reduce(intersect, list(date_2014, construction,region_east_asia))])

#east asia banking
sum(general_df$NetWorth[Reduce(intersect, list(date_1996, banking,region_east_asia))])
sum(general_df$NetWorth[Reduce(intersect, list(date_2014, banking,region_east_asia))])

#east asia oil
sum(general_df$NetWorth[Reduce(intersect, list(date_1996, oil,region_east_asia))])
sum(general_df$NetWorth[Reduce(intersect, list(date_2014, oil,region_east_asia))])

### east asia software
sum(general_df$NetWorth[Reduce(intersect, list(date_1996, software,region_east_asia))])
sum(general_df$NetWorth[Reduce(intersect, list(date_2014, software,region_east_asia))])


### North america technology
sum(general_df$NetWorth[Reduce(intersect, list(date_1996, technology,region_north_america))])
sum(general_df$NetWorth[Reduce(intersect, list(date_2014, technology,region_north_america))])

### North america realestate
sum(general_df$NetWorth[Reduce(intersect, list(date_1996, realestate,region_north_america))])
sum(general_df$NetWorth[Reduce(intersect, list(date_2014, realestate,region_north_america))])

### North america construction
sum(general_df$NetWorth[Reduce(intersect, list(date_1996, construction,region_north_america))])
sum(general_df$NetWorth[Reduce(intersect, list(date_2014, construction,region_north_america))])

### North america bankin
sum(general_df$NetWorth[Reduce(intersect, list(date_1996, banking,region_north_america))])
sum(general_df$NetWorth[Reduce(intersect, list(date_2014, banking,region_north_america))])

### North america oil
sum(general_df$NetWorth[Reduce(intersect, list(date_1996, oil,region_north_america))])
sum(general_df$NetWorth[Reduce(intersect, list(date_2014, oil,region_north_america))])

### North america software
sum(general_df$NetWorth[Reduce(intersect, list(date_1996, software,region_north_america))])
sum(general_df$NetWorth[Reduce(intersect, list(date_2014, software,region_north_america))])




### region_europe technology
sum(general_df$NetWorth[Reduce(intersect, list(date_1996, technology,region_europe))])
sum(general_df$NetWorth[Reduce(intersect, list(date_2014, technology,region_europe))])

### region_europe realestate
sum(general_df$NetWorth[Reduce(intersect, list(date_1996, realestate,region_europe))])
sum(general_df$NetWorth[Reduce(intersect, list(date_2014, realestate,region_europe))])

### region_europe construction
sum(general_df$NetWorth[Reduce(intersect, list(date_1996, construction,region_europe))])
sum(general_df$NetWorth[Reduce(intersect, list(date_2014, construction,region_europe))])

### region_europe bankin
sum(general_df$NetWorth[Reduce(intersect, list(date_1996, banking,region_europe))])
sum(general_df$NetWorth[Reduce(intersect, list(date_2014, banking,region_europe))])

### region_europe oil
sum(general_df$NetWorth[Reduce(intersect, list(date_1996, oil,region_europe))])
sum(general_df$NetWorth[Reduce(intersect, list(date_2014, oil,region_europe))])

### region_europe software
sum(general_df$NetWorth[Reduce(intersect, list(date_1996, software,region_europe))])
sum(general_df$NetWorth[Reduce(intersect, list(date_2014, software,region_europe))])






### region_middle_east technology
sum(general_df$NetWorth[Reduce(intersect, list(date_1996, technology,region_middle_east))])
sum(general_df$NetWorth[Reduce(intersect, list(date_2014, technology,region_middle_east))])

### region_middle_east realestate
sum(general_df$NetWorth[Reduce(intersect, list(date_1996, realestate,region_middle_east))])
sum(general_df$NetWorth[Reduce(intersect, list(date_2014, realestate,region_middle_east))])

### region_middle_east construction
sum(general_df$NetWorth[Reduce(intersect, list(date_1996, construction,region_middle_east))])
sum(general_df$NetWorth[Reduce(intersect, list(date_2014, construction,region_middle_east))])

### region_middle_east bankin
sum(general_df$NetWorth[Reduce(intersect, list(date_1996, banking,region_middle_east))])
sum(general_df$NetWorth[Reduce(intersect, list(date_2014, banking,region_middle_east))])

### region_middle_east oil
sum(general_df$NetWorth[Reduce(intersect, list(date_1996, oil,region_middle_east))])
sum(general_df$NetWorth[Reduce(intersect, list(date_2014, oil,region_middle_east))])

### region_middle_east software
sum(general_df$NetWorth[Reduce(intersect, list(date_1996, software,region_middle_east))])
sum(general_df$NetWorth[Reduce(intersect, list(date_2014, software,region_middle_east))])





### region_south_asia technology
sum(general_df$NetWorth[Reduce(intersect, list(date_1996, technology,region_south_asia))])
sum(general_df$NetWorth[Reduce(intersect, list(date_2014, technology,region_south_asia))])

### region_south_asia realestate
sum(general_df$NetWorth[Reduce(intersect, list(date_1996, realestate,region_south_asia))])
sum(general_df$NetWorth[Reduce(intersect, list(date_2014, realestate,region_south_asia))])

### region_south_asia construction
sum(general_df$NetWorth[Reduce(intersect, list(date_1996, construction,region_south_asia))])
sum(general_df$NetWorth[Reduce(intersect, list(date_2014, construction,region_south_asia))])

### region_south_asia bankin
sum(general_df$NetWorth[Reduce(intersect, list(date_1996, banking,region_south_asia))])
sum(general_df$NetWorth[Reduce(intersect, list(date_2014, banking,region_south_asia))])

### region_south_asia oil
sum(general_df$NetWorth[Reduce(intersect, list(date_1996, oil,region_south_asia))])
sum(general_df$NetWorth[Reduce(intersect, list(date_2014, oil,region_south_asia))])

### region_south_asia software
sum(general_df$NetWorth[Reduce(intersect, list(date_1996, software,region_south_asia))])
sum(general_df$NetWorth[Reduce(intersect, list(date_2014, software,region_south_asia))])




### region_latin_america technology
sum(general_df$NetWorth[Reduce(intersect, list(date_1996, technology,region_latin_america))])
sum(general_df$NetWorth[Reduce(intersect, list(date_2014, technology,region_latin_america))])

### region_latin_america realestate
sum(general_df$NetWorth[Reduce(intersect, list(date_1996, realestate,region_latin_america))])
sum(general_df$NetWorth[Reduce(intersect, list(date_2014, realestate,region_latin_america))])

### region_latin_america construction
sum(general_df$NetWorth[Reduce(intersect, list(date_1996, construction,region_latin_america))])
sum(general_df$NetWorth[Reduce(intersect, list(date_2014, construction,region_latin_america))])

### region_latin_america bankin
sum(general_df$NetWorth[Reduce(intersect, list(date_1996, banking,region_latin_america))])
sum(general_df$NetWorth[Reduce(intersect, list(date_2014, banking,region_latin_america))])

### region_latin_america oil
sum(general_df$NetWorth[Reduce(intersect, list(date_1996, oil,region_latin_america))])
sum(general_df$NetWorth[Reduce(intersect, list(date_2014, oil,region_latin_america))])

### region_latin_america software
sum(general_df$NetWorth[Reduce(intersect, list(date_1996, software,region_latin_america))])
sum(general_df$NetWorth[Reduce(intersect, list(date_2014, software,region_latin_america))])



### region_sub_saharan_africa technology
sum(general_df$NetWorth[Reduce(intersect, list(date_1996, technology,region_sub_saharan_africa))])
sum(general_df$NetWorth[Reduce(intersect, list(date_2014, technology,region_sub_saharan_africa))])

### region_sub_saharan_africa realestate
sum(general_df$NetWorth[Reduce(intersect, list(date_1996, realestate,region_sub_saharan_africa))])
sum(general_df$NetWorth[Reduce(intersect, list(date_2014, realestate,region_sub_saharan_africa))])

### region_sub_saharan_africa construction
sum(general_df$NetWorth[Reduce(intersect, list(date_1996, construction,region_sub_saharan_africa))])
sum(general_df$NetWorth[Reduce(intersect, list(date_2014, construction,region_sub_saharan_africa))])

### region_sub_saharan_africa bankin
sum(general_df$NetWorth[Reduce(intersect, list(date_1996, banking,region_sub_saharan_africa))])
sum(general_df$NetWorth[Reduce(intersect, list(date_2014, banking,region_sub_saharan_africa))])

### region_sub_saharan_africa oil
sum(general_df$NetWorth[Reduce(intersect, list(date_1996, oil,region_sub_saharan_africa))])
sum(general_df$NetWorth[Reduce(intersect, list(date_2014, oil,region_sub_saharan_africa))])

### region_sub_saharan_africa software
sum(general_df$NetWorth[Reduce(intersect, list(date_1996, software,region_sub_saharan_africa))])
sum(general_df$NetWorth[Reduce(intersect, list(date_2014, software,region_sub_saharan_africa))])









#Add the column names
colnames(sector_table) <- c("Total #of Billionaires in 1996", "Total #of Billionaires in 2014", "Total Networth of Billionaires in 1996 (b$)", "Total Networth of Billionaires in 2014 (b$)", "The percentage increase in number of people between 1996 and 2014 (%)", "The percentage increase in total earned money between 1996 and 2014 (%)", "2014's profit ratio of the sector compared to 1996 in percentage (%)", "Expected profit / loss ratio in 2014 compared to 1996 in percentage (%)")



###########################################OLD CODES TO TEST AND UNDERSTAND THE LOGIC############################################

#sum(general_df$IsAdvancedEconomy[Reduce(intersect, list(region_north_america, date_1996))])

#Total NetWorths in Not Advanced Regions per Year
networth_sum_in_1996_not_advanced <- 0
networth_sum_in_2001_not_advanced <- 0
networth_sum_in_2014_not_advanced <- 0

for(i in rownames(general_df)) {
  if (general_df[i, "Year"] == "1996" && general_df[i, "IsAdvancedEconomy"] == "0") {
    networth_sum_in_1996_not_advanced <- networth_sum_in_1996_not_advanced + general_df[i, "NetWorth"]
  } else if (general_df[i, "Year"] == "2001" && general_df[i, "IsAdvancedEconomy"] == "0") {
    networth_sum_in_2001_not_advanced <- networth_sum_in_2001_not_advanced + general_df[i, "NetWorth"]
  } else if (general_df[i, "Year"] == "2014" && general_df[i, "IsAdvancedEconomy"] == "0") {
    networth_sum_in_2014_not_advanced <- networth_sum_in_2014_not_advanced + general_df[i, "NetWorth"]
  }
}

total_groth_in_not_advanced <- 100*networth_sum_in_2014_not_advanced/networth_sum_in_1996_not_advanced

#Total NetWorths in Advanced Regions per Year
networth_sum_in_1996_advanced <- 0
networth_sum_in_2001_advanced <- 0
networth_sum_in_2014_advanced <- 0

for(i in rownames(general_df)) {
  if (general_df[i, "Year"] == "1996" && general_df[i, "IsAdvancedEconomy"] == "1") {
    networth_sum_in_1996_advanced <- networth_sum_in_1996_advanced + general_df[i, "NetWorth"]
  } else if (general_df[i, "Year"] == "2001" && general_df[i, "IsAdvancedEconomy"] == "1") {
    networth_sum_in_2001_advanced <- networth_sum_in_2001_advanced + general_df[i, "NetWorth"]
  } else if (general_df[i, "Year"] == "2014" && general_df[i, "IsAdvancedEconomy"] == "1") {
    networth_sum_in_2014_advanced <- networth_sum_in_2014_advanced + general_df[i, "NetWorth"]
  }
}

total_groth_in_advanced <- 100*networth_sum_in_2014_advanced/networth_sum_in_1996_advanced

##########

sector_freq <- count(general_df$Sector)
tail(sector_freq[order(sector_freq$freq),c(1,2)], 10)

for(i in rownames(general_df)) {
  if (general_df[i, "Sector"] == "technology") {
    tech <- rbind(general_df[i])
  }
}



#network için descriptive
library(dplyr)
#install.packages("Hmisc",dependencies = TRUE)
library(Hmisc)
describe(general_df)
#install.packages("propagate",dependencies = TRUE)
library (propagate)




#Descriptive for NORTH AMERICA
describe(match(general_df[general_df$Region=="North America",]$NetWorth,general_df$NetWorth))
round(skewness(match(general_df[general_df$Region=="North America",]$NetWorth,general_df$NetWorth)),2)
round(kurtosis(match(general_df[general_df$Region=="North America",]$NetWorth,general_df$NetWorth)),2)
round(sd(match(general_df[general_df$Region=="North America",]$NetWorth,general_df$NetWorth)),2)
round(var(match(general_df[general_df$Region=="North America",]$NetWorth,general_df$NetWorth)),2)


#Descriptive for Middle East/North Africa
describe(match(general_df[general_df$Region=="Middle East/North Africa",]$NetWorth,general_df$NetWorth))
round(skewness(match(general_df[general_df$Region=="Middle East/North Africa",]$NetWorth,general_df$NetWorth)),2)
round(kurtosis(match(general_df[general_df$Region=="Middle East/North Africa",]$NetWorth,general_df$NetWorth)),2)
round(sd(match(general_df[general_df$Region=="Middle East/North Africa",]$NetWorth,general_df$NetWorth)),2)
round(var(match(general_df[general_df$Region=="Middle East/North Africa",]$NetWorth,general_df$NetWorth)),2)


#Descriptive for Sub-Saharan Africa
describe(match(general_df[general_df$Region=="Sub-Saharan Africa",]$NetWorth,general_df$NetWorth))
round(skewness(match(general_df[general_df$Region=="Sub-Saharan Africa",]$NetWorth,general_df$NetWorth)),2)
round(kurtosis(match(general_df[general_df$Region=="Sub-Saharan Africa",]$NetWorth,general_df$NetWorth)),2)
round(sd(match(general_df[general_df$Region=="Sub-Saharan Africa",]$NetWorth,general_df$NetWorth)),2)
round(var(match(general_df[general_df$Region=="Sub-Saharan Africa",]$NetWorth,general_df$NetWorth)),2)


#Descriptive for Latin America 
describe(match(general_df[general_df$Region=="Latin America",]$NetWorth,general_df$NetWorth))
round(skewness(match(general_df[general_df$Region=="Latin America",]$NetWorth,general_df$NetWorth)),2)
round(kurtosis(match(general_df[general_df$Region=="Latin America",]$NetWorth,general_df$NetWorth)),2)
round(sd(match(general_df[general_df$Region=="Latin America",]$NetWorth,general_df$NetWorth)),2)
round(var(match(general_df[general_df$Region=="Latin America",]$NetWorth,general_df$NetWorth)),2)



#Descriptive for Europe  
describe(match(general_df[general_df$Region=="Europe",]$NetWorth,general_df$NetWorth))
round(skewness(match(general_df[general_df$Region=="Europe",]$NetWorth,general_df$NetWorth)),2)
round(kurtosis(match(general_df[general_df$Region=="Europe",]$NetWorth,general_df$NetWorth)),2)
round(sd(match(general_df[general_df$Region=="Europe",]$NetWorth,general_df$NetWorth)),2)
round(var(match(general_df[general_df$Region=="Europe",]$NetWorth,general_df$NetWorth)),2)

#Descriptive for East Asia
describe(match(general_df[general_df$Region=="East Asia",]$NetWorth,general_df$NetWorth))
round(skewness(match(general_df[general_df$Region=="East Asia",]$NetWorth,general_df$NetWorth)),2)
round(kurtosis(match(general_df[general_df$Region=="East Asia",]$NetWorth,general_df$NetWorth)),2)
round(sd(match(general_df[general_df$Region=="East Asia",]$NetWorth,general_df$NetWorth)),2)
round(var(match(general_df[general_df$Region=="East Asia",]$NetWorth,general_df$NetWorth)),2)




#Descriptive for South Asia
describe(match(general_df[general_df$Region=="South Asia",]$NetWorth,general_df$NetWorth))
round(skewness(match(general_df[general_df$Region=="South Asia",]$NetWorth,general_df$NetWorth)),2)
round(kurtosis(match(general_df[general_df$Region=="South Asia",]$NetWorth,general_df$NetWorth)),2)
round(sd(match(general_df[general_df$Region=="South Asia",]$NetWorth,general_df$NetWorth)),2)
round(var(match(general_df[general_df$Region=="South Asia",]$NetWorth,general_df$NetWorth)),2)

#descrivtive read
 regiondescc<-read_excel("Networth.xlsx")
  
library(plotly)
packageVersion('plotly')
 
getwd()
sector_table
write.csv(region_table,file="region.csv")
write.csv(billionaires_table,file="billioner.csv")
write.csv(sector_table, file = "MyData.csv")

sectorvector<-as.table(as.matrix(sector_table$`2014's profit ratio of the sector compared to 1996 in percentage (%)`))
colnames(sectorvector)<-c("RealEstate","Retail","Media","Construction","Banking","Oil","Pharmacy","Software","HedgeFunds","Technology")                     

sector_table
namessector<-c("RealEstate","Retail","Media","Construction","Banking","Oil","Pharmacy","Software","HedgeFunds","Technology")
barplot(sectorvector,names.arg = namessector)


