#------Loading Dataset------

setwd("D:\\College work\\Semester 4\\R programming\\ipl_analysis")
df <- read.csv("ipl_matches.csv", header = TRUE)

#------Basic Information------

sapply(df, class)   #shows datatype
summary(df)         #summary of the data frame
names(df)           #name of columns
head(df)            #first 5 rows

#------Analysis-----

#Top 11 stadium with most IPL matches
print(sort(table(df$venue), decreasing = TRUE)[seq(0:10)])

#Top 11 player who won most Man of The Match
player_of_match <- sort(table(df$player_of_match), decreasing = TRUE)
print(player_of_match[seq(0:10)])

#Toss Decisions
print(table(df$toss_decision))

#------Data Cleaning------

#Removing unnecessary columns
df <- subset(df, select = -c(id,player_of_match, neutral_venue, method, umpire1, umpire2))

#Extracting year from date
date <- as.POSIXct(df$date, format = "%d-%m-%Y")
year <- format(date, format = "%Y")
df['year'] <- as.integer(c(year))

#Checking missing values
sum(is.na(df))

#Handling Missing Values
df <- na.omit(df)
nrow(df)

head(df)

#------Visualization-----

library(ggplot2)       # R library for visualization

#Most wins in IPL(pie chart)
winner <- table(df$winner)

ggplot(data = as.data.frame(winner), aes(x = "", y = Freq, fill = Var1)) +
  geom_col(width = 1, color = "black") +
  coord_polar("y", start = 0) +
  scale_fill_manual(values = rainbow(length(winner))) +
  labs(title = "Most Wins in IPL") +
  theme_void()

#Top 10 cities with most IPL matches(bar graph)
venue <- sort(table(df$city), decreasing = TRUE)
top_10_venues <- head(venue, 10)

ggplot(data = data.frame(city = names(top_10_venues), 
                         matches = as.numeric(top_10_venues)), 
       aes(x = city, y = matches)) +
  geom_col(color = 'red', fill = 'navy') +
  xlab('City Names') + ylab('Number of Matches') +
  ggtitle('Top 10 Cities with Most IPL Matches') +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

#------Chi-Square test-----

#Taking a random sample of size 100 from the population
sample <- df[sample(nrow(df), 100), ]

# Making the contingency Table
twmw <- 0           # Toss Won, Match Won
tlmw <- 0           # Toss Lost, Match Won
twml <- 0           # Toss Won, Match Lost
tlml <- 0           # Toss Lost, Match Lost

for(i in 1:nrow(sample)){
  if(sample[i, "team1"] != sample[i, "toss_winner"]){
   if(sample[i, "team1"] == sample[i, "winner"]){
     tlmw <- tlmw + 1
   } else{
     tlml <- tlml + 1
   }
  } else{
    if(sample[i, "team1"] == sample[i, "winner"]){
      twmw <- twmw + 1
    } else{
      twml <- twml + 1
    }
  }
}

contingency <- data.frame('Toss Won' = c(twmw,twml, twmw + twml),
                          'Toss Lost' = c(tlmw, tlml, tlml + tlml),
                          'Sum' = c(twmw + tlmw, twml + tlml, 
                                    twmw+twml+tlmw+tlml),
                          row.names = c('Match Won', 'Match Lost', 'Sum'))
print(contingency)

#Visualization of cotingency table(bar graph)
ggplot(data = contingency, aes(fill = factor(Toss.Won), 
      y = Sum, x = row.names(contingency))) + 
  geom_bar(position = "stack", stat = "identity") +
  geom_col(color = 'red', fill = c("yellow",'green','purple'))+
  labs(title = "Toss Results vs Match Results", x = "", 
       y = "Number of Matches") +
  theme_minimal()

#Testing
print(chisq.test(contingency))