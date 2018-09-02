library(dplyr)
library(tidyr)
library(ggplot2)
library(rworldmap)
library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)
library(plotrix)
library(highcharter)


##########################################

odata <- read.csv("athlete_events.csv")
data1 <- read.csv("noc_regions.csv")
head(odata,4)
str(odata)

text <- readLines("name.txt", warn=FALSE)
docs <- Corpus(VectorSource(text))
inspect(docs)

toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")

# Convert the text to lower case
docs <- tm_map(docs, content_transformer(tolower))
# Remove numbers
docs <- tm_map(docs, removeNumbers)
# Remove english common stopwords
docs <- tm_map(docs, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docs <- tm_map(docs, removeWords, c("blabla1", "blabla2")) 
# Remove punctuations
docs <- tm_map(docs, removePunctuation)
# Eliminate extra white spaces
docs <- tm_map(docs, stripWhitespace)


dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)

# Wordcloud of games
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=500, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(10, "Dark2"))

# Sex Ratio of Participants
sex <- c("F","M")
values <- c(74522,196594)
pct <- round(values/sum(values)*100)
label <- paste(sex, pct)
label <- paste(label,"%",sep="")
pie3D(values,labels=label,explode=0.1,
      main="Sex Ratio of Participants ")


# Age of Medal winners
fig1 <- ggplot(odata, aes(x=Age))
fig1 <- fig1 + geom_histogram(stat="bin", binwidth=1, color='blue') + 
  labs(x="Age", y="Count", title="Age of Olympic medal winners, 1896-2016\n")
plot(fig1)
##ggsave(plot=fig1, filename="figure_1.pdf", width=7, height=5, units="in")



# TOp 10 Team, Sport and Athletes
Team <-  odata%>%filter(!is.na(Medal),Season=='Summer',Year==2016)%>%group_by(Team)%>%summarize(medal=n())%>%arrange(desc(medal))%>%head(n=10)
Sport <- odata%>%filter(!is.na(Medal),Season=='Summer',Year==2016)%>%group_by(Sport)%>%summarize(medal=n())%>%arrange(desc(medal))%>%head(n=10)
athlete <- odata%>%filter(!is.na(Medal),Season=='Summer',Year==2016)%>%group_by(Name)%>%summarize(medal=n())%>%arrange(desc(medal))%>%head(n=10)

# Team <-  odata%>%filter(!is.na(Medal),Season=='Winter',Year==2016)%>%group_by(Team)%>%summarize(medal=n())%>%arrange(desc(medal))%>%head(n=10)
# 
# Sport <- odata%>%filter(!is.na(Medal),Season=='Winter',Year==2016)%>%group_by(Sport)%>%summarize(medal=n())%>%arrange(desc(medal))%>%head(n=10)
# 
# athlete <- odata%>%filter(!is.na(Medal),Season=='Winter',Year==2016)%>%group_by(Name)%>%summarize(medal=n())%>%arrange(desc(medal))%>%head(n=10)
athlete$sn <- gsub("([A-Za-z]+).*", "\\1", athlete$Name)

highchart(height = "700px") %>% 
  hc_title(text = "Top 10 Teams, Sports and Athletes ") %>%
  hc_subtitle(text = "Summer Olympics 2016") %>%
  hc_credits(enabled = TRUE, text = "120 years of Olympic history: athletes and results", 
             style = list(fontSize = "10px")) %>%
  hc_add_theme(hc_theme_economist()) %>%
  hc_add_series_labels_values(Team$Team, Team$medal, name = "Show/ hide bar chart", 
                              dataLabels = list(align = "center", enabled = TRUE),
                              colors = substr(topo.colors(10), 0 , 7),
                              colorByPoint = TRUE, type = "column") %>% 
  hc_add_series_labels_values(Sport$Sport, Sport$medal, name = "Pie chart- Sports", 
                              colors = substr(topo.colors(10), 0 , 7),
                              type = "pie", innerSize= '40%', size= "30%", showInLegend=F,
                              colorByPoint = TRUE, center = c('33%', '30%'),
                              size = 100, dataLabels = list(align = "center", enabled = TRUE)) %>% 
  hc_add_series_labels_values(athlete$sn, athlete$medal, name = "Pie chart- Athletes", 
                              colors = substr(topo.colors(10), 0 , 7),
                              type = "pie", innerSize= '40%', size= "30%", showInLegend=F,
                              colorByPoint = TRUE, center = c('76%', '30%'),
                              size = 100, dataLabels = list(align = "center", enabled = TRUE)) %>% 
  hc_yAxis(title = list(text = "Total medals won"),
           labels = list(format = "{value}"), max = 415) %>% 
  hc_xAxis(categories = Team$Team, title = list(text = "Team name")) %>% 
  hc_legend(enabled = T, align= "left", verticalAlign = "bottom") %>% 
  hc_tooltip(pointFormat = "{point.y}")




#  group by data
by_year <- odata %>% group_by(Year) %>% summarise(Total = n())
by_country <- odata %>% group_by(Team) %>% summarise(Total = n())
by_sport <- odata %>% group_by(Sport) %>% summarise(Total = n())
by_medal <- odata %>% group_by(Medal) %>% summarise(Total = n())

# Diffreent medals won in Olympics
hchart(by_year, "column", hcaes(Year, Total, color = Year)) %>% 
  hc_add_series_labels_values(by_medal$Medal, by_medal$Total,
                              type = "pie",
                              center = c('60%', '25%'),
                              size = 150, dataLabels = list(align = "center",enabled = TRUE)) %>%
  hc_add_theme(hc_theme_google()) %>%
  hc_title(text = "Medals Won in Olympics (1896 - 2016)")  %>%
  hc_subtitle(text = "Different Medals Won")


