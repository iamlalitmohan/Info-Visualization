#----To clear the console-----
dev.off() # Clear the graph window
cat('\014')  # Clear the console
rm(list=ls()) # Clear all user objects from the environment!!!

######-------- Part 1---------------------------
#Import data

fname <- file.choose()
fname
df <- read.csv(fname, header=T, stringsAsFactors = F)  ##reading the csv file


library(tidyverse)
all_complaints <- df %>% select(2, 6, 25) #Select "Complaint Type" and "Borough" columns
all_complaints$Created.Date <- as.POSIXct(strptime(all_complaints$Created.Date, "%m/%d/%Y %I:%M:%S %p"))
all_complaints$Year <- format(all_complaints$Created.Date, "%Y")
all_complaints$Complaint.Type[grepl("^Noise.*", all_complaints$Complaint.Type)] <- "Noise" 

all_complaints <- all_complaints %>%
  group_by(Borough,Complaint.Type) %>% summarise(Count = n())  ##complains by borough,complain type

all_complaints_NY <- all_complaints %>%
  group_by(Complaint.Type) %>%
  summarise(Count = sum(Count)) %>%
  arrange(desc(Count))

top10_complaints_NY <- top_n(all_complaints_NY, 10, Count)  #top 10 complains in NY


# function below to fix all capitalized words
capwords <- function(s, strict = FALSE) {
  cap <- function(s) paste(toupper(substring(s, 1, 1)),
                           {s <- substring(s, 2); if(strict) tolower(s) else s},
                           sep = "", collapse = " " )
  sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))
}


# function below to make subplots
figure1 <- function(df){
  df <- df[order(df$Count),]
  order <- df$Complaint.Type
  order <- sapply(order, capwords, T)
  df$Complaint.Type <- order
  df$Complaint.Type <- factor(df$Complaint.Type, levels = order)
  return(ggplot(df) + geom_bar(aes(x=Complaint.Type , y = Count
  ), fill = "Orange",
  stat = "identity") + theme_minimal() +
    xlab("") + ylab("Number of Complaints") + coord_flip() )
}

# subplot 1
p_top10_complaints_NY  <- figure1(top10_complaints_NY) + ggtitle("New York City \nTop 10 Complaints")
top10_complaints_borough <- top_n(group_by(all_complaints, Borough), 10, Count)
top10_complaints_Man <- filter(top10_complaints_borough, Borough == "MANHATTAN")

# subplot 2
p_top10_complaints_Man  <- figure1(top10_complaints_Man) + ggtitle("Manhattan \nTop 10 Complaints") 
top10_complaints_Qns <- filter(top10_complaints_borough, Borough == "QUEENS")
# subplot 3
p_top10_complaints_Qns  <- figure1(top10_complaints_Qns) + ggtitle("Queens \nTop 10 Complaints")
top10_complaints_Bn <- filter(top10_complaints_borough, Borough == "BROOKLYN")
# subplot 4
p_top10_complaints_Bn  <- figure1(top10_complaints_Bn) + ggtitle("Brooklyn \nTop 10 Complaints")
top10_complaints_Brx <- filter(top10_complaints_borough, Borough == "BRONX")
# subplot 5
p_top10_complaints_Brx  <- figure1(top10_complaints_Brx) + ggtitle("Bronx \nTop 10 Complaints")
top10_complaints_SI <- filter(top10_complaints_borough, Borough == "STATEN ISLAND")
top10_complaints_SI$Complaint.Type[4] <- "Missed Collection"

# subplot 6
p_top10_complaints_SI  <- figure1(top10_complaints_SI) + ggtitle("Staten Island \nTop 10 Complaints")

#To plot multiple plot on single page

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    
     layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}


multiplot(p_top10_complaints_NY, p_top10_complaints_Man, p_top10_complaints_Bn,
          p_top10_complaints_Qns, p_top10_complaints_Brx, p_top10_complaints_SI,
          cols=2)

####--------------------------------------------------------------------------

library(wordcloud)
a <- as.matrix(all_complaints$Complaint.Type)
a <- as.matrix(df$Complaint.Type)
wordcloud(a,max.words=30, min.freq=10, colors=brewer.pal(8, 'Dark2'))



####----------------------------------Part 2---------------

library(zipcode)
library(tidyverse)
library(maps)
install.packages("ggplot2")
library(ggplot2)      #library

fname1 <- file.choose()
fname1
df1 <- read.csv(fname1, header=T, stringsAsFactors = F)  ### reading file
resptime <- df1 %>% select(2,3,4,9,17)   # taking the required columns
df <- df1[c(17,4)]
df<-df[!(df$City==""),]

 
length(unique(df$City))
df
df$City <-tolower(df$City)
x <- aggregate(x = df$ResponseTime,                                 
          by = list(df$City),                                 
          FUN = mean)                  #mean

names(x)[1] <- "City"
names(x)[2]  <- "AvgResTime"
x
top <- x[order(-x$AvgResTime),]
top
top <- top_n(top,5)
top

fastest <-x[order(x$AvgResTime),]
fastest <- top_n(fastest,5)
fastest
barplot(height=top$AvgResTime, main="City vs Response time", xlab="City", ylab="Response Time",
       names.arg=top$City,
       border="blue", col = "skyblue")               #bar plot

barplot(height=fastest$AvgResTime, main="City vs Response time", xlab="City", ylab="Response Time",
        names.arg=fastest$City,
        border="green",col = "skyblue")    


ggplot(top, aes(x=City, y=AvgResTime)) + geom_point()+  ylim(300,425)
ggplot(top, aes(x=City, y=AvgResTime)) + geom_bar(stat = "identity", color="blue", fill="red")

ggplot(fastest, aes(x=City, y=AvgResTime)) + geom_point()+  ylim(300,425)
ggplot(fastest, aes(x=City, y=AvgResTime)) + geom_bar(stat = "identity", color="blue", fill="red")

###--------------------------------------------------------------------------------------------------------------------------------












