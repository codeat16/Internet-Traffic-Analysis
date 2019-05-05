## ----echo=FALSE, message=FALSE, warning=FALSE, packages------------------
library(reshape2)
library(ggplot2)
library(GGally)
library(dplyr)
library(tidyr)


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------
# Load the Data
data <- read.csv("equinix-chicago.dirA.20160406-130000.UTC.df.txt", sep="",
                 comment.char = ';')


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------
names(data)



## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------
sample_n(data, 5)


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------
ggplot(data=data)  +
  geom_histogram( aes(SIZE), binwidth=1)


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------
head(data$SIZE, 10)
tail(data$SIZE, 10)


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------
ggplot( gather( subset(data, select=-c(SIZE))), aes(value)) +
  geom_histogram() +
  facet_wrap(~key)


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------
ggplot( gather( subset(data, select=-c(SIZE))), aes(value)) +
  geom_histogram() +
  scale_x_log10() +
  facet_wrap(~key)


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------
ggplot(data=data)  +
  geom_point( aes(x=SIZE, y=X.IPv4.) )


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------
ggplot(data=data)  +
  geom_point( aes(x=SIZE, y=X.IPv4.), alpha=1/5 ) +
  scale_y_log10()


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------
ggplot(data=subset(data, SIZE<2000))  +
  geom_point( aes(x=SIZE, y=X.IPv4.), alpha=1/5 ) +
  scale_y_log10()


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------
ggplot(data=subset(data, SIZE<2000))  +
  geom_point( aes(x=SIZE, y=X.IPv4.), alpha=1/5 ) +
  scale_y_log10() +
  geom_smooth( aes(x=SIZE, y=X.IPv4.), method='lm', color='red')


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------
ggplot(data=subset(data, SIZE<2000))  +
  geom_point( aes(x=SIZE, y=X.IPv6.), alpha=1/5 ) +
  scale_y_log10() +
  geom_smooth( aes(x=SIZE, y=X.IPv6.), method='lm', color='red')


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------
ggplot(data=subset(data, SIZE<2000))  +
  geom_point( aes(x=SIZE, y=X.IPv6t.), alpha=1/5 ) +
  scale_y_log10() +
  geom_smooth( aes(x=SIZE, y=X.IPv6t.), method='lm', color='red')


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------
ggplot(data=data)  +
  geom_point( aes(x=SIZE, y=X.IPv4.), alpha=1/5 ) +
  scale_y_log10() +
  scale_x_log10() +
  geom_smooth( aes(x=SIZE, y=X.IPv4.), method='lm', color='red')


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------
ggplot(data=data)  +
  geom_point( aes(x=SIZE, y=X.IPv6.), alpha=1/5 ) +
  scale_y_log10() +
  scale_x_log10() +
  geom_smooth( aes(x=SIZE, y=X.IPv6.), method='lm', color='red')


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------
ggplot(data=data)  +
  geom_point( aes(x=SIZE, y=X.IPv6t.), alpha=1/5 ) +
  scale_y_log10() +
  scale_x_log10() +
  geom_smooth( aes(x=SIZE, y=X.IPv6t.), method='lm', color='red')


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------
summary(data)


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------
colSums(data)


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------
# To do the hierarchical categories, we first separate data into subets,
# perform the melt. Then finally combine them.

# Select the subset to perform melt on.
# Other uninteresting types are ignored intentionally.
data4 <- subset(data, select=c("SIZE", "X.IPv4.", "TCP", "UDP", "ICMP"))

# Rename the columns to be more readable
names(data4) <- c("SIZE", "TOTAL", "TCP", "UDP", "ICMP")

# Do the melt
data4m <- melt(data4, id=c("SIZE"),
               variable.name = 'TYPE', value.name='PACKET_COUNTS')

# Create the new group variable
data4m$GROUP = "IPv4"


# The same step for the 2nd group
data6 <- subset(data,
                select=c("SIZE", "X.IPv6.", "TCP.1", "UDP.1", "ICMP6"))
names(data6) <- c("SIZE", "TOTAL", "TCP", "UDP", "ICMP")
data6m <- melt(data6, id=c("SIZE"),
               variable.name = 'TYPE', value.name='PACKET_COUNTS')
data6m$GROUP = "IPv6"

# The same step for the 3rd group
data6t <- subset(data,
                 select=c("SIZE", "X.IPv6t.", "TCP.2", "UDP.2", "ICMP6.1"))
names(data6t) <- c("SIZE", "TOTAL", "TCP", "UDP", "ICMP")
data6tm <- melt(data6t, id=c("SIZE"),
                variable.name = 'TYPE', value.name='PACKET_COUNTS')
data6tm$GROUP = "IPv6t"


# Finally, concatenate all three
data_final <- rbind(data4m, data6m, data6tm)

# convert from char to factor
data_final$GROUP = factor(data_final$GROUP)

names(data_final)


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------
sample_n(data_final, 5)


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------
ggplot(data=subset(data_final, GROUP=="IPv4"))  +
  ggtitle("IPv4") +
  geom_point( aes(x=SIZE, y=PACKET_COUNTS, color=TYPE), alpha=1/10 ) +
  scale_y_log10() +
  scale_x_log10() +
  geom_smooth( aes(x=SIZE, y=PACKET_COUNTS, group=TYPE, color=TYPE),
               method='lm')


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------
summarize( 
  group_by(subset(data_final, GROUP=="IPv4"), TYPE),
  Mean=mean(PACKET_COUNTS),
  Median=median(PACKET_COUNTS),
  Sum=sum(PACKET_COUNTS))


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------
ggplot(data=subset(data_final, GROUP=="IPv6"))  +
  ggtitle("IPv6") +
  geom_point( aes(x=SIZE, y=PACKET_COUNTS, color=TYPE), alpha=1/10 ) +
  scale_y_log10() +
  scale_x_log10() +
  geom_smooth( aes(x=SIZE, y=PACKET_COUNTS, group=TYPE, color=TYPE),
               method='lm')


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------
summarize( 
  group_by(subset(data_final, GROUP=="IPv6"), TYPE),
  Mean=mean(PACKET_COUNTS),
  Median=median(PACKET_COUNTS),
  Sum=sum(PACKET_COUNTS))


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------
ggplot(data=subset(data_final, GROUP=="IPv6t"))  +
  ggtitle("IPv6t") +
  geom_point( aes(x=SIZE, y=PACKET_COUNTS, color=TYPE), alpha=1/10 ) +
  scale_y_log10() +
  scale_x_log10() +
  geom_smooth( aes(x=SIZE, y=PACKET_COUNTS, group=TYPE, color=TYPE),
               method='lm')


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------
summarize( 
  group_by(subset(data_final, GROUP=="IPv6t"), TYPE),
  Mean=mean(PACKET_COUNTS),
  Median=median(PACKET_COUNTS),
  Sum=sum(PACKET_COUNTS))


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------
ggplot(data=subset(data_final, TYPE=="TOTAL"))  +
  ggtitle("TOTAL") +
  geom_point( aes(x=SIZE, y=PACKET_COUNTS, color=GROUP), alpha=1/10 ) +
  scale_y_log10() +
  scale_x_log10() +
  geom_smooth( aes(x=SIZE, y=PACKET_COUNTS, group=GROUP, color=GROUP),
               method='lm')


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------
summarize( 
  group_by(subset(data_final, TYPE=="TOTAL"), GROUP),
  Mean=mean(PACKET_COUNTS),
  Median=median(PACKET_COUNTS),
  Sum=sum(PACKET_COUNTS))


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------
ggplot(data=subset(data_final, TYPE=="TCP"))  +
  ggtitle("TCP") +
  geom_point( aes(x=SIZE, y=PACKET_COUNTS, color=GROUP), alpha=1/10 ) +
  scale_y_log10() +
  scale_x_log10() +
  geom_smooth( aes(x=SIZE, y=PACKET_COUNTS, group=GROUP, color=GROUP),
               method='lm')


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------
summarize( 
  group_by(subset(data_final, TYPE=="TCP"), GROUP),
  Mean=mean(PACKET_COUNTS),
  Median=median(PACKET_COUNTS),
  Sum=sum(PACKET_COUNTS))


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------
ggplot(data=subset(data_final, TYPE=="UDP"))  +
  ggtitle("UDP") +
  geom_point( aes(x=SIZE, y=PACKET_COUNTS, color=GROUP), alpha=1/10 ) +
  scale_y_log10() +
  scale_x_log10() +
  geom_smooth( aes(x=SIZE, y=PACKET_COUNTS, group=GROUP, color=GROUP),
               method='lm')


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------
summarize( 
  group_by(subset(data_final, TYPE=="UDP"), GROUP),
  Mean=mean(PACKET_COUNTS),
  Median=median(PACKET_COUNTS),
  Sum=sum(PACKET_COUNTS))


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------
ggplot(data=subset(data_final, TYPE=="ICMP"))  +
  ggtitle("ICMP") +
  geom_point( aes(x=SIZE, y=PACKET_COUNTS, color=GROUP), alpha=1/10 ) +
  scale_y_log10() +
  scale_x_log10() +
  geom_smooth( aes(x=SIZE, y=PACKET_COUNTS, group=GROUP, color=GROUP),
               method='lm')


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------
summarize( 
  group_by(subset(data_final, TYPE=="ICMP"), GROUP),
  Mean=mean(PACKET_COUNTS),
  Median=median(PACKET_COUNTS),
  Sum=sum(PACKET_COUNTS))


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------
ggplot()  +
  geom_point( data=subset(data_final, GROUP=="IPv4"),
              aes(x=SIZE, y=PACKET_COUNTS, color=TYPE),
              alpha=1/10, palette = "Greens" ) +
  #scale_colour_brewer(palette = "Greens") +
  geom_line( data=subset(data_final, GROUP=="IPv6"),
             aes(x=SIZE, y=PACKET_COUNTS, color=TYPE),
             alpha=1/10, palette = "Purples" ) +  
  #scale_colour_brewer(palette = "Purples") +
  scale_y_log10() +
  scale_x_log10()


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------
ggplot()  +
  geom_point( data=data_final,
              aes(x=SIZE, y=PACKET_COUNTS, color=TYPE, shape=GROUP),
              alpha=1/3 ) +
  scale_y_log10() +
  scale_x_log10()


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------
ggplot()  +
  geom_point( data=data_final,
              aes(x=SIZE, y=PACKET_COUNTS, color=GROUP, shape=TYPE),
              alpha=1/3 ) +
  scale_y_log10() +
  scale_x_log10()


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------
ggplot()  +
  geom_point( data=subset(data_final, TYPE!="TOTAL"),
              aes(x=SIZE, y=PACKET_COUNTS, color=TYPE, shape=GROUP),
              alpha=1/3 ) +
  scale_y_log10() +
  scale_x_log10()


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------
ggplot()  +
  geom_point( data=subset(data_final, TYPE!="TOTAL"),
              aes(x=SIZE, y=PACKET_COUNTS, color=GROUP, shape=TYPE),
              alpha=1/3 ) +
  scale_y_log10() +
  scale_x_log10()


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------
data_final$GROUP_TYPE <- paste(data_final$GROUP, data_final$TYPE)
data_final$GROUP_TYPE <- factor(data_final$GROUP_TYPE)
sample_n(data_final,5)


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------
ggplot()  +
  geom_point( data=subset(data_final, TYPE!="TOTAL"),
              aes(x=SIZE, y=PACKET_COUNTS, color=GROUP_TYPE),
              alpha=1/5 ) +
  scale_y_log10() +
  scale_x_log10() +
  scale_colour_brewer(palette = "Set1")


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------
ggplot()  +
  ggtitle("Packet counts by size for all groups/types of packets") +
  geom_point( data=subset(data_final, TYPE!="TOTAL"),
              aes(x=SIZE, y=PACKET_COUNTS, color=GROUP_TYPE),
              alpha=1/3 ) +
  scale_y_log10() +
  scale_x_log10() +
  scale_color_manual(values=c("#ea6c1f", "#fd9e0c", "#edc374", # orange
                              "#008000", "#a1cc3a", "#d3ffce", # green
                              "#0a4f75", "#91bbe4", "#d7e4f5")) # blue  


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------
ggplot(data=subset(data_final, TYPE=="TOTAL"))  +
  ggtitle("Packet counts by size for IPv4, IPv6, and IPv6t group") +
  geom_point( aes(x=SIZE, y=PACKET_COUNTS, color=GROUP),
              alpha=1/10 ) +
  scale_y_log10() +
  scale_x_log10() +
  scale_color_manual(values=c("#fd9e0c", # orange
                              "#a1cc3a", # green
                              "#91bbe4")) +     # blue  
  geom_smooth( aes(x=SIZE, y=PACKET_COUNTS, group=GROUP, color=GROUP),
               method='lm')


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------
ggplot(data=subset(data_final, TYPE=="UDP"))  +
  ggtitle("UDP packet counts by size for IPv4, IPv6, and IPv6t") +
  geom_point( aes(x=SIZE, y=PACKET_COUNTS, color=GROUP),
              alpha=1/10 ) +
  scale_y_log10() +
  scale_x_log10() +
  scale_color_manual(values=c("#fd9e0c", # orange
                              "#a1cc3a", # green
                              "#91bbe4")) +     # blue   
  geom_smooth( aes(x=SIZE, y=PACKET_COUNTS, group=GROUP, color=GROUP),
               method='lm')

