library(readr)
library(readxl)
library(dplyr)
library(lubridate)
library(chron)
library(wesanderson)
library(stargazer)
# Before loading data, make column headers in excel more R friendly (remove spaces, etc).  
# Load data
d <- read_excel("./data/AdultChinook_YBP.xlsx", sheet=2)
head(d)
str(d)
names(d)
d <- select(d, -c(Floy, genetics, Adipose))
d <- d[130:180,] # 2016 tagging data only
# Extract times only
d$TOC <- as.POSIXlt(d$TOC)
d$TOC <- strftime(d$TOC, format = "%H:%M:%S") 
d$TOC <- times(d$TOC) # This is what I was looking for

# Do the same for the other time columns:
d$SurgStart <- as.POSIXlt(d$SurgStart)
d$SurgStart <- strftime(d$SurgStart, format = "%H:%M:%S") 
d$SurgStart <- times(d$SurgStart) 
head(d$SurgStart)

d$SurgEnd <- as.POSIXlt(d$SurgEnd)
d$SurgEnd <- strftime(d$SurgEnd, format = "%H:%M:%S") 
d$SurgEnd <- times(d$SurgEnd) 

d$TOR <- as.POSIXlt(d$TOR)
d$TOR <- strftime(d$TOR, format = "%H:%M:%S") 
d$TOR <- times(d$TOR)

# Add handling time column
d$handlingtime <- times(d$TOR - d$TOC, out.format = "h:m")
head(d$handlingtime)
class(d$handlingtime)
d$handlingtime <- as.numeric(as.character(d$handlingtime))

# Add surgery time column
d$surgtime <- times(d$SurgEnd - d$SurgStart, out.format = "h:m")
d$surgtime <- as.numeric(as.character(d$surgtime))
head(d$surgtime)

detach("package:chron", unload = TRUE)

#One of the entries in the Sex column has an extra space, change value
which(d$Sex=="U")
d$Sex
d$Sex[25]="U"

# Begin Data Summaries

# How many males and females? 
t1 <- 
  d %>% count(Sex)
stargazer(d2, summary = F)

# How many surgeries did each person do?
t2 <- 
  d %>% count(Surgeon)

# What was the average surgery time, by surgeon?
t3 <- 
  d %>% 
  group_by(Surgeon) %>% 
  summarise(mean_surgtime = mean(surgtime))

# How was handling time across the total tagging range and by location?
library(ggplot2)
ggplot(d, aes(x = Date, y = handlingtime)) + geom_point(aes(color = TagLoc), position = "jitter", size = 3) +
  ggtitle("Handling Times Across Tagging Season") + ylab("Handling Time (mins)") + xlab("Date") + geom_smooth()

ggplot(d, aes(x = Date, y = handlingtime)) + geom_bar(aes(fill = TagLoc), position = "dodge", stat = "identity") +
  facet_wrap(~TagLoc) +
  ggtitle("Handling Times Across Tagging Season and Tagging Locations") + ylab("Handling Time (mins)") + xlab("Date")

# where did we catch fish over time?
d %>% 
  group_by(Date, TagLoc) %>% 
  summarise(count = n()) %>% 
ggplot(., aes(x = Date, y = count)) + geom_point(aes(color = TagLoc), size = 3) + facet_wrap(~TagLoc)

# what tide did we catch fish on?
d %>% 
  group_by(Tide) %>% 
  summarise(count = n()) %>% 
  ggplot(., aes(x = Tide, y = count)) + geom_bar(aes(fill = Tide, alpha = 0.8, width = 0.5), stat = "identity") + theme(legend.position = "none") + ggtitle("More Fish Caught During Flood Tide \n (reflects documented catches, not total effort)") +
  #  scale_fill_manual(values = dar)   <--- Mo's original code, changed it to below, verify that this is the same thing
  scale_fill_manual(values = wes_palette("Darjeeling"))

#########################Stopped here 12/22/2016 GS need to address sex determination before moving forward################

# mean difference in fork length/total length between males and females?
ggplot(d, aes(x = Sex, y = FLl)) + 
  geom_boxplot(aes(color = Sex, fill = Sex, alpha = 0.8)) + 
  ylab("Fork Length (mm)") + scale_x_discrete(labels = c("Female", "Male", "Unknown")) + 
  ggtitle("Fork Length By Sex") + 
  theme(legend.position = "none") + xlab(" ") + scale_color_manual(values = wes_palette("Darjeeling")) + scale_fill_manual(values = wes_palette("Darjeeling"))

ggplot(d, aes(x = sex, y = tl)) + 
  geom_boxplot(aes(color = sex, fill = sex, alpha = 0.8)) + 
  ylab("Total Length (mm)") + scale_x_discrete(labels = c("Female", "Male", "Unknown")) + 
  ggtitle("Total Length By Sex") + 
  theme(legend.position = "none") + xlab(" ") + scale_color_manual(values = wes_palette("Darjeeling")) + scale_fill_manual(values = wes_palette("Darjeeling"))

## Set palette from here on out to Darjeeling:
dar <- wes_palette("Darjeeling", type = "discrete")
darc <- wes_palette("Darjeeling", type = "continuous")


  