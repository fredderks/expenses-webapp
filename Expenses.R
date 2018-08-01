#####======================== Copy to new Script ==============================#####
rm(list=ls()) # Clean up workplace
library(ggplot2);library(xlsx);library(reshape2);library(pastecs);library(ggpubr);library(dplyr);library(RColorBrewer)
library(tidyr);library(stringr);library(janitor);library(readxl);library(ggthemes);library(shiny);library(tools)
#####======================== Copy to new Script ==============================#####
setwd("E:/Google Drive/Personal")
#### Load Dataset ####
{
transactions <- as.data.frame(read_xlsx("Expenses.xlsx", sheet = 2, range = cell_cols(c(1,5)), trim_ws = TRUE , na = ""))
colnames(transactions) <- c("Week", "Date", "Description", "Category", "Amount")
transactions$Week <- as.numeric(strftime(transactions$Date,format="%W"))
transactions$Category <- as.factor(transactions$Category)
levels(transactions$Category)<-toTitleCase(levels(transactions$Category))
str(transactions)}

#### Calculate Weekly Expenses ####
currentweek <- as.numeric(strftime(Sys.Date(),format="%W"))
weeks <- c(3:currentweek)
catlist <- c("Car","Gifts","Subscriptions","Cash","Household","Gas","Transport","Leisure","Drinks","Lunch","Dinner","Groceries")

for (week in weeks){
      sub <- as.vector(subset(transactions, Week == week, select = Category))
      sub <- as.vector(sub$Category)
      
      for (cat in catlist){
            if (cat %in% sub == F){
              sum <- as.data.frame(0)
              colnames(sum) <- paste(get("cat"))
              #assign(paste(get("cat"),sep=""),as.data.frame(sum))
            } else {
              sum <- as.data.frame(sum(subset(transactions, Week == week & Category == cat, select=Amount)))
              colnames(sum) <- paste(get("cat"))
              #assign(paste(get("cat"),sep=""),as.data.frame(sum))
            }
            if (exists("dfweek")){
              dfweek<-cbind(dfweek, sum)
            }
            # if the merged dataset doesn't exist, create it
            if (!exists("dfweek")){
              dfweek <- sum
            } 
        rm(sum,cat)
      }
      
      #dfweek <- as.data.frame(cbind(noquote(catlist)))
      #rm(noquote(catlist))
      # if the merged dataset doesn't exist, create it
      if (exists("dataset")){
        temp_dataset <- dfweek
        dataset<-rbind(dataset, temp_dataset)
        rm(temp_dataset)
      }
      # if the merged dataset doesn't exist, create it
      if (!exists("dataset")){
        dataset <- dfweek
      } 
      rm(dfweek,sub,week)
}
dataset$Week <- weeks

#### Define Colour Palette ####
myColors <- brewer.pal(12,"Paired")
names(myColors) <- colnames(dataset[,1:12])
colScale <- scale_fill_manual(name = "grp",values = myColors)
#### Creating the stacked bar chart ####
selects <- catlist[2:12]
exp <- ({
        expenses1 <- dataset[, selects, drop = FALSE]
        expenses1$Week <- c(3:27)
        expenses1 <- subset(expenses1, Week %in% weeks)
        melt(expenses1, id.vars = "Week")
        })
totals <- 

totals <- exp %>%
  group_by(Week) %>%
  summarize(total = sum(value))

p1 <- ggplot(exp, aes(Week, value, group = variable, fill = variable))+ 
      theme_economist(base_family = "sans") +  colScale+
      geom_bar(stat = 'identity')+
      theme(legend.position = "right", legend.title = element_blank())+
      theme(text=element_text(family = "Verdana", size = 14))+
      scale_x_continuous(breaks=seq(3,27,1))+
      scale_y_continuous(breaks = seq(0,700,50),expand = c(0,10))+
      labs(x="Week", y="Amount spent ($)")+coord_cartesian(ylim=c(0,700))+ 
      geom_text(data = totals, inherit.aes = F, aes(x=Week, y=total, label=round(total)), vjust=-.7, size=5)+ 
      geom_smooth(data = totals,inherit.aes = F, aes(Week, total), method = 'loess', span = 0.5, se= F, size = 3) 

p1

lo <- loess(totals$total~totals$Week)

p2 <- ggplot(totals, aes(Week, total))+ theme_economist(base_family = "sans")+
      theme(text=element_text(family = "Verdana", size = 14))+
      scale_x_continuous(breaks=seq(3,27,1))+
      scale_y_continuous(breaks = seq(0,700,50),expand = c(0,10))+
      labs(x="Week", y="Amount spent ($)")
p3 <- p2+ geom_smooth(data = totals, method = "loess", span = 0.5) 
p4 <- p2+ geom_smooth(data = totals, method = "gam", formula = y ~ poly(x, 4))
p2
   