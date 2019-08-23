rm(list=ls()) # Clean up workplace
library(ggplot2);library(xlsx);library(reshape2);library(pastecs);library(ggpubr);library(dplyr);library(RColorBrewer)
library(tidyr);library(stringr);library(janitor);library(readxl);library(ggthemes);library(shiny);library(tools)
library(magrittr);library(glue);library(tools);library(svDialogs);library(lubridate)
setwd("E:/Google Drive/Coding/GitHub/expenses-webapp")

source("global.R")

#### Load Dataset ####
transactions <- load_transactions("0778067408_21082019_182827.csv")
cat.list <- readRDS("categories.RDS")

# cat.list <- remove_from_categories(cat.list, "max guitars co b.v.")
# write.csv(cat.list,"categories.csv", row.names = F)

transactions <- apply_categories(transactions,cat.list)
# cat.list <- append_category(179,"Leisure",cat.list = cat.list, trans.df = transactions)

question <- transactions[grep("\\?",transactions$Description),]

catnames <- names(cat.list)
selects <- catnames[c(2:11)]

#### Define Colour Palette ####
colScale <- categories_color_pal(cat.list)

#### Calculate Monthly Expenses
dv <- "Month"

if (dv == "Week"){
  vals <- transactions$Week %>% unique()
} else if (dv == "Month"){
  transactions$Month <- month(transactions$Date, label = T, abbr = F)
  vals <- transactions$Month %>% unique() %>% match(month.name)
}

slider1 <- c(min(vals),max(vals))


expenses <- transactions %>%
  filter(Category %in% selects) %>%
  group_by(DV = get(dv), Category = factor(Category, levels = catnames)) %>%
  summarise(value = sum(Amount)) 

if (class(expenses$DV) == "ordered" || class(expenses$DV) == "factor"){
  expenses %<>%
    filter(DV %in% month.name[seq(slider1[1],slider1[2],1)])
} else if (class(expenses$DV) == "numeric"){
  expenses %<>%
    filter(DV %in% seq(slider1[1],slider1[2],1))
}

totals <- expenses %>%
  group_by(DV) %>%
  summarize(total = sum(value))


ggplot(expenses, aes(DV, value, fill = Category)) + 
  theme_economist(base_family = "sans", base_size = 12) + colScale +
  geom_bar(stat = 'identity')+
  theme(legend.position = "right", legend.title = element_blank()) +
  labs(x="", y="Amount spent (\U20AC)") +
  geom_text(data = totals, inherit.aes = F, aes(x=DV, y=total, label=round(total)), vjust=-.7, size=5)



write.csv(cat.list,"categories.csv",row.names = F)
cat.list.csv <- read.csv("categories.csv",stringsAsFactors = F)