library(glue);library(magrittr)
#### Append Category to Cat.list Function ####

# line <- c(110)
# category <- "Car"
# cat.list <- read.csv("categories.csv",stringsAsFactors = F)
# trans.df <- load_transactions("ASNExpenses.csv")

append_category <- function(line,category,cat.list = catlist, trans.df = transactions_categorised){
  category %<>% toTitleCase()
  line %<>% as.integer()
  key <- trans.df$Description[line]
  for (entry in key) {
    pattern <- gsub(pattern = "([[:punct:]])",replacement="\\\\\\1",entry)
    found <- grep(pattern,cat.list) # Current category to be removed from
    for (i in found) {
      cat.list[[i]] <- cat.list[[i]][!grepl(pattern, cat.list[[i]])]
    }
    if (!grepl(pattern,cat.list[category])){# If entry is NOT already present in category
      cat.list[[category]] %<>% c(., entry)
    } 
  }
  cat.list
}

# cat.list <- append_category(line,category,cat.list,trans.df)
# file <-  "0778067408_21082019_182827.csv"

load_transactions <- function(file){
  #### Load Dataset ####
  transactions <- data.frame(read.csv(file, na = ""), stringsAsFactors = F)[,c(1,4,11,15,18)]
  colnames(transactions) <- c("Date", "Reference", "Amount","Type", "Description")
  transactions %<>% filter(Amount < 0) %>% mutate(Date = as.POSIXct(Date, format = "%d-%m-%Y") %>% as.character(),
                                                  Week = as.numeric(strftime(Date,format="%W")),
                                                  Reference = as.character(Reference),
                                                  Amount = abs(Amount),
                                                  Description = Description %>% as.character() %>% gsub("\'","",.) %>% tolower(),
                                                  Category = as.character("")
  )
  
  # Cleanup Description column in transactions dataframe
  for (i in 1:nrow(transactions)) {
    if (transactions$Type[i] == "BEA" || transactions$Type[i] == "GEA"){
      first <- gsub(">.*","",transactions$Description[i]) %>% sub("ccv\\*","",.) %>% trimws("right")
      second <- gsub(".*>","",transactions$Description[i]) %>% substr(1,9) %>% trimws("right")
      transactions$Description[i] <- glue("{first} ({second})") %>%
        gsub("[?]","&",.)
    } else if (transactions$Type[i] == "IDB"){
      transactions$Description[i] %<>% 
        sub(".*? (.+)", "\\1",.) %>% 
        sub(".*? (.+)", "\\1",.) %>% 
        sub(" referentie.*", "",.) %>% 
        trimws("right")
    } else if (transactions$Type[i] == "INC"){
      transactions$Description[i] %<>% 
        gsub("europese incasso door:","",.) %>%
        substr(1, 20) %>% 
        trimws("right")
    }
    if (transactions$Type[i] != "BEA"){
      transactions$Description[i] <- glue('{transactions$Reference[i]} : {transactions$Description[i]}')
    }
  }
  transactions %>% select(Week,Date,Description,Category,Amount)
}

# trans <- load_transactions("ASNExpenses.csv")

#### Apply categories to selected dataframe ####

# trans.df <- transactions
# cat.list <- categories

# Match categories to recognized keywords in Descriptions
apply_categories <- function(trans.df, cat.list){
  for (cat in 1:length(cat.list)) {
    stringmatch <- gsub(pattern = "([[:punct:]])",replacement="\\\\\\1",cat.list[[cat]])
    pattern <- paste(stringmatch, collapse = "|", sep="")
    trans.df$Category[grep(pattern, trans.df$Description)] <- glue('{names(cat.list[cat])}')
  }
  trans.df
}


# # write.csv(cat.list[[category]],glue('cat_{category}.csv'),row.names = F)
# 
# for (category in colnames(cat.list)){
#   temp <- read.csv(glue('cat_{category}.csv'), sep = "|", stringsAsFactors = F, header=F)
#   assign(category, as.character(temp))
# }
# 
# categories <- list(Huur=Huur,Subscriptions=Subscriptions,Car=Car,Gas=Gas,Drinks=Drinks,Leisure=Leisure,Lunch=Lunch,Transport=Transport,Household=Household,Dinner=Dinner,Groceries=Groceries)
# 
# # Test
# datamatch <- c("total (ingeldorf)","kaas-bv (woerden)","ch?teau")
# stringmatch <- c("total (ingeldorf)","kaas-bv (woerden)","ch?teau")
# stringmatch <- gsub(pattern = "([[:punct:]])",replacement="\\\\\\1",stringmatch)
# 
# pattern <- paste(stringmatch, collapse = "|", sep="")
# 
# grepl(pattern,datamatch)
# 
# # Test
# 
# categories <- readRDS("categories.RDS")
# 
# pattern <- paste("[",categories$Groceries,"]", collapse = "|", sep="")
# 
# pattern <- paste(categories$Groceries, collapse = "|", sep="")
# 
# grep(pattern, transactions$Description,value=T)
# 
# entry <- "les amis  ch&teau vian (vianden)"
# 
# # Add new entries
# categories$Leisure %<>% c(., entry)
# 
# # Remove entries
# 
# entry <- gsub(pattern = "([[:punct:]])",replacement="\\\\\\1",entry) # escape all punctuation
# category <- grep(entry,categories)
# categories[[category]] <- categories[[category]][!grepl(entry, categories[[category]])]
# saveRDS(categories,"categories.RDS")

# Remove from Categories
remove_from_categories <- function(cat.list, description){
  description %<>% as.character()
  for (entry in description) {
    pattern <- gsub(pattern = "([[:punct:]])",replacement="\\\\\\1",entry)
    found <- grep(pattern,cat.list) # Current category to be removed from
    for (i in found) {
      cat.list[[i]] <- cat.list[[i]][!grepl(pattern, cat.list[[i]])]
    }
  }
  cat.list
}

# Create Color palette length of categories
categories_color_pal <- function(cat.list){
  myColors <- brewer.pal(length(cat.list),"Paired")
  names(myColors) <- names(cat.list)
  scale_fill_manual(name = "grp",values = myColors)
}
