library(ggplot2);library(reshape2);library(dplyr);library(RColorBrewer)
library(readxl);library(ggthemes);library(shiny);library(tools);library(plotly)
catlist <- c("Car","Gifts","Subscriptions","Cash","Household","Gas","Transport","Leisure","Drinks","Lunch","Dinner","Groceries")
preselected <- c("Gifts","Subscriptions","Household","Gas","Transport","Leisure","Drinks","Lunch","Dinner","Groceries")
currentweek <- as.numeric(strftime(Sys.Date(),format="%W"))
weeks <- c(3:currentweek)
#### Load Dataset ####
{
  transactions <- as.data.frame(read_xlsx("Expenses.xlsx", sheet = 2, range = cell_cols(c(1,5)), trim_ws = TRUE , na = ""))
  colnames(transactions) <- c("Week", "Date", "Description", "Category", "Amount")
  transactions$Week <- as.numeric(strftime(transactions$Date,format="%W"))
  transactions$Category <- as.factor(transactions$Category)
  levels(transactions$Category)<-toTitleCase(levels(transactions$Category))
  
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
}
#### Define Colour Palette ####
myColors <- brewer.pal(12,"Paired")
names(myColors) <- colnames(dataset[,1:12])
colScale <- scale_fill_manual(name = "grp",values = myColors)

# Define UI ---- ########################################################################################################################
ui <- fluidPage(
        sidebarLayout(
          sidebarPanel(
            tags$head(
              tags$style(type='text/css', ".well { padding: 20px; margin-bottom: 5px; max-width: 500px; }")
            ),
            checkboxGroupInput("selects", label = "Choose categories to show", 
                               choices = catlist,selected = preselected),
            checkboxInput('bar', 'Check all', value = T),
            sliderInput("slider1", label = "Choose data range to show", min = 3, 
                        max = currentweek, value = c(3,currentweek)),
            checkboxInput("smooth", label = "Project a smoothed line?",value = F)
                        
          ),
          mainPanel(h2("Weekly Expenses", align = "center"),
                    plotlyOutput("Plot",height = "500px")
          )
        )
)

# Define server logic ---- #############################################################################################################
server <- function(input, output, session) {
  observe({
    updateCheckboxGroupInput(
      session, 'selects', choices = catlist,
      selected = if (input$bar) catlist
    )
  })
  
weeks <- reactive({seq(input$slider1[1],input$slider1[2],1)})
selects <- reactive({input$selects})

exp <- reactive({
  expenses1 <- dataset[, selects(), drop = FALSE]
  expenses1$Week <- c(3:currentweek)
  expenses1 <- subset(expenses1, Week %in% weeks())
  melt(expenses1, id.vars = "Week")
  })

totals <- reactive({
  exp() %>% group_by(Week) %>% summarize(total = sum(value))
  })

lim <- reactive({max(totals())*1.2})

myPlot <- reactive({
  p <- paste(
    "ggplot(exp(), aes(Week, value, group = variable, fill = variable))+ theme_economist() + colScale+
    geom_bar(stat = 'identity')+
    theme(legend.position = 'right', legend.title = element_blank())+
    theme(text=element_text(size = 10, family='Helvetica'))+
    scale_x_continuous(breaks=weeks())+ 
    scale_y_continuous(breaks = seq(0,lim(),50),expand = c(0,10))+
    labs(x='Week', y='Amount spent ($)')+  coord_cartesian(ylim=c(0,lim()))+
    geom_text(data = totals(),inherit.aes = F, aes(x=Week, y=total, label=round(total)), vjust=-.5, size=4)",
    if (input$smooth)
      paste("+ geom_smooth(data = totals(),inherit.aes = F, aes(Week, total), method = 'loess', span = 0.5, se= F, size = 2)"),
    sep=""
    )
  })

output$Plot <- renderPlotly({
  plot1 <- eval(parse(text = myPlot()))
  plot1 #ggplotly(plot1)
})
}
# Run the app ----
shinyApp(ui = ui, server = server)