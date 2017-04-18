#Library
if (!require("reshape2")) {install.packages("reshape2")}
if (!require("shiny")) {install.packages("shiny")}
if (!require("ggvis")) {install.packages("ggvis")}
if (!require("dplyr")) {install.packages("dplyr")}
library(shiny)
library(ggvis)
library(reshape2)
library(dplyr)

# load data
le <-read.csv(text=readLines('API_SP.DYN.LE00.IN_DS2_en_csv_v2.csv')[-(1:4)],check.names=FALSE, stringsAsFactors = FALSE)
tfrt <-read.csv(text=readLines('API_SP.DYN.TFRT.IN_DS2_en_csv_v2.csv')[-(1:4)],check.names=FALSE, stringsAsFactors = FALSE)
pop <- read.csv(text=readLines('API_SP.POP.1564.TO.ZS_DS2_en_csv_v2.csv')[-(1:4)],check.names=FALSE,stringsAsFactors = FALSE)
metadata <- read.csv('Metadata_Country_API_SP.DYN.LE00.IN_DS2_en_csv_v2.csv')

# drop useless columns
le <- le[,-(3:4),drop=FALSE]
tfrt <- tfrt[,-(3:4),drop=FALSE]
pop <- pop[,-(3:4),drop=FALSE]

# wide to long
le_long <- melt(le, id.vars = c("Country Name","Country Code"))
tfrt_long <- melt(tfrt, id.vars = c("Country Name","Country Code"))
pop_long <- melt(pop, id.vars = c("Country Name","Country Code"))

# change colum names and left join
colnames(le_long) <- c("Country.Name","Country.Code",
                       "Year","Life.Expectancy")
colnames(tfrt_long) <- c("Country.Name","Country.Code",
                         "Year","Fertility.Rate")
colnames(pop_long) <- c("Country.Name","Country.Code",
                        "Year","Population")

df <- merge(le_long,tfrt_long)
df <- merge(df,pop_long)
df1 <- merge(df,metadata[c("Country.Code","Region")])
df2 <- na.omit(df1)
df2 <- df2[which(df2$Region != ''),]

# add a column of index and opacity
df2$id <- 1:nrow(df2)
df2$Opacity <- 0.05
# select useful columns
df2$Year <- as.integer(as.character(df2$Year))
# fillna
df2[is.na(df2)] <- 0

data <- read.csv('cleaned.csv',stringsAsFactors = FALSE)
data[is.na(data)] <- 0
# =============================================Shiny=============================================
# UI
ui <- fluidPage(
  headerPanel('HW2_MaxineQian'),
  
  sidebarPanel(
    radioButtons("region", "Region",
                 c("All" = "All",
                   "East Asia & Pacific" = "East Asia & Pacific",
                   "Europe & Central Asia" = "Europe & Central Asia",
                   "Latin America & Caribbean" = "Latin America & Caribbean",
                   "Middle East & North Africa" = "Middle East & North Africa",
                   "North America" = "North America",
                   "South Asia" = "South Asia",
                   "Sub-Saharan Africa" = "Sub-Saharan Africa")
    ),
    position = 'right'
  ),
  
  sidebarPanel(
    sliderInput("year", 
                "Year", 
                min = 1960, 
                max = 2014, 
                value = 1, 
                animate = animationOptions(interval = 300)),
    position = 'left'
  ),
  
  mainPanel(
    uiOutput("ggvis_ui"),
    ggvisOutput("ggvis")
  )
)

# Server
server <- function(input, output) {
  # for country hover
  all_values <- function(x) {
    if(is.null(x)) return(NULL)
    row <- data[data$id == x$id, ]
    paste(row$Country.Name)
  }
  
  yearData <- reactive({
    data2 <- data
    if(input$region == 'All') data2$Opacity <- 0.8
    else{
      data2$Opacity[data2$Region == input$region] <- 0.8
    }
    # filter year and select data
    df <- 
      data2 %>% filter(Year == input$year) %>%
      select(Country.Name, Fertility.Rate, Life.Expectancy,
             Region, Population, id, Opacity) %>%
      arrange(Region)
    return(df)
  })
  
  yearData %>%
    ggvis(~Life.Expectancy, ~Fertility.Rate, size := ~Population / 200000, key := ~id, fill = ~Region, 
          fillOpacity := ~Opacity, fillOpacity.hover := 0.5) %>%
    add_tooltip(all_values, "hover") %>%
    layer_points(fill = ~Region) %>%
    add_axis("x", title = 'Life expectancy', orient = "bottom") %>%
    add_axis("y", title = 'Fertility rate', orient = "left") %>%
    scale_numeric("x", domain = c(20, 85), nice = T, clamp = F) %>%
    scale_numeric("y", domain = c(1, 9), nice = T, clamp = F) %>%
    bind_shiny("ggvis")
}

shinyApp(ui = ui, server = server)