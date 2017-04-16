#Library
library(reshape2)
library(shiny)
library(plotly)
library(varhandle)

# load data
le <-read.csv(text=readLines('API_SP.DYN.LE00.IN_DS2_en_csv_v2.csv')[-(1:4)],check.names=FALSE)
tfrt <-read.csv(text=readLines('API_SP.DYN.TFRT.IN_DS2_en_csv_v2.csv')[-(1:4)],check.names=FALSE)
pop <- read.csv(text=readLines('API_SP.POP.1564.TO.ZS_DS2_en_csv_v2.csv')[-(1:4)],check.names=FALSE)
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

slope <- 2.666051223553066e-05
df2$size <- sqrt(df2$Population * slope)
colors <- c('#4AC6B7', '#1972A4', '#965F8A', '#FF7070', '#C61951')

# Shiny
ui <- fluidPage(
  headerPanel('Life Expectancy vs. Fertility Rate'),
  sidebarPanel(
    sliderInput("year", "Year:", 
                min = 1960, max = 2014, value = 1960, step = 1, 
                animate=animationOptions(interval=300, loop=T))
                ),
  mainPanel(plotlyOutput("plot1"))
)

server <- function(input, output) {
  
  output$plot1 <- renderPlotly({
    df2_year <- df2[which(df2$Year == input$year),]
    plot_ly(df2_year, x = Life.Expectancy, y = Fertility.Rate, color = Region, size = size, colors = colors,
            type = 'scatter', mode = 'markers', sizes = df2_year$size,
            marker = list(symbol = 'circle', sizemode = 'diameter',
                          line = list(width = 2, color = '#FFFFFF')),
            text = paste('Country:', df2_year$Country.Name)) %>%
      layout(
             xaxis = list(title = 'Life Expectancy',
                          gridcolor = 'rgb(255, 255, 255)',
                          #range = c(2.003297660701705, 5.191505530708712),
                          type = 'log',
                          zerolinewidth = 1,
                          ticklen = 5,
                          gridwidth = 2),
             yaxis = list(title = 'Fertility Rate',
                          gridcolor = 'rgb(255, 255, 255)',
                          #range = c(36.12621671352166, 91.72921793264332),
                          zerolinewidth = 1,
                          ticklen = 5,
                          gridwith = 2),
             paper_bgcolor = 'rgb(243, 243, 243)',
             plot_bgcolor = 'rgb(243, 243, 243)')
  })
}

shinyApp(ui = ui, server = server)