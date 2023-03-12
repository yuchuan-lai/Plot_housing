library(ggplot2)
library(dplyr)
library(leaflet)
library(leaflet.minicharts)
library(RColorBrewer)
library(sf)
library(leafpop)
library(shiny)
library(scales)
library(plotly)

hist.data <- read.csv("https://yuchuanlai.com/Data/Metro_zhvi_uc_sfrcondo_tier_0.33_0.67_sm_sa_month.csv")
fcast.data <- read.csv("https://yuchuanlai.com/Data/Metro_zhvf_growth_uc_sfrcondo_tier_0.33_0.67_sm_sa_month.csv")
city.comb.sum <- read.csv("https://yuchuanlai.com/Data/Region.perc.ts.csv")[-c(1)]
city.info.df <- read.csv("https://yuchuanlai.com/Data/Region.info.csv")[-c(1)]

na.idx <- c(1:nrow(hist.data))[apply(hist.data, 1, function(data) is.na(data[6]))]
hist.data.filtered <- hist.data[-c(1, na.idx), ]
region.names <- hist.data.filtered$RegionName

cal_hist.df <- function(city.data) {
  city.data.ts <- t(city.data[-c(1:5)])
  city.data.date <- as.Date(gsub("[.]", "-", gsub("X", "", names(city.data[-c(1:5)]))))
  city.data.xts <- xts::xts(city.data.ts / 1e3, order.by = city.data.date)
  city.data.df <- data.frame("Date" = city.data.date, "price" = city.data.xts)
  colnames(city.data.df) <- c("Date", "price")
  city.info <- city.data[c(1:4)]
  # colnames(city.data.df) <- city.info[3]
  return(city.data.df)
}
cal_fcast.df <- function(city.data) {
  city.data.ts <- t(city.data[-c(1:6)])
  city.data.date <- as.Date(gsub("[.]", "-", gsub("X", "", names(city.data[-c(1:6)]))))
  city.data.xts <- xts::xts(city.data.ts, order.by = city.data.date)
  city.data.df <- data.frame("Date" = city.data.date, "price" = city.data.xts)
  colnames(city.data.df) <- c("Date", "price")
  city.data.df$price <- (city.data.df$price  + 100)  / 100 
  # city.info <- city.data[c(1:4)]
  # colnames(city.data.df) <- city.info[3]
  return(city.data.df)
}
plot_ts <- function(select.region) {
  
  select.row <- c(1:length(region.names))[region.names == select.region]
  select.row_fcast <- c(1:length(fcast.data$RegionName))[fcast.data$RegionName == select.region]
  
  city.df <- cal_hist.df(hist.data.filtered[c(select.row), ])
  city.fcast.df <- cal_fcast.df(fcast.data[c(select.row_fcast), ])
  city.fcast.df$price <- city.fcast.df$price * city.df$price[c(nrow(city.df))]
  # city.fcast.df <- rbind(city.df[c(nrow(city.df)), ], city.fcast.df)
  
  arima.fcast.st.date <- as.Date(paste0("2023-02", "-01"))
  fit.df <- filter(city.df, Date < arima.fcast.st.date)
  arima.fit <- forecast::auto.arima(fit.df$price, allowdrift = TRUE)
  arima.fcast <- forecast::forecast(arima.fit, h = 12, level = c(80, 95), biasadj = TRUE)
  arima.fcast.df <- data.frame(arima.fcast)
  arima.fcast.df$Date <- (seq(from = fit.df$Date[nrow(fit.df)] + 1, length.out = 13, by = "month") - 1)[-c(1)]
  
  max.y.axis <- max(city.df$price, arima.fcast.df$Hi.95, city.fcast.df$price, na.rm = TRUE)
  min.y.axis <- min(city.df$price, arima.fcast.df$Lo.95, city.fcast.df$price, na.rm = TRUE)
  
  y.bound <- seq(min.y.axis, max.y.axis, length.out = 100)
  y.breaks <- as.numeric(gsub("\\(|\\[", "", gsub(",.*", "", levels(cut(y.bound, breaks = pretty(y.bound, n = 10), include.lowest = TRUE)))))
  y.breaks <- c(y.breaks, y.breaks[length(y.breaks)] + y.breaks[2] - y.breaks[1])
  
  city.df$var <- rep("hist", nrow(city.df))
  city.fcast.df$var <- rep("fcast", nrow(city.fcast.df))
  arima.fcast.mean.df <- select(arima.fcast.df, c(Date, Point.Forecast))
  colnames(arima.fcast.mean.df)[2] <- "price"
  arima.fcast.mean.df$var <- rep("arima", nrow(city.fcast.df))
  
  comb.df <- rbind(city.df, city.fcast.df) 
  comb.df$var <- factor(comb.df$var, levels = c('hist', 'fcast'))
  comb.df$Date <- as.Date(comb.df$Date)
  
  city.full.df <- rbind(city.df, city.fcast.df)
  
  city.full.df <-  mutate(city.full.df, perc.change = (price/lag(price) - 1) * 100)
  city.full.df$perc.ma <- stats::filter(city.full.df$perc.change, rep(1/12, 12), sides = 1)
  city.full.df$var <- rep("hist", nrow(city.full.df))
  city.full.df$var[city.full.df$Date > city.df$Date[nrow(city.df)]] <- "fcast"
  city.full.df$var <- factor(city.full.df$var, levels = c('hist', 'fcast'))
  city.full.df$Date <- as.Date(city.full.df$Date)
  
  mean.price <- mean(comb.df$price, na.rm = TRUE)
  max.perc.axis <- max(city.full.df$perc.ma, na.rm = TRUE) * 2
  min.perc.axis <- min(city.full.df$perc.ma, na.rm = TRUE) * 2
  mean.perc.change <- mean(city.full.df$perc.change, na.rm = TRUE)
  
  trans.value <- (max.perc.axis - min.perc.axis) / (max.y.axis - min.y.axis)
  
  city.full.df$perc.trans <- round((city.full.df$perc.ma - min.perc.axis ) / trans.value + min.y.axis, digits = 2)
  
  y.bound.alter <- seq(min.perc.axis, max.perc.axis, length.out = 100)
  y.breaks.alter <- as.numeric(gsub("\\(|\\[", "", gsub(",.*", "", levels(cut(y.bound.alter, breaks = pretty(y.bound.alter, n = 5), include.lowest = TRUE)))))
  y.breaks.alter <- c(y.breaks.alter, y.breaks.alter[length(y.breaks.alter)] + y.breaks.alter[2] - y.breaks.alter[1])
  y.breaks.alter <- c(y.breaks.alter, round(mean.perc.change, digits = 2))
  
  theme_set(theme_bw())
  
  ts.p <- ggplot() + 
    geom_hline(yintercept = (0 - min.perc.axis ) / trans.value + min.y.axis, color = "#FF330060", linetype = "longdash") +
    geom_hline(yintercept = (mean.perc.change - min.perc.axis ) / trans.value + min.y.axis, color = "#33660060", linetype = "longdash") +
    geom_line(data = comb.df, aes(x = Date, y = price, color = var), size = 1) +
    geom_line(data = city.full.df, aes(x = Date, y = perc.trans , color = var), linetype = "longdash") +
    scale_color_manual(values = c("#333333", "#0000FF"), 
                       labels = c("Historical data", "Zillow forecast"),
                       name = "") +
    scale_x_date(date_breaks = "2 year", labels = date_format("%m/%Y")) +
    scale_y_continuous(breaks = y.breaks,  sec.axis = sec_axis(~ (. - min.y.axis ) * trans.value + min.perc.axis, breaks = y.breaks.alter, 
                                                               name = "12 month moving average m-to-m percent change (%)")) +
    xlab("Date") + ylab(paste0("Zillow median price (thousand dollars)")) +
    theme(legend.position = "bottom", plot.title = element_text(size = 18), 
          axis.title.x = element_text(size = 15), axis.title.y = element_text(size = 15), legend.text = element_text(size = 15),
          axis.text.y = element_text(size = 14), axis.text.x = element_text(size = 12)) +
    ggtitle(paste0("Metropolitan area: ", select.region))
  
  
  return(ts.p)
}
plot_all.ts <- function(select.var) {
  if (select.var == "Price change relative to 2000") {
    
    city.info.conus <- filter(city.info.df, is.na(Lat) == FALSE)
    city.info.conus$idx <- c(1:nrow(city.info.conus))
    city.comb.sum <- dplyr::filter(city.comb.sum, idx <= 100)
    city.comb.sum$perc <- city.comb.sum$perc * 100
    city.comb.sum$Lat <- apply(city.comb.sum, 1, function(data) city.info.conus$Lat[city.info.conus$idx == as.numeric(data[6])])
    city.comb.sum$Lon <- apply(city.comb.sum, 1, function(data) city.info.conus$Lon[city.info.conus$idx == as.numeric(data[6])])
    city.comb.sum$city <- apply(city.comb.sum, 1, function(data) city.info.conus$Region[city.info.conus$idx == as.numeric(data[6])])
    city.comb.sum.sql <- highlight_key(city.comb.sum, ~ city)
    
    plot_ly(city.comb.sum.sql, color = I("#FF6600"), alpha = 0.5) %>% group_by(city) -> ts.p
    ts.p %>% group_by(city) %>% add_lines(x = ~ Date, y = ~ perc) -> obs.ts
    obs.ts %>% layout(title = "Zillow median list price (seasonaly adjusted) of top 100 metro area", xaxis = list(title = "Date", dtick = 12), 
                      yaxis = list(title =  "Median price compared to 2000 (%)")) -> obs.ts
    
    highlight(
      obs.ts, 
      on = "plotly_hover",
      selectize = FALSE, 
      dynamic = FALSE,
      color = "red",
      persistent = FALSE
    )
    
  } else {
    
    city.info.conus <- filter(city.info.df, is.na(Lat) == FALSE)
    city.info.conus$idx <- c(1:nrow(city.info.conus))
    city.comb.sum <- dplyr::filter(city.comb.sum, idx <= 100)
    
    city.comb.sum$Lat <- apply(city.comb.sum, 1, function(data) city.info.conus$Lat[city.info.conus$idx == as.numeric(data[6])])
    city.comb.sum$Lon <- apply(city.comb.sum, 1, function(data) city.info.conus$Lon[city.info.conus$idx == as.numeric(data[6])])
    city.comb.sum$city <- apply(city.comb.sum, 1, function(data) city.info.conus$Region[city.info.conus$idx == as.numeric(data[6])])
    city.comb.sum <- filter(city.comb.sum, Date < as.Date("2023-03-01"))
    city.comb.sum.sql <- highlight_key(city.comb.sum, ~ city)
    
    plot_ly(city.comb.sum.sql, color = I("#FF6600"), alpha = 0.2) %>% group_by(city) -> ts.p
    ts.p %>% group_by(city) %>% add_lines(x = ~ Date, y = ~ perc.ma) -> obs.ts
    obs.ts %>% layout(title = "12-month moving average of M-to-M percentage changes of top 100 metro area", xaxis = list(title = "Date", dtick = 12), 
                      yaxis = list(title =  "M-to-M percent change (%)")) -> obs.ts
    
    highlight(
      obs.ts, 
      on = "plotly_hover",
      selectize = FALSE, 
      dynamic = FALSE,
      color = "red",
      persistent = FALSE
    )
  }
  
  
}

all.states <- gsub(".*[, ]", "", region.names)
all.states.names <- state.name[match(all.states, state.abb)]
all.states.names[is.na(all.states.names)] <- "Washington DC"
region.names.df <- data.frame("abb" = all.states, "name" = all.states.names, "region" = region.names)
unique.states.names <- unique(sort(state.name[match(all.states, state.abb)]))

ts.list <- c("Price change relative to 2000", "Price change rate")

ui <- fluidPage(
  titlePanel("Median home price tracking at US metro (Zillow)"),
  sidebarLayout(
    sidebarPanel(
      selectInput("var", "For the top 100 metro", choices = ts.list, selected = ts.list[1]),
      selectInput("state", "For plotting the selected metro: select the state", choices = unique.states.names, selected = unique.states.names[1]),
      selectInput("region", "For  plotting the selected metro: select the region", choices = region.names.df$region[region.names.df$name == unique.states.names[1]], 
                  selected = region.names.df$region[region.names.df$name == unique.states.names[1]][1]),
      width = 2
    ),
    mainPanel(
      shinycssloaders::withSpinner(
        fluidRow(plotlyOutput("all.region.p", height = "500px", width = "800px"), 
                 plotOutput("select.region.p", height = "500px", width = "800px")
        ),
        hide.ui = FALSE, type = 3, color = "#666666", color.background = "#FFFFFF")
    )
  )
)

server <- function(input, output, session) {
  
  observeEvent(input$state, {
    x <- input$state
    updateSelectInput(session, "region",
                      choices = region.names.df$region[region.names.df$name == x],
                      selected = region.names.df$region[region.names.df$name == x][1])
  })
  
  
  output$all.region.p <- renderPlotly({
    plot_all.ts(input$var)
  })
  output$select.region.p <- renderPlot({
    plot_ts(input$region)
  })
  
}

shinyApp(ui = ui, server = server)

