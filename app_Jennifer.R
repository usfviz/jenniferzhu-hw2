library(shiny)
library(ggvis)
library(reshape2)
library(dplyr)

ui <- fluidPage(
  headerPanel('HW2 by Jennifer Zhu'),
  
  sidebarPanel(
    sliderInput("year", 
                "Year", 
                min = 1960, 
                max = 2014, 
                value = 1, 
                animate = animationOptions(interval = 100)),
    position = 'left'
  ),
  
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
  
  mainPanel(
    uiOutput("ggvis_ui"),
    ggvisOutput("ggvis")
  )
)

server <- function(input, output) {
  ## Process data into a single data frame
  # load life expectancy
  le <- read.csv('API_SP.DYN.LE00.IN_DS2_en_csv_v2/API_SP.DYN.LE00.IN_DS2_en_csv_v2.csv', header = 5, skip = 4, stringsAsFactors = F)
  le <- le[, c(1:2, 5:(ncol(le)-3))]
  # make wide table long
  le <- melt(le, id.vars = colnames(le)[1:2])
  colnames(le) <- c('Country.Name', 'Country.Code', 'Year', 'Life.Expectancy')
  
  # load fertility rate
  fr <- read.csv('API_SP.DYN.TFRT.IN_DS2_en_csv_v2/API_SP.DYN.TFRT.IN_DS2_en_csv_v2.csv', header = 5, skip = 4, stringsAsFactors = F)
  fr <- fr[, c(1:2, 5:(ncol(fr)-3))]
  # make wide table long
  fr <- melt(fr, id.vars = colnames(fr)[1:2])
  colnames(fr) <- c('Country.Name', 'Country.Code', 'Year', 'Fertility.Rate')
  
  # load country metadata
  ct <- read.csv('API_SP.DYN.TFRT.IN_DS2_en_csv_v2/Metadata_Country_API_SP.DYN.TFRT.IN_DS2_en_csv_v2.csv', header = 1, stringsAsFactors = F)[, 1:2]
  # get rid of na level
  ct <- ct[ct$Region != "",]  
  
  # load population metadata
  pl <- read.csv('population.csv', header = 1, stringsAsFactors = F)
  pl <- pl[, c(2, 5:(ncol(pl)-2))]
  # make wide table long
  pl <- melt(pl, id.vars = 'Country.Code')
  colnames(pl) <- c('Country.Code', 'Year', 'Population')
  
  # merge all data into the same table
  dat <- merge(le, fr, by = c('Country.Name', 'Country.Code', 'Year'))
  dat <- merge(dat, ct, by = 'Country.Code')
  dat <- merge(dat, pl, by = c('Country.Code', 'Year'))
  # remove rows with na
  dat <- dat[!is.na(dat), ]
  # convert Year from factor to number
  dat$Year <- as.integer(as.character(substring(dat$Year, 2)))
  # add a column of index
  dat$id <- 1:nrow(dat)
  # colors to fill
  defaultColors <- factor(c("#dc3912", "#ff9900", "#109618", "#990099", "#0099c6", "#3366cc", "#dd4477"))
  dat$color <- defaultColors[as.numeric(factor(dat$Region))]
  # opacity
  dat$Opacity <- 0.2
  
  
  all_values <- function(x) {
    if(is.null(x)) return(NULL)
    row <- dat[dat$id == x$id, ]
    paste(row$Country.Name)
  }

  
  yearData <- reactive({
    dat2 <- dat
    if(input$region == 'All') dat2$Opacity <- 0.8
    else{
      dat2$Opacity[dat2$Region == input$region] <- 0.8
    }
    
    # Filter to the desired year
    # Also sort by region
    df <- 
      dat2 %>%
      filter(Year == input$year) %>%
      select(Country.Name, Fertility.Rate, Life.Expectancy,
             Region, Population, id, color, Opacity) %>%
      arrange(Region)
    
    return(df)
  })
  
    ggvis(yearData, ~Life.Expectancy, ~Fertility.Rate, size := ~Population / 1000000, key := ~id, fill = ~Region, opacity := ~Opacity) %>%
    add_tooltip(all_values, "hover") %>%
    layer_points() %>%
    hide_legend('fill') %>%
    hide_legend('size') %>%
    add_axis("x", title = 'Life expectancy', orient = "bottom") %>%
    add_axis("y", title = 'Fertility rate', orient = "left") %>%
    scale_numeric("x", domain = c(0, 90), nice = FALSE, clamp = TRUE) %>%
    scale_numeric("y", domain = c(0.5, 9), nice = FALSE, clamp = TRUE) %>%
    bind_shiny("ggvis", "ggvis_ui")
}

shinyApp(ui = ui, server = server)
