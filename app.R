## Packages
library(rvest)
library(ggplot2)
library(ggrepel)
library(shiny)
library(dplyr)
library(stringr)

# setwd('OneDrive/Documents/Quizbowl Stats')
all_stat_reports <- read.csv('all_stat_reports.csv')

# Gets individual stat table from a link
get_raw_tables <- function(link){
  raw_html <- read_html(link)
  tbls_ls <- raw_html %>%
    html_nodes("table") %>%
    .[2] %>%
    html_table(fill = TRUE)
  return(tbls_ls[1])
}

# Cleans the data frame, removes first column
clean_data_frame <- function(data){
  data <- as.data.frame(data)
  colnames(data) <- data[1,]
  cleaned <- data[-c(1),]
  return(cleaned)
}

# Converts all columns that can be expressed as numbers into numeric values instead of strings
numeric_columns <- function(data){
  for (i in 1:ncol(data)){
    if (!(T %in% is.na(suppressWarnings(as_num(data[,i]))))){
      data[,i] <- as_num(data[,i])
    }
    if (colnames(data)[i]==''){
      colnames(data)[i]='X'
    }
  }
  return(data)
}

# Converts - into Inf
as_num <- function(vec){
  fin <- NULL
  for (i in vec){
    if (i %in% c('—','—	')){
      fin <- c(fin,Inf)
    } else {
      fin <- c(fin,as.numeric(i))
    }
  }
  return(fin)
}

# Gets a full, cleaned stat table
get_stats <- function(link){
  return(addDisplay(add_power_perc(addPG(numeric_columns(clean_data_frame(get_raw_tables(link)))))))
}

# Adds a P/G column
addPG <- function(data){
  if (is.null(data$`P/G`) & !is.null(data$`15`) & !is.null(data$`GP`)){
    data$`P/G` = data$`15` / data$`GP`
  } else if (is.null(data$`P/G`) & !is.null(data$`20`) & !is.null(data$`GP`)){
    data$`P/G` = data$`20` / data$`GP`
  }
  return(data)
}

# Adds a P% column
add_power_perc <- function(data){
  if (is.null(data$`P%`) & !is.null(data$`15`) & !is.null(data$`10`)){
    data$`P%` = (data$`15` / (data$`15` + data$`10`))*100
  } else if (is.null(data$`P%`) & !is.null(data$`20`) & !is.null(data$`10`)){
    data$`P%` = (data$`20` / (data$`20` + data$`10`))*100
  }
  # Cleans it to remove any NaNs or NAs, replacing them with 0
  if (!is.null(data$`P%`)){
    for (i in 1:nrow(data)){
      if (is.na(data[i,]$`P%`)){
        data[i,]$`P%` = 0
      }
    }
  }
  return(data)
}

# Adds display names
addDisplay <- function(data){
  data$Display = paste0(data$Player, ' [', data$Team, ']')
  return(data)
}

# Plots data using ggplot
# plot_stats <- function(data,xvar='PPG',yvar='P/G',show_regression=T,show_labels=T,title='Tournament'){
#   gg <- ggplot(data,aes(x=data[,xvar], y=data[,yvar]))
#   if (show_regression){
#     suppressWarnings(gg <- gg + geom_smooth())
#   }  
#   gg <- gg + geom_point(aes(color=Team)) + 
#     theme(legend.position="bottom") + xlab(xvar) + ylab(yvar) + ggtitle(title)
#   if (show_labels){
#       gg <- gg + geom_text_repel(aes(label=Player,color=Team),size=2.5,max.overlaps = 15)
#   }
#   return(gg)
# }
plot_stats <- function(data,xmin,xmax,ymin,ymax,xvar='PPG',yvar='P/G',plot_color='Team',show_regression=T,show_labels=T,title='Tournament'){
  gg <- ggplot(data,aes(x=data[,xvar], y=data[,yvar]))
  if (show_regression){
    suppressWarnings(gg <- gg + geom_smooth())
  }
  if (is.character(data[,plot_color])){
    gg <- gg + geom_point(aes(color=str_wrap(data[,plot_color],width=20)))
  } else {
    gg <- gg + geom_point(aes(color=data[,plot_color],width=20))
  }
  gg <- gg + 
    theme(legend.position="bottom") +
    labs(color = plot_color) +
    xlab(xvar) + ylab(yvar) + ggtitle(title) + xlim(xmin,xmax) + ylim(ymin,ymax)
  if (is.numeric(data[,plot_color])){
    # Creates the color gradient for continuous numeric vectors
    gg <- gg + # scale_color_paletteer_c('grDevices::Warm')
      scale_color_gradientn(colours=c('#9f39e6','#dd00bf','#ff0092','#ff0065','#ff373a','#fc6600','#e28800','#c1a400','#99ba00','#65cc33','#26b462','#109977'))
  }
  if (show_labels){
    if (is.character(data[,plot_color])){
      gg <- gg + geom_text_repel(aes(label=Player,color=str_wrap(data[,plot_color],20)),size=2.5,max.overlaps = 10)
    } else {
      gg <- gg + geom_text_repel(aes(label=Player,color=data[,plot_color]),size=2.5,max.overlaps = 10)
    }
  }
  # print(is.numeric(data[,plot_color]))
  return(gg)
}

# Gets the names of all columns in data that contain a numeric variable
get_numeric_columns <- function(data){
  cols <- NULL
  for (i in 1:ncol(data)){
    if (is.numeric(data[,i])){
      cols <- c(cols,colnames(data)[i])
    }
  }
  return(cols)
}

# Gets all stat reports from a tournament link, places them in a dataframe
get_reports <- function(link){
  raw_html <- read_html(link)
  report_names <- raw_html %>% html_nodes('ul.Stats.NoHeader') %>% html_nodes('li') %>% html_text
  report_links <- raw_html %>% html_nodes('ul.Stats.NoHeader') %>% html_nodes('li') %>% html_nodes('a') %>% html_attr('href')
  report_links <- paste0('https://hsquizbowl.org/db/', report_links, 'individuals')
  return(
    data.frame(
      name = report_names,
      link = report_links
    )
  )
}


string_getlink <- function(str){
  return(
    all_stat_reports[which(all_stat_reports$display==str),'link']
  )
}
get_link <- function(reports,str){
  return(reports[which(reports[,1]==str),2])
}

# Makes sure the dropdowns have default values
get_selection <- function(str,colnames){
  if (str %in% colnames){
    return(str)
  } else {
    return('Rank')
  }
}



get_regression <- function(x,y){
  if (Inf %in% x | Inf %in% y){
    return(c(NA,NA))
  } else {
    return(as.numeric(lm(formula=y ~ x)[[1]]))
  }
}


filter_players <- function(data,players=NULL){ # A vector of players. Make sure if it's null, it just returns the original
  if (is.null(players)){
    return(data)
  } else {
    return(
      filter(data,Display %in% players)
    )
  }
}

# Creates the modal dialog box for the explanation of statistics
stats_explanation <- modalDialog(
  p(strong('GP '),'Total number of games played'),
  p(strong('15 '),em('or'),strong(' 20 '),'Total powers: early buzzes, for which extra points are awarded'),
  p(strong('10 '),'Total tens: correct answers, however not early enough for a power'),
  p(strong('-5 '),'Total negs: incorrect answers, for which points are deducted'),
  p(strong('P/N '),'Ratio of powers to negs'),
  p(strong('G/N '),'Ratio of gets (correctly answered questions) to negs'),
  p(strong('TUH '),'Total number of tossups heard'),
  p(strong('PPTUH '),em('or'),strong(' P/TU '),'Average number of points scored per tossup heard'),
  p(strong('PP20 '),em('or'),strong(' PP20TUH '),'Average number of points scored per 20 tossups heard'),
  p(strong('Pts '),'Total number of points scored',),
  p(strong('PPG '),'Average number of points per game'),
  p(strong('P/G '),'Average number of powers per game'),
  p(strong('P% '),'Power percentage: the proportion of correct tossup answers that were for a power'),
  title='Stat Key',
  easyClose=T
)

# ui <- dashboardPage(
#   dashboardHeader(title='Quizbowl Scatterplots'),
#   dashboardSidebar(
#     selectInput('tourney_name','Tournament Name',choices=all_tournaments_filtered$display),
#     selectInput('stat_report','Stat Report',choices='Prelims'),
#     selectInput('x_axis','x-axis',choices='PPG'),
#     selectInput('y_axis','y-axis',choices='P/G'),
#     checkboxInput('show_regression','Show regression line',value=T),
#     checkboxInput('show_labels','Show names',value=T),
#     p('Note: Some names may not show due to overlap.'),
#     p(textOutput('corr')),
#     p(textOutput('mux')),
#     p(textOutput('medianx')),
#     p(textOutput('sdx')),
#     p(textOutput('muy')),
#     p(textOutput('mediany')),
#     p(textOutput('sdy')),
#     p(textOutput('test'))
#   ),
#   dashboardBody(
#     plotOutput("plot")
#   )
# )

# Shiny
ui <- fluidPage(
  titlePanel('Quizbowl Scatterplots'),
  sidebarLayout(
    sidebarPanel(
      selectInput('tourney_name','Stat report',
                  choices=all_stat_reports$display,
                  selected="2023 Chicago Open <8262> - Combined"),
      selectInput('x_axis','x-axis',choices='PPG'),
      selectInput('y_axis','y-axis',choices='P/G'),
      selectInput('color','Color',choices='Team'),
      actionLink('stats_expl', 'What do all of these stats mean?'),
      checkboxInput('show_regression','Show smoother',value=F),
      checkboxInput('show_labels','Show names',value=T),
      p(em('Note: '),'Some names may not show due to overlap.'),
      selectizeInput('filter_by','Find player(s)',multiple=T,choices='Player...'),
      p(textOutput('size')),
      p(textOutput('corr')),
      p(textOutput('mux')),
      p(textOutput('medianx')),
      p(textOutput('sdx')),
      p(textOutput('muy')),
      p(textOutput('mediany')),
      p(textOutput('sdy')),
      p(uiOutput('link')),
      # p(textOutput('intercept')),
      # p(textOutput('slope')),
      width=3
    ),
    mainPanel(
      plotOutput("plot")
    )
  )
)

server <- function(input,output,session){
  the_data <- reactive({
    get_stats(string_getlink(input$tourney_name))
  })
  filtered <- reactive({
    filter_players(the_data(),input$filter_by)
  })
  # Defines the limits of the plot
  xmin <- reactive(min(the_data()[,input$x_axis]))
  xmax <- reactive(max((the_data()[,input$x_axis])[is.finite(the_data()[,input$x_axis])]))
  ymin <- reactive(min(the_data()[,input$y_axis]))
  ymax <- reactive(max((the_data()[,input$y_axis])[is.finite(the_data()[,input$y_axis])]))
  # Plot function
  output$plot <- renderPlot({
    plot_stats(data=filtered(),xmin(),xmax(),ymin(),ymax(),
               input$x_axis,input$y_axis,input$color,
               input$show_regression,input$show_labels,
               title = input$tourney_name)
  },height=800,res=96)
  # # Dynamically updates the selectInput
  observe({
     updateSelectInput(session,'x_axis',
                       choices=get_numeric_columns(the_data()),
                       selected=get_selection('PPG',colnames(the_data())))
  })
  observe({
     updateSelectInput(session,'y_axis',
                       choices=get_numeric_columns(the_data()),
                       selected=get_selection('P/G',colnames(the_data())))
  })
  observe({
    updateSelectizeInput(session,'filter_by',choices=sort(the_data()[,'Display']))
  })
  observe({
    updateSelectInput(session,'color',
                      choices= colnames(the_data())[! colnames(the_data()) %in% c('Player', 'Display', 'X')],
                      selected='Team'
                      )
  })
  observeEvent(
    input$stats_expl, {showModal(stats_explanation)}
  )
  # # START ----
  output$size <- renderText({
    paste0('n = ',nrow(filtered()))
  })
  output$corr <- renderText({
    paste0('r = ', cor(filtered()[,input$x_axis],filtered()[,input$y_axis]))
  })
  output$mux <- renderText({
   paste0('µx = ', mean(filtered()[,input$x_axis]))
  })
  output$medianx <- renderText({
    paste0('median x = ',median(filtered()[,input$x_axis]))
  })
  output$sdx <- renderText({
    paste0('σx = ', sd(filtered()[,input$x_axis]))
  })
  output$muy <- renderText({
    paste0('µy = ', mean(filtered()[,input$y_axis]))
  })
  output$mediany <- renderText({
    paste0('median y = ',median(filtered()[,input$y_axis]))
  })
  output$sdy <- renderText({
    paste0('σy = ', sd(filtered()[,input$y_axis]))
  })
  output$link <- renderUI({
    a('Link to stats',href=string_getlink(input$tourney_name),target='_blank')
  })
  
  # output$intercept <- renderText({
  #   paste0('intercept = ', (get_regression(filtered()[,input$x_axis],filtered()[,input$y_axis]))[1])
  # })
  # output$slope <- renderText({
  #   paste0('slope = ', (get_regression(filtered()[,input$x_axis],filtered()[,input$y_axis]))[2])
  # })
  # END ----
}

## Run shiny
shinyApp(ui,server)