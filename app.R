library(shiny)
library(shinydashboard)
library(rCharts)
library(dygraphs)
library(httr)
library(twitteR)
library(tm)
require(xts)
require(stringr)
library(rvest)

#### data 
nrl <- readRDS("data/nrl.rds")
nrl$Date <- as.Date(as.character(nrl$Date),"%d/%m/%Y")
nrl$year <- format(strptime(nrl$Date, format="%Y-%m-%d"),"%Y")
win.freq <- table(nrl$winer,nrl$year)
win.df <- as.data.frame(win.freq)
colnames(win.df) <- c("Team","year","Score")

teams <- as.character(unique(nrl$HomeTeam))
twitterHandle <- readRDS("data/twitterHandle.rds")

### scraping twitter for followers
web <- list()
followers <- NULL

for(i in 1: nrow(twitterHandle)) {
    web[[i]] <- html(as.character(twitterHandle$web[i]))
    followers[i] <- web[[i]] %>% 
    html_node(".ProfileNav-item--followers") %>%
    html_text() 
  followers[i] <- gsub(" ", "", unlist(strsplit(followers[i], "\n"))[4],fixed = TRUE)
}

twitterHandle$followers <- followers


options(httr_oauth_cache = TRUE) 
setup_twitter_oauth("IvrDnTHaczIAQBOAumidAw", "FyUIBVMuwCyyHu0H2yjDttVtVSAGPGU6E4nfEoWOBBg",
                    "1429490635-rdvUYDtPERc6iZdCe1bwyaZ8QyWXiLvX0CgOsxl","DPdSRQ5todzxa2hfDcmxDoL0ysci15muI0Aon2vezw" )

########## ui Shiny 

header <- dashboardHeader(title = " ")
side <- dashboardSidebar(selectInput('homeTeam', 'Home team', teams),
                        selectInput('awayTeam', 'Away team', rev(teams)),
                        sliderInput("winer", "Year:", 2009, 2015, 1),
                        actionButton("goButton", "Get tweets!"),br(),p(),
                        div(img(src="samOnline.png")),br(),p("Samuel Shamiri"),p("Starcom-Australia"),
                        a(href= "https://twitter.com/SamuelShamiri",target="_blank",icon("twitter-square", "fa-2x")),
                        a(href= "https://au.linkedin.com/pub/samuel-shamiri/2a/701/530",target="_blank",icon("linkedin-square", "fa-2x"))
)

body <- dashboardBody(
  includeCSS("custom.css"),
  fixedRow(
    column(width = 6, div(class="col-md-6" ,h2("National Rugby League")
    )),
    column(width = 6,img(src="clublinks-sprite.png",align="bottom",style= "margin: 0.6cm"))
    
    
  ),
  fixedRow(
    box(status = "info", width = 5, title = "Twitter Trends Timeline", 
        solidHeader = TRUE,collapsible = TRUE,dygraphOutput("dygraph")),
    box(status = "info", width = 7, title = "Home team versus Away team scores:", 
        solidHeader = TRUE,collapsible = TRUE,showOutput("Chart2", "nvd3"))
  ),
  fixedRow(
    column(width = 4,box(title = "Team Rankings", status = "warning",solidHeader = TRUE,showOutput("Chart3", "nvd3"),width = 12)),
  
    column(width = 4, box(width = 12,status = "danger",div(infoBoxOutput("follower_01"),style="width: 40% ; height: 60px ; margin: 10px 10px 10px 10px ;float:left "),
                          div(imageOutput("image1"),style="width: 40% ; height: 60px ; margin: 10px 10px 10px 10px ;float:left "),
                          div(style="width: 100% ; height: 90px"), tableOutput("tw_top5_01"))
           ),
    
    column(width = 4, box(width = 12,status = "danger",div(infoBoxOutput("follower_02"),style="width: 40% ; height: 60px ; margin: 10px 10px 10px 10px ;float:left "),
                           div(imageOutput("image2"),style="width: 40% ; height: 60px ; margin: 10px 10px 10px 10px ;float:right "),
                         div(style="width: 100% ; height: 90px"), div(tableOutput("tw_top5_02")))
    )
  )
  
)

ui <- dashboardPage(header, side, body) 


############ server shiny

server = function(input, output){
  dataInput <- reactive({
    filter_dat <- nrl[nrl$HomeTeam == input$homeTeam & nrl$AwayTeam == input$awayTeam, ]
    home_team <- filter_dat[,c(1,2,4)]
    away_team <- filter_dat[,c(1,3,5)]
    colnames(home_team) <- c("Date","Team","Score")
    colnames(away_team) <- c("Date","Team","Score")
    home_away <- rbind(home_team,away_team)
    home_away
  })
  
  dataWiner <- reactive({
    winer <- win.df[win.df$year == input$winer ,]
    winer <- winer[order(- winer$Score),]
    winer
  })
 
  tw_data_entry_01 <- reactive({
	input$goButton
	isolate({
	entry_01 <- as.character(twitterHandle[twitterHandle$Team == input$homeTeam,2])
	nrl_entry_01 <- userTimeline(entry_01,n = 500)
	nrl_01.df <- twListToDF(nrl_entry_01)
	 nrl_01.df
	})
  })
  
  tw_data_entry_02 <- reactive({
	input$goButton
	isolate({
	entry_02 <- as.character(twitterHandle[twitterHandle$Team == input$awayTeam,2])  
    nrl_entry_02 <- userTimeline(entry_02,n = 500)
	 nrl_02.df <- twListToDF(nrl_entry_02)
	 nrl_02.df
	})
  })
  
  tw_top5_entry_01 <- reactive({
  	input$goButton
	isolate({
	tw_top5 <- tw_data_entry_01()
	tw_top5 <- as.data.frame(tw_top5[1:5,1])
	colnames(tw_top5) <- "most recent tweets"
	tw_top5
	})
  })
  
  tw_top5_entry_02 <- reactive({
  	input$goButton
	isolate({
	tw_top5 <- tw_data_entry_02()
	tw_top5 <- as.data.frame(tw_top5[1:5,1])
	colnames(tw_top5) <- " Recent tweets"
	tw_top5
	})
  })
  
   output$tw_top5_01 <- renderTable(tw_top5_entry_01())
  output$tw_top5_02 <- renderTable(tw_top5_entry_02()) 
  
  dataTwitter = reactive({
    input$goButton
    isolate({
      ## to data frame
      nrl_01.df <- tw_data_entry_01()
      nrl_02.df <- tw_data_entry_02()
      
      trim <- function (x) sub('@','',x)
      
      nrl_01.df$rt <- sapply(nrl_01.df$text,function(tweet) trim(str_match(tweet,"^RT (@[[:alnum:]_]*)")[2]))
      nrl_01.df$rtt <- sapply(nrl_01.df$rt,function(rt) if (is.na(rt)) 'T' else 'RT')
      nrl_02.df$rt <- sapply(nrl_02.df$text,function(tweet) trim(str_match(tweet,"^RT (@[[:alnum:]_]*)")[2]))
      nrl_02.df$rtt <- sapply(nrl_02.df$rt,function(rt) if (is.na(rt)) 'T' else 'RT')
    
      ts_01 <- xts(rep(1,times=nrow(nrl_01.df)),nrl_01.df$created,timezone="GMT")
      ts_01.sum <- apply.daily(ts_01,sum) 
      ts_02 <- xts(rep(1,times=nrow(nrl_02.df)),nrl_02.df$created,timezone="GMT")
      ts_02.sum <- apply.daily(ts_02,sum)
      Twitter_dat <- merge(ts_01.sum,ts_02.sum,all = TRUE)
      Twitter_dat[is.na(Twitter_dat)] <- 0
      colnames(Twitter_dat) <- c(input$homeTeam,input$awayTeam)
      Twitter_dat
    })
  })
  #####
  
output$Chart2 <- renderChart2({
    n1 = nPlot(Score ~ Date, group = "Team", data =dataInput(), type = "multiBarChart")
    n1$xAxis(
      tickFormat =   "#!
      function(d) {return d3.time.format('%Y-%m-%d')(new Date(d*1000*3600*24));}!#")
    n1$setTemplate(afterScript='<style> svg text {font-size: 6px;}</style>')
    n1$chart(reduceXTicks = F,
             showYAxis=FALSE,
             margin=list(left=0,right = 0),
			       width = 600
    )
    return(n1)
  })
  
  output$Chart3 <- renderChart2({
    nn = nPlot(Score ~ Team, group = "year", data =dataWiner(), type = "multiBarHorizontalChart")
    nn$chart(
      color=c("grey","blue"),
      margin=list( left=170),
      showControls=FALSE,
	    width = 300
    )
    return(nn)
  })
  
  output$dygraph <- renderDygraph({
    input$goButton
    isolate({
      dygraph(dataTwitter()) %>%
      dyRangeSelector() %>%
      dyOptions(stackedGraph = TRUE,drawGrid = FALSE) %>%
      dyLegend(width = 210)  
      
    })
  })
  
  output$text_01 <- renderText({
    as.character(twitterHandle[twitterHandle$Team==input$homeTeam,3])
  })
  output$image1 <- renderImage({ 
    link_01 = as.character(twitterHandle[twitterHandle$Team==input$homeTeam,4])
    list(src = link_01
    )
  },deleteFile = FALSE)
  
  output$text_02 <- renderText({
    
    as.character(twitterHandle[twitterHandle$Team==input$awayTeam,3])
  })
  output$image2 <- renderImage({ 
    link_01 = as.character(twitterHandle[twitterHandle$Team==input$awayTeam,4])
    list(src = link_01,
         width=80,
         height = 48,
         float ="right")
    
  },deleteFile = FALSE)
  
  output$follower_01 <- renderInfoBox({
    entry_01 = as.character(twitterHandle[twitterHandle$Team==input$homeTeam,6])
    link_01 = as.character(twitterHandle[twitterHandle$Team==input$homeTeam,5])
    infoBox(title ="", entry_01, icon = icon("twitter"),
      color = "purple",subtitle = "Followers" , href = link_01
    )
  })
  
  output$follower_02 <- renderUI({
    entry_02 = as.character(twitterHandle[twitterHandle$Team==input$awayTeam,6])
    link_02 = as.character(twitterHandle[twitterHandle$Team==input$awayTeam,5])
    infoBox(title ="", entry_02, icon = icon("twitter"), 
             color = "purple",subtitle = "Followers", href = link_02
    )
  })
  
}

shinyApp(ui, server)





