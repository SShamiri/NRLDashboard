# This is for 2015 from nrl website to be updated weekly
dat_update <- function(ladder = "data/ladder.rds",score = "data/score.rds"){
  # updating ladder 
  web <- html("http://www.nrl.com/DrawResults/TelstraPremiership/Ladder/tabid/10251/Default.aspx")
  nrl_15 <-  html_table(html_nodes(web, "table"),fill = TRUE)[[1]][,c(2,13)]
  colnames(nrl_15)[1] <- "nrl"
  ### lookup 
  lookup <- read.csv("data/lookup.csv",T)
  nrl_15 <- merge(nrl_15, lookup, by = 'nrl' ,all=TRUE)
  nrl_15$Year <- rep(2015, nrow(nrl_15))
  nrl_15 <- nrl_15[-17,c(2,6,3)]
  colnames(nrl_15)[3] <- "Team"
  old <- readRDS(ladder) 
  old <- old[old$Year != 2015,]
  ladder = rbind(old,nrl_15)
  
  ## update score data
  wiki <- html("http://en.wikipedia.org/wiki/2015_NRL_season_results")
  score_15 <-  html_table(html_nodes(wiki, "table"),fill = TRUE)[[10]] ## table number 10
  score_15 <- score_15[-1,1:4]
  colnames(score_15) <- c("HomeTeam","score","AwayTeam","Date")
  score_15$Date <- as.character(as.Date(score_15$Date,"%d %b %Y"))
  score_15$HomeScore <- as.numeric(lapply(strsplit(as.character(score_15$score), split="-"), "[", 1))
  score_15$AwayScore <- as.numeric(lapply(strsplit(as.character(score_15$score), split="-"), "[", 2))
  # re-arrange columns
  score_15 <- score_15[,c(4,1,3,5,6)]
  score_15$year <- format(strptime(score_15$Date, format="%Y-%m-%d"),"%Y")
  
  score_15$HomeTeam <- sub("Canterbury-Bankstown Bulldogs","Canterbury Bulldogs",score_15$HomeTeam)
  score_15$HomeTeam <- sub("Manly-Warringah Sea Eagles","Manly Sea Eagles",score_15$HomeTeam)
  score_15$HomeTeam <- sub("Cronulla-Sutherland Sharks","Cronulla Sharks",score_15$HomeTeam)
  
  score_15$AwayTeam <- sub("Canterbury-Bankstown Bulldogs","Canterbury Bulldogs",score_15$AwayTeam)
  score_15$AwayTeam <- sub("Manly-Warringah Sea Eagles","Manly Sea Eagles",score_15$AwayTeam)
  score_15$AwayTeam <- sub("Cronulla-Sutherland Sharks","Cronulla Sharks",score_15$AwayTeam)
  
  old_score <- readRDS(score) 
  old_score <- old_score[old_score$year != 2015,]
  new_score <- rbind(old_score,score_15)
  
  result <- list(ladder =ladder,score= new_score)
  return(result)
  
}
