#' Retrieve Tweet Data for a Twitter User and Chart it
#'
#' @param user Twitter user (handle)
#'
#' @return a list containing the ggplot object and data object
#' 
#' @importFrom twitteR userTimeline twListToDF
#' @import magrittr
#' @import dplyr
#' 
#' @export
get_twitter <- function(user, loadSaved=NULL, addNumbers=TRUE, showPlot=TRUE) {
  
  if (is.null(loadSaved)) {
    
    twitter_setup()
    
    timeline <- twitteR::userTimeline(user, n = 3200, includeRts = TRUE)
    
    Sys.setlocale("LC_ALL", "C")
    
    timelineDF <- twitteR::twListToDF(timeline)
    
    timelineDF$date <- as.Date(timelineDF$created)
    
    timelineDF_agg <- timelineDF %>% group_by(date) %>% 
      summarise(nTweets = n()) %>% 
      merge(data.frame(date = seq(min(timelineDF$date), max(timelineDF$date), "days")), all = TRUE)
    timelineDF_agg[is.na(timelineDF_agg)] <- 0
    
    if (addNumbers) {
      gh_data <- prepare_for_github_chart(data_agg = timelineDF_agg, 
                                          primaryData = "nTweets", 
                                          secondaryData = "nTweets")
    } else {
      gh_data <- prepare_for_github_chart(data_agg = timelineDF_agg, 
                                          primaryData = "nTweets")
    }
    
  } else {
    
    gh_data <- loadSaved$data
    if (!addNumbers) gh_data$data$secondaryTF <- "" 
    
  }
  
  gg <- create_github_chart(gh_data, user, network = "Twitter")
  
  if (showPlot) print(gg)
  
  ret <- list(plot = gg, data = gh_data)
  
  return(ret)
  
}






# get_twitter <- function(user) {
#   
#   twitter_setup()
#   
#   timeline <- twitteR::userTimeline(user, n=3200, includeRts=TRUE)
#   
#   Sys.setlocale("LC_ALL", "C")
#   
#   timelineDF <- twitteR::twListToDF(timeline)  
#   
#   timelineDF$date <- as.Date(timelineDF$created)
# 
#   timelineDF_agg <- timelineDF %>% group_by(date) %>% summarise(nTweets=n()) %>% merge(data.frame(date=seq(min(timelineDF$date),max(timelineDF$date),"days")), all=TRUE)
#   timelineDF_agg[is.na(timelineDF_agg)] <- 0
#   
#   if (min(timelineDF_agg$date) <= today() - years(1)) {
#     ## restrict to the last year
#     timelineDF_agg <- timelineDF_agg %>% filter(date > today() - years(1))
#   } else {
#     ## extend to the last year (e.g. 3200 limit reached or too few)
#     timelineDF_agg %<>% merge(data.frame(date=seq(today() - years(1), min(timelineDF_agg$date), "days"), nTweets=-1), all=TRUE)
#   }
# 
#   # timelineDF_agg$t.fill <- cut(timelineDF_agg$nTweets, breaks=c(0,5,10,15,20,1e5), labels=c("#eeeeee","#d6e685","#1e6823","#8cc665","#44a340"))
#   # timelineDF_agg$t.fill <- cut(timelineDF_agg$nTweets, breaks=c(-1,0,1,5,10,20,1e5), right=FALSE, labels=c("#bbbbbb","#eeeeee",tolower(brewer.pal(9, "Blues")[4:7])))
#   timelineDF_agg$t.fill <- cut(timelineDF_agg$nTweets, breaks=c(-1,0,1,5,10,20,1e5), right=FALSE, labels=c("#bbbbbb","#eeeeee","#9ecae1","#6baed6","#4292c6","#2171b5"))
#     
#     ## split into weeks
#   timelineDF_agg$c.week <- cut(timelineDF_agg$date, breaks="week", start.on.monday=FALSE, labels=FALSE)
#   timelineDF_agg$c.month <- month(timelineDF_agg$date, abbr=TRUE, label=TRUE)
#   timelineDF_agg$c.day  <- as.integer(lubridate::wday(timelineDF_agg$date))
#   # timelineDF_agg$id <- 1:nrow(timelineDF_agg)
#   
#   ## unique values of month
#   rl <- rle(as.character(timelineDF_agg$c.month))
#   month.pos <- timelineDF_agg$c.week[cumsum(rl$lengths)]
#   month.pos <- month.pos[-length(month.pos)]
#   month.lab <- rl$values[-1]  
# 
#   gg <- ggplot(timelineDF_agg, aes(x=c.week, y=c.day, label=nTweets))
#   gg <- gg + geom_tile(fill=timelineDF_agg$t.fill, color="white", size=0.75)
#   gg <- gg + scale_fill_manual(values=timelineDF_agg$t.fill, guide=FALSE)
#   # gg <- gg + scale_x_discrete(limits=range(contribsDF$c.date), breaks=month.pos, labels=month.lab)
#   gg <- gg + scale_x_continuous(limits=c(0,max(timelineDF_agg$c.week)+1), breaks=month.pos, labels=month.lab)
#   # gg <- gg + scale_x_continuous(limits=c(1,54), breaks=c(1,20,30), labels=as.character(LETTERS[1:3]))
#   # gg <- gg + scale_x_discrete(limits=unique(contribsDF$c.week), minor_breaks=seq(0, 1000, 1), breaks=seq(1,1000,30))
#   # gg <- gg + scale_x_discrete(limits=unique(contribsDF$c.week), minor_breaks="day", breaks="month")
#   # gg <- gg + geom_text()
#   # gg <- gg + scale_y_discrete(limits=c(1,7))
#   gg <- gg + scale_y_reverse(breaks=seq(1,7,1), labels=c("","M","","W","","F",""))
#   gg <- gg + theme_minimal()
#   gg <- gg + theme(panel.grid.major=element_blank(),
#                    panel.grid.minor=element_blank())
#   # gg <- gg + theme(axis.text.x=element_text(vjust=0.5))
#   gg <- gg + labs(x="", y="", title="Past 12 months on Twitter (max 3200 Tweets)", subtitle=paste0("@",user), caption="http://github.com/jonocarroll/buttRfly")
#   gg <- gg + coord_fixed(ratio=1)
#   print(gg)
#   
#   return(gg)
#     
# }