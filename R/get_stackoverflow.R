get_stackoverflow <- function(user) {
  
  stackoverflow_setup()
  
  # timeline <- twitteR::userTimeline(user, n=3200, includeRts=TRUE)
  # answers <- me <- stack_users(4168169, "answers", num_pages = 10, pagesize = 100)
  answers <- stackr::stack_users(user, "answers", num_pages=10, pagesize=100)
  
  Sys.setlocale("LC_ALL", "C")
  
  # timelineDF <- twitteR::twListToDF(timeline)  
  
  answers$date <- as.Date(answers$creation_date)

  answers_agg <- answers %>% group_by(date) %>% summarise(nAnswers=n(), nAccepted=sum(is_accepted)) %>% merge(data.frame(date=seq(min(answers$date),max(answers$date),"days")), all=TRUE)
  answers_agg[is.na(answers_agg)] <- 0
  
  if (min(answers_agg$date) <= lubridate::today() - lubridate::years(1)) {
    ## restrict to the last year
    answers_agg <- answers_agg %>% filter(date > lubridate::today() - lubridate::years(1))
  } else {
    ## extend to the last year (e.g. 3200 limit reached or too few)
    answers_agg %<>% merge(data.frame(date=seq(lubridate::today() - lubridate::years(1), min(answers_agg$date), "days"), nAnswers=-1, nAccepted=-1), all=TRUE)
  }

  # timelineDF_agg$t.fill <- cut(timelineDF_agg$nTweets, breaks=c(0,5,10,15,20,1e5), labels=c("#eeeeee","#d6e685","#1e6823","#8cc665","#44a340"))
  # timelineDF_agg$t.fill <- cut(timelineDF_agg$nTweets, breaks=c(-1,0,1,5,10,20,1e5), right=FALSE, labels=c("#bbbbbb","#eeeeee",tolower(brewer.pal(9, "Blues")[4:7])))
  # answers_agg$t.fill <- cut(answers_agg$nAnswers, breaks=c(-1,0,1,5,10,20,1e5), right=FALSE, labels=c("#bbbbbb","#eeeeee","#9ecae1","#6baed6","#4292c6","#2171b5"))
  answers_agg$t.fill <- cut(answers_agg$nAnswers, breaks=c(-1,0,1,5,10,20,1e5), right=FALSE, labels=1:6)
  answers_agg$nAcceptedTF <- ifelse(answers_agg$nAccepted > 0, answers_agg$nAccepted, "")
      
    ## split into weeks
  answers_agg$c.week <- cut(answers_agg$date, breaks="week", start.on.monday=FALSE, labels=FALSE)
  answers_agg$c.month <- lubridate::month(answers_agg$date, abbr=TRUE, label=TRUE)
  answers_agg$c.day  <- as.integer(lubridate::wday(answers_agg$date))
  # timelineDF_agg$id <- 1:nrow(timelineDF_agg)
  
  ## unique values of month
  rl <- rle(as.character(answers_agg$c.month))
  month.pos <- answers_agg$c.week[cumsum(rl$lengths)]
  month.pos <- month.pos[-length(month.pos)]
  month.lab <- rl$values[-1]  

  gg <- ggplot(answers_agg, aes(x=c.week, y=c.day, label=nAnswers))
  gg <- gg + geom_tile(aes(fill=answers_agg$t.fill), color="white", size=0.75)
  gg <- gg + geom_text(aes(label=nAcceptedTF), size=3, col="grey20")
  # gg <- gg + scale_fill_manual(values=answers_agg$t.fill, guide=FALSE)
  # gg <- gg + scale_x_discrete(limits=range(contribsDF$c.date), breaks=month.pos, labels=month.lab)
  gg <- gg + scale_x_continuous(limits=c(0,max(answers_agg$c.week)+1), breaks=month.pos, labels=month.lab)
  # gg <- gg + scale_x_continuous(limits=c(1,54), breaks=c(1,20,30), labels=as.character(LETTERS[1:3]))
  # gg <- gg + scale_x_discrete(limits=unique(contribsDF$c.week), minor_breaks=seq(0, 1000, 1), breaks=seq(1,1000,30))
  # gg <- gg + scale_x_discrete(limits=unique(contribsDF$c.week), minor_breaks="day", breaks="month")
  # gg <- gg + geom_text()
  # gg <- gg + scale_y_discrete(limits=c(1,7))
  gg <- gg + scale_y_reverse(breaks=seq(1,7,1), labels=c("","M","","W","","F",""))
  # gg <- gg + theme_minimal()
  # gg <- gg + theme(panel.grid.major=element_blank(),
  #                  panel.grid.minor=element_blank())
  gg <- gg + theme_github()
  gg <- gg + scale_fill_social("StackOverflow")
  # gg <- gg + theme(axis.text.x=element_text(vjust=0.5))
  # gg <- gg + labs(x="", y="", title="Past 12 months on StackOverflow", subtitle=paste0("@",user), caption="http://github.com/jonocarroll/buttRfly")
  gg <- gg + labs(x="", y="", title="Past 12 months on StackOverflow", subtitle=paste0("@",user))
  gg <- gg + coord_fixed(ratio=1)
  print(gg)
  
  return(gg)
    
}