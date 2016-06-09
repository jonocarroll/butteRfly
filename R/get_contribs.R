#' Title
#'
#' @param user 
#'
#' @return
#' 
#' @import httr
#' 
#' @export
#'
#' @examples
get_contribs <- function(user) {
  
  gh_url <- paste0("https://github.com/users/",user,"/contributions")
  
  contrib_data <- xmlParse(read_xml(gh_url))

  c.date <- as.Date(xpathSApply(contrib_data, '//g/rect', xmlGetAttr, "data-date"), origin="01-01-1970")
  c.data <- as.integer(xpathSApply(contrib_data, '//g/rect', xmlGetAttr, "data-count"))
  c.fill <- xpathSApply(contrib_data, '//g/rect', xmlGetAttr, "fill")

  contribsDF <- data.frame(c.date, c.data, c.fill, stringsAsFactors=FALSE)
      
  ## split into weeks
  contribsDF$c.week <- cut(contribsDF$c.date, breaks="week", labels=FALSE)
  contribsDF$c.month <- month(contribsDF$c.date, abbr=TRUE, label=TRUE)
  contribsDF$c.day  <- as.integer(lubridate::wday(contribsDF$c.date))
  contribsDF$id     <- rownames(contribsDF)
  
  ## unique values of month
  rl <- rle(as.character(contribsDF$c.month))
  month.pos <- contribsDF$c.week[cumsum(rl$lengths)]
  month.pos <- month.pos[-length(month.pos)]
  month.lab <- rl$values[-1]
  # month.lab <- month.lab[-length(month.lab)]
  
  gg <- ggplot(contribsDF, aes(x=c.week, y=c.day))
  gg <- gg + geom_tile(fill=c.fill, color="white", size=0.75)
  gg <- gg + scale_fill_manual(values=c.fill, guide=FALSE)
  # gg <- gg + scale_x_discrete(limits=range(contribsDF$c.date), breaks=month.pos, labels=month.lab)
  gg <- gg + scale_x_continuous(limits=c(0,max(contribsDF$c.week)+1), breaks=month.pos, labels=month.lab)
  # gg <- gg + scale_x_continuous(limits=c(1,54), breaks=c(1,20,30), labels=as.character(LETTERS[1:3]))
  # gg <- gg + scale_x_discrete(limits=unique(contribsDF$c.week), minor_breaks=seq(0, 1000, 1), breaks=seq(1,1000,30))
  # gg <- gg + scale_x_discrete(limits=unique(contribsDF$c.week), minor_breaks="day", breaks="month")
  # gg <- gg + geom_text(data=NULL, aes(x=seq(1,54,4), y=rep(0,length(seq(1,54,4))), text=rep("a",length(seq(1,54,4)))))
  # gg <- gg + scale_y_discrete(limits=c(1,7))
  gg <- gg + scale_y_reverse(breaks=seq(1,7,1), labels=c("","M","","W","","F",""))
  gg <- gg + theme_minimal()
  gg <- gg + theme(panel.grid.major=element_blank(),
                   panel.grid.minor=element_blank())
  # gg <- gg + theme(axis.text.x=element_text(vjust=0.5))
  gg <- gg + labs(x="", y="", title="Past 1 year on GitHub", subtitle=paste0("@",user), caption="http://github.com/jonocarroll/buttRfly")
  gg <- gg + coord_fixed(ratio=1)
  print(gg)
  
  return(NULL)
  
}