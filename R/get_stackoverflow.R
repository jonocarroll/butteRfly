#' Retrieve Answers Data for a Stack Overflow User and Chart it
#'
#' @param user StackOverflow user id
#'
#' @return a \code{ggplot} object of the user's answers (and whether they were accepted)
#'
#' @importFrom stackr stack_users
#' @import magrittr
#' @import dplyr
#' @importFrom lubridate today years month wday 
#' @import ggplot2
#'  
#' @export
#'
get_stackoverflow <- function(user) {
  
  Sys.setlocale("LC_ALL", "C")
  
  answers <- stackr::stack_users(user, "answers", num_pages = 10, pagesize = 100)
  
  answers$date <- as.Date(answers$creation_date)
  
  answers_agg <- answers %>% group_by(date) %>% 
    summarise(nAnswers = n(), nAccepted = sum(is_accepted)) %>% 
    merge(data.frame(date = seq(min(answers$date), max(answers$date), "days")), all = TRUE)
  answers_agg[is.na(answers_agg)] <- 0
  
  if (min(answers_agg$date) <= lubridate::today() - lubridate::years(1)) {
    ## restrict to the last year
    answers_agg <- answers_agg %>% filter(date > lubridate::today() - lubridate::years(1))
  } else {
    ## extend to the last year (e.g. 3200 limit reached or too few)
    answers_agg %<>% merge(data.frame(date = seq(lubridate::today() - lubridate::years(1), 
                                                 min(answers_agg$date), "days"), 
                                      nAnswers = -1, nAccepted = -1), 
                           all = TRUE)
  }
  
  answers_agg$nAcceptedTF <- ifelse(answers_agg$nAccepted > 0, answers_agg$nAccepted, "")
  
  ### THIS CAN BE EXTRACTED !?! ###
  answers_agg$t.fill      <- cut(answers_agg$nAnswers, breaks = c(-1,0,1,5,10,20,1e5), right = FALSE, labels = 1:6)
  
  ## split into weeks
  answers_agg$c.week  <- cut(answers_agg$date, breaks = "week", start.on.monday = FALSE, labels = FALSE)
  answers_agg$c.month <- lubridate::month(answers_agg$date, abbr = TRUE, label = TRUE)
  answers_agg$c.day   <- as.integer(lubridate::wday(answers_agg$date))

  ## unique values of month
  rl        <- rle(as.character(answers_agg$c.month))
  month.pos <- answers_agg$c.week[cumsum(rl$lengths)]
  month.pos <- month.pos[-length(month.pos)]
  month.lab <- rl$values[-1]  
  
  gg <- ggplot(answers_agg, aes(x = c.week, y = c.day, label = nAnswers))
  gg <- gg + geom_tile(aes(fill = answers_agg$t.fill), color = "white", size = 0.75)
  gg <- gg + geom_text(aes(label = nAcceptedTF), size = 3, col = "grey20")
  gg <- gg + scale_x_continuous(limits = c(0, max(answers_agg$c.week) + 1), breaks = month.pos, labels = month.lab)
  gg <- gg + scale_y_reverse(breaks = seq(1,7,1), labels = c("","M","","W","","F",""))
  gg <- gg + theme_github()
  gg <- gg + scale_fill_social("StackOverflow")
  gg <- gg + labs(title = "Past 12 months on StackOverflow", subtitle = paste0("@", user), 
                  x = "", y = "")
  gg <- gg + coord_fixed(ratio = 1)
  print(gg)

  ### END | THIS CAN BE EXTRACTED !?! ###
    
  return(gg)
  
}