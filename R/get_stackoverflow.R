#' Retrieve Answers Data for a Stack Overflow User and Chart it
#'
#' @param user StackOverflow user id
#'
#' @return a \code{ggplot} object of the user's answers (and whether they were accepted)
#'
#' @importFrom stackr stack_users
#' @import dplyr
#'  
#' @export
#'
get_stackoverflow <- function(user, addAccepted=TRUE, showPlot=TRUE) {
  
  Sys.setlocale("LC_ALL", "C")
  
  answers <- stackr::stack_users(user, "answers", num_pages = 10, pagesize = 100)
  
  answers$date <- as.Date(answers$creation_date)
  
  answers_agg <- answers %>% group_by(date) %>% 
    summarise(nAnswers = n(), nAccepted = sum(is_accepted)) %>% 
    merge(data.frame(date = seq(min(answers$date), max(answers$date), "days")), all = TRUE)
  answers_agg[is.na(answers_agg)] <- 0
  
  if (addAccepted) {
  gh_data <- prepare_for_github_chart(data_agg = answers_agg, 
                                      primaryData = "nAnswers", 
                                      secondaryData = "nAccepted")
  } else {
  gh_data <- prepare_for_github_chart(data_agg = answers_agg, 
                                      primaryData = "nAnswers")
  }

  gg <- create_github_chart(gh_data, user, network = "StackOverflow")
  
  if (showPlot) print(gg)
  
  return(gg)
  
}
