#' @title Chart StackOverflow Answer Activity
#'   
#' @details As per the API 
#'   \url{https://api.stackexchange.com/docs/answers-on-users}, returns a
#'   \code{data.frame} of columns described in
#'   \url{https://api.stackexchange.com/docs/types/answer} for the requested \code{user}.
#'   
#'   This function utilises both \code{\link[githubtools]{theme_github}} and 
#'   \code{\link[githubtools]{scale_fill_social}} and is subject to updates to 
#'   those functions.
#'   
#' @param user StackOverflow user id
#' @param addAccepted logical. Should the number of accepeted answers be 
#'   overlayed?
#' @param showPlot logical. Should the plot be displayed? Otherwise 
#'   \code{ggplot2} object is just returned.
#' @param max.pages how many pages of results (100 per page) should be 
#'   traversed?
#'   
#' @return a list containing the ggplot object and data object:
#'   
#'   list(plot = <ggplot2 object>, data = <data.frame>)
#'   
#' @importFrom stackr stack_users
#' @import dplyr
#' @importFrom githubtools prepare_for_github_chart create_github_chart
#'   
#' @export
#' 
#' @examples
#' \dontrun{
#' library(butteRfly)
#' get_stackoverflow_chart(user = 4168169) # @Jonathan Carroll
#' get_stackoverflow_chart(user = 496803)  # @thelatemail }
get_stackoverflow_chart <- function(user = NULL, addAccepted = TRUE, showPlot = TRUE, max.pages = 10) {
  
  if (is.null(user) | is.na(as.integer(user))) stop("Requires a valid user ID to search for.")

  ## ensure that returned results can be processed.  
  Sys.setlocale("LC_ALL", "C")
  
  answers  <- stackr::stack_users(user, "answers", num_pages = max.pages, pagesize = 100)
  username <- unique(answers$owner_display_name)
  
  answers$date <- as.Date(answers$creation_date)
  
  answers_agg <- answers %>% group_by(date) %>% 
    summarise(nAnswers = n(), nAccepted = sum(is_accepted)) %>% 
    merge(data.frame(date = seq(min(answers$date), max(answers$date), "days")), all = TRUE)
  answers_agg[is.na(answers_agg)] <- 0
  
  if (addAccepted) {
  gh_data <- githubtools::prepare_for_github_chart(data_agg      = answers_agg, 
                                      primaryData   = "nAnswers", 
                                      secondaryData = "nAccepted")
  } else {
  gh_data <- githubtools::prepare_for_github_chart(data_agg    = answers_agg, 
                                      primaryData = "nAnswers")
  }

  gg <- githubtools::create_github_chart(gh_data, user, network = "StackOverflow")
  
  gg <- gg + labs(title = "Past 12 months on StackOverflow", 
                  subtitle = paste0(user, "; @", username))
  
  if (showPlot) print(gg)
  
  ret <- list(plot = gg, data = gh_data)
  
  return(invisible(ret))
  
}
