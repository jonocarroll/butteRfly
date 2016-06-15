#' @title Chart Twitter Tweet Activity
#'   
#' @description This function *does* require you to be authenticated. Follow
#'   directions from \code{?twitter_setup} (construct a \code{~/.twitter} file
#'   with OAuth credentials). The search is performed using
#'   \code{\link[twitteR]{userTimeline}} which has a rate limit maximum of 3200
#'   Tweets. Allowances are made for this liimt.
#'   
#' @details As per the API 
#'   \url{https://dev.twitter.com/rest/reference/get/statuses/user_timeline}, "[sic]
#'   Native retweets of other statuses by the user is included in this total, 
#'   regardless of whether include_rts is set to false when requesting this 
#'   resource." so this is unavoidable behaviour.
#'   
#'   This function utilises both \code{\link[githubtools]{theme_github}} and
#'   \code{\link[githubtools]{scale_fill_social}} and is subject to updates to
#'   those functions.
#'   
#' @param user Twitter user (handle)
#' @param loadSaved if provided, a saved output from this function for which to
#'   repeat evaluation
#' @param addNumbers logical. Should number of Tweets be overlayed?
#' @param showPlot logical. Should the plot be displayed? Otherwise
#'   \code{ggplot2} object is just returned.
#'   
#' @return a list containing the ggplot object and data object:
#'   
#'   list(plot = <ggplot2 object>, data = <data.frame>)
#'   
#' @importFrom twitteR userTimeline twListToDF
#' @import dplyr
#'   
#' @export
#' 
#' @examples
#' \dontrun{
#' get_twitter_chart("carroll_jono")}
get_twitter_chart <- function(user = NULL, loadSaved = NULL, addNumbers = TRUE, showPlot = TRUE) {
  
  if (is.null(loadSaved)) {
    
    if (is.null(user)) stop("Requires a valid user ID to search for.")
    
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
      gh_data <- githubtools::prepare_for_github_chart(data_agg      = timelineDF_agg, 
                                          primaryData   = "nTweets", 
                                          secondaryData = "nTweets")
    } else {
      gh_data <- githubtools::prepare_for_github_chart(data_agg    = timelineDF_agg, 
                                          primaryData = "nTweets")
    }
  } else {
    gh_data <- loadSaved$data
    if (!addNumbers) gh_data$data$secondaryTF <- "" 
  }
  
  gg <- githubtools::create_github_chart(gh_data, user, network = "Twitter")
  
  gg <- gg + labs(title = "Past 12 months on Twitter", 
                  subtitle = paste0("@", user))
  
  if (showPlot) print(gg)
  
  ret <- list(plot = gg, data = gh_data)
  
  return(invisible(ret))
  
}