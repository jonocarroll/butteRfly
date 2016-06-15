#' @title Collate Social Network Charts
#'   
#' @details Collates social network contributions with GitHub-style tile charts.
#'   Values of contributions (number of commits to GitHub, number of Tweets,
#'   number of accepted answers to StackOverflow) are overlayed if requested.
#'   
#'   \if{html}{\figure{demo.png}{options: width=700}} 
#'   \if{latex}{\figure{demo.png}{options: width=0.5in}}
#'   
#'   This function utilises both \code{\link[githubtools]{theme_github}} and
#'   \code{\link[githubtools]{scale_fill_social}} and is subject to updates to
#'   those functions. 
#'   
#' @param user vector of social network IDs to collate
#' @param additionalInfo vector of logical. Should additional info (nCommits,
#'   nTweets, nAnswers) be overlayed?
#' @param socials vector of networks to collate.
#'   
#' @return a grid object of collated charts.
#'   
#' @importFrom gridExtra grid.arrange
#'   
#' @export
#' 
#' @examples
#' \dontrun{
#' collate_socials(user = c("jonocarroll", "carroll_jono", 4168169), 
#'                 socials = c("GitHub", "Twitter", "StackOverflow"))} 
collate_socials <- function(user = NULL, additionalInfo = NULL, socials = c("GitHub", "Twitter", "StackOverflow")) {
  
  use_social <- match.arg(arg        = socials, 
                          choices    = c("GitHub", "Twitter", "StackOverflow"), 
                          several.ok = TRUE)
  
  charts_to_build <- vector("list", length = length(use_social))
  
  if (is.null(user) | is.null(socials) | length(user) != length(socials)) {
    stop("Requires a vector of user IDs the same length as 'socials'")
  } 
  if (!is.null(user) & !is.null(socials)) {
    stopifnot(length(user) == length(socials))
  }
  if (!is.null(additionalInfo)) {
    stopifnot(length(additionalInfo) == length(socials)) 
  } else {
    additionalInfo <- rep(TRUE, length(socials))
  }
  
  i <- 1
  if ("GitHub" %in% use_social) {
    resp_no <- which("GitHub" == socials)
    charts_to_build[[i]] <- get_github_chart(user = user[resp_no], addCommits = additionalInfo[resp_no], showPlot = FALSE)$plot
    i <- i + 1
  }
  if ("Twitter" %in% use_social) {
    resp_no <- which("Twitter" == socials)
    charts_to_build[[i]] <- get_twitter_chart(user = user[resp_no], addNumbers = additionalInfo[resp_no], showPlot = FALSE)$plot
    i <- i + 1
  }
  if ("StackOverflow" %in% use_social) {
    resp_no <- which("StackOverflow" == socials)
    charts_to_build[[i]] <- get_stackoverflow_chart(user = user[resp_no], addAccepted = additionalInfo[resp_no], showPlot = FALSE)$plot
    i <- i + 1
  }
 
  allgg <- do.call("grid.arrange", charts_to_build)
  
  print(allgg)
  
  return(allgg)
  
}