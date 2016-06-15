#' @title Chart GitHub User Activity
#'   
#' @details As per the API 
#'   \url{https://help.github.com/articles/why-are-my-contributions-not-showing-up-on-my-profile/},
#'    returns a \code{data.frame} of columns containing data obtained from the 
#'   requested \code{user}'s contributions chart, typically found at
#'   \url{https://github.com/users/<USER>/contributions}.
#'   
#'   This function utilises both \code{\link[githubtools]{theme_github}} and 
#'   \code{\link[githubtools]{scale_fill_social}} and is subject to updates to 
#'   those functions.
#'   
#' @param user GitHub user (handle)
#' @param addCommits logical. Should the number of commits be overlayed?
#' @param showPlot logical. Should the plot be displayed? Otherwise 
#'   \code{ggplot2} object is just returned.
#'   
#' @return a list containing the ggplot object and data object:
#'   
#'   list(plot = <ggplot2 object>, data = <data.frame>)
#' 
#' @import httr
#' @import magrittr
#' @import dplyr
#' @importFrom lubridate today years month wday
#' @import ggplot2
#' @import XML
#' @importFrom xml2 read_xml
#' @importFrom githubtools prepare_for_github_chart create_github_chart
#'   
#' @export
#' 
#' @examples
#' \dontrun{
#' library(butteRfly)
#' get_github_chart("jonocarroll") # Jonathan Carroll
#' get_github_chart("hadley")      # Hadley Wickham}
get_github_chart <- function(user, addCommits = TRUE, showPlot = TRUE) {
  
  gh_url <- paste0("https://github.com/users/",user,"/contributions")
  
  contrib_data <- XML::xmlParse(xml2::read_xml(gh_url))

  c.date <- as.Date(XML::xpathSApply(contrib_data, '//g/rect', XML::xmlGetAttr, "data-date"), origin = "01-01-1970")
  c.data <- as.integer(XML::xpathSApply(contrib_data, '//g/rect', XML::xmlGetAttr, "data-count"))
  c.fill <- XML::xpathSApply(contrib_data, '//g/rect', XML::xmlGetAttr, "fill")

  contribsDF <- data.frame(date = c.date, c.data, c.fill, stringsAsFactors = FALSE)
  
  if (addCommits) {
    gh_data <- githubtools::prepare_for_github_chart(data_agg      = contribsDF, 
                                        primaryData   = "c.data", 
                                        secondaryData = "c.data")
  } else {
    gh_data <- githubtools::prepare_for_github_chart(data_agg    = contribsDF, 
                                        primaryData = "c.data")
  }
  
  gg <- githubtools::create_github_chart(gh_data, user, network = "GitHub")
  
  gg <- gg + labs(title = "Past 12 months on GitHub", 
                  subtitle = paste0("@", user))
  
  if (showPlot) print(gg)

  ret <- list(plot = gg, data = gh_data)
  
  return(invisible(ret))
  
}