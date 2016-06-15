#' Perform Twitter OAuth setup.
#'
##' #@param config_file location of the configuration file, default ~/.twitter
#'
##' #@param echo if TRUE print the credentials read from the file to the console.
#'
#' @return NULL (used for side-effect)
#'
#' @importFrom twitteR setup_twitter_oauth
#' @importFrom jsonlite toJSON
#'
#' @export
#'
#' @examples
#' \dontrun{
#' twitter_setup()
#' }
# githuburlcheckr_setup <- function(config_file="~/.githuburlcheckr", echo=FALSE) {
twitter_setup <- function(config_file="~/.twitter", echo=FALSE) {

  if(file.exists(config_file)) {
    config <- read.dcf(config_file, fields=c("TWITTER_KEY", "TWITTER_SECRET", "TWITTER_ACCESS_TOKEN", "TWITTER_ACCESS_SECRET"))
    Sys.setenv(TWITTER_KEY = config[, "TWITTER_KEY"])
    Sys.setenv(TWITTER_SECRET = config[, "TWITTER_SECRET"])
    Sys.setenv(TWITTER_ACCESS_TOKEN = config[, "TWITTER_ACCESS_TOKEN"])
    Sys.setenv(TWITTER_ACCESS_SECRET = config[, "TWITTER_ACCESS_SECRET"])

    if(echo) {
      print(jsonlite::toJSON(as.list(Sys.getenv(c("TWITTER_KEY",
                                                  "TWITTER_SECRET",
                                                  "TWITTER_ACCESS_TOKEN",
                                                  "TWITTER_ACCESS_SECRET"))), pretty = TRUE))
    }

    twitteR::setup_twitter_oauth(consumer_key=Sys.getenv("TWITTER_KEY"),
                                 consumer_secret=Sys.getenv("TWITTER_SECRET"),
                                 access_token=Sys.getenv("TWITTER_ACCESS_TOKEN"),
                                 access_secret=Sys.getenv("TWITTER_ACCESS_SECRET"))

  } else {
    warning("PLEASE ADD A ~/.twitter FILE WITH CONFIGURATION\n
            TWITTER_KEY: <YOURKEY>\n
            TWITTER_SECRET: <YOURSECRET>\n
            TWITTER_ACCESS_TOKEN: <YOURACCESSTOKEN>\n
            TWITTER_ACCESS_SECRET: <YOURACCESSSECRET>\n")

  }

  return(invisible(NULL))

}
