#Load libraries & Data
library(plumber)
library(tidyverse)
library(httr)

# Load data
# setwd('dev//')
holidays <- read_csv('holidays.csv')
jokes <- readRDS('jokes.rds')

# Save env info
preflight_cmd <- paste0("Sys.setenv('SLACK_SIGNING_SECRET' = '",
                        Sys.getenv('SLACK_SIGNING_SECRET'), "'); ")
working_dir <- getwd()
env_vars <- preflight_cmd
payload <- list(working_dir = working_dir,
                env_vars = env_vars)
saveRDS(payload, '/tmp/curkeys.rds')

# Base URL for API requests
base_url <- config::get("base_url")

# Get Rpath in current install
Rpath <- Find(file.exists, c(commandArgs()[[1]], file.path(R.home("bin"), commandArgs()[[1]]),
                             file.path(R.home("bin"), "R"), file.path(R.home("bin"), "Rscript.exe")))

# Get current path
curpath <- getwd()

# Slack authorization
slack_auth <- function(req) {
  # Verify request came from Slack ----
  if (is.null(req$HTTP_X_SLACK_REQUEST_TIMESTAMP)) {
    return("401")
  }

  base_string <- paste(
    "v0",
    req$HTTP_X_SLACK_REQUEST_TIMESTAMP,
    req$postBody,
    sep = ":"
  )

  # Slack Signing secret is available as environment variable
  # SLACK_SIGNING_SECRET
  computed_request_signature <- paste0(
    "v0=",
    openssl::sha256(base_string, Sys.getenv("SLACK_SIGNING_SECRET"))
  )
  cat('SlackSig: ', Sys.getenv("SLACK_SIGNING_SECRET"))
  cat('Computed:', computed_request_signature, '\n')
  cat('Provided: ',  req$HTTP_X_SLACK_SIGNATURE, '\n')

  # If the computed request signature doesn't match the signature provided in the
  # request, return an error
  if (!identical(req$HTTP_X_SLACK_SIGNATURE, computed_request_signature)) {
    "401"
  } else {
    "200"
  }
}

# Reddit Data
reddit_data <- function(){
  reddit_df <- NULL
  try(reddit_df <- RedditExtractoR::get_reddit(subreddit = 'ProgrammerHumor',
                                               cn_threshold = 10,
                                               page_threshold = 1,
                                               sort_by = 'new',
                                               wait_time = 0))
  if (!is.null(reddit_df)) {
    reddit_post <- reddit_df %>%
      filter(grepl('imgur', domain)) %>%
      sample_n(1)
  }
  return(reddit_post)
}

get_help_data <- function(){
  list(
    # response type - ephemeral indicates the response will only be seen by the
    # user who invoked the slash command as opposed to the entire channel
    response_type = "in_channel",
    # attachments is expected to be an array, hence the list within a list
    attachments = list(
      list(
        image_url = "https://res.cloudinary.com/retina-ai/image/upload/v1539441427/tina_banner_cujdvq.png"
      ),
      list(
        text = 'Hi, I am Tina. I can probalby do a few things around here but not everything.',
        fallback = "/tina help",
        fields = list(
          list(
            title = "/cs joke",
            value = "I'll tell you a random reddit joke. *Warning:* These have not been filtered.",
            short = F
          ),
          list(
            title = "/cs holidays",
            value = "List out company holidays.",
            short = F
          )
        )
      )
    )
  )
}


list_holidays <- function(retina_holidays. = retina_holidays) {

  remaining_holidays <- holidays %>%
    mutate(date_dt = as.Date(date, '%a, %B %d, %Y')) %>%
    filter(date_dt > now())

  hlist <- NULL
  y <- length(remaining_holidays$holiday)

  for (i in c(1:y)) {
    hlist <- c(hlist,
               list( list(
                 title = remaining_holidays$holiday[i],
                 value = remaining_holidays$date[i],
                 short = T
               )))
  }


  return(list(
    # response type - ephemeral indicates the response will only be seen by the
    # user who invoked the slash command as opposed to the entire channel
    response_type = "in_channel",
    # attachments is expected to be an array, hence the list within a list
    attachments = list(
      list(
        title = "Holidays at Acme",
        text = 'Below are the holidays at Amce These should be the same as Gusto.',
        fallback = "/tina help",
        fields = hlist
      )
    )
  ))

}

gen_joke_question <- function(jokes = jokes) {
  idx <- sample(c(1:nrow(jokes)), 1)

  list(
    # response type - ephemeral indicates the response will only be seen by the
    # user who invoked the slash command as opposed to the entire channel
    response_type = "in_channel",
    replace_original = "false",
    # attachments is expected to be an array, hence the list within a list
    attachments = list(
      list(
        text = jokes$Question[idx],
        callback_id = 'jokequestion',
        fallback = paste(jokes$Question[idx], jokes$Answer[idx]),
        color =  "#3AA3E3",
        attachment_type = 'default',
        actions = list(
          list(
            name = idx,
            text = "Answer",
            type = "button",
            value = "answer1" #jokes$Answer[idx]
          )
        )
      )
    )
  )
}

#kickoff kanban
kickoff_async_get_kanban <- function(response_url){
  preflight_cmd <- paste0("Sys.setenv('SLACK_SIGNING_SECRET' = '",
                          Sys.getenv('SLACK_SIGNING_SECRET'), "'); ")
  working_dir <- getwd()
  env_vars <- preflight_cmd
  payload <- list(working_dir = working_dir,
                  env_vars = env_vars,
                  response_url = response_url,
                  func = 'genrate_kanban_summary()')
  saveRDS(payload, '/tmp/om121k.rds')

  async_cmd <- paste0("Rscript -e \"source('", working_dir, "/async_scripts.R')\"")

  system(async_cmd, wait = F)
}

#kickoff facebook request
kickoff_async_get_facebook <- function(response_url){
  preflight_cmd <- paste0("Sys.setenv('SLACK_SIGNING_SECRET' = '",
                          Sys.getenv('SLACK_SIGNING_SECRET'), "'); ",
                          "Sys.setenv('FB_ACCESS_TOKEN' = '",
                          Sys.getenv('FB_ACCESS_TOKEN'), "'); ",
                          "Sys.setenv('AWS_ACCESS_KEY_ID' = '",
                          Sys.getenv('AWS_ACCESS_KEY_ID'), "'); ",
                          "Sys.setenv('AWS_SECRET_ACCESS_KEY' = '",
                          Sys.getenv('AWS_SECRET_ACCESS_KEY'), "'); ",
                          "Sys.setenv('AWS_REGION' = '",
                          Sys.getenv('AWS_REGION'), "'); ")
  working_dir <- getwd()
  env_vars <- preflight_cmd
  payload <- list(working_dir = working_dir,
                  env_vars = env_vars,
                  response_url = response_url,
                  func = 'build_fb_slack_message()')
  saveRDS(payload, '/tmp/om121k.rds')

  async_cmd <- paste0("Rscript -e \"source('", working_dir, "/async_scripts.R')\"")

  system(async_cmd, wait = F)
}
