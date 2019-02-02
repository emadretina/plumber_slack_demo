# Standup app /plumber_demo
base::source(file = 'functions.R')
#source(file = 'set_cron.R')

#* @apiTitle Customer Service Slack Application API
#* @apiDescription API that interfaces with Slack slash command /cs

#* Parse the incoming request and route it to the appropriate endpoint
#* @filter route-endpoint
function(req, text = "") {

  # Identify endpoint
  split_text <- urltools::url_decode(text) %>%
    strsplit(" ") %>%
    unlist()

  if (length(split_text) >= 1) {
    endpoint <- split_text[[1]]

    # Modify request with updated endpoint
    req$PATH_INFO <- paste0("/", endpoint)

    # Modify request with remaining commands from text
    req$ARGS <- split_text[-1] %>%
      paste0(collapse = " ")
  }

  if (req$PATH_INFO == "/" & slack_auth(req) == "200") {

    # If no endpoint is provided (PATH_INFO is just "/") then forward to /help
    if (is.null(req$args$payload)) {
      req$PATH_INFO <- "/help"
    } else {
      args <- jsonlite::fromJSON(req$args$payload)
      if (args$type == "interactive_message" &&
          args$callback_id == 'jokequestion') {
        req$PATH_INFO <- "/jokeanswer"
      }
    }

  }

  # Forward request
  forward()

}

#* Log information about the incoming request
#* @filter logger
function(req){
  log_data <- paste0(as.character(Sys.time()), "- PLUMBER -",
                     req$REQUEST_METHOD, req$PATH_INFO, "-",
                     req$HTTP_USER_AGENT, "@", req$REMOTE_ADDR, '-', req$postBody, "\n")
  cat(log_data)
  # Search for these logs in /var/log/syslog using this command
  # # grep -rnw '/var/log/' -e 'PLUMBER'
  # Forward request
  forward()
}

# unboxedJSON is used b/c that is what Slack expects from the API
#* Return a message containing status details about the customer
#* @serializer unboxedJSON
#* @post /verbatim
function(req, res) {
  # Authenticate request
  status <- slack_auth(req)
  if (status == "401") {
    res$status <- 401
    return(
      list(text = "Error: Invalid request.")
    )
  }

  list(
    # response type - ephemeral indicates the response will only be seen by the
    # user who invoked the slash command as opposed to the entire channel
    response_type = "in_channel",
    text = paste0(as.character(Sys.time()), "-",
                  req$REQUEST_METHOD, req$PATH_INFO, "-",
                  req$HTTP_USER_AGENT, "@", req$REMOTE_ADDR, "\n",
                  req$postBody)
  )
}

#* Help for /cs command
#* @serializer unboxedJSON
#* @post /help
function(req, res) {
  # Authorize request
  status <- slack_auth(req)
  if (status == "401") {
    res$status <- 401
    return(
      list(
        text = "Error: Invalid request."
      )
    )
  }

  get_help_data()
}


# unboxedJSON is used b/c that is what Slack expects from the API
#* Return a message containing company holidays for Retina
#* @serializer unboxedJSON
#* @post /holidays
function(req, res) {
  # Authenticate request
  status <- slack_auth(req)
  if (status == "401") {
    res$status <- 401
    return(
      list(text = "Error: Invalid request.")
    )
  }

  list_holidays()
}

# unboxedJSON is used b/c that is what Slack expects from the API
#* Return a message containing status details about the customer
#* @serializer unboxedJSON
#* @post /joke
function(req, res) {
  # Authenticate request
  status <- slack_auth(req)
  if (status == "401") {
    res$status <- 401
    return(
      list(text = "Error: Invalid request.")
    )
  }

  gen_joke_question(jokes)
}


# unboxedJSON is used b/c that is what Slack expects from the API
#* Return a message containing status details about the customer
#* @serializer unboxedJSON
#* @post /jokeanswer
function(req, res) {
  # Authenticate request
  status <- slack_auth(req)
  if (status == "401") {
    res$status <- 401
    return(
      list(text = "Error: Invalid request.")
    )
  }

  args <- jsonlite::fromJSON(req$args$payload)
  idx <- as.numeric(args$original_message$attachments$actions[[1]]$name)

  list(
    # response type - ephemeral indicates the response will only be seen by the
    # user who invoked the slash command as opposed to the entire channel
    response_type = "in_channel",
    replace_original = "true",
    # attachments is expected to be an array, hence the list within a list
    attachments = list(
      list(
        text = paste0(jokes$Question[idx], '\n',
                      jokes$Answer[idx]),
        callback_id = 'jokequestion1',
        color =  "#3AA3E3",
        fallback = paste(jokes$Question[idx], jokes$Answer[idx])
      )
    )
  )
}


# unboxedJSON is used b/c that is what Slack expects from the API
#* Return a message containing response to trello commands
#* @serializer unboxedJSON
#* @post /trello
function(req, res) {
  # Authenticate request
  status <- slack_auth(req)
  if (status == "401") {
    res$status <- 401
    return(
      list(text = "Error: Invalid request.")
    )
  }

  if (test_mode) {
    aws.s3::s3saveRDS(req, object = 'last_sub_req.rds', bucket = 'retina-emad')
    aws.s3::s3saveRDS(res, object = 'last_res.rds', bucket = 'retina-emad')
  }

  # Build response
  #genrate_kanban_summary(base_url, members_map)

  # Kick off async script
  kickoff_async_get_kanban(req$args$response_url)

  list(
    # response type - ephemeral indicates the response will only be seen by the
    # user who invoked the slash command as opposed to the entire channel
    response_type = "in_channel",
    text = paste0('Got it <@', req$args$user_id,
                  '> let me reach out to the Trello gods and get this data for you. Give me a min...')

  )

}

#* Plot trello status data
#* @png (width = 450, height = 200)
#* @get /trello_kanban/<setkey>
function(getkey, req, res, setkey) {
  # cat('PLUMBER', getkey)
  # setkey <- Sys.getenv('setkey')
  # if (setkey != getkey) {
  #   res$status <- 401
  #   return(
  #     list(text = "Error: Invalid request.")
  #   )
  # }

  # Load plots from RDS on AWS

  # cat('PLUMBER - Reading Trello Token')
  # orged_card_data <- aws.s3::s3readRDS(object = 'orged_card_data.rds',
  #                                   bucket = 'retina-emad')
  #
  # #Validate request came from this script
  #
  # # Plot data
  cat('PLUMBER - Kicking off function')
  kanban_plot <- load_strategy_board_plot(members_map)

  cat('PLUMBER - Printing plot')
  print(kanban_plot)
}


# unboxedJSON is used b/c that is what Slack expects from the API
#* Return a message containing response to facebook campaign status
#* @serializer unboxedJSON
#* @post /async
function(req, res) {
  # Authenticate request
  status <- slack_auth(req)
  if (status == "401") {
    res$status <- 401
    return(
      list(text = "Error: Invalid request.")
    )
  }

  if (test_mode) {
    aws.s3::s3saveRDS(req, object = 'last_sub_req.rds', bucket = 'retina-emad')
    aws.s3::s3saveRDS(res, object = 'last_res.rds', bucket = 'retina-emad')
  }

  # Kick off async script
  kickoff_async_data_request(req$args$response_url)

  gifs <- c('https://media.giphy.com/media/o0vwzuFwCGAFO/giphy.gif',
            'https://media.giphy.com/media/YAnpMSHcurJVS/giphy.gif',
            'https://media.giphy.com/media/hOzfvZynn9AK4/giphy.gif',
            'https://media.giphy.com/media/l3E6BG56dhjuawAX6/giphy.gif',
            'https://media.giphy.com/media/26uf7rl7j6RVibDz2/giphy.gif',
            'https://media.giphy.com/media/xT8qBdT8h6Xfg9HP7a/giphy.gif',
            'https://media.giphy.com/media/hpEzeWC2Pbrag/giphy.gif')

  gif <- sample(gifs,1)

  list(
    # response type - ephemeral indicates the response will only be seen by the
    # user who invoked the slash command as opposed to the entire channel
    response_type = "in_channel",
    text = paste0('Alright <@', req$args$user_id,
                  '>, got your request! let me get back to you in a few minutes with that request...'),
    attachments = list(
      list(
        image_url = gif
      )
    )

  )

}

