cat('PLUMBER Async Scripts')
cat('Load latest request data')
pre_data <- readRDS('/tmp/om121k.rds')
# readRDS('/tmp/om121k.rds') -> pre_data

cat('Set working directory')
setwd(pre_data$working_dir)

cat('Set env vars')
eval(parse(text = pre_data$env_vars))

cat('Load all functions')
base::source(file = 'functions.R')

cat('Perform Analysis')

#eval(parse(text = pre_data$func)) -> slack_message

slack_message <- tryCatch({
  slack_message <- eval(parse(text = pre_data$func))
}, warning = function(w) {
  # nothing
}, error = function(e) {

  message <- list(
    # response type - ephemeral indicates the response will only be seen by the
    # user who invoked the slash command as opposed to the entire channel
    response_type = "in_channel",
    text = paste0('**ERROR:** Sorry, this piece of crap did not work: ', e)

  )
  return(message)
  # nothing
})

cat('Post message response')
(body <- jsonlite::toJSON(slack_message, pretty = T, auto_unbox = T))
POST(pre_data$response_url, body = body, encode = 'json')

cat('Remove tmp RDS file that was created')
#file.remove('/tmp/om121k.rds')
