

#setup slack bot for notifications
setup_slack <- function(user_name = "rStudio",
                        channel_name = "#code_status",
                        token_path = "../../api_tokens/api_tokens - list.csv" ){
  
  #read the file containing API tokens
  token_df = read_csv(token_path)
  
  #select token for slack bot
  SLACK_BOT_TOKEN = token_df %>% filter(service_name == "slack_bot") %>% select(api_token)
  
  #slack - select channel and assign token
  slackrSetup(channel= channel_name, username = user_name, api_token=SLACK_BOT_TOKEN)
  
}
