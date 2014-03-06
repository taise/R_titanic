# for Titanic of the kaggle
categolizedTicket <- function(df){
  df$gsub_ticket <- gsub(pattern="\\.", replacement="", df$Ticket)
  df$gsub_ticket <- gsub("/", "", df$gsub_ticket)
  df$Ticket_factor <- factor(substr(df$gsub_ticket, 1,1))
  df$Ticket = NULL
  df$gsub_ticket = NULL
  return(df)
}