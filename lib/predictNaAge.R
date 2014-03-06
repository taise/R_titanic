# for Titanic of the kaggle
predictNaAge <- function(df) {
  age_df <- subset(df, !is.na(Age))
  age_df.tree <- rpart(Age ~., data=age_df, cp=-1, maxdepth=4)
  nRow <- nrow(df)
  for(i in c(1:nRow)) {
    df[i, "Age"] <- ifelse(is.na(df[i, "Age"]) ,
                           predict(age_df.tree, newdata=df[i,]),
                           df[i, "Age"])
  }
  return(df)
}
