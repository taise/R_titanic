# for Titanic of the kaggle
#train$splitedName <- strsplit(as.character(train$Name), split=",[ ]+|\\.[ ]+")
categolizedName <- function(df) {
  df$categolizedName <- factor(gsub(".+, |\\. .+", "", as.character(df$Name)))
  df$Name = NULL
  return(df)
}



