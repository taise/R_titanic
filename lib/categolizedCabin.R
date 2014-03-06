# for Titanic of the kaggle
categolizeCabin <- function(df) {
  nRow <- nrow(df)
  for(i in c(1:nRow)) {
    df[i, "Cabin_factor"] <- ifelse(df[i, "Cabin"] == "" ,
                                    switch(df[i, "Pclass"],"X", "Y", "Z"),
                                    substr(df[i, "Cabin"] , 1 ,1)
    )
  }
  df$Cabin_factor <- factor(df$Cabin_factor)
  df$Cabin = NULL
  return(df)
}