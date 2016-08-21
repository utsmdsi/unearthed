library(caret)
library(randomForest)

modelTune <- readRDS("~/model.rds")

frame_to_vector <- function(frame_in) {
  height <- nrow(frame_in)
  width  <- ncol(frame_in)-1
  out_vec <- rep(NA,height*width)
  for (i in 1:height) {
    for (k in 1:width) {
      row_start <- (i-1) * width
      idx <- row_start + k
      names(out_vec)[idx] <- paste0("T",i,"-",names(frame_in)[k+1])
      out_vec[idx] <- as.numeric(frame_in[[i,k+1]])
    }
  }
  return(out_vec)
}


test_in <- read_csv("/mnt/raw/csvs/test.csv")

test <- test_in[test_in$Date > "2016-07-28" & test_in$Date < "2016-07-29",]

warning_samples <- 5*12
warning_window <- 5*12

anchor_vec <- seq(from=(warning_samples + warning_window+1), to=nrow(test), by=12)

# Build out the ML table
# Start with 1 spew row
anchor <- anchor_vec[1]
range  <- (anchor-eligibility_window):(anchor-warning_window)
df.feature <- test[range,c(1:120)]
spew_test <- data.frame(t(frame_to_vector(df.feature)))
# Add the other rows
for (i in 2:length(anchor_vec)) {
  anchor <- anchor_vec[i]
  range  <- (anchor-eligibility_window):(anchor-warning_window)
  df.feature <- test[range,c(1:120)]
  test_row <- data.frame(t(frame_to_vector(df.feature)))
  spew_test <- rbind(spew_test, test_row)
  message("Building score table for minute ", i, " of ", length(anchor_vec))
}

scoresFinal <- predict(modelTune,spew_test, na.action = na.pass, type = "prob")

scores_for_matt <- data.frame(
  Date = test$Date[anchor_vec],
  Score = scoresFinal$VOMIT
)

scores_for_matt$Date <- format.Date(scores_for_matt$Date, format="%d/%m/%Y %H:%M:%S")
write.csv(scores_for_matt, "/mnt/raw/scores_for_matt.csv", row.names = F)
