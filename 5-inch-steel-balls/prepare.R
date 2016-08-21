library(dplyr)
library(readr)

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

data_in <- read_csv("/mnt/raw/csvs/train.csv")
#data_in <- read_csv("/mnt/raw/csvs/2016-02.csv")

# Shim to allow joining multiple files together (no longer required)
data <- data_in

# Mark the active spew events
data$spew_active <- data$`3311WI671.PV` > 300
data$spew_active[is.na(data$`3311WI671.PV`)] <- FALSE

# Let's allow for a rest following a spew
downtime_samples <- 20*12
chundex <- which(data$spew_active)
data$spew_downtime <- FALSE
for (i in chundex) {
  data$spew_downtime[i:(i+downtime_samples)] <- TRUE
}
data$off <- data$`3311WI671.PV` < 5
data$off[is.na(data$`3311WI671.PV`)] <- TRUE
data$eligible <- !data$spew_active & !data$spew_downtime & !data$off

warning_samples <- 5*12
warning_window <- 5*12
eligibility_window <- warning_samples + warning_window
data$spew_anchor <- FALSE
data$spew_window <- FALSE
for (i in chundex) {
  data$spew_anchor[i] <- all(data$eligible[(i-warning_samples):(i-1)])
  data$spew_window[(i-warning_samples):(i-1)] <- all(data$eligible[(i-warning_samples):(i-1)])
}

# Iterate through a random sample until enough normal anchors are found
data$normal_anchor <- FALSE
candidates <- sample(1:nrow(data),nrow(data))
i = 1
repeat{
  idx <- candidates[i]
  normal_bool <- 
    !data$spew_active[idx] & 
    !data$spew_downtime[idx] & 
    !data$off[idx] & 
    !data$spew_anchor[idx] & 
    !data$spew_window[idx] &
    idx > eligibility_window
  data$normal_anchor[idx] <- normal_bool
  i <- i+1
  if(sum(data$normal_anchor) >= sum(data$spew_anchor)){
    break
  }
}

# Build out the ML table
# Start with 1 spew row
anchor <- which(data$spew_anchor)[1]
range  <- (anchor-eligibility_window):(anchor-warning_window)
df.feature <- data[range,c(1:120)]
spew_train <- data.frame(t(frame_to_vector(df.feature)))
# Add the other rows
for (i in 2:sum(data$spew_anchor)) {
  anchor <- which(data$spew_anchor)[i]
  range  <- (anchor-eligibility_window):(anchor-warning_window)
  df.feature <- data[range,c(1:120)]
  spew_row <- data.frame(t(frame_to_vector(df.feature)))
  spew_train <- rbind(spew_train, spew_row)
}
spew_train$LABEL <- "VOMIT"

# Start with 1 normal row
anchor <- which(data$normal_anchor)[1]
range  <- (anchor-eligibility_window):(anchor-warning_window)
df.feature <- data[range,c(1:120)]
normal_train <- data.frame(t(frame_to_vector(df.feature)))
# Add the other rows
for (i in 2:sum(data$normal_anchor)) {
  anchor <- which(data$normal_anchor)[i]
  range  <- (anchor-eligibility_window):(anchor-warning_window)
  df.feature <- data[range,c(1:120)]
  normal_row <- data.frame(t(frame_to_vector(df.feature)))
  normal_train <- rbind(normal_train, normal_row)
}
normal_train$LABEL <- "NORMAL"

training <- rbind(spew_train, normal_train)
training$LABEL <- as.factor(training$LABEL)
training <- training[sample(1:nrow(training),nrow(training)),]


