# ============================================ #
#                                              #
# Title: Newcrest challenge 2 import           #
# Author: Daniel Booth                         #
#                                              #
# ============================================ #

# ================ HEADER ================ #
# Set wd
setwd("~/Dropbox/unearthed2016/")

# Load packages
library(dplyr)
library(stringr)
library(lubridate)
library(ggplot2)

# ======================================== #

# ================ Inspecting, Reading and Cleaning the data ================ #
# Inspect data
data_path <- "data_in/raw/Surge-2015-11.csv.txt"

read_lines(data_path, n_max = 5)

# Read in the data
grinding.df <- read_csv(file = data_path,
                        col_names = TRUE,
                        col_types = "cinccnnnnnnnnnnnnnnnnccnnnnnnnnnnnnnnnnccnnnnnnnnn",
                        na = c("NULL"))

# Convert date column
grinding.df$measurement_date <- dmy_hms(str_sub(grinding.df$measurement_date, 1, 19))

# Summaries
names_order <- names(grinding.df)
str(grinding.df)
summary(grinding.df)

# Find null character fields
table(grinding.df$`3311FV380B.PV`) # All NA
table(grinding.df$`3311FV690B.PV`) # All NA
table(grinding.df$`CR002RUN`) # All NA
table(grinding.df$`CR003RUN`) # All NA
table(grinding.df$`SN2001.PV`) # All NA
table(grinding.df$`SN2002.PV`) # All NA

# = Read in rest of files = #
# Doing this manually
data_path <- "data_in/raw/Surge-2015-12.csv.txt"
data_path <- "data_in/raw/Surge-2016-01.csv.txt"
data_path <- "data_in/raw/Surge-2016-02.csv.txt"
data_path <- "data_in/raw/Surge-2016-03.csv.txt"
data_path <- "data_in/raw/Surge-2016-04.csv.txt"
data_path <- "data_in/raw/Surge-2016-05.csv.txt"
data_path <- "data_in/raw/Surge-2016-06.csv.txt"
data_path <- "data_in/raw/Surge-2016-07.csv.txt"

tmp.df <- read_csv(file = data_path,
                   col_names = TRUE,
                   col_types = "cinccnnnnnnnnnnnnnnnnccnnnnnnnnnnnnnnnnccnnnnnnnnn",
                   na = c("NULL"))

# Convert date
tmp.df$measurement_date <- dmy_hms(str_sub(tmp.df$measurement_date, 1, 19))

# Feb specific
# tmp.df <- read_csv(file = data_path,
#                    skip = 1,
#                    col_names = FALSE,
#                    col_types = "cinccnnnnnnnnnnnnnnnnccnnnnnnnnnnnnnnnnccnnnnnnnnn",
#                    na = c("NULL"))

# Feb names
# names(tmp.df) <- names_order

# tmp.df$measurement_date <- ymd_hms(str_sub(tmp.df$measurement_date, 1, 19))

# Check the columns are the same
identical(names_order, names(tmp.df))

# Merge
grinding.df <- bind_rows(grinding.df,
                         tmp.df)

# Clean up
rm(tmp.df)

# Save DF
saveRDS(grinding.df, "data_in/processed/grinding.df.rds")
# Load in data frame
# grinding.df <- readRDS("~/Dropbox/unearthed2016/data_in/processed/grinding.df.rds")

# ======================================== #

# ================ Exploratory Data Analysis ================ #
surge_column <- "3311WI671.PV"
feb_grinding.df <- grinding.df[grinding.df$measurement_date >= "2016-02-12" & grinding.df$measurement_date <= "2016-02-23",]
ggplot(data = feb_grinding.df, aes(x = feb_grinding.df$`3311WI671.PV`)) +
    geom_density(fill = "grey50") +
    labs(title = "Surge Distribution",
         x = "3311WI671.PV",
         y = "Density")

ggplot(data = grinding.df, aes(x = grinding.df$`3311WI671.PV`)) +
    geom_density(fill = "#2CB5C0") +
    geom_vline(xintercept = 250, color = "#FC719E", linetype = "longdash") +
    labs(title = "Surge Distribution",
         x = "3311WI671.PV",
         y = "Density")

ggplot(data = grinding.df, aes(x = grinding.df$measurement_date, y = grinding.df$`3311WI671.PV`)) +
    geom_point(colour = "#2CB5C0", alpha = 1/2, size = 0.6) +
    geom_hline(yintercept = 250, colour = "#FC719E") +
    labs(title = "Surge Timeline",
         x = "date",
         y = "3311WI671.PV")

# = Cleaning = #
# Remove the redundant fields
grinding.df <- grinding.df[,-c(4:5,22:23,40:41)]

# Remove rows where surge is NA
grinding.df <- grinding.df[!is.na(grinding.df$`3311WI671.PV`),]

# Created a basic surge classifier
grinding.df$surge <- as.factor(grinding.df$`3311WI671.PV` >= 250)

# Fix levels
levels(grinding.df$surge) <- c("N","Y")

# ======================================== #

# ================ Exploratory Data Analysis ================ #
# = Correlations = #
M <- cor(grinding.df[,-1])
# Only plot fields with strong correlation (>0.5) with response
M_strong <- M[abs(M[,"3311WI671.PV"]) > 0.4,abs(M["3311WI671.PV",]) > 0.5]
# Plot
corrplot(M_strong, method = "square", type = "upper",
         title = "Largest correlations",
         mar = c(0,0,1,0),
         tl.srt = 0,
         tl.offset = 1,
         tl.col = "#000000",
         tl.cex = 0.9)

# = Distributions = #
# Create a function to plot distributions
plot_dist <- function(data, var_name) {
    ggplot(data = data, aes(x = data[,var_name])) +
        geom_density(fill = "grey50") +
        labs(title = paste(var_name, " Distribution"),
             x = var_name,
             y = "Density")
    ggsave(paste0("image_out/",var_name,".png"))
}

# plot all dists
for (name in names(grinding.df[,-1])) {
    plot_dist(data = grinding.df[,-1], var_name = name)
}

# Plot against Surge
plot_dist_by_surge <- function(data, var_name) {
    ggplot(data = data, aes(x = data[,var_name], fill = surge)) +
        geom_density(alpha = 1/2) +
        labs(title = paste(var_name, " Distribution by Surge"),
             x = var_name,
             y = "Density")
    ggsave(paste0("image_out/distributions_against_surge/",var_name,".png"))
}

# plot all dists
for (name in names(grinding.df[,-1])) {
    plot_dist_by_surge(data = grinding.df[,-1], var_name = name)
}


# ======================================== #

# ================ Feature Engineering ================ #

# Date as int
grinding.df$date_int <- as.integer(grinding.df$measurement_date)

# ======================================== #