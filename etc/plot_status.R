# Plot status.txt
library(ggplot2)
theme_set(theme_classic())

status <- read.csv("status.txt",
                   header = FALSE,
                   stringsAsFactors = FALSE,
                   col.names = c("datetime", "count"))
status$datetime <- strptime(status$datetime, format = "%Y-%m-%d %H:%M:%S")

ggplot(status, aes(datetime, count)) +
  geom_point(shape = 1, size = 0.25) +
  geom_line()

(status[nrow(status),"count"] - status[1,"count"]) /
  as.numeric(difftime(status[nrow(status),"datetime"], status[1,"datetime"], units ="hours"))
