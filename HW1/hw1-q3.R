library(rstudioapi)  # This is a external library of functions
library(dplyr)
library(tidyr)
library(tidyverse)
#install.packages("rmarkdown", dep = TRUE)
#install.packages("tinytex")
#tinytex::install_tinytex()  # install TinyTeX

# Getting the path of your current open file
current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path ))
rm(list=ls())

load("OHLC.rdata")
sector_data <- read.csv("sectors.csv")

sector_monthly_returns <- stock %>%
  #as.xxx is the transfer keyword, here it transfer 'date' column into 'Date' format
  #format() extracts only year and month from full date
  #mutate() adds a new column into the dataset, which is the month, so that we can
  #see monthly output
  mutate(month = format(as.Date(date), "%Y-%m")) %>% 
  group_by(symbol, month) %>%
  summarise(monthly_return = (last(close) / first(open) - 1) * 100, .groups = 'drop') %>%
  
  inner_join(sector_data, by = "symbol") %>%
  
  group_by(sector, month) %>%
  summarise(avg_monthly_return = mean(monthly_return, na.rm = TRUE), .groups = 'drop') %>%
  
  #Here we need a wide format, if not pivot, then it will be long format. Long format is that the sector will be A, then several months; B, several months.
  pivot_wider(names_from = month, values_from = avg_monthly_return)

#generate graph


sector_long <- sector_monthly_returns %>%
  pivot_longer(cols = -sector, names_to = "month", values_to = "avg_monthly_return")

sector_long <- sector_monthly_returns %>%
  pivot_longer(cols = -sector, names_to = "month", values_to = "avg_monthly_return")

ggplot(sector_long, aes(x = month, y = avg_monthly_return, group = sector, color = sector)) +
  geom_line() +
  geom_point() +
  facet_wrap(~ sector, scales = "free_y", ncol = 3, labeller = label_wrap_gen(width = 30)) +  # Fewer columns, better spacing
  theme_minimal(base_size = 18) +  # Increase base font size
  labs(title = "Monthly Returns by Sector", x = "Month", y = "Average Monthly Return") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 14),  # Rotate and enlarge x-axis labels
    strip.text = element_text(size = 16),  # Increase facet title size
    legend.text = element_text(size = 14),  # Increase legend text
    plot.title = element_text(size = 22, face = "bold")  # Make title larger and bold
  ) +
  scale_x_discrete(guide = guide_axis(angle = 45))  # Improve x-axis label readability

# Save the improved plot
ggsave("sector_returns_improved.png", width = 18, height = 10)
