# For each host, count the number of events per year

## Read in file with host column
new_df <- read.csv(here("outputs/merged_dbs_host.csv"))

summ_df <- new_df %>%
  count(Start_Year, Host.system)

summ_df <- summ_df %>%
  filter(!is.na(Host.system)) # get rid of NA hosts

summ_df_1990 <- summ_df %>%
  filter(as.numeric(Start_Year) >= 1990)

# Figure generation
library(ggplot2)



plot_events <- ggplot(summ_df_1990, aes(x=Start_Year, y=n, color=Host.system, group=Host.system)) +
  geom_line(linewidth=0.6) +
  geom_point(size=2) +
  ggtitle("LLM Events Over Time") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(color="Host", x="Year", y="No. Events") +
  scale_x_continuous(
    limits=c(1990, 2030),
    breaks = c(seq(1990, 2025, by = 5), 2030)
  )

ggsave("outputs/events_over_time_point.pdf", plot=plot_events, width=9.92, height=5.85)

