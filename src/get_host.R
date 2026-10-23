#### Generate figures


# Load files
epp_names <- read.csv(here("data/processed/EPPs_list_final.csv"))
df <- read.csv(here("outputs/merged_dbs.csv"))

# Check for duplicate EPP scientific names, and then check if any of those duplicates have different host systems
epp_dups <- epp_names %>% 
  group_by(Scientific.name) %>% filter(n() > 1)

dup_names <- unique(epp_dups$Scientific.name)

# Flag all rows where the scientific name is a duplicate and there is more than one host
epp_names <- epp_names %>% 
  mutate(screen_host=FALSE)

for (n in dup_names) {
  curr_name <- n
  
  subset_names <- epp_dups %>%
    filter(Scientific.name == curr_name)
  
  if (length(unique(subset_names$Host.system)) > 1) {
    epp_names <- epp_names %>%
      mutate(screen_host = case_when(
        (Scientific.name == curr_name) ~ TRUE,
        (screen_host == TRUE) ~ TRUE,
        .default = FALSE
      ))
  }
  else {
    epp_names <- epp_names %>%
      mutate(screen_host = case_when(
        (Scientific.name == curr_name) ~ FALSE,
        (screen_host == TRUE) ~ TRUE,
        .default = FALSE
      ))
  }
  
}

# Get rid of unnecessary columns in epp_names and rename the scientific name column
epp_names <- epp_names %>%
  select(-(names(epp_names)[3:(length(epp_names)-1)]))

epp_names <- epp_names %>% rename(EPP.scientific.name = Scientific.name)

# Add the host system to the main df
new_df <- dplyr::left_join(
  x=df,
  y=epp_names,
  by="EPP.scientific.name", 
)

# For each host, count the number of events per year

## MANUALLY READ IN VALIDATED/REVIEWED HOST INFORMATION
new_df <- read.csv(here("outputs/merged_dbs_host_reviewed.csv"))

summ_df <- new_df %>%
  count(Start_Year, Host.system)

summ_df <- summ_df %>%
  filter(!is.na(Host.system)) # get rid of NA hosts

summ_df_1990 <- summ_df %>%
  filter(as.numeric(Start_Year) >= 1990)

# Figure generation
library(ggplot2)
ggplot(summ_df, aes(x=Start_Year, y=n, color=Host.system, group=Host.system)) +
  geom_line(linewidth=0.6) +
  ggtitle("LLM Events Over Time") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(color="Host", x="Year", y="No. Events") +
  scale_x_continuous(
    limits=c(1990, 2030),
    breaks = c(seq(1990, 2025, by = 5), 2030)
  )

plot_events <- ggplot(summ_df_1990, aes(x=Start_Year, y=n, color=Host.system, group=Host.system)) +
  geom_line(linewidth=0.6) +
  ggtitle("LLM Events Over Time") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(color="Host", x="Year", y="No. Events") +
  scale_x_continuous(
    limits=c(1990, 2030),
    breaks = c(seq(1990, 2025, by = 5), 2030)
  )

plot_events_2 <- ggplot(summ_df_1990, aes(x=Start_Year, y=n, color=Host.system, group=Host.system)) +
  geom_line(linewidth=0.6) +
  geom_point(size=2) +
  ggtitle("LLM Events Over Time") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(color="Host", x="Year", y="No. Events") +
  scale_x_continuous(
    limits=c(1990, 2030),
    breaks = c(seq(1990, 2025, by = 5), 2030)
  )

ggsave("outputs/events_over_time_point.pdf", plot=plot_events_2, width=9.92, height=5.85)

