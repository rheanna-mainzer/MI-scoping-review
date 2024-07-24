# figures_for_paper
# This script produces Figure 2 for the paper
#
# Written by R Mainzer
#

# Set up folder for results
dir.create("figures/")

# Read data
load("data_clean.R")

# Figure 2 ---------------------------------------------------------------------

# Summarise data using gtsummary package
fig2_dat <- data[, c(
  "cov_ID", "study_ID", "complete_p_estab", "complete_p_ub",
  "exposure_miss_p_estab", "exposure_miss_p_lb",
  "outcome_miss_p_estab", "outcome_miss_p_lb"
)]

# Panel a
fig2a_dat <- pivot_longer(fig2_dat,
  cols = c("complete_p_estab", "complete_p_ub"),
  names_to = "complete_p", values_to = "p"
)
fig2a_dat$complete_p <- factor(fig2a_dat$complete_p)
levels(fig2a_dat$complete_p) <- c("Studies where able to establish", 
                                  "Studies where upper bound available")

fig2a <- ggplot(fig2a_dat, aes(x = positional, y = p)) +
  geom_dotplot(aes(x = "1"), binaxis = "y", stackdir = "center", binwidth = 3, dotsize = 0.8) +
  geom_boxplot(aes(x = "2"), width = 0.5) +
  facet_wrap(~complete_p, strip.position = "bottom") +
  labs(y = "% of studies") +
  theme_bw() +
  theme(
    panel.spacing.x = unit(0, "npc"),
    strip.background = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    plot.title = element_blank(),
    axis.title.x = element_blank()
  )
fig2a

# Panel b
fig2b_dat <- pivot_longer(fig2_dat,
  cols = c("exposure_miss_p_estab", "exposure_miss_p_lb"),
  names_to = "exposure_miss", values_to = "p"
)
fig2b_dat$exposure_miss <- factor(fig2b_dat$exposure_miss)
levels(fig2b_dat$exposure_miss) <- c("Studies where able to establish", 
                                     "Studies where lower bound available")

fig2b <- ggplot(fig2b_dat, aes(x = positional, y = p)) +
  geom_dotplot(aes(x = "1"), binaxis = "y", stackdir = "center") +
  geom_boxplot(aes(x = "2"), width = 0.5) +
  facet_wrap(~exposure_miss, strip.position = "bottom") +
  labs(y = "% of studies") +
  ylim(0, 100) +
  theme_bw() +
  theme(
    panel.spacing.x = unit(0, "npc"),
    strip.background = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    plot.title = element_blank(),
    axis.title.x = element_blank()
  )
fig2b

# Panel c
fig2c_dat <- pivot_longer(fig2_dat,
  cols = c("outcome_miss_p_estab", "outcome_miss_p_lb"),
  names_to = "outcome_miss", values_to = "p"
)
fig2c_dat$outcome_miss <- factor(fig2c_dat$outcome_miss)
levels(fig2c_dat$outcome_miss) <- c("Studies where able to establish", 
                                    "Studies where lower bound available")

fig2c <- ggplot(fig2c_dat, aes(outcome_miss, p)) +
  geom_dotplot(aes(x = "1"), binaxis = "y", stackdir = "center") +
  geom_boxplot(aes(x = "2"), width = 0.5) +
  facet_wrap(~outcome_miss, strip.position = "bottom") +
  labs(y = "% of studies") +
  ylim(0, 100) +
  theme_bw() +
  theme(
    panel.spacing.x = unit(0, "npc"),
    strip.background = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    plot.title = element_blank(),
    axis.title.x = element_blank()
  )
fig2c

# Combine
fig2 <- ggarrange(fig2a, fig2b, fig2c, labels = c("A", "B", "C"), nrow = 3)
fig2

ggsave("figures/Figure2.pdf", fig2, width = 17.6, height = 17.6, units = "cm")
