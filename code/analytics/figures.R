
# setup -------------------------------------------------------------------

library(tidyverse)
library(cowplot)

source('code/analytics/success_comparison.R')
source('code/analytics/size_comparison.R')
source('code/analytics/neighbor_aquisition.R')

allResight <- read_csv('data/allresight_reference_copy.csv')

# F2 Subcolony Success Values -------------------------------------------

transMort.labels <-
  tibble(
    season = c(2016, 2017, 2018, 2019),
    transMort = as.character(
      paste(
        'Δ = ',
        (1 * round(fishtagComp$transMort,2))
        )),
    x = 1.5,
    y = c(0.82, 0.93, 0.721, 1.2))

Subcol.Succ <- 
  fishtagComp %>% 
  transmute(
    season = sapply(season, seas_fy),
    `Success Metric` = 'Brood Success',
    `Chicks Per Nest` = BrSucc,
    `Standard Error` = Br.se) %>% 
  rbind(
    fishtagComp %>% 
      transmute(
        season = sapply(season, seas_fy),
        `Success Metric` = 'Crèching Success',
        `Chicks Per Nest` = CrSucc,
        `Standard Error` = CR.se)) %>% 
  rbind(
    All.Success %>% 
      filter(`Nest Type` == 'Subcolony') %>% 
      transmute(
        season = 2021,
        `Success Metric`,
        `Chicks Per Nest`,
        `Standard Error`)) %>% 
  filter(!is.na(`Standard Error`))

fig.2 <-
  ggplot(data = Subcol.Succ, aes(x = `Success Metric`,
                                 y = `Chicks Per Nest`,
                                 group = season,
                                 color = factor(season))) +
  geom_pointrange(aes(ymin =`Chicks Per Nest` - `Standard Error`,
                      ymax = `Chicks Per Nest` + `Standard Error`),
                  position = position_dodge(width = -0.35, preserve = 'total'),
                  show.legend = T) +
  geom_line(position = position_dodge(width = -0.35), show.legend = FALSE) +
  geom_text(data = transMort.labels,
            aes(x =x,
                y = y,
                label = transMort),
            size = 4,
            fontface = 'bold',
            position = position_dodge(width = -0.35),
            show.legend = FALSE) +
  scale_color_manual(name = 'Season', values = c("#A8780DFF", "#DF2A92FF", "#7E71F0FF", "#0C987DFF", "#CB593CFF")) +
  # guides(color = 'none') +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 15),
    axis.text.x = element_text(size = 12),
    axis.title.y = element_text(size = 15),
    axis.text.y = element_text(size = 10))

# F3: Anomaly values ------------------------------------------------------

F3.transMort.labels <-
  tibble(
    `Nest Type` = c('Solitary', 'Subcolony'),
    transMort = as.character(
      paste(
        'Δ = ',
        (1 * round(slice_head(Transition.Mortality, n = 2)$transMort,2))
      )),
    x = c(1.8,1.6),
    y = c(0.56, 0.75))

# combine both success metrics into a single table for viz

fig3 <-
  All.Success %>%
  ggplot(aes(x = `Success Metric`, y = `Chicks Per Nest`)) +
  geom_line(aes(group = `Nest Type`), color = 'black', linetype = 'dashed', alpha = 0.75) +
  geom_pointrange(aes(ymin = (`Chicks Per Nest` - `Standard Error`),
                      ymax = (`Chicks Per Nest` + `Standard Error`),
                      fill = `Nest Type`,
                      color = `Nest Type`,
                      shape = `Data`),
                  size = 0.75,
                  position = position_dodge(width = 0.075)) +
  geom_point(data = SubcolCS.Estimates,
             aes(x = `Success Metric`,
                 y = `Chicks Per Nest`,
                 group = `Nest Type`,
                 fill = `Nest Type`,
                 color = `Nest Type`,
                 shape = `Data`),
             position = position_nudge(x = 0.1),
             size = 3,
             alpha = 0.65) +
  geom_text(data = F3.transMort.labels,
            aes(x =x,
                y = y,
                label = transMort,
                color = `Nest Type`,),
            size = 4,
            fontface = 'bold',
            position = position_dodge(width = -0.35)) +
  scale_shape_manual(values = c(23, 21), name = NULL) +
  scale_fill_manual(values = c("#FFCC33", "#33CCFF")) +
  scale_color_manual(values = c("#FFCC33", "#33CCFF")) +
  # scale_y_continuous(limits = c(0,1)) +
  labs(y = 'Nest Success (chicks/nest)', x = 'Success Metric', color = 'Nest Type') +
  facet_wrap(~`Nest Type`) +
  theme_classic()  + 
  theme(axis.title.x = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        axis.title.y = element_text(size = 12),
        axis.text.y = element_text(size = 10),
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 9),
        strip.text = element_text(size = 12))

# visuzlize difference in transition mortality
fig3_subplot <-
  Transition.Mortality %>%
  mutate(`Nest Type` = factor(`Nest Type`, levels = c('Solitary', 'Historic Subcolony'), ordered = T)) %>% 
  filter(Mean == 'y') %>% 
  ggplot(aes(x = `Nest Type`, y = transMort)) +
  geom_pointrange(aes(ymin = transMort-std.error,
                      ymax = transMort+std.error,
                      fill = `Nest Type`,
                      color = `Nest Type`,
                      shape = `Nest Type`),
                  size = 1) +
  geom_point(data = Transition.Mortality %>% filter(Mean == 'n'),
             aes(x = `Nest Type`,
                 y = transMort,
                 fill = `Nest Type`,
                 color = `Nest Type`,
                 shape = `Nest Type`),
             alpha = 0.65,
             size = 4,
             position = position_nudge(x = 0.075)) +
  scale_shape_manual(values = c(21, 23)) +
  scale_fill_manual(values = c("#FFCC33", "#33CCFF")) +
  scale_color_manual(values = c("#FFCC33", "#33CCFF")) +
  #scale_y_continuous(limits = c(-0.1,0.35)) +
  geom_hline(yintercept = 0) +
  # guides(shape = FALSE) +
  labs(y = 'Transition Mortality (chicks/nest)', x = NULL, color = 'Nest Type') +
  # facet_wrap(~`Success Type`) +
  theme_minimal() + 
  theme(axis.text.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.y = element_text(size = 11),
        legend.text = element_text(size = 9),
        legend.title = element_text(size = 11))

plot_grid(fig3_subplot, fig3, labels = "AUTO", nrow = 2)


# F4: Size distribution ---------------------------------------------------

# plot outcome size frequency
fig4 <-
  size_outcome %>% 
  group_by(type, size) %>% 
  summarize(count = n(), .groups = 'drop') %>%
  left_join(
    (size_outcome %>% 
       group_by(type) %>% 
       summarize(total = n(), .groups = 'drop'))) %>% 
  mutate(perc = (count/total)*100) %>% # View()
  ggplot() +
  geom_bar(aes(x = factor(size), y = perc, fill = type), color = 'black', stat = 'Identity') +
  scale_fill_manual(values = c('Subcolony' = '#33CCFF', 'Solitary' = '#FFCC33'), name = "Nest Type") +
  scale_x_discrete(expand = c(0.1,0.1)) +
  scale_y_continuous(expand = c(0,0.1))+
  labs(y = 'Percentage (%)', x = 'Chick Size Class') +
  facet_grid(type~.) +
  guides(fill = 'none')+
  theme_classic() + 
  theme(axis.title.x = element_text(size = 15),
        axis.text.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        strip.text = element_text(size = 15),
        legend.text = element_text(size = 5),
        legend.title = element_text(size = 10),
        legend.background = element_rect(fill = 'transparent'),
        legend.position = 'bottom',
        panel.background = element_rect(fill = 'transparent'),
        plot.background = element_rect(fill = 'transparent'))

# compare creche date between types
# center dates around October 1st as start of Adelie penguin breeding season
fig4_subplot <-
  size_outcome %>% 
  mutate(
    type = factor(type,levels = c('Subcolony', 'Solitary')),
    yday = lubridate::yday(date),
    yday = if_else(
      yday >= 358,
      (yday - 358),
      yday + 7)) %>% # group_by(type) %>% summarize(min = min(yday), q1 = quantile(yday, 0.25), mean = mean(yday), median = median(yday), q3 = quantile(yday, 0.75), max = max(yday))
  filter(yday <= 300)  %>% # remove dates which are too early to represent accurate data
  ggplot() +
  geom_boxplot(aes(x = type, y = yday, fill = type)) +
  scale_fill_manual(values = c('Subcolony' = '#33CCFF', 'Solitary' = '#FFCC33'), name = "Nest Type") +
  coord_flip() +
  labs(x = NULL, y = 'Days since Median Hatch') +
  guides(fill = 'none') +
  # coord_flip() +
  theme_classic() + 
  theme(axis.title.x = element_text(size = 15),
        axis.text.x = element_text(size = 12),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        strip.text = element_text(size = 15),
        legend.text = element_text(size = 5),
        legend.title = element_text(size = 10),
        legend.background = element_rect(fill = 'transparent'),
        legend.position = 'bottom',
        panel.background = element_rect(fill = 'transparent'),
        plot.background = element_rect(fill = 'transparent'))

# ggsave('products/figure4/fig4_subplot.png', plot = fig4_subplot, device = 'png', width = 4, height = 4, bg = 'transparent')

# fig4.panel <- 
  plot_grid(fig4,
            fig4_subplot,
            rel_widths = c(2,1),
            labels = paste0(c('a', 'b')),
            hjust = -1.5,
            ncol = 2)
