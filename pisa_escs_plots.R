library(tidyverse)
library(ggcorrplot)
library(rnaturalearth)
library(rnaturalearthdata)

df <- read_csv("escs_trend.csv", na = c(".", "", "NA")) %>%
  mutate(
    cycle_year = case_when(
      cycle == 5 ~ 2012,
      cycle == 6 ~ 2015,
      cycle == 7 ~ 2018
    ),
    oecd_label = if_else(oecd == 1, "OECD", "Non-OECD")
  )


df_country <- df %>%
  group_by(cnt, cycle_year, oecd_label) %>%
  summarise(
    escs_mean   = mean(escs_trend,   na.rm = TRUE),
    hisei_mean  = mean(hisei_trend,  na.rm = TRUE),
    homepos_mean= mean(homepos_trend,na.rm = TRUE),
    paredint_mean = mean(paredint_trend, na.rm = TRUE),
    .groups = "drop"
  )


theme_pisa <- theme_minimal(base_size = 13) +
  theme(
    plot.title    = element_text(face = "bold", size = 14, margin = margin(b = 6)),
    plot.subtitle = element_text(color = "grey45", size = 11, margin = margin(b = 12)),
    plot.caption  = element_text(color = "grey55", size = 9, margin = margin(t = 10)),
    panel.grid.minor = element_blank(),
    legend.position = "bottom"
  )

caption_src <- "Джерело: OECD PISA Trend ESCS, цикли 2012–2018"


countries_box <- c("FIN", "EST", "POL", "DEU", "FRA", "GBR",
                   "USA", "CAN", "JPN", "KOR", "CHN", "SGP",
                   "BRA", "MEX", "TUR", "IDN")


# 1

p1 <- df %>%
  filter(cnt %in% countries_box) %>%
  mutate(cnt = fct_reorder(cnt, escs_trend, median, na.rm = TRUE)) %>%
  ggplot(aes(x = cnt, y = escs_trend, fill = oecd_label)) +
  geom_boxplot(outlier.size = 0.3, outlier.alpha = 0.2, linewidth = 0.4) +
  scale_fill_manual(values = c("OECD" = "#5B8DB8", "Non-OECD" = "#E07B54"),
                    name = NULL) +
  labs(
    title    = "Розподіл соціально-економічного статусу учнів",
    subtitle = "Медіана та розкид ESCS по вибраних країнах (усі цикли разом)",
    x = NULL, y = "ESCS (стандартизований)",
    caption  = caption_src
  ) +
  theme_pisa +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(p1)


# 2

top_bottom <- df_country %>%
  filter(cycle_year == 2018) %>%
  arrange(escs_mean) %>%
  slice(c(1:5, (n()-4):n())) %>%
  pull(cnt)

p2 <- df_country %>%
  filter(cnt %in% top_bottom) %>%
  ggplot(aes(x = cycle_year, y = escs_mean, color = cnt, group = cnt)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2.5) +
  geom_text(
    data = . %>% filter(cycle_year == 2018),
    aes(label = cnt), hjust = -0.3, size = 3.2, fontface = "bold"
  ) +
  scale_x_continuous(breaks = c(2012, 2015, 2018),
                     limits = c(2012, 2019.5)) +
  scale_color_viridis_d(option = "turbo", guide = "none") +
  labs(
    title    = "Динаміка середнього ESCS: топ-5 і боттом-5 країн",
    subtitle = "Країни відібрані за значенням у 2018 році",
    x = NULL, y = "Середній ESCS",
    caption  = caption_src
  ) +
  theme_pisa


print(p2)


#  3

p3 <- df_country %>%
  ggplot(aes(x = paredint_mean, y = escs_mean,
             color = oecd_label, label = cnt)) +
  geom_point(aes(shape = factor(cycle_year)), size = 2.8, alpha = 0.8) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 0.8,
              aes(group = oecd_label), alpha = 0.15) +
  ggrepel::geom_text_repel(
    data = df_country %>% filter(cycle_year == 2018),
    size = 2.8, max.overlaps = 12, segment.color = "grey70"
  ) +
  scale_color_manual(values = c("OECD" = "#3A7CC3", "Non-OECD" = "#D95F3B"),
                     name = NULL) +
  scale_shape_manual(values = c("2012" = 16, "2015" = 17, "2018" = 15),
                     name = "Цикл") +
  labs(
    title    = "Освіта батьків і загальний ESCS",
    subtitle = "Кожна точка — середнє по країні за один цикл",
    x = "Середній рівень освіти батьків (PAREDINT)",
    y = "Середній ESCS",
    caption  = caption_src
  ) +
  theme_pisa




#  4

countries_violin <- c("FIN", "EST", "KOR", "POL", "BRA", "MEX", "TUR", "IDN")


p4 <- df %>%
  filter(cnt %in% countries_violin, cycle_year == 2018) %>%
  mutate(cnt = fct_reorder(cnt, escs_trend, median, na.rm = TRUE)) %>%
  ggplot(aes(x = cnt, y = escs_trend, fill = cnt)) +
  geom_violin(trim = TRUE, alpha = 0.7, linewidth = 0.3) +
  geom_jitter(width = 0.12, alpha = 0.04, size = 0.4, color = "grey30") +
  stat_summary(fun = median, geom = "crossbar",
               width = 0.4, linewidth = 0.6, color = "white") +
  scale_fill_viridis_d(option = "plasma", guide = "none") +
  labs(
    title    = "Нерівність ESCS всередині країн (2018)",
    subtitle = "Форма розподілу: широкий violin = більша нерівність",
    x = NULL, y = "ESCS (стандартизований)",
    caption  = caption_src
  ) +
  theme_pisa

print(p4)


# 5

world <- ne_countries(scale = "medium", returnclass = "sf")


map_data <- df_country %>%
  filter(cycle_year == 2018) %>%
  select(cnt, escs_mean)

world_joined <- world %>%
  left_join(map_data, by = c("iso_a3" = "cnt"))

p5 <- ggplot(world_joined) +
  geom_sf(aes(fill = escs_mean), color = "white", linewidth = 0.2) +
  scale_fill_viridis_c(
    option = "mako", direction = -1, na.value = "grey85",
    name = "Середній\nESCS",
    breaks = c(-1, -0.5, 0, 0.5, 1)
  ) +
  labs(
    title   = "Середній ESCS учнів по країнах (2018)",
    subtitle = "Сірий колір = країна не брала участі в PISA",
    caption = caption_src
  ) +
  theme_void(base_size = 12) +
  theme(
    plot.title    = element_text(face = "bold", size = 14, margin = margin(b = 6)),
    plot.subtitle = element_text(color = "grey45", size = 11, margin = margin(b = 8)),
    plot.caption  = element_text(color = "grey55", size = 9),
    legend.position = "right"
  )

print(p5)


# 6

p6 <- df %>%
  filter(!is.na(escs_trend)) %>%
  ggplot(aes(x = escs_trend, fill = oecd_label, color = oecd_label)) +
  geom_density(alpha = 0.35, linewidth = 0.8) +
  geom_vline(
    data = df %>%
      filter(!is.na(escs_trend)) %>%
      group_by(oecd_label) %>%
      summarise(m = mean(escs_trend, na.rm = TRUE)),
    aes(xintercept = m, color = oecd_label),
    linetype = "dashed", linewidth = 0.8
  ) +
  facet_wrap(~ cycle_year, ncol = 3) +
  scale_fill_manual(values  = c("OECD" = "#3A7CC3", "Non-OECD" = "#D95F3B"), name = NULL) +
  scale_color_manual(values = c("OECD" = "#3A7CC3", "Non-OECD" = "#D95F3B"), name = NULL) +
  labs(
    title    = "Розподіл ESCS: країни ОЕСР vs решта світу",
    subtitle = "Пунктир — середнє по групі; по панелях — цикл PISA",
    x = "ESCS (стандартизований)", y = "Щільність",
    caption  = caption_src
  ) +
  theme_pisa

print(p6)


# 7

cor_data <- df %>%
  select(escs_trend, hisei_trend, homepos_trend, paredint_trend) %>%
  drop_na() %>%
  # для великого датасету беремо sample
  slice_sample(n = 50000) %>%
  cor()

rownames(cor_data) <- colnames(cor_data) <-
  c("ESCS (загальний)", "HISEI\n(профес. статус батьків)",
    "HOMEPOS\n(ресурси вдома)", "PAREDINT\n(освіта батьків)")

p7 <- ggcorrplot(
  cor_data,
  method = "circle",
  type   = "lower",
  lab    = TRUE,
  lab_size = 4,
  colors = c("#D95F3B", "white", "#3A7CC3"),
  outline.color = "white",
  title  = "Кореляції між компонентами індексу ESCS"
) +
  labs(caption = caption_src) +
  theme_pisa +
  theme(axis.text.x = element_text(size = 9),
        axis.text.y = element_text(size = 9))

print(p7)
