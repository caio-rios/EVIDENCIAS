if(require(bibliometrix) == F) install.packages("bibliometrix"); require(bibliometrix)
if(require(here) == F) install.packages("here"); require(here)
if(require(tidyverse) == F) install.packages("tidyverse"); require(tidyverse)
if(require(ggrepel) == F) install.packages("ggrepel"); require(ggrepel)
if(require(readxl) == F) install.packages("readxl"); require(readxl)
if(require(gganimate) == F) install.packages("gganimate"); require(gganimate)

theme_set(theme_bw() + theme(panel.grid = element_blank(),
                             axis.text = element_text(size = 11)))

setwd(here("dados"))

M <- convert2df(list.files(), dbsource = "wos", format = "bibtex")

M <- M |> rename(SO_FULL = SO)

setwd(here())

abb_so <- read_xlsx("Periodicos.xlsx")

M <- M |> left_join(abb_so)

# consulta dos experimentos e quase-experimentos

M <- M |> mutate(TIAB = paste(TI, AB, DE, sep = " "))

M$EXPERIMENT <- str_detect(M$TIAB, regex(pattern = "FIELD EXPERIMENT|LABORATORY EXPERIMENT|SURVEY EXPERIMENT|VIGNETTE EXPERIMENT|LIST EXPERIMENT|EXPERIMENTAL DESIGN|RANDOM EXPERIMENT", ignore_case = T))
M$`QUASI EXPERIMENTS` <- str_detect(M$TIAB, regex(pattern = "propensity score|RDD|quasi experiment|quasi-experiment|quantile regression|DIF-IN-DIF|2SLS|DIFF-IN-DIFF|DIFFERENCE-IN-DIFFERENCE|DIFFERENCE IN DIFFERENCE|Instrumental Variable|double difference|synthetic control|Matching|Regression Discontinuit", ignore_case = T))
library(stringr)

# EXPERIMENTOS
M$FIELD_EXPERIMENT      <- str_detect(M$TIAB, regex("FIELD EXPERIMENT", ignore_case = T))
M$LABORATORY_EXPERIMENT <- str_detect(M$TIAB, regex("LABORATORY EXPERIMENT", ignore_case = T))
M$SURVEY_EXPERIMENT     <- str_detect(M$TIAB, regex("SURVEY EXPERIMENT", ignore_case = T))
M$VIGNETTE_EXPERIMENT   <- str_detect(M$TIAB, regex("VIGNETTE EXPERIMENT", ignore_case = T))
M$LIST_EXPERIMENT       <- str_detect(M$TIAB, regex("LIST EXPERIMENT", ignore_case = T))

# QUASE-EXPERIMENTOS

M$MATCHING         <- str_detect(M$TIAB, regex("Matching|propensity score", ignore_case = T))
M$QUANTILE_REGRESSION <- str_detect(M$TIAB, regex("quantile regression", ignore_case = T))
M$INSTRUMENTAL_VARIABLE <- str_detect(M$TIAB, regex("Instrumental Variable|2SLS|Two-Stage Least Squares", ignore_case = T))
M$SYNTHETIC_CONTROL   <- str_detect(M$TIAB, regex("synthetic control", ignore_case = T))
M$RDD <- str_detect(M$TIAB, regex("RDD|Regression Discontinuit", ignore_case = T))
M$DIFF_IN_DIFF <- str_detect(M$TIAB, regex("DIF-IN-DIF|DIFF-IN-DIFF|DIFFERENCE-IN-DIFFERENCE|DIFFERENCE IN DIFFERENCE|double difference", ignore_case = T))
M$INTERRUPTED_TIME_SERIES <- str_detect(M$TIAB, regex("INTERRUPTED TIME SERIES|INTERRUPTED TIME-SERIES", ignore_case = T))


# Evolucao do uso de experimentos e q-exp

exp_qexp <- M |> gather(key = "design", value = "check", c(EXPERIMENT, `QUASI EXPERIMENTS`)) |> 
  filter(check == 1)

ev_total <- M |> group_by(PY) |> summarise(n_total = n())

ev <- exp_qexp |> group_by(PY) |> summarise(n = n()) |> 
  left_join(ev_total) |> mutate(prop = scales::percent(n/n_total, accuracy = 0.1))

setwd(here("graficos"))

ggplot(ev, aes(x = PY, y = n)) +
  geom_line() +
  geom_label(aes(label = paste0(n, "\n(", prop, ")")),
            size = 3, 
            data = ev |> filter(PY %in% c(2000, 2010, 2015, 2020, 2026))) +
  scale_x_continuous(breaks = seq(2000, 2026, 2)) +
  labs(x = "Ano", y = "Quantidade de Artigos")

ggsave("ev expqexp.png")

ev <- exp_qexp |> group_by(PY, design) |> summarise(n = n()) |> 
  left_join(ev_total) |> mutate(prop = scales::percent(n/n_total, accuracy = 0.1))



ggplot(ev, aes(x = PY, y = n)) +
  geom_line(aes(color = design)) +
  theme(legend.position = "top") +
  geom_label(aes(label = paste0(n, "\n(", prop, ")")),
             size = 3, 
             data = ev |> filter(PY %in% c(2015, 2020, 2026))) +
  scale_x_continuous(breaks = seq(2000, 2026, 2)) +
  labs(color = "", x = "Ano", y = "Quantidade de Artigos") +
  scale_color_manual(values = c("EXPERIMENT" = "darkorange",
                                "QUASI EXPERIMENTS" = "deepskyblue"))

ggsave("ev exp qexp separado.png")

# Evolucao do uso por periodico

ev <- exp_qexp |> group_by(PY, SO, design) |> summarise(n = n()) |> 
  spread(design, n) |> 
  mutate(EXPERIMENT = replace_na(EXPERIMENT, 0),
         `QUASI EXPERIMENTS` = replace_na(`QUASI EXPERIMENTS`, 0),
         total = EXPERIMENT + `QUASI EXPERIMENTS`,
         PY = as.integer(PY))


g1 <- ggplot(ev, aes(x = EXPERIMENT, y = `QUASI EXPERIMENTS`)) +
  geom_hline(yintercept = c(10, 15), color = "grey") +
  geom_vline(xintercept = c(10, 20, 40), color = "grey") +
  geom_point(aes(size = total),
             color = "darkorange",
             alpha = .45) +
  geom_text_repel(aes(label = SO), 
                  size = 3) +
  transition_time(PY) +
  labs(title = 'Ano: {frame_time}',
       size = "Experimentos + Quase-Experimentos",
       x = "Experimentos", y = "Quase-Experimentos") +
  theme(legend.position = "top") +
  scale_x_continuous(breaks = seq(0, 60, 10))

g2 <- animate(
  g1, 
  nframes = 27, # Número de quadros 
  fps = 2,      # Quadros por segundo
  width = 700, 
  height = 500, 
  renderer = gifski_renderer()
)

anim_save("grafico_animado.gif", animation = g2)


g1 <- ggplot(ev |> filter(PY == 2026), aes(x = EXPERIMENT, y = `QUASI EXPERIMENTS`)) +
  geom_hline(yintercept = c(10, 15), color = "grey") +
  geom_vline(xintercept = c(10, 20, 40), color = "grey") +
  geom_point(aes(size = total),
             color = "darkorange",
             alpha = .45) +
  geom_text_repel(aes(label = SO), 
                  size = 3) +
  labs(title = 'Ano: 2026',
       size = "Experimentos + Quase-Experimentos",
       x = "Experimentos", y = "Quase-Experimentos") +
  theme(legend.position = "top") +
  scale_x_continuous(breaks = seq(0, 60, 10))

g1

ggsave("exp qexp x so 2026.png")

g2 <- ggplot(ev |> filter(PY == 2026), aes(x = EXPERIMENT, y = `QUASI EXPERIMENTS`)) +
  geom_hline(yintercept = c(10, 15), color = "grey") +
  geom_vline(xintercept = c(10, 20, 40), color = "grey") +
  geom_point(aes(size = total),
             color = "darkorange",
             alpha = .45,
             data = ev |> filter(PY == 2000)) +
  geom_text_repel(aes(label = SO), 
                  size = 3,
                  data = ev |> filter(PY == 2000)) +
  labs(title = 'Ano: 2000',
       size = "Experimentos + Quase-Experimentos",
       x = "Experimentos", y = "Quase-Experimentos") +
  theme(legend.position = "top") +
  scale_x_continuous(breaks = seq(0, 60, 10))
g2

ggsave("exp qexp x so 2000.png")


# Distribuicao por tipo

exp <- M |> gather(key = "design", value = "check", c(FIELD_EXPERIMENT, 
                                                      LABORATORY_EXPERIMENT,
                                                      SURVEY_EXPERIMENT,
                                                      VIGNETTE_EXPERIMENT,
                                                      LIST_EXPERIMENT)) |> 
  filter(check == 1)


exp_dist <- exp |> group_by(design) |> summarise(n = n()) |> 
  mutate(prop = scales::percent(n/sum(n), accuracy = 0.1),
         design = gsub("_EXPERIMENT", "", design),
         design = str_to_title(design))

ggplot(exp_dist, aes(x = reorder(design, n), y = n)) +
  geom_col(width = .6,
           fill = "darkorange",
           color = "black") +
  geom_text(aes(label = paste0(n, " (", prop, ")")),
            vjust = -.5) +
  labs(x = "Tipos de Experimento",
       y = "Quantidade de Artigos")

ggsave("distribuicao exp.png")

qexp <- M |> gather(key = "design", value = "check", c(MATCHING, 
                                                       QUANTILE_REGRESSION,
                                                       INSTRUMENTAL_VARIABLE,
                                                       SYNTHETIC_CONTROL,
                                                       RDD,
                                                       DIFF_IN_DIFF,
                                                       INTERRUPTED_TIME_SERIES)) |> 
  filter(check == 1)


qexp_dist <- qexp |> group_by(design) |> summarise(n = n()) |> 
  mutate(prop = scales::percent(n/sum(n), accuracy = 0.1),
         design = gsub("_", " ", design),
         design = str_to_title(design))

ggplot(qexp_dist, aes(x = reorder(design, n), y = n)) +
  geom_col(width = .6,
           fill = "deepskyblue",
           color = "black") +
  coord_flip() +
  geom_text(aes(label = paste0(n, " (", prop, ")")),
            hjust = -.1) +
  labs(x = "Tipos de Experimento",
       y = "Quantidade de Artigos") +
  scale_y_continuous(expand = expansion(mult = c(.05, .3)))

ggsave("distribuicao qexp.png")


# evolucao por tipo facet

ev_exp <- M |> gather(key = "design", value = "check", c(FIELD_EXPERIMENT, 
                                                          LABORATORY_EXPERIMENT,
                                                          SURVEY_EXPERIMENT,
                                                          VIGNETTE_EXPERIMENT,
                                                          LIST_EXPERIMENT)) |> 
  filter(check == 1) |> 
  group_by(design, PY) |> summarise(n = n()) |> 
  mutate(design = str_to_title(gsub("_EXPERIMENT", "", design)))


ggplot(ev_exp, aes(x = PY, y = n)) +
  geom_line() +
  geom_smooth(method = "lm", linewidth = .5, se = F,
              color = "darkorange") +
  facet_wrap(~design, scale = "free_y") +
  labs(x = "Ano", y = "Quantidade de Artigos")
  
ggsave("evolucao exp facet.png")



ev_exp <- M |> gather(key = "design", value = "check", c(MATCHING, 
                                                          QUANTILE_REGRESSION,
                                                          INSTRUMENTAL_VARIABLE,
                                                          SYNTHETIC_CONTROL,
                                                          RDD,
                                                          DIFF_IN_DIFF,
                                                          INTERRUPTED_TIME_SERIES)) |> 
  filter(check == 1) |> 
  group_by(design, PY) |> summarise(n = n()) |> 
  mutate(design = str_to_title(gsub("_", " ", design)))


ggplot(ev_qexp, aes(x = PY, y = n)) +
  geom_line() +
  geom_smooth(method = "lm", linewidth = .5, se = F,
              color = "deepskyblue") +
  facet_wrap(~design, scale = "free_y") +
  labs(x = "Ano", y = "Quantidade de Artigos")

ggsave("evolucao qexp facet.png")

# network das keywords

data <- M |> filter(EXPERIMENT == 1) |> group_by(PY) |> count()
LC <- lifeCycle(data, forecast_years = 20, plot = TRUE, verbose = FALSE)
exp <- LC$forecast

data <- M |> filter(`QUASI EXPERIMENTS` == 1) |> group_by(PY) |> count()
LC <- lifeCycle(data, forecast_years = 20, plot = TRUE, verbose = FALSE)
qexp <- LC$forecast

exp_real <- M |> filter(EXPERIMENT == 1) |> group_by(PY) |> 
  summarise(predicted_annual = n())

qexp_real <- M |> filter(`QUASI EXPERIMENTS` == 1) |> group_by(PY) |> 
  summarise(predicted_annual = n())

exp <- bind_rows(exp_real, exp)

g1 <- ggplot(exp, aes(x = PY, y = predicted_annual)) +
  geom_vline(xintercept = 2040) +
  geom_text(aes(label = round(predicted_annual)),
                  data = exp |> filter(PY == 2040),
            vjust = -.1,
            hjust = 1.5) +
  geom_line() +
  geom_point(size = 3, data = exp_real) +
  scale_x_continuous(breaks = seq(2000, 2100, 20)) +
  labs(x = "Ano", y = "Produção Anual Estimada")

g2 <- ggplot(qexp, aes(x = PY, y = predicted_annual)) +
  geom_vline(xintercept = 2042) +
  geom_text(aes(label = round(predicted_annual)),
            data = qexp |> filter(PY == 2042),
            vjust = -.1,
            hjust = 1.5) +
  geom_line() +
  geom_point(size = 3, data = qexp_real) +
  scale_x_continuous(breaks = seq(2000, 2100, 20)) +
  labs(x = "Ano", y = "Produção Anual Estimada") +
  scale_y_continuous(expand = expansion(mult = c(0, 1.38)))

g1

ggsave("expected experiments.png")

g2

ggsave("expected quasi_experiments.png")

# Create a co-citation network experimentos

exp <- M |> filter(EXPERIMENT == 1)

NetMatrix <- biblioNetwork(exp, analysis = "co-occurrences", network = "keywords", sep = ";")

png("keyword coocc exp.png", width = 8, height = 6, units = "in", res = 300)

net <- networkPlot(NetMatrix, normalize = "association", weighted = TRUE, n = 30, 
                   Title = "", type = "fruchterman", 
                   size = TRUE, edgesize = 5, labelsize = 0.7, cluster = "optimal")

dev.off()

# Create a co-citation network quase-experimentos

qexp <- M |> filter(`QUASI EXPERIMENTS` == 1)

NetMatrix <- biblioNetwork(qexp, analysis = "co-occurrences", network = "keywords", sep = ";")


png("keyword coocc qexp.png", width = 8, height = 6, units = "in", res = 300)

net <- networkPlot(NetMatrix, normalize = "association", weighted = TRUE, n = 30, 
                   Title = "", type = "fruchterman", 
                   size = TRUE, edgesize = 5, labelsize = 0.7, cluster = "optimal")

dev.off()
