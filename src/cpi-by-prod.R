setwd("~/Documents/various-charts/src/")

source("~/.Rprofile")
library(fpp3)
library(cansim)
library(tidyverse)
library(janitor)
library(canbank)
library(lubridate)
library(paletteer)
library(ggthemes)
library(scales)

source("functions/tsibble_cansim.R")

products <- c(
  "Food",
  "Shelter",
  "Transportation",
  "Household operations, furnishings and equipment",
  "Clothing and footwear",
  "Health and personal care",
  "Recreation, education and reading",
  "Alcoholic beverages, tobacco products and recreational cannabis"
)

weights <- get_cansim("18-10-0007-01") %>% clean_names()

# cpi.data <- get_cansim("18-10-0004-13") %>% clean_names()

cpi.data <- get_cansim("18-10-0004-01") %>% clean_names()


# boc.cpi <- canbank::get_boc_series("STATIC_ATABLE_V41690973")
# boc.cpi <- boc.cpi %>% select(Date, Value)
cpi.core <- tsibble_cansim("v41690973", name = "total_cpi")
cpi.core <- cpi.core %>%
  mutate(total_cpi = (difference(log(total_cpi), 12) * 100)) %>%
  drop_na() %>%
  filter(date >= yearmonth("2021-01-01")) %>%
  rename("cpi.date" = 1) %>%
  mutate(cpi.date = as_date(cpi.date))

weights.cleaned <- weights %>%
  filter(products_and_product_groups %in% products, geo == "Canada", price_period_of_weight == "Weight at basket link month prices") %>%
  filter(date >= "1995-01-01") %>%
  mutate(year = year(date)) %>%
  select(year, products_and_product_groups, value) %>%
  rename("weight" = 3)

cpi.cleaned <- cpi.data %>%
  filter(products_and_product_groups %in% products, geo == "Canada", date >= "1995-01-01") %>%
  group_by(products_and_product_groups) %>%
  mutate(inflation = difference(log(value), 12)) %>%
  ungroup() %>%
  select(date, products_and_product_groups, inflation) %>%
  mutate(year = year(date)) %>%
  filter(date >= "1995-01-01")


cleaned <- cpi.cleaned %>%
  left_join(weights.cleaned, by = c("year", "products_and_product_groups")) %>%
  group_by(products_and_product_groups) %>%
  arrange(date) %>%
  fill(weight, .direction = "downup") %>%
  ungroup()


cleaned.weighted <- cleaned %>%
  mutate(contribution = inflation * weight) %>%
  mutate(
    products_and_product_groups = gsub("Alcoholic beverages, tobacco products and recreational cannabis", "Alcohol, tobacco & cannabis", products_and_product_groups), products_and_product_groups = gsub("Household operations, furnishings and equipment", "Household Ops", products_and_product_groups),
    products_and_product_groups = gsub("Recreation, education and reading", "Recreation & Education", products_and_product_groups),
    products_and_product_groups = gsub("Health and personal care", "Health & Care", products_and_product_groups)
  ) %>%
  mutate(products_and_product_groups = str_to_title(products_and_product_groups), date = as_date(date))
myCol <- c(
  "Alcohol, Tobacco & Cannabis" = "#AA3129",
  "Food" = "#53B36B",
  "Shelter" = "#A32583",
  "Household Ops" = "#7AB5D8",
  "Clothing And Footwear" = "#005F3A",
  "Transportation" = "#F97D29",
  "Health & Care" = "#003D57",
  "Recreation & Education" = "#F6D548"
)

# boc_colors <- c(
#   "#67b7dc", "#fdd400", "#0eb663", "#b92020",
#   "#f6871e", "#a92c90", "#f089b8", "#8f7456",
#   "#e5c273", "#ee3344", "#0069eb", "#cb7eff",
#   "#006090", "#f8b79c", "#20a0a0", "#375c15",
#   "#b0d050", "#ff5166", "#86346a", "#897cb0",
#   "#a35e00", "#74b8ff", "#732eaa", "#ff7e86",
#   "#6f7800", "#40db47", "#4434a5", "#ffb64e",
#   "#059103", "#ff62dd", "#010101", "#c21d61",
#   "#cc6402", "#8f5468", "#194fff", "#5e88f2",
#   "#cd6884", "#c1876b", "#08809c", "#cc0bb2",
#   "#588836", "#9a0cff", "#d1361c", "#a38440",
#   "#9e4ab5", "#018f7c", "#9e9621", "#925ef5",
#   "#5e8892", "#3266ad", "#a07dff", "#fefefe"
# )


start_date <- as_date(Sys.Date() - (years(2) + months(11)))
plot <- ggplot() +
  geom_col(data = cleaned.weighted %>%
    filter(date >= start_date), aes(x = date, y = contribution, fill = products_and_product_groups)) +
  geom_line(data = cpi.core %>% filter(cpi.date >= start_date), aes(x = cpi.date, y = total_cpi, col = "All Items"), linewidth = 1) +
  # geom_line(data = boc.cpi%>%filter(Date >= "2023-01-01"), aes(x = Date, y = Value, col = "Headline CPI"),linewidth = 1)+
  # scale_fill_paletteer_d("ggthemes::Tableau_10")+
  scale_fill_manual(values = myCol, name = "") +
  scale_color_manual(values = c("Headline CPI" = "black", "All Items" = "#5e8892")) +
  scale_x_yearmonth(
    expand = c(0, 0),
    breaks = date_breaks("3 month"),
    date_labels = "%b\n%Y"
  ) +
  scale_y_continuous(
    expand = c(0, 0),
    labels = label_number(accuracy = 0.1, suffix = "%"),
    breaks = breaks_pretty(10)
  ) +
  theme_minimal() +
  # theme(
  #   plot.background = element_rect(fill = "black"),
  #   panel.background = element_rect(fill = "black"),
  #   legend.background = element_rect(fill = "black"),
  #   legend.box.background = element_rect(fill = "black"))+

  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    axis.line.x = element_line(color = "gray"),
    plot.subtitle = element_text(colour = "grey50", hjust = 0, face = "italic"),
    plot.title = element_text(hjust = 0),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    plot.margin = unit(c(.5, .5, 0.5, .5), "cm"),
    axis.line.y = element_line(color = "gray"),
    plot.caption = element_text(face = "italic", hjust = 0, colour = "grey50"),
    panel.grid.minor = element_blank(),
    # axis.text.x = element_text(angle = 30),
    axis.ticks = element_line()
  ) +
  labs(x = "", y = "", title = "Contributions To Canadian CPI Inflation", subtitle = "Year-Over-Year Percentage Change", caption = "Data via Statistics Canada, Graph by Tegen Hilker Readman") +
  guides(fill = guide_legend(
    keywidth = unit(1, "cm"),
    keyheight = unit(.5, "cm"),
    nrow = 2
  )) +
  NULL
plot

# nice <- c(
#   "#7AB5D8",
#   "#53B36B",
#   "#F6D548",
#   "#AA3129"
# )

ggsave(plot = plot, filename = "~/Documents/various-charts/assets/cpi-components.png", dpi = 600, width = 11, height = 8)
