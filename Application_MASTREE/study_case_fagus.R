####################################################
#main review update,
#1-change axis title x #before it was days reversed
#2-update figure 2 main text (average r2) with window for sliding time window and updated factor order in fig
#3-check trends in data before and after 1980
#4-Updated figure in Supplement (still kept previous version here as commented)
####################################################

library(weatheRcues)
library(here)
library(tidyverse)


#for analysis, but these package should be automatically loaded if you are loading weatherRcues
library(climwin)
library(broom)
library(mgcv)
library(here)

#for maps and figures
library(sf)
library("rworldmap")
library("rworldxtra")
library(ggspatial)
library(ggsci)
library(patchwork)
library(RColorBrewer)

#for table
library(knitr)
library(kableExtra)

#some function for the application used here in this specific study
source("Application_MASTREE/Additional_functions_application_mastree.R")

new.folder = FALSE
if (new.folder == TRUE) {
  dir.create(here::here('Application_MASTREE/climate_dailyEOBS'))
  dir.create(here::here('Application_MASTREE/figures'))
}

#access climate zip file
#https://osf.io/287ms/ and add it in climate_dailyEOBS folder

nfile = check_folder_and_contents(
  here::here('Application_MASTREE/climate_dailyEOBS'),
  file_pattern = '.qs'
)
if (nfile$folder_exists == FALSE) {
  stop(print('missing folder climate_dailyEOBS'))
}
if (length(nfile$files_present) == 0) {
  stop(print(
    'missing file to add in the folder of interest name climate_dailyEOBS'
  ))
}
#SHOULD HAVE NO WARNING HERE

# I am generating calendar, starting from 1st of November, and going back 600 days before (to capture previous year cue)
calendar = generate_reverse_day_calendar(
  refday = 305, #double check later, because climwin start at 1 or 0
  lastdays = 600,
  yearback = 2,
  start_year = 1949
) %>%
  #filter(YEAR == 1950) %>%
  dplyr::select(MONTHab, DOY, days.reversed, DATE) %>%
  rename(date.fake = DATE) %>%
  mutate(
    #datefake = as.Date(DOY, origin = "1948-01-01"),
    day.month = format(date.fake, "%m-%d"),
    year = case_when(
      days.reversed < 366 ~ 1,
      days.reversed >= 366 & days.reversed < 730 ~ 2,
      days.reversed >= 730 & days.reversed < 1095 ~ 3,
      TRUE ~ 4
    )
  )

#load data from JJ Foest, last V2 version
#url.mastreev2 = 'https://raw.githubusercontent.com/JJFoest/MASTREEplus/main/Data/MASTREEplus_2024-06-26_V2.csv'
#or the one from your content
initial.data.mastree <- read.csv(
  here("Application_MASTREE/mastreedata_copy/MASTREEplus_2024-06-26_V2.csv"),
  stringsAsFactors = F
)
#Filtering data for Fagus sylvatica
Fagus.seed = formatting_mastree_fagus(initial.data.mastree)

#get the mothod collection
methods.collection.mv2 = obtained_cleaned_method_collection(Fagus.seed)

#make figure 1
#and it will save it in the figure folder
Figure1.main(Fagus.seed)

#get some information about the data
forsummary = Fagus.seed %>%
  dplyr::select(n, sitenewname, Collection_method) %>%
  distinct()
summary(forsummary$n)
table(forsummary$Collection_method)

#################################################################
#################################################################
#############Sliding method
#################################################################
#################################################################

#here for climwin
optionwindows = 'absolute'
referenceclimwin = c(01, 11)
range = c(600, 0)

#climate data path
climate.beech.path <- here::here('Application_MASTREE/climate_dailyEOBS/')
#get unique site id
beech.site.all = unique(Fagus.seed$plotname.lon.lat)
# Define a function to process each fagus site
#option to run in parallel to make it faster (faster than map_dfr, but need double checking because if not well defined = issues)
#because it takes some time you can use directly the last file and upload it directly to the session
run.climwin <- F
#take some time ! more than 1 h
if (run.climwin == T) {
  library(furrr)
  plan(multisession)
  #if it is craching you can just use map_dfr but it will take a while
  statistics_absolute_climwin <- future_map_dfr(
    beech.site.all,
    ~ {
      library(weatheRcues)
      # format the climate, .x is the current site
      climate_data <- weatheRcues::format_climate_data(
        site = .x,
        path = climate.beech.path,
        scale.climate = TRUE,
        date_column = "DATEB"
      )
      # Run climwin per site
      weatheRcues:::runing_climwin(
        climate_data = climate_data,
        bio_data = Fagus.seed %>% filter(plotname.lon.lat == .x),
        site.name = .x,
        range = range,
        cinterval = 'day',
        refday = c(01, 11),
        optionwindows = "absolute",
        climate_var = "TMEAN"
      )
    },
    .options = furrr_options(seed = TRUE) #setting the seed across the parallel workers.
  )
  qs::qsave(
    statistics_absolute_climwin,
    here('Application_MASTREE/outputs/statistics_absolute_climwin.qs')
  )
} else {
  statistics_absolute_climwin = qs::qread(
    'Application_MASTREE/outputs/statistics_absolute_climwin.qs'
  )
}

#get some stat about sliding method
mean(statistics_absolute_climwin$window.open)
mean(statistics_absolute_climwin$window.close)
quibble2(statistics_absolute_climwin$window.open, q = c(0.25, 0.5, 0.75))
quibble2(statistics_absolute_climwin$window.close, q = c(0.25, 0.5, 0.75))


climwin.dd = statistics_absolute_climwin %>%
  dplyr::select(sitenewname, window.open, window.close) %>%
  pivot_longer(window.open:window.close, values_to = 'days.reversed') %>%
  left_join(calendar %>% dplyr::select(MONTHab, DOY, days.reversed))


# climwin.all.sites = climwin.dd %>%
#   dplyr::select(sitenewname, name, days.reversed) %>%
#   pivot_wider(names_from = "name", values_from = "days.reversed") %>%
#   left_join(methods.collection.mv2) %>%
#   left_join(
#     Fagus.seed %>%
#       dplyr::select(
#         Country,
#         Alpha_Number,
#         Latitude,
#         Longitude,
#         plotname.lon.lat,
#         sitenewname
#       ) %>%
#       distinct()
#   ) %>%
#   mutate(
#     sitenewname = paste0(Country, "_", Alpha_Number, "_", round(Latitude, 4))
#   ) %>%
#   arrange(Collection_method) %>%
#   mutate(
#     sitenewname = fct_reorder(sitenewname, as.character(Collection_method))
#   ) %>%
#   ggplot() +
#   geom_segment(
#     aes(
#       y = window.open,
#       yend = window.close,
#       x = sitenewname,
#       col = Collection_method
#     ),
#     linewidth = 2
#   ) +
#   coord_flip() +
#   geom_hline(yintercept = 133, color = 'black', linetype = 'dashed') +
#   geom_hline(yintercept = 498, color = 'black', linetype = 'dashed') +
#   ylim(0, 600) +
#   scale_color_futurama() +
#   scale_fill_futurama() +
#   ggpubr::theme_cleveland() +
#   theme(legend.position = 'bottom', legend.title = element_blank()) +
#   ylab('Days before seed fall (0 = Nov 1 of seed-fall year).')

climwin.all.sites = climwin.dd %>%
  dplyr::select(sitenewname, name, days.reversed) %>%
  pivot_wider(names_from = "name", values_from = "days.reversed") %>%
  left_join(methods.collection.mv2) %>%
  left_join(
    statistics_absolute_climwin %>%
      dplyr::select(sitenewname, slope.estimate, r2)
  ) %>%
  ungroup() %>%
  arrange(Collection_method) %>%
  left_join(
    Fagus.seed %>%
      dplyr::select(
        Country,
        Alpha_Number,
        Latitude,
        Longitude,
        plotname.lon.lat,
        sitenewname
      ) %>%
      distinct()
  ) %>%
  mutate(
    sitenewname = paste0(Country, "_", round(Latitude, 4))
  ) %>%
  arrange(Collection_method) %>%
  mutate(
    sitenewname = fct_reorder(sitenewname, Latitude)
  ) %>%
  ggplot() +
  geom_segment(
    aes(
      y = window.open,
      yend = window.close,
      x = sitenewname,
      col = slope.estimate
    ),
    linewidth = 2
  ) +
  facet_grid(Collection_method ~ ., scale = "free") +
  coord_flip() +
  geom_hline(yintercept = 133, color = 'black', linetype = 'dashed') +
  geom_hline(yintercept = 498, color = 'black', linetype = 'dashed') +
  ylim(0, 600) +
  ggpubr::theme_cleveland() +
  scale_color_viridis_c(
    option = "mako",
    limits = c(0, 0.7)
  ) +
  scale_color_gradient2(
    low = "blue",
    mid = "white", # Color for the midpoint
    high = "red",
    midpoint = 0
  ) +
  labs(colour = "Slope (std)") +
  theme(
    legend.position = 'right',
    #legend.title = element_blank(),
    panel.background = element_rect(fill = 'white', colour = 'white')
  ) +
  ylab('Days before seed fall (0 = Nov 1 of seed-fall year).')
climwin.all.sites
cowplot::save_plot(
  here("Application_MASTREE/figures/climwin.all.png"),
  climwin.all.sites,
  ncol = 1.5,
  nrow = 1.8,
  dpi = 300
)

#################################################################
#################################################################
#############daily relation used for Climate sensitivity profile and Peak detection
#################################################################
#################################################################
#This function here is basically making correlation or regression for each climate date to seed production
results.moving.site = ATS_moving_climate(
  bio_data_all = Fagus.seed,
  climate.path = climate.beech.path,
  refday = 305,
  lastdays = max(range),
  rollwin = 1,
  formula_model = formula('log.seed ~ TMEAN'),
  model_type = 'lm'
)

#create one list for each sites
Results_daily = results.moving.site %>%
  arrange(plotname.lon.lat) %>%
  group_by(plotname.lon.lat) %>%
  group_split()

name_daily = results.moving.site %>%
  arrange(plotname.lon.lat) %>%
  group_by(plotname.lon.lat) %>%
  group_keys()


# Set names of the list based on group keys
names(Results_daily) <- apply(name_daily, 1, paste)

###############
#now focus on climate sensivitiy profile method
#map dfr is enough, quite fast if not optimize gam model
statistics_csp_method = map_dfr(
  1:length(Results_daily),
  ~ ATS_CSP(
    Results_daily[[.]],
    unique(Results_daily[[.]]$plotname.lon.lat),
    bio_data = Fagus.seed %>% arrange(plotname.lon.lat),
    climate.path = climate.beech.path,
    refday = 305,
    lastdays = max(range), #here it should be 600
    rollwin = 1,
    optim.k = F
  )
)
qs::qsave(
  statistics_csp_method,
  here('Application_MASTREE/outputs/statistics_csp.qs')
)

#because it might be able to detect multplie cues sometimes
output_fit_summary.best.csp = statistics_csp_method %>%
  group_by(sitenewname) %>%
  dplyr::slice(which.max(r2))

#some stat, comparing if I kept all cues
quibble2(output_fit_summary.best.csp$window.open, q = c(0.25, 0.5, 0.75))
quibble2(output_fit_summary.best.csp$window.close, q = c(0.25, 0.5, 0.75))
quibble2(statistics_csp_method$window.open, q = c(0.25, 0.5, 0.75))
quibble2(statistics_csp_method$window.close, q = c(0.25, 0.5, 0.75))

#previous version
# csp.all.sites = output_fit_summary.best.csp %>%
#   left_join(methods.collection.mv2) %>%
#   ungroup() %>%
#   arrange(Collection_method) %>%
#   left_join(
#     Fagus.seed %>%
#       dplyr::select(
#         Country,
#         Alpha_Number,
#         Latitude,
#         Longitude,
#         plotname.lon.lat,
#         sitenewname
#       ) %>%
#       distinct()
#   ) %>%
#   mutate(
#     sitenewname = paste0(Country, "_", Alpha_Number, "_", round(Latitude, 4))
#   ) %>%
#   arrange(Collection_method) %>%
#   mutate(
#     sitenewname = fct_reorder(sitenewname, as.character(Collection_method))
#   ) %>%
#   ggplot() +
#   geom_segment(
#     aes(
#       y = window.open,
#       yend = window.close,
#       x = sitenewname,
#       col = Collection_method
#     ),
#     linewidth = 2
#   ) +
#   coord_flip() +
#   geom_hline(yintercept = 133, color = 'black', linetype = 'dashed') +
#   geom_hline(yintercept = 498, color = 'black', linetype = 'dashed') +
#   ylim(0, 600) +
#   scale_color_futurama() +
#   scale_fill_futurama() +
#   ggpubr::theme_cleveland() +
#   theme(legend.position = 'bottom', legend.title = element_blank()) +
#   ylab('Days before seed fall (0 = Nov 1 of seed-fall year).')

csp.all.sites = output_fit_summary.best.csp %>%
  left_join(methods.collection.mv2) %>%
  ungroup() %>%
  arrange(Collection_method) %>%
  left_join(
    Fagus.seed %>%
      dplyr::select(
        Country,
        Alpha_Number,
        Latitude,
        Longitude,
        plotname.lon.lat,
        sitenewname
      ) %>%
      distinct()
  ) %>%
  mutate(
    sitenewname = paste0(Country, "_", round(Latitude, 4))
  ) %>%
  arrange(Collection_method) %>%
  mutate(
    sitenewname = fct_reorder(sitenewname, Latitude)
  ) %>%
  ggplot() +
  geom_segment(
    aes(
      y = window.open,
      yend = window.close,
      x = sitenewname,
      col = slope.estimate
    ),
    linewidth = 2
  ) +
  facet_grid(Collection_method ~ ., scale = "free") +
  coord_flip() +
  geom_hline(yintercept = 133, color = 'black', linetype = 'dashed') +
  geom_hline(yintercept = 498, color = 'black', linetype = 'dashed') +
  ylim(0, 600) +
  ggpubr::theme_cleveland() +
  scale_color_viridis_c(
    option = "mako",
    limits = c(0, 0.7)
  ) +
  scale_color_gradient2(
    low = "blue",
    mid = "white", # Color for the midpoint
    high = "red",
    midpoint = 0
  ) +
  labs(colour = "Slope (std)") +
  theme(
    legend.position = 'right',
    #legend.title = element_blank(),
    panel.background = element_rect(fill = 'white', colour = 'white')
  ) +
  ylab('Days before seed fall (0 = Nov 1 of seed-fall year).')
csp.all.sites

cowplot::save_plot(
  here("Application_MASTREE/figures/csp.all.png"),
  csp.all.sites,
  ncol = 1.5,
  nrow = 1.8,
  dpi = 300
)


###############
#now focus on peak detection method
#map dfr is enough, quite fast if not optimize gam model
statistics_basic_method = map_dfr(
  1:length(Results_daily),
  ~ ATS_peak_detection(
    Results_daily[[.]],
    unique(Results_daily[[.]]$plotname.lon.lat),
    bio_data_all = Fagus.seed,
    climate.path = climate.beech.path,
    refday = 305,
    lag = 100,
    threshold = 3,
    lastdays = max(range),
    rollwin = 1
  )
)
qs::qsave(
  statistics_basic_method,
  here('Application_MASTREE/outputs/statistics_basic.qs')
)

output_fit_summary.basic.best = statistics_basic_method %>%
  group_by(sitenewname) %>%
  slice(which.max(r2)) %>%
  ungroup()
#some stat
quibble2(output_fit_summary.basic.best$window.open, q = c(0.25, 0.5, 0.75))
quibble2(output_fit_summary.basic.best$window.close, q = c(0.25, 0.5, 0.75))

#now make the figure
# basic.all.sites = output_fit_summary.basic.best %>%
#   left_join(methods.collection.mv2) %>%
#   left_join(
#     Fagus.seed %>%
#       dplyr::select(
#         Country,
#         Alpha_Number,
#         Latitude,
#         Longitude,
#         plotname.lon.lat,
#         sitenewname
#       ) %>%
#       distinct()
#   ) %>%
#   mutate(
#     sitenewname = paste0(Country, "_", Alpha_Number, "_", round(Latitude, 4))
#   ) %>%
#   arrange(Collection_method) %>%
#   mutate(
#     sitenewname = fct_reorder(sitenewname, as.character(Collection_method))
#   ) %>%
#   ggplot() +
#   geom_segment(
#     aes(
#       y = window.open,
#       yend = window.close,
#       x = sitenewname,
#       col = Collection_method
#     ),
#     size = 2
#   ) +
#   coord_flip() +
#   geom_hline(yintercept = 133, color = 'black', linetype = 'dashed') +
#   geom_hline(yintercept = 498, color = 'black', linetype = 'dashed') +
#   ylim(0, 600) +
#   scale_color_futurama() +
#   scale_fill_futurama() +
#   ggpubr::theme_cleveland() +
#   theme(legend.position = 'bottom', legend.title = element_blank()) +
#   ylab('Days before seed fall (0 = Nov 1 of seed-fall year).')

basic.all.sites = output_fit_summary.basic.best %>%
  left_join(methods.collection.mv2) %>%
  ungroup() %>%
  arrange(Collection_method) %>%
  left_join(
    Fagus.seed %>%
      dplyr::select(
        Country,
        Alpha_Number,
        Latitude,
        Longitude,
        plotname.lon.lat,
        sitenewname
      ) %>%
      distinct()
  ) %>%
  mutate(
    sitenewname = paste0(Country, "_", round(Latitude, 4))
  ) %>%
  arrange(Collection_method) %>%
  mutate(
    sitenewname = fct_reorder(sitenewname, Latitude)
  ) %>%
  ggplot() +
  geom_segment(
    aes(
      y = window.open,
      yend = window.close,
      x = sitenewname,
      col = slope.estimate
    ),
    linewidth = 2
  ) +
  facet_grid(Collection_method ~ ., scale = "free") +
  coord_flip() +
  geom_hline(yintercept = 133, color = 'black', linetype = 'dashed') +
  geom_hline(yintercept = 498, color = 'black', linetype = 'dashed') +
  ylim(0, 600) +
  ggpubr::theme_cleveland() +
  scale_color_viridis_c(
    option = "mako",
    limits = c(0, 0.7)
  ) +
  scale_color_gradient2(
    low = "blue",
    mid = "white", # Color for the midpoint
    high = "red",
    midpoint = 0
  ) +
  labs(colour = "Slope (std)") +
  theme(
    legend.position = 'right',
    #legend.title = element_blank(),
    panel.background = element_rect(fill = 'white', colour = 'white')
  ) +
  ylab('Days before seed fall (0 = Nov 1 of seed-fall year).')
basic.all.sites
cowplot::save_plot(
  here("Application_MASTREE/figures/peak.detection.all.png"),
  basic.all.sites,
  ncol = 1.5,
  nrow = 1.5,
  dpi = 300
)

#################################################################
#################################################################
#############P spline regression method
#################################################################
#################################################################
#here it will fit gam using p and b spline
statistics_psr_method = map_dfr(
  beech.site.all,
  ~ ATS_PSR(
    site = .x,
    climate.path = climate.beech.path,
    bio_data_all = Fagus.seed, # Use .x correctly here
    lastdays = max(range),
    matrice = c(3, 1),
    refday = 305,
    knots = NULL,
    tolerancedays = 7
  )
)

qs::qsave(
  statistics_psr_method,
  here('Application_MASTREE/outputs/statistics_psr.qs')
)

output_fit_summary.psr.best = statistics_psr_method %>%
  group_by(sitenewname) %>%
  slice(which.max(r2))
#some stats
quibble2(output_fit_summary.psr.best$window.close, q = c(0.25, 0.5, 0.75))
quibble2(output_fit_summary.psr.best$window.open, q = c(0.25, 0.5, 0.75))

#in the previous version
# psr.all.sites = output_fit_summary.psr.best %>%
#   left_join(methods.collection.mv2) %>%
#   ungroup() %>%
#   arrange(Collection_method) %>%
#   left_join(
#     Fagus.seed %>%
#       dplyr::select(
#         Country,
#         Alpha_Number,
#         Latitude,
#         Longitude,
#         plotname.lon.lat,
#         sitenewname
#       ) %>%
#       distinct()
#   ) %>%
#   mutate(
#     sitenewname = paste0(Country, "_", Alpha_Number, "_", round(Latitude, 4))
#   ) %>%
#   arrange(Collection_method) %>%
#   mutate(
#     sitenewname = fct_reorder(sitenewname, as.character(Collection_method))
#   ) %>%
#   ggplot() +
#   geom_segment(
#     aes(
#       y = window.open,
#       yend = window.close,
#       x = sitenewname,
#       col = Collection_method
#     ),
#     size = 2
#   ) +
#   geom_point(
#     aes(y = window.open, x = sitenewname, col = Collection_method),
#     size = 2,
#     shape = 15
#   ) +
#   coord_flip() +
#   geom_hline(yintercept = 133, color = 'black', linetype = 'dashed') +
#   geom_hline(yintercept = 498, color = 'black', linetype = 'dashed') +
#   ylim(0, 600) +
#   scale_color_futurama() +
#   scale_fill_futurama() +
#   ggpubr::theme_cleveland() +
#   theme(legend.position = 'bottom', legend.title = element_blank()) +
#   ylab('Days before seed fall (0 = Nov 1 of seed-fall year).')
# psr.all.sites

psr.all.sites = output_fit_summary.psr.best %>%
  left_join(methods.collection.mv2) %>%
  ungroup() %>%
  arrange(Collection_method) %>%
  left_join(
    Fagus.seed %>%
      dplyr::select(
        Country,
        Alpha_Number,
        Latitude,
        Longitude,
        plotname.lon.lat,
        sitenewname
      ) %>%
      distinct()
  ) %>%
  mutate(
    sitenewname = paste0(Country, "_", round(Latitude, 4))
  ) %>%
  arrange(Collection_method) %>%
  mutate(
    sitenewname = fct_reorder(sitenewname, Latitude)
  ) %>%
  ggplot() +
  geom_segment(
    aes(
      y = window.open,
      yend = window.close,
      x = sitenewname,
      col = slope.estimate
    ),
    linewidth = 2
  ) +
  facet_grid(Collection_method ~ ., scale = "free") +
  coord_flip() +
  geom_hline(yintercept = 133, color = 'black', linetype = 'dashed') +
  geom_hline(yintercept = 498, color = 'black', linetype = 'dashed') +
  ylim(0, 600) +
  ggpubr::theme_cleveland() +
  scale_color_viridis_c(
    option = "mako",
    limits = c(0, 0.7)
  ) +
  scale_color_gradient2(
    low = "blue",
    mid = "white", # Color for the midpoint
    high = "red",
    midpoint = 0
  ) +
  labs(colour = "Slope (std)") +
  theme(
    legend.position = 'right',
    #legend.title = element_blank(),
    panel.background = element_rect(fill = 'white', colour = 'white')
  ) +
  ylab('Days before seed fall (0 = Nov 1 of seed-fall year).')
psr.all.sites

cowplot::save_plot(
  here("Application_MASTREE/figures/psr.all.png"),
  psr.all.sites,
  ncol = 1.5,
  nrow = 1.8,
  dpi = 300
)

#####################################################################################
#main figure results method
windows.avg = output_fit_summary.basic.best %>%
  dplyr::select(sitenewname, window.open, window.close) %>%
  mutate(method = 'Peak signal detection') %>%
  bind_rows(
    output_fit_summary.psr.best %>%
      dplyr::select(sitenewname, window.open, window.close) %>%
      mutate(method = 'P-spline regression')
  ) %>%
  bind_rows(
    statistics_absolute_climwin %>%
      dplyr::select(sitenewname, window.open, window.close) %>%
      mutate(method = 'Sliding window')
  ) %>%
  bind_rows(
    output_fit_summary.best.csp %>%
      dplyr::select(sitenewname, window.open, window.close) %>%
      mutate(method = 'Climate sensitivity profile')
  ) %>%
  group_by(method) %>%
  summarise(
    wind.open_median = median(window.open, na.rm = TRUE),
    wind.close_median = median(window.close, na.rm = TRUE),
    wind.close_q25 = quantile(window.close, 0.25, na.rm = TRUE),
    wind.close_q75 = quantile(window.close, 0.75, na.rm = TRUE),
    wind.open_q25 = quantile(window.open, 0.25, na.rm = TRUE),
    wind.open_q75 = quantile(window.open, 0.75, na.rm = TRUE),
    wind.open_mean = mean(window.open),
    wind.close_mean = mean(window.close),
    wind.open_se = se(window.open),
    wind.close_se = se(window.close),
    n.obs = n()
  )
#get nb of observation per method
n_per_method <- windows.avg %>%
  dplyr::select(method, wind.open_q75, n.obs) %>%
  mutate(label = paste0("n=", n.obs), y_position = wind.open_q75 + 30) %>%
  mutate(label = paste0("italic('", label, "')"))


median.windows.plot = windows.avg %>%
  dplyr::select(method, wind.open_median:wind.open_q75) %>%
  pivot_longer(starts_with('wind'), names_to = 'typewind') %>%
  separate_wider_delim(
    typewind,
    delim = '_',
    names = c('windows.type', 'metric')
  ) %>%
  pivot_wider(names_from = 'metric', values_from = value) %>%
  mutate(
    method = factor(
      method,
      levels = rev(c(
        "Sliding window",
        "Peak signal detection",
        "Climate sensitivity profile",
        "P-spline regression"
      ))
    ),
    windows.type = factor(
      dplyr::recode(
        as_factor(windows.type),
        "wind.open" = "Open",
        "wind.close" = "Close"
      ),
      levels = c("Open", "Close")
    )
  ) %>%
  ggplot(aes(
    x = method,
    group = windows.type,
    col = windows.type,
    shape = windows.type
  )) +
  geom_rect(
    aes(ymin = 519, ymax = 428, xmin = -Inf, xmax = Inf),
    fill = 'grey',
    col = 'white',
    alpha = 0.05
  ) +
  geom_point(
    aes(y = median),
    size = 2,
    position = position_dodge(width = 0.2)
  ) +
  geom_pointrange(
    mapping = aes(y = median, ymin = q25, ymax = q75),
    position = position_dodge(width = 0.2)
  ) +
  coord_flip() +
  xlab('') +
  ylab('Days before seed fall (0 = Nov 1 of seed-fall year).') +
  scale_color_manual(values = c("Open" = "#56B4E9", "Close" = "#D55E00")) +
  scale_shape_manual(values = c("Open" = 15, "Close" = 19)) +
  ggpubr::theme_pubr() +
  theme(
    legend.position = c(.25, .2),
    legend.title = element_blank(),
    legend.text = element_text(size = 12),
    axis.text = element_text(size = 13),
    axis.title = element_text(size = 14)
  ) +
  geom_hline(yintercept = 498, color = 'black', linetype = 'dashed') +
  ylim(200, 600) +
  geom_text(
    data = n_per_method,
    aes(
      x = method,
      y = y_position,
      label = label
    ),
    inherit.aes = F,
    size = 4,
    parse = TRUE
  )

median.windows.plot

#GENERIC PLOT R2 and AIC
bestr2 = output_fit_summary.basic.best %>%
  dplyr::select(sitenewname, r2, AIC) %>%
  mutate(method = 'Peak signal detection') %>%
  bind_rows(
    output_fit_summary.psr.best %>%
      dplyr::select(sitenewname, r2, AIC) %>%
      mutate(method = 'P-spline regression')
  ) %>%
  bind_rows(
    statistics_absolute_climwin %>%
      dplyr::select(sitenewname, r2, AIC) %>%
      mutate(method = 'Sliding window')
  ) %>%
  bind_rows(
    output_fit_summary.best.csp %>%
      dplyr::select(sitenewname, r2, AIC) %>%
      mutate(method = 'Climate sensitivity profile')
  )

#get qunatile and other stat
bestr2 %>%
  group_by(method) %>%
  summarise(
    mean_r2 = round(median(r2, na.rm = TRUE), 2),
    lq = round(quantile(r2, 0.25, na.rm = TRUE), 2),
    hq = round(quantile(r2, 0.75, na.rm = TRUE), 2)
  )

#for density plot
mean_r2 <- bestr2 %>%
  group_by(method) %>%
  summarise(
    median_r2 = median(r2, na.rm = TRUE),
    mean_r2 = mean(r2, na.rm = T)
  ) %>%
  arrange(mean_r2)
mean_r2$y_label <- seq(5.4, 6.2, length.out = nrow(mean_r2))

averagedensr2method = bestr2 %>%
  ggplot(aes(x = r2, fill = method, col = method)) +
  geom_density(alpha = 0.2, size = .8) +
  labs(x = expression(R^2), y = "Density") +
  ggpubr::theme_pubclean() +
  geom_vline(
    data = mean_r2,
    aes(color = method, xintercept = mean_r2), #label = round(mean_r2, 3), with geomtextpath::
    #vjust = -0.3,
    #hjust = 1,
    #fontface = 'bold',
    linetype = 'dashed',
    show.legend = FALSE
  ) +
  scale_color_viridis_d(name = 'Method') +
  scale_fill_viridis_d(name = 'Method') +
  guides(col = guide_legend(ncol = 2)) +
  theme(
    legend.position = "bottom",
    legend.box = "vertical",
    legend.margin = margin(),
    legend.text = element_text(size = 12),
    axis.text = element_text(size = 13),
    axis.title = element_text(size = 14)
  ) +
  geom_text(
    data = mean_r2,
    aes(
      x = mean_r2,
      y = y_label,
      label = round(mean_r2, 3),
      color = method
    ),
    vjust = -0.5,
    angle = 0,
    fontface = "bold",
    show.legend = FALSE
  )
averagedensr2method

#make a plot for main figure
cowplot::save_plot(
  here("Application_MASTREE/figures/averager2.method.png"),
  averagedensr2method +
    median.windows.plot +
    plot_annotation(tag_levels = 'a') &
    theme(plot.tag = element_text(size = 12)),
  ncol = 1.9,
  nrow = 1.4,
  dpi = 300
)

###################
cliwin.performance.plot = climwin.dd %>%
  dplyr::select(sitenewname, name, days.reversed) %>%
  pivot_wider(names_from = "name", values_from = "days.reversed") %>%
  left_join(methods.collection.mv2) %>%
  left_join(
    statistics_absolute_climwin %>%
      dplyr::select(sitenewname, slope.estimate, r2)
  ) %>%
  rename(`R2 sliding \nwindow` = r2) %>%
  rename(`Slope (Std) \nsliding window` = slope.estimate) %>%
  left_join(
    Fagus.seed %>%
      dplyr::select(
        Country,
        Alpha_Number,
        Latitude,
        Longitude,
        plotname.lon.lat,
        sitenewname,
        n
      ) %>%
      distinct()
  ) %>%
  mutate(
    sitenewname = paste0(round(Latitude, 4), ' (N=', n, ")") #Country, "_",
  ) %>%
  arrange(Collection_method) %>%
  mutate(
    sitenewname = fct_reorder(sitenewname, Latitude)
  ) %>%
  ggplot() +
  geom_segment(
    aes(
      y = window.open,
      yend = window.close,
      x = sitenewname,
      col = `Slope (Std) \nsliding window`
      #alpha = `R2 sliding \nwindow`,
    ),
    linewidth = 2
  ) +
  coord_flip() +
  geom_hline(yintercept = 133, color = 'black', linetype = 'dashed') +
  geom_hline(yintercept = 498, color = 'black', linetype = 'dashed') +
  ylim(0, 600) +
  ggpubr::theme_cleveland() +
  #scale_color_viridis_c(option = "mako") +
  scale_color_gradient2(
    low = "blue", # Color for low values
    mid = "white", # Color for the midpoint
    high = "red", # Color for high values
    midpoint = 0 # Set the midpoint at data value 0
  ) +
  theme(
    legend.position = 'right',
    #legend.title = element_blank(),
    panel.background = element_rect(fill = 'white', colour = 'white')
  ) +
  ylab('Days before seed fall (0 = Nov 1 of seed-fall year).')
cliwin.performance.plot

cowplot::save_plot(
  here("Application_MASTREE/figures/averager2.method.png"),
  ((averagedensr2method +
    median.windows.plot) /
    cliwin.performance.plot) +
    plot_annotation(tag_levels = 'a') &
    theme(plot.tag = element_text(size = 12)),
  ncol = 2.2,
  nrow = 3,
  dpi = 300
)

cowplot::save_plot(
  here::here("Application_MASTREE/figures/averager2.method.png"),
  ((averagedensr2method + median.windows.plot) /
    cliwin.performance.plot +
    plot_layout(heights = c(.7, 1))) + # <-- Adjust this ratio
    plot_annotation(tag_levels = 'a') &
    theme(plot.tag = element_text(size = 12)),
  ncol = 2.1,
  nrow = 2.9,
  dpi = 300
)
#################################################################
#################################################################
#############Evaluation of time series length on cue identification
#################################################################
#################################################################
#take more than 12 hours if 10 iteration (climwin takes some times)
#here for 50 iterations it took me arround 4 days

# Initialize a list to store all results
#focus only on long term series, and using  a for loop. It is possible sometimes
#than you have an error, related to the sampling consecutive process
#because some times series have gap inside time series (for example you will go from 2000 till 2005, but no sampling in 2003)
#if you have this, please do not erase result_list, and just change 1:n_iter, and replace 1 where you got the crash
dont.run.this.long.process = T

if (dont.run.this.long.process == F) {
  subset.long.term = Fagus.seed %>% filter(n > 50)
  beech.site.subset.longterm = Fagus.seed %>%
    filter(n > 50) %>%
    dplyr::select(plotname.lon.lat) %>%
    distinct() %>%
    as.matrix()

  #test one site
  # subset.long.term = Fagus.seed %>%
  # filter(plotname.lon.lat == "longitude=19.51_latitude=51.82")
  # beech.site.subset.longterm = c("longitude=19.51_latitude=51.82")

  n_iter <- 50
  result_list <- list()
  for (i in 1:n_iter) {
    sample_result = run_sampling_fraction_all_methods(
      years_to_extract = c(5, 10, 15, 20, 25, 30),
      range,
      bio.data = subset.long.term,
      site.needed = beech.site.subset.longterm,
      climate.path = climate.beech.path,
      matrix.psr = c(2, 1) #if lower it would not work if sample size is way too small
    )
    merged_df <- imap_dfr(sample_result, function(sublist, name) {
      bind_rows(lapply(sublist, function(df) {
        df %>% mutate(source = name) # Add column with list name
      }))
    }) %>%
      group_by(method, source, sitenewname) %>%
      mutate(
        r2_highest = ifelse(
          method %in% c('csp', 'psr', 'signal'),
          r2 == max(r2),
          T
        ) #because it gave us all R2, and I just want the best one
      ) %>%
      dplyr::filter(r2_highest)

    result_list[[i]] <- merged_df
  }
  qs::qsave(
    result_list,
    here(paste0(
      "Application_MASTREE/outputs/year.selection.testing",
      today(),
      ".qs"
    ))
  )
} else {
  #I am loading the last longest version...
  result_list = qs::qread(here(
    "Application_MASTREE/outputs/year.selection.testing.2025-05-20.qs"
  ))
}

#combine list resuts
summary.sample.size = rbind(
  result_list
) %>%
  bind_rows() %>%
  mutate(
    method = recode(
      method,
      "climwin" = "Sliding window",
      "csp" = "Climate sensitivity profile",
      "psr" = "P-spline regression",
      'signal' = 'Peak signal detection'
    ),
    source = recode(
      source,
      "years_5" = 5,
      "years_10" = 10,
      "years_15" = 15,
      'years_20' = 20,
      'years_25' = 25,
      'years_30' = 30
    )
  ) %>%
  group_by(method, source) %>%
  dplyr::slice_sample(n = 540) %>%
  dplyr::select(method, source, window.open, window.close) %>%
  summarise(
    wind.open_median = median(window.open, na.rm = TRUE),
    wind.close_median = median(window.close, na.rm = TRUE),
    wind.close_q25 = quantile(window.close, 0.25, na.rm = TRUE),
    wind.close_q75 = quantile(window.close, 0.75, na.rm = TRUE),
    wind.open_q25 = quantile(window.open, 0.25, na.rm = TRUE),
    wind.open_q75 = quantile(window.open, 0.75, na.rm = TRUE),
    wind.open_mean = mean(window.open, na.rm = TRUE),
    wind.close_mean = mean(window.close, na.rm = T),
    wind.open_se = se(window.open),
    wind.close_se = se(window.close),
    n = n()
  )

#reformat for table in SI
summary.sample.size_formatted <- summary.sample.size %>%
  mutate(
    wind_open_IQR = paste0(
      round(wind.open_median),
      " [",
      round(wind.open_q25),
      ", ",
      round(wind.open_q75),
      "]"
    ),
    wind_close_IQR = paste0(
      round(wind.close_median),
      " [",
      round(wind.close_q25),
      ", ",
      round(wind.close_q75),
      "]"
    )
  ) %>%
  select(method, source, wind_open_IQR, wind_close_IQR)

# Create LaTeX table
kable(
  summary.sample.size_formatted,
  format = "latex",
  booktabs = TRUE,
  col.names = c("Method", "Length Time series", "Window Open", "Window Close"),
  longtable = TRUE
) %>%
  kable_styling(latex_options = c("hold_position"))


sensi.data.plot = summary.sample.size %>%
  dplyr::select(method, n, source, wind.open_median:wind.open_q75) %>%
  pivot_longer(starts_with('wind'), names_to = 'typewind') %>%
  separate_wider_delim(
    typewind,
    delim = '_',
    names = c('windows.type', 'metric')
  ) %>%
  pivot_wider(names_from = 'metric', values_from = value) %>%
  mutate(
    source = as.numeric(source),
    method_sample = paste(method, source, sep = " (n=") %>% paste0(")"),
    sample_fraction_factor = as_factor(paste0(source, ' years'))
  ) %>%
  mutate(
    n = ifelse(windows.type == "wind.close", NA, n),
    windows.type = factor(
      dplyr::recode(
        as_factor(windows.type),
        "wind.open" = "Open",
        "wind.close" = "Close"
      ),
      levels = c("Close", "Open")
    )
  ) %>%
  mutate(
    method = factor(
      method,
      levels = c(
        "Sliding window",
        "Peak signal detection",
        "Climate sensitivity profile",
        "P-spline regression"
      )
    )
  ) %>%
  ggplot(aes(
    x = sample_fraction_factor,
    group = windows.type,
    col = windows.type,
    shape = windows.type
  )) +
  geom_rect(
    aes(ymin = 519, ymax = 428, xmin = -Inf, xmax = Inf),
    fill = 'grey',
    col = 'white',
    alpha = 0.05
  ) +
  geom_point(
    aes(y = median),
    size = 2,
    position = position_dodge(width = 0.5)
  ) +
  geom_pointrange(
    mapping = aes(y = median, ymin = q25, ymax = q75),
    position = position_dodge(width = 0.5)
  ) +
  coord_flip() +
  geom_text(
    aes(
      x = sample_fraction_factor,
      y = q25 - 10,
      label = ifelse(!is.na(n), paste0("italic('n = ", n, "')"), NA)
    ),
    inherit.aes = F,
    parse = T,
    size = 3
  ) +
  facet_wrap(. ~ method, scales = 'free_y') +
  xlab('') +
  ylab('Days before seed fall (0 = Nov 1 of seed-fall year).') +
  theme(legend.position = 'bottom', legend.title = element_blank()) +
  geom_hline(yintercept = 498, color = 'black', linetype = 'dashed') +
  scale_color_manual(values = c("Open" = "#56B4E9", "Close" = "#D55E00")) +
  scale_shape_manual(values = c("Open" = 15, "Close" = 19)) +
  ggpubr::theme_pubr() +
  theme(
    legend.position = 'bottom',
    legend.title = element_blank(),
    legend.text = element_text(size = 12),
    axis.text = element_text(size = 13),
    axis.title = element_text(size = 14)
  )
sensi.data.plot
cowplot::save_plot(
  here("Application_MASTREE/figures/sensitivity.method.png"),
  sensi.data.plot,
  ncol = 1.5,
  nrow = 1.4,
  dpi = 300
)

#for reporting results in main text
summary.sample.size %>% filter(source == 20)
summary.sample.size %>%
  filter(method == "Climate sensitivity profile") %>%
  View()
summary.sample.size %>% filter(method == "Sliding window") %>% View()
summary.sample.size %>%
  filter(method == "Peak signal identification") %>%
  View()

#################################################################
#################################################################
#############Block cross validation
#################################################################
#################################################################
#CLEANED VERSION
#take 1 day (more 12 hours)
#the idea here is to subset 5 pop with more than 30 years of data, after 1980, and from the three methods
#seed count, mostly in UK, seed trap and visual crop assement

dont.run.block.cross.becausetoolong = T

if (dont.run.block.cross.becausetoolong == F) {
  num_blocks <- 5
  test_block_nb = 2
  num_iterations <- 10 # Specify the number of iterations

  #final output interested in
  output_fit_summary_cv_fin = NULL

  #restricted this analysis to specific collection method sampled
  population.matching.random.5methods = Fagus.seed %>%
    filter(Year > 1980) %>%
    group_by(sitenewname, Collection_method) %>%
    summarise(years = list(unique(Year)), .groups = "drop") %>%
    mutate(
      has_30 = map_lgl(
        years,
        ~ length(.) >= 30 && !is.null(get_30_year_block(.))
      )
    ) %>%
    filter(has_30) %>%
    group_by(Collection_method) %>%
    slice_sample(n = 5)

  filtered_data <- Fagus.seed %>%
    filter(Year > 1980) %>%
    semi_join(
      population.matching.random.5methods,
      by = c("sitenewname", "Collection_method")
    ) %>%
    group_by(sitenewname, Collection_method) %>%
    group_modify(
      ~ {
        block_years <- get_30_year_block(.x$Year)
        if (is.null(block_years)) return(tibble()) # Return empty if no valid block
        .x %>% filter(Year %in% block_years)
      }
    ) %>%
    ungroup()

  #looks OK
  filtered_data %>%
    group_by(sitenewname) %>%
    summarise(
      min_year = min(Year, na.rm = TRUE),
      max_year = max(Year, na.rm = TRUE),
    )

  beech.site.all.5method = unique(filtered_data$plotname.lon.lat)
  save.bc = TRUE

  for (i in 1:length(beech.site.all.5method)) {
    output_fit_summary_cv_fin_part1 = NULL

    site.cv <- filtered_data %>%
      filter(plotname.lon.lat == beech.site.all.5method[i])

    # Make 5 blocks of data
    blocks <- create_blocks(site.cv, num_blocks)

    # Perform cross-validation multiple times
    for (j in 1:num_iterations) {
      block_indices <- seq_along(blocks) # Should be the same as block size
      test_indices <- sample(
        block_indices,
        size = test_block_nb,
        replace = FALSE
      ) # Randomly select 2 validation blocks
      train_indices <- setdiff(block_indices, test_indices) # Remaining blocks for training

      # Combine the block of training
      train_blocks <- do.call(rbind, blocks[train_indices]) # Combine training blocks
      validation_blocks <- do.call(rbind, blocks[test_indices]) # Combine validation blocks

      climate_data <- format_climate_data(
        site = beech.site.all.5method[i],
        path = climate.beech.path,
        scale.climate = T,
        date_column = "DATEB"
      )

      # Run climwin per site
      climwin1 <- weatheRcues::runing_climwin(
        climate_data = climate_data,
        bio_data = train_blocks,
        site.name = beech.site.all.5method[i],
        range = range, #should be a vector with 2 value
        cinterval = 'day',
        refday = c(01, 11),
        optionwindows = 'absolute',
        climate_var = 'TMEAN',
      ) %>%
        dplyr::mutate(method = 'climwin')

      psr1 <- runing_psr(
        bio_data = train_blocks,
        site = beech.site.all.5method[i],
        climate_data = climate_data,
        lastdays = max(range),
        refday = refday,
        rollwin = 1,
        formula_model = formula('log.seed ~ TMEAN'),
        matrice = c(3, 1),
        knots = NULL,
      ) %>%
        dplyr::slice(which.max(r2)) %>%
        dplyr::mutate(method = 'psr')

      moving.site1 <- runing_daily_relationship(
        bio_data = train_blocks,
        climate_data = climate_data,
        lastdays = lastdays,
        refday = 305,
        formula_model = formula('log.seed ~ TMEAN'),
        model_type = 'lm'
      )

      csp1 <- runing_csp(
        Results_days = moving.site1,
        bio_data = train_blocks,
        siteneame.forsub = beech.site.all.5method[i],
        option.check.name = TRUE,
        climate_data = climate_data,
        refday = 305,
        lastdays = max(range),
        rollwin = 1,
        myform.fin = formula('log.seed ~ TMEAN'),
        model_type = 'lm',
        optim.k = F
      ) %>%
        dplyr::slice(which.max(r2)) %>%
        dplyr::mutate(method = 'csp')

      basic1 <- runing_peak_detection(
        refday = 305,
        lastdays = max(range),
        rollwin = 1,
        siteforsub = beech.site.all[i],
        climate_data = climate_data,
        Results_days = moving.site1,
        formula_model = formula('log.seed ~ TMEAN'),
        model_type = 'lm',
        bio_data = train_blocks
      ) %>%
        dplyr::slice(which.max(r2)) %>%
        dplyr::mutate(method = 'signal')

      temp.win <- bind_rows(climwin1, psr1, csp1, basic1) %>%
        dplyr::select(
          window.open,
          window.close,
          intercept.estimate,
          slope.estimate,
          method
        )

      yearneed <- 2
      yearperiod <- (min(climate_data$year) + yearneed):max(climate_data$year)

      # Apply the function across all years in yearperiod and combine results
      rolling.data <- map_dfr(
        yearperiod,
        reformat_climate_backtothepast, #year period would be used as yearref
        climate_data = climate_data,
        yearneed = yearneed,
        refday = refday,
        lastdays = lastdays,
        rollwin = 1,
        covariates.of.interest = 'TMEAN'
      )

      output_fit_summary.temp <- purrr::map_dfr(
        1:nrow(temp.win),
        ~ cross_validation_outputs_windows_modelling(
          .,
          bio_data = validation_blocks,
          window_ranges_df = temp.win,
          rolling.data = rolling.data,
          formula_model = formula('log.seed ~ TMEAN')
        )
      )
      #here i saved it to double check in case of any mistake
      output_fit_summary_cv_fin_part1 <- bind_rows(
        output_fit_summary_cv_fin_part1,
        output_fit_summary.temp
      )
    }
    output_fit_summary_cv_fin = bind_rows(
      output_fit_summary_cv_fin,
      output_fit_summary_cv_fin_part1
    )
  }
  if (save.bc == T) {
    qs::qsave(
      output_fit_summary_cv_fin,
      here(paste0(
        'Application_MASTREE/outputs/outputs_blockcrosstotal_',
        num_blocks,
        '_train_',
        test_block_nb,
        "_",
        lubridate::today(),
        '.qs'
      ))
    )
  }
} else {
  output_fit_summary_cv_fin = qs::qread(
    here(
      "Application_MASTREE/outputs/outputs_blockcrosstotalv2504_5_train_2.qs"
    )
  )
}


formatting.data.block.cross = output_fit_summary_cv_fin %>%
  distinct() %>%
  as_tibble() %>%
  drop_na(mae) %>%
  left_join(methods.collection.mv2) %>%
  dplyr::mutate(
    mae = as.numeric(mae)
  ) %>%
  mutate(
    method.cues = recode(
      method.cues,
      "climwin" = "Sliding window",
      "csp" = "Climate sensitivity profile",
      "psr" = "P-spline regression",
      'signal' = 'Peak signal detection'
    )
  )

formatting.data.block.cross %>%
  group_by(method.cues) %>%
  summarise(
    grand_mean_r2 = mean(r2.validation, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(grand_mean_r2)

#make figure
block.cross.figure =
  formatting.data.block.cross %>%
  mutate(
    method.cues = factor(
      method.cues,
      levels = c(
        "Sliding window",
        "Peak signal detection",
        "Climate sensitivity profile",
        "P-spline regression"
      )
    )
  ) %>%
  ggplot(aes(
    x = Collection_method,
    y = r2.validation,
    fill = Collection_method
  )) +
  gghalves::geom_half_boxplot(
    center = T,
    errorbar.draw = FALSE,
    width = 0.8,
    nudge = 0.02,
    alpha = .9
  ) +
  gghalves::geom_half_violin(
    side = "r",
    nudge = 0.02,
    alpha = .65,
    col = "black"
  ) +
  ggsignif::geom_signif(
    comparisons = list(
      c("Seed trap", "Seed count"),
      c("Seed trap", "Visual crop"),
      c("Seed count", "Visual crop")
    ),
    map_signif_level = TRUE,
    test.args = list(exact = FALSE),
    step_increase = 0.1
  ) +
  facet_grid(. ~ method.cues) +
  geom_vline(xintercept = 0) +
  scale_color_futurama() +
  scale_fill_futurama() +
  ggpubr::theme_pubclean() +
  geom_hline(yintercept = 0) +
  theme(
    legend.position = 'none',
    legend.title = element_blank(),
    axis.text.x = element_text(angle = 90, size = 12)
  ) +
  xlab('') +
  ylim(0, 1.05) +
  ylab(expression(R^2))
block.cross.figure

cowplot::save_plot(
  here("Application_MASTREE/figures/Figure.block.png"),
  block.cross.figure,
  ncol = 1.6,
  nrow = 1.4,
  dpi = 300
)

block.cross.figure.mae =
  formatting.data.block.cross %>%
  mutate(
    method.cues = factor(
      method.cues,
      levels = c(
        "Sliding window",
        "Peak signal detection",
        "Climate sensitivity profile",
        "P-spline regression"
      )
    )
  ) %>%
  ggplot(aes(
    x = Collection_method,
    y = scaled.rmse,
    fill = Collection_method
  )) +
  gghalves::geom_half_boxplot(
    center = T,
    errorbar.draw = FALSE,
    width = 0.8,
    nudge = 0.02,
    alpha = .9
  ) +
  gghalves::geom_half_violin(
    side = "r",
    nudge = 0.02,
    alpha = .65,
    col = "black"
  ) +
  ggsignif::geom_signif(
    comparisons = list(
      c("Seed trap", "Seed count"),
      c("Seed trap", "Visual crop"),
      c("Seed count", "Visual crop")
    ),
    test.args = list(exact = FALSE),
    map_signif_level = TRUE,
    step_increase = 0.1
  ) +
  facet_grid(. ~ method.cues) +
  scale_color_futurama() +
  scale_fill_futurama() +
  ggpubr::theme_pubclean() +
  theme(
    legend.position = 'none',
    legend.title = element_blank(),
    axis.text.x = element_text(angle = 90, size = 12)
  ) +
  xlab('') +
  ylab("normalised RMSE")
block.cross.figure.mae

cowplot::save_plot(
  here("Application_MASTREE/figures/Figure.block.MAE.png"),
  block.cross.figure.mae,
  ncol = 1.6,
  nrow = 1.4,
  dpi = 300
)

#now summary of block
summary_table_bc <- formatting.data.block.cross %>%
  group_by(method.cues, Collection_method) %>%
  summarise(
    wind.open = paste0(
      round(median(window.open, na.rm = TRUE)),
      " [",
      round(quantile(window.open, 0.25, na.rm = TRUE)),
      ", ",
      round(quantile(window.open, 0.75, na.rm = TRUE)),
      "]"
    ),
    wind.close = paste0(
      round(median(window.close, na.rm = TRUE)),
      " [",
      round(quantile(window.close, 0.25, na.rm = TRUE)),
      ", ",
      round(quantile(window.close, 0.75, na.rm = TRUE)),
      "]"
    ),
    r2 = paste0(
      round(mean(r2.validation, na.rm = TRUE), 2),
      " ± ",
      round(sd(r2.validation, na.rm = TRUE), 2)
    ),
    rmse = paste0(
      round(mean(scaled.rmse, na.rm = TRUE), 2),
      " ± ",
      round(sd(scaled.rmse, na.rm = TRUE), 2)
    ),
    .groups = "drop"
  ) %>%
  tidyr::pivot_longer(
    cols = c(wind.open, wind.close),
    names_to = "WindowType",
    values_to = "Window"
  ) %>%
  dplyr::mutate(
    WindowType = ifelse(WindowType == "wind.open", "Opening", "Closing")
  ) %>%
  dplyr::select(method.cues, Collection_method, WindowType, Window, r2, rmse)

#get latex tab
summary_table_bc %>%
  arrange(method.cues, Collection_method, WindowType) %>%
  kbl(
    format = "latex",
    booktabs = TRUE,
    linesep = "",
    col.names = c(
      "Weather cues method",
      "Seed collection method",
      "Window",
      "Median [IQR]",
      "$R^2$",
      "$nRMSE$"
    ),
    caption = "Summary results of cross-validation of window identification and model performance."
  )


#################################################################
#################################################################
#############Simulation study case
#################################################################
#################################################################
#first simulate fake data and window
# Simulate climate data: assume daily data for 20 years

run.simulation = F

if (run.simulation == T) {
  nsimulation = 10000 #takes 3 daysif 10000 sim
  set.seed(123)
  startyear = 1980
  years <- startyear:2020
  yday <- 365
  climate_data_simulated <- data.frame(
    date = seq.Date(
      from = as.Date(paste0(startyear, "-01-01")),
      by = "day",
      length.out = length(years) * yday
    ),
    temp = runif(length(years) * yday, min = 10, max = 30) # random temperatures
  )

  climate_data_simulated <- climate_data_simulated %>%
    mutate(
      year = format(date, "%Y"),
      yday = yday(date),
      year = as.numeric(year),
      LONGITUDE = NA,
      LATITUDE = NA
    ) %>%
    mutate(across(c(temp), scale)) %>%
    mutate(across(c(temp), as.vector)) %>%
    rename(TMEAN = temp)

  #qs::qsave(
  #  climate_data_simulated,
  #  here::here('Application_MASTREE/outputs/climate_data_simulated10000.qs')
  #)

  # Select ~ one week in June
  june_week <- climate_data_simulated %>%
    filter(yday >= 150 & yday <= 160)

  #check if previous model loaded or not
  if (!exists("statistics_absolute_climwin")) {
    statistics_absolute_climwin <- qread(here(
      "Application_MASTREE/outputs/statistics_absolute_climwin.qs"
    ))
  }

  if (!exists("statistics_basic_method")) {
    statistics_basic_method <- qread(here(
      "Application_MASTREE/outputs/statistics_basic.qs"
    ))
  }

  if (!exists("statistics_csp_method")) {
    statistics_csp_method <- qread(here(
      "Application_MASTREE/outputs/statistics_csp.qs"
    ))
  }

  if (!exists("statistics_psr_method")) {
    statistics_psr_method <- qread(here(
      "Application_MASTREE/outputs/statistics_psr.qs"
    ))
  }

  #get all parameters from real data
  raw.data.param.alpha = c(
    statistics_absolute_climwin$intercept.estimate,
    statistics_basic_method$intercept.estimate,
    statistics_csp_method$intercept.estimate,
    statistics_psr_method$intercept.estimate
  )
  raw.data.param.beta = c(
    statistics_absolute_climwin$slope.estimate,
    statistics_basic_method$slope.estimate,
    statistics_csp_method$slope.estimate,
    statistics_psr_method$slope.estimate
  )
  raw.data.param.sigma = c(
    statistics_absolute_climwin$sigma,
    statistics_basic_method$sigma,
    statistics_csp_method$sigma,
    statistics_psr_method$sigma
  )

  #generate parameter range values for the simulation
  setup.param = parameter.range(
    raw.data.param.alpha,
    raw.data.param.beta,
    raw.data.param.sigma,
    option = 'min.max'
  )

  # Container to store results
  results_list_fakedata = simulated_fake_bio_data(
    fakeclimatewindow = june_week,
    setup.param = setup.param,
    num_simulations = nsimulation,
    save_fake_data = F,
    overwrite = F,
    save_path = here("Application_MASTREE/outputs")
  )

  if (run.simulation == T) {
    result_simulations = simulation_runing_window_parallel(
      climate_data_simulated,
      results_list_fakedata,
      save = T
    )
  }
} else {
  #result_simulations = qs::qread(here(
  # 'Application_MASTREE/outputs/result_simulations2025-05-22.qs'
  #)) #here it is 5000 sim
  result_simulations = qs::qread(here(
    'Application_MASTREE/outputs/result_simulations2025-05-24.qs'
  )) #last one with 10000sim and psr matrix 2,1 and not 3,1
}

#calendar for simulation study case
calendar.sim = generate_reverse_day_calendar(
  refday = 304,
  lastdays = 400,
  yearback = 2
) %>%
  filter(YEAR == 1950) %>%
  dplyr::select(MONTHab, DOY, days.reversed) %>%
  mutate(
    datefake = as.Date(DOY, origin = "1948-01-01"),
    day.month = format(datefake, "%m-%d"),
    year = case_when(
      days.reversed < 366 ~ 1,
      days.reversed >= 366 & days.reversed < 730 ~ 2,
      days.reversed >= 730 & days.reversed < 1095 ~ 3,
      TRUE ~ 4
    )
  )

#do the round if not issues by merging :")
data.plot.sim = result_simulations %>%
  mutate(
    r2_category = case_when(
      r2.before.simulate < 0.25 ~ "Weak",
      r2.before.simulate >= 0.25 & r2.before.simulate < 0.5 ~ "Moderate",
      r2.before.simulate >= 0.5 & r2.before.simulate < 0.75 ~ "Strong",
      r2.before.simulate >= 0.75 ~ "Very strong"
    )
  ) %>%
  mutate(
    r2_category = factor(
      r2_category,
      levels = c("Weak", "Moderate", "Strong", "Very strong")
    )
  ) %>%
  group_by(method, r2_category) %>%
  summarise(
    wind.open_median = round(median(window.open, na.rm = TRUE)),
    wind.close_median = round(median(window.close, na.rm = TRUE)),
    wind.close_q25 = quantile(window.close, 0.25, na.rm = TRUE),
    wind.close_q75 = quantile(window.close, 0.75, na.rm = TRUE),
    wind.open_q25 = quantile(window.open, 0.25, na.rm = TRUE),
    wind.open_q75 = quantile(window.open, 0.75, na.rm = TRUE)
  ) %>%
  pivot_longer(cols = starts_with('wind'), names_to = 'typewind') %>%
  separate_wider_delim(
    typewind,
    delim = '_',
    names = c('windows.type', 'metric')
  ) %>%
  pivot_wider(names_from = 'metric', values_from = value) %>%
  ungroup() %>%
  dplyr::left_join(
    calendar.sim %>%
      mutate(median = days.reversed) %>%
      dplyr::select(DOY, median) %>%
      distinct() %>%
      as_tibble(),
    join_by(median)
  ) %>%
  mutate(
    method = factor(
      recode(
        method,
        "climwin" = "Sliding window",
        "csp" = "Climate sensitivity profile",
        "psr" = "P-spline regression",
        'signal' = 'Peak signal detection'
      ),
      levels = rev(c(
        "Climate sensitivity profile",
        "P-spline regression",
        'Peak signal detection',
        "Sliding window"
      ))
    ),
    windows.type = factor(
      dplyr::recode(
        as_factor(windows.type),
        "wind.open" = "Open",
        "wind.close" = "Close"
      ),
      levels = c("Close", "Open")
    )
  )


#check probabilities
result_simulations %>%
  mutate(
    r2_category = case_when(
      r2.before.simulate < 0.25 ~ "Weak",
      r2.before.simulate >= 0.25 & r2.before.simulate < 0.5 ~ "Moderate",
      r2.before.simulate >= 0.5 & r2.before.simulate < 0.75 ~ "Strong",
      r2.before.simulate >= 0.75 ~ "Very strong"
    )
  ) %>%
  mutate(
    r2_category = factor(
      r2_category,
      levels = c("Weak", "Moderate", "Strong", "Very strong")
    ),
    correct_open = if_else(window.open >= 150 & window.open <= 160, 1, 0)
  ) %>%
  group_by(method, r2_category) %>%
  summarise(
    n_total = n(),
    n_correct_open = sum(correct_open, na.rm = TRUE),
    perc_correct_open = round(100 * n_correct_open / n_total, 1),
    .groups = "drop"
  )

#here with the mean and sd
result_simulations %>%
  group_by(method) %>%
  summarise(
    wind.open_mean = round(mean(window.open, na.rm = TRUE)),
    wind.close_mean = round(mean(window.close, na.rm = TRUE)),
    wind.close_sd = sd(window.close, na.rm = TRUE),
    wind.open_sd = sd(window.open, na.rm = TRUE)
  ) %>%
  pivot_longer(cols = starts_with('wind'), names_to = 'typewind') %>%
  separate_wider_delim(
    typewind,
    delim = '_',
    names = c('windows.type', 'metric')
  ) %>%
  pivot_wider(names_from = 'metric', values_from = value) %>%
  dplyr::left_join(
    calendar.sim %>%
      mutate(mean = days.reversed) %>%
      dplyr::select(DOY, mean) %>%
      distinct() %>%
      as_tibble(),
    join_by(mean)
  )

#now the figures
sim.wind = ggplot(
  data.plot.sim %>%
    mutate(
      method = factor(
        method,
        levels = rev(c(
          "Sliding window",
          "Peak signal detection",
          "Climate sensitivity profile",
          "P-spline regression"
        ))
      )
    ),
  aes(
    x = method,
    group = windows.type,
    col = windows.type,
    shape = windows.type
  )
) +
  geom_rect(
    aes(ymin = 156 + 10, ymax = 146 - 10, xmin = -Inf, xmax = Inf),
    fill = 'grey',
    col = 'white',
    alpha = 0.05
  ) +
  geom_hline(yintercept = 156, color = 'black', linetype = 'dotted') +
  geom_hline(yintercept = 146, color = 'black', linetype = 'dotted') +
  geom_point(
    aes(y = median),
    size = 2,
    position = position_dodge(width = 0.2)
  ) +
  geom_pointrange(
    mapping = aes(y = median, ymin = q25, ymax = q75),
    position = position_dodge(width = 0.2)
  ) +
  coord_flip() +
  facet_wrap(. ~ r2_category, ncol = 2) +
  xlab('') +
  ylab('Days before seed fall (0 = Nov 1 of seed-fall year).') +
  scale_color_manual(values = c("Open" = "black", "Close" = "#009E73")) +
  scale_shape_manual(values = c("Open" = 15, "Close" = 19)) +
  ggpubr::theme_pubr() +
  theme(legend.position = c(.8, .8), legend.title = element_blank()) +
  theme(legend.position = "bottom")

r2sim = ggplot(result_simulations, aes(x = r2.before.simulate)) +
  geom_histogram(
    aes(y = ..density..),
    binwidth = 0.05,
    fill = "#0073C2FF",
    color = "white",
    alpha = 0.8
  ) +
  geom_density(color = "#FC4E07", size = 1) +
  geom_vline(
    aes(xintercept = 0.25),
    color = "black",
    linetype = "dashed",
    size = 0.8
  ) +
  geom_vline(
    aes(xintercept = 0.5),
    color = "black",
    linetype = "dashed",
    size = 0.8
  ) +
  geom_vline(
    aes(xintercept = 0.75),
    color = "black",
    linetype = "dashed",
    size = 0.8
  ) +
  labs(x = expression(R^2 ~ "from simulated relationship"), y = "Density") +
  theme_minimal(base_size = 15) +
  ggpubr::theme_cleveland()


cowplot::save_plot(
  here("Application_MASTREE/figures/FigureSim.png"),
  (r2sim + sim.wind) +
    patchwork::plot_annotation(tag_levels = 'a') &
    theme(plot.tag = element_text(size = 12)) +
      patchwork::plot_layout(widths = c(1, 3)), # Left panel is 2x wider than right
  ncol = 2,
  nrow = 2,
  dpi = 300
)

#find when correct value
result_simulations %>%
  mutate(
    r2_category = case_when(
      r2.before.simulate < 0.25 ~ "Low",
      r2.before.simulate >= 0.25 & r2.before.simulate < 0.5 ~ "Medium Low",
      r2.before.simulate >= 0.5 & r2.before.simulate < 0.75 ~ "Medium High",
      r2.before.simulate >= 0.75 ~ "High"
    ),
    r2_category = factor(
      r2_category,
      levels = c("Low", "Medium Low", "Medium High", "High")
    ),
    correct_open = if_else(window.open >= 150 & window.open <= 160, 1, 0)
  ) %>%
  group_by(method, r2_category) %>%
  summarise(
    n_total = n(),
    n_correct_open = sum(correct_open, na.rm = TRUE),
    perc_correct_open = round(100 * n_correct_open / n_total, 1),
    .groups = "drop"
  )


#for revision
#check CV trends
Fagus.seed %>%
  group_by(Year) %>%
  summarise(n_sites = n_distinct(plotname.lon.lat)) %>%
  ggplot(aes(x = Year, y = n_sites)) +
  geom_line(color = "steelblue", size = 1)

Fagus.seed %>%
  group_by(Year) %>%
  summarise(
    mean = mean(log.seed, na.rm = TRUE),
    sd = sd(log.seed, na.rm = TRUE),
    n = n()
  ) %>%
  mutate(CV = sd / mean) %>%
  ggplot(aes(x = Year, y = CV)) +
  #geom_point(aes(size = n)) +
  geom_line() +
  scale_size_continuous(name = "Number of sites")

model.checing.cv <- Fagus.seed %>%
  group_by(Year) %>%
  summarise(
    mean = mean(log.seed, na.rm = TRUE),
    sd = sd(log.seed, na.rm = TRUE),
    n = n()
  ) %>%
  mutate(CV = sd / mean) %>%
  lm(CV ~ Year + n, data = .)

summary(model.checing.cv)

Fagus.seed %>%
  group_by(plotname.lon.lat) %>%
  mutate(log.seed.scaled = scale(log.seed)) %>%
  ungroup() %>%
  group_by(Year) %>%
  summarise(
    mean_scaled = mean(log.seed.scaled, na.rm = TRUE),
    sd_scaled = sd(log.seed.scaled, na.rm = TRUE)
  ) %>%
  mutate(CV_scaled = sd_scaled / mean_scaled) %>%
  ggplot(aes(x = Year, y = CV_scaled)) +
  geom_line(col = "red")
