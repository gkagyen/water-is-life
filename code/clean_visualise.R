# clean data and visualize

suppressPackageStartupMessages(library(tidyverse))
library(glue)
suppressPackageStartupMessages(library(lubridate))


# Get Data ----------------------------------------------------------------

# list csv files in dir
files <- fs::dir_ls("data_raw", glob = "*csv")

# get file info to identify most recent
files_info <- fs::dir_info("data_raw",
                           regexp = "nfa_updated")

# get most recent file only if multiple exists
(file_recent <- files_info %>%
    select(path, modification_time) %>%
    arrange(modification_time) %>%
    slice_max(order_by = modification_time))

# remove any other files
files_info %>% filter(path!=file_recent$path) %>%
  pull(path) %>%
  fs::file_delete()


# READ IN -----------------------------------------------------------------

# read in most recent
df <- read_csv(glue("{file_recent$path}"), show_col_types = F)

# clean AND RENAME
df_clean <- df %>%
  rename(date = Date) %>%
  mutate(year = year(date), month = month(date),
         water_year = year(date) + ifelse(month(date) >= 10, 1, 0),
         water_day = (date - as.Date(paste(water_year,'10','01', sep = '-')))) %>%
  group_by(water_year) %>%
  mutate(water_day = as.numeric(water_day - min(water_day) + 1)) %>%
  ungroup()

# Get Water Year Types
wyt <- read_csv("data_raw/WYT_1906-2022.csv") %>%
  mutate(Sac_WY_type=factor(Sac_WY_type, levels=c("W","AN","BN","D","C")))


# SUMMARIZE  --------------------------------------------------------------

library(FlowScreen)

# select data of interest and make into time series
df_ts <- df_clean %>%
  select(site_no, date, Flow, Flow_cd, agency_cd) %>%
  as.data.frame() %>%
  # convert to cms
  mutate(Flow = Flow * 0.0283168,
         PARAM = 1) %>%
  # rename
  rename(Date=date, ID=site_no, SYM=Flow_cd, Agency=agency_cd) %>%
  select(ID, PARAM, Date, Flow, SYM, Agency) %>%
  create.ts()

# join with water year:
df_ts <- left_join(df_ts, wyt %>% select(WY, Sac_WY_type), by=c("hyear"="WY"))


# STATS -------------------------------------------------------------------

# use pk.cov function to calc center of volume
df_cov <- pk.cov(df_ts)

# join w water year data
df_cov <- left_join(df_cov, wyt %>% select(WY, Sac_WY_type), by=c("hYear"="WY"))

# VISUALIZE ---------------------------------------------------------------

# plot all with current year:
p1<-ggplot(data=df_ts) +
  
  geom_line(aes(x=hdoy, y=Flow, group=hyear), color="gray", alpha=0.8, lwd=0.3) +
  geom_line(data=df_ts %>% filter(hyear>=year(Sys.Date())),
            aes(x=hdoy, y=Flow), color="steelblue", lwd=1) +
  geom_point(data=df_ts %>% filter(Date==last(Date)),
             aes(x=hdoy, y=Flow), color="white", pch=21, fill="black", size=5) +
  # add drought years
  geom_line(data=df_ts %>% filter(hyear==2017),
            aes(x=hdoy, y=Flow), color="darkblue", lwd=0.5) +
  geom_line(data=df_ts %>% filter(hyear==2020),
            aes(x=hdoy, y=Flow), color="red4", lwd=0.5) +
  geom_line(data=df_ts %>% filter(hyear==2021),
            aes(x=hdoy, y=Flow), color="maroon", lwd=0.5) +
  geom_line(data=df_ts %>% filter(hyear==2022),
            aes(x=hdoy, y=Flow), color="brown3", lwd=0.5) +
  # current date point
  geom_point(data=df_ts %>% filter(hyear>=year(Sys.Date())+1) %>%
               slice_max(hdoy, n = 1),
             aes(x=hdoy, y=Flow), pch=21, size=4, fill="orange") +
  theme_classic() +
  labs(title=glue("Flow from USGS: {unique(df_clean$site_no)},
                  {min(df_clean$water_year)}-{max(df_clean$water_year)}"),
       caption=glue("Updated {Sys.Date()}: Current value is black dot"),
       x="Day of Water Year", y="Flow (cms)")
#p1
# save
ggsave(p1, filename = "output/figure_flow_spaghetti_plot_all.png",
       dpi=200, width=11, height = 8)


# Facet by wy type:
p2 <- ggplot(data=df_ts) +
  geom_ribbon(data=df_ts,
              aes(x=hdoy, ymax=Flow, ymin=0,
                  fill=Sac_WY_type, group=hyear),
              alpha=0.5, lwd=0.3, show.legend = FALSE) +
  # current year
  geom_line(data=df_ts %>% filter(hyear>=year(Sys.Date())+1),
            aes(x=hdoy, y=Flow), color="black", lwd=0.8) +
  ggrepel::geom_label_repel(data=df_ts %>% filter(hyear>=year(Sys.Date())+1) %>%
                              slice_max(hdoy, n = 1),
                            aes(x=hdoy, y=Flow, label="Current date"),
                            box.padding = 2.5, segment.alpha=0.2,
                            min.segment.length = 0.01, nudge_x = 5, nudge_y = 2)+
  geom_point(data=df_ts %>% filter(hyear>=year(Sys.Date())+1) %>%
               slice_max(hdoy, n = 1),
             aes(x=hdoy, y=Flow), pch=21, size=4, fill="orange") +
  theme_classic() +
  scale_fill_viridis_d("Water \nYear Type")+
  scale_color_viridis_d("Water \nYear Type")+
  labs(title=glue("Flow from USGS: {unique(df_clean$site_no)}"),
       subtitle = glue("{min(df_clean$water_year)}-{max(df_clean$water_year)}"),
       caption=glue("Updated {Sys.Date()}"),
       x="Day of Water Year", y="Flow (cms)") +
  facet_wrap(vars(Sac_WY_type), scales = "free_y")
p2
ggsave(p2, filename = "output/figure_flow_ribbon_wy_facet.png",
       dpi=200, width=11, height = 8)


# Compare within same wy type:
p3 <- df_ts %>% filter(Sac_WY_type=="AN") %>%
  ggplot() +
  geom_ribbon(aes(x=hdoy, ymax=Flow, ymin=0,
                  group=hyear), fill="#FDE725FF",
              alpha=0.5, lwd=0.3, show.legend = FALSE) +
  geom_line(aes(x=hdoy, y=Flow, group=hyear), color="gray40",
            alpha=0.4, lwd=0.3, show.legend=FALSE) +
  # current year:
  geom_line(data=df_ts %>% filter(hyear>=year(Sys.Date())+1),
            aes(x=hdoy, y=Flow), color="black", lwd=0.9) +
  ggrepel::geom_label_repel(data=df_ts %>% filter(hyear>=year(Sys.Date())+1) %>%
                              slice_max(hdoy, n = 1),
                            aes(x=hdoy, y=Flow, label="Current date"),
                            box.padding = 2.5, segment.alpha=0.5,
                            min.segment.length = 0.01, nudge_x = 5, nudge_y = 2) +
  geom_point(data=df_ts %>% filter(hyear>=year(Sys.Date())+1) %>%
               slice_max(hdoy, n = 1),
             aes(x=hdoy, y=Flow), pch=21, size=4, fill="orange") +
  theme_classic() +
  scale_color_viridis_d("Water \nYear Type") +
  scale_fill_viridis_d("Water \nYear Type") +
  labs(title=glue("Flow from USGS: {unique(df_clean$site_no)}"),
       subtitle = glue("Year Range: {min(df_clean$water_year)}-{max(df_clean$water_year)}"),
       caption=glue("Updated {Sys.Date()}"),
       x="Day of Water Year", y="Flow (cms)")
#p3
ggsave(p3, filename = "output/figure_flow_ribbon_wy_just_current.png",
       dpi=200, width=11, height = 8)


# Drought Types -----------------------------------------------------------

# look at drought severity:
df_drought <- FlowScreen::dr.seas(df_ts, WinSize = 30, Season = 1:12)
df_drought$hyear <- year(attr(df_drought$StartDay, "times"))

# drought metrics
ggplot(data=df_drought) +
  geom_linerange(aes(xmax=EndDay, xmin=StartDay, y=hyear, color=Severity), lwd=2) +
  geom_point(aes(x=StartDay, y=hyear), pch=21, size=2, fill="red2", alpha=0.7) +
  geom_point(aes(x=EndDay, y=hyear), pch=21, size=3, fill="maroon4") +
  scale_color_viridis_c("Severity", option = "A") +
  scale_y_continuous(breaks = c(seq(1941, 2021, 4))) +
  theme_classic() +
  labs(title="Start and end of significant droughts", y="Water Year",
       x="Day of Water Year",
       subtitle="Severity of drought based on 30-day window using a flow duration curve (Beyene et al. 2014)")
# save
#ggsave(filename = "output/figure_drought_periods.png",
#       dpi=200, width=10, height = 6.5)

# center of volume plot
(plot_cov <-
    ggplot() +
    geom_point(data=df_cov, aes(y=hYear, x=Q50),
               pch=21, size=3, alpha=0.5,
               color="orange", fill="gray70",show.legend=TRUE) +
    geom_vline(xintercept=150, color="steelblue", lty=2) +
    scale_fill_viridis_d("Water Year\n Type") +
    scale_color_viridis_d("Water Year\n Type") +
    scale_y_continuous(breaks = seq(1940, year(Sys.Date()),4)) +
    hrbrthemes::theme_ft_rc() +
    labs(subtitle = "NFA Center of Volume",
         y="Water Year", x="Day of year for 50% of tot. ann streamflow")
)

