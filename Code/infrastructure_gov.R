#### Data ####

composting_all_facilities <- read.csv(
  "C:/Users/fa24575/Dropbox/Organic Waste Bans/06. Post SYP/03.1.Composting Infrastructure/composting_infrastructure_all_states_gov.csv"
) %>% as_tibble() %>%  filter(data_type=="gov")


composting_capacities <- read.csv(
  "C:/Users/fa24575/Dropbox/Organic Waste Bans/06. Post SYP/03.1.Composting Infrastructure/composting_capacity_all_states.csv"
) %>% as_tibble()

cities <- 
  read.csv(
    "C:/Users/fa24575/Dropbox/Organic Waste Bans/03. State_Data/00. Controls/uscities.csv"
  ) %>% as_tibble() %>% 
  mutate(
    county_name = str_to_lower(county_name),
    city = str_to_lower(city),
  ) %>% 
  select(city, state_id, county_name, lat, lng, population)

state_area <- 
  tibble(
    state_id = state.abb, 
    sq_miles = state.area
  )


composting_all_facilities <- 
  composting_all_facilities %>% 
  select(state_name, composting_facility, long, lat) %>%  
  as_tibble() %>%  
  left_join(
    tibble(
      state_name = state.name, 
      state_id = state.abb
    ), 
    by = c("state_name")
  )
  

# states that have data, we exclude Nevada because we only have data for a few counties in NV
data_states <- 
  power2 %>% 
  filter(
    type=="disposal") %>% 
  summarise(
    state_id = unique(state_id)) %>% 
  pluck("state_id")


#### Counties centroids ####
groups <- # counties for which we want to find the centroids
  composting_all_facilities %>% 
  summarise(state_name=unique(state_name)) %>% 
  rename(
    region = state_name
  ) %>% 
  mutate(
    region = str_to_lower(region)
  ) %>% 
  left_join(
    map_data("county"), 
    by = c("region")
  ) %>% 
  ungroup %>% 
  summarise(
    group = unique(group)
  ) %>% 
  pluck(
    "group"
  )


counties_centroids <- function (group_chosen)
{
  df <- map_data("county") %>% filter(group==group_chosen)
  coordinates <- df[, c("long", "lat")]
  df_sf <- st_as_sf(df, coords = c("long", "lat"), crs = 4326)
  
  centroid <- st_centroid(st_combine(df_sf))

  c(
    st_coordinates(centroid) %>% c, 
    group_chosen
  )
}

# counties_centroids_res <- lapply(groups, counties_centroids)
# 
# counties_centroids_res <- 
#   do.call(rbind, counties_centroids_res) %>% 
#   as_tibble() %>% 
#   rename(
#     long_gen = V1,
#     lat_gen = V2, 
#     group = V3
#   )


#### Minimum distance functions ####

min_distance_counties <- function (state_id_given) #county-level waste, counties centroids
{
  region_given <- 
    tibble(state.name, state.abb) %>% 
    filter(state.abb==state_id_given) %>% 
    mutate(state.name = str_to_lower(state.name)) %>% 
    pluck("state.name")
  
  map_data("county") %>% 
    filter(
      region==region_given
    ) %>% as_tibble() %>% 
    left_join(
      counties_centroids_res,
      by = "group"
    ) %>% 
    group_by(subregion) %>% 
    summarise(
      long_gen = unique(long_gen),
      lat_gen = unique(lat_gen)
    ) %>% 
    rename(county_name = subregion) %>% 
    mutate(
      county_name = str_to_lower(county_name), 
      state_id=state_id_given) %>% 
    left_join(
      power2 %>% as_tibble() %>% 
        filter(type=="disposal", year==2014, state_id == state_id_given),
      by= c("state_id", "county_name")) %>% 
    expand_grid(
      composting_all_facilities %>% 
        filter(state_id==state_id_given) %>%
        select(composting_facility, long, lat) %>% 
        rename(
          long_pro = long, 
          lat_pro = lat
        )) %>% 
    mutate(
      lat_gen = lat_gen*pi/180,
      long_gen = long_gen*pi/180,
      lat_pro = lat_pro*pi/180,
      long_pro = long_pro*pi/180, 
      delta_lat = lat_gen-lat_pro, 
      delta_long = long_gen - long_pro, 
      a = sin(delta_lat / 2)^2 + cos(lat_gen) * cos(lat_pro) * sin(delta_long / 2)^2, 
      c = 2 * atan2(sqrt(a), sqrt(1 - a)), 
      distance = 3959*c
    ) %>% 
    select(year, state_id, county_name , tons, distance) %>% 
    group_by(county_name, tons) %>% 
    summarise(min_distance = min(distance, na.rm=TRUE)) %>% 
    ungroup %>% 
    filter(!is.na(tons)) %>%
    filter(!county_name %in% c(rural_counties_CA)) %>% 
    filter(min_distance < 100) %>% 
    mutate(
      total_tons = sum(tons), 
      distance_weighted = tons*min_distance/total_tons) %>% 
    summarise(dis=sum(distance_weighted)) %>% 
    mutate(state_id= state_id_given, type="counties")
}

min_distance_state <-  function (state_id_given) #state-level waste, counties centroids
{
  region_given <- 
    tibble(state.name, state.abb) %>% 
    filter(state.abb==state_id_given) %>% 
    mutate(state.name = str_to_lower(state.name)) %>% 
    pluck("state.name")
  
  map_data("county") %>% 
    filter(
      region==region_given
    ) %>% as_tibble() %>% 
    left_join(
      counties_centroids_res,
      by = "group"
    ) %>% 
    group_by(subregion) %>% 
    summarise(
      long_gen = unique(long_gen),
      lat_gen = unique(lat_gen)
    ) %>% 
    rename(county_name = subregion) %>% 
    mutate(
      county_name = str_to_lower(county_name), 
      state_id=state_id_given) %>% 
    left_join(
      population %>%  filter(year== 2014, state_id==state_id_given),
      by= c("state_id", "county_name")) %>% 
    expand_grid(
      composting_all_facilities %>% 
        filter(state_id==state_id_given) %>%
        select(composting_facility, long, lat) %>% 
        rename(
          long_pro = long, 
          lat_pro = lat
        )) %>% 
    mutate(
      lat_gen = lat_gen*pi/180,
      long_gen = long_gen*pi/180,
      lat_pro = lat_pro*pi/180,
      long_pro = long_pro*pi/180, 
      delta_lat = lat_gen-lat_pro, 
      delta_long = long_gen - long_pro, 
      a = sin(delta_lat / 2)^2 + cos(lat_gen) * cos(lat_pro) * sin(delta_long / 2)^2, 
      c = 2 * atan2(sqrt(a), sqrt(1 - a)), 
      distance = 3959*c
    ) %>% 
    select(year, state_id, county_name , pop, distance) %>% 
    group_by(county_name, pop) %>% 
    summarise(min_distance = min(distance, na.rm=TRUE)) %>% 
    ungroup %>% 
    filter(!is.na(pop)) %>%
    filter(!county_name %in% c(rural_counties_CA)) %>% 
    mutate(
      total_tons = sum(pop), 
      distance_weighted = pop*min_distance/total_tons) %>% 
    summarise(dis=sum(distance_weighted)) %>% 
    mutate(state_id= state_id_given, type = "state")
}


min_distance_cities_county <- function (state_id_given)  #county-level waste, waste allocated to cities
{

  cities %>% 
    filter(
      state_id==state_id_given
    ) %>% 
    group_by(city, state_id, population, county_name) %>% 
    summarise(
      long_gen = unique(lng),
      lat_gen = unique(lat)
    ) %>%
    rename (
      pop_city = population
    ) %>% 
    left_join(
      power2 %>% 
        as_tibble() %>% 
        filter(
          type=="disposal", 
          year==2014, 
          state_id == state_id_given),
      by= c("state_id", "county_name")) %>% 
    group_by(county_name) %>%  
    mutate(
      total_pop = sum(pop_city), 
      weight = tons * pop_city/total_pop) %>% 
    expand_grid(
      composting_all_facilities %>% 
        filter(state_id==state_id_given) %>%
        select(composting_facility, long, lat) %>% 
        rename(
          long_pro = long, 
          lat_pro = lat
        )) %>% 
    mutate(
      lat_gen = lat_gen*pi/180,
      long_gen = long_gen*pi/180,
      lat_pro = lat_pro*pi/180,
      long_pro = long_pro*pi/180, 
      delta_lat = lat_gen-lat_pro, 
      delta_long = long_gen - long_pro, 
      a = sin(delta_lat / 2)^2 + cos(lat_gen) * cos(lat_pro) * sin(delta_long / 2)^2, 
      c = 2 * atan2(sqrt(a), sqrt(1 - a)), 
      distance = 3959*c
    ) %>% 
    select(year, state_id, county_name , weight, distance) %>% 
    group_by(county_name, weight) %>% 
    summarise(
      min_distance = min(distance, na.rm=TRUE)) %>% 
    ungroup %>% 
    filter(!is.na(weight)) %>%
    filter(!county_name %in% c(rural_counties_CA)) %>% 
    filter(min_distance < 100) %>% 
    mutate(
      total_tons = sum(weight), 
      distance_weighted = weight*min_distance/total_tons) %>% 
    summarise(dis=sum(distance_weighted)) %>% 
    mutate(state_id= state_id_given, type="counties")
}

min_distance_cities_state <- function (state_id_given) #state-level waste, waste allocated to cities
{
  cities %>% 
    filter(
      state_id==state_id_given
    ) %>% 
    group_by(city, state_id, population) %>% 
    summarise(
      long_gen = unique(lng),
      lat_gen = unique(lat)
    ) %>%
    rename (
      pop_city = population
    ) %>% 
    left_join(
      power2 %>% 
        as_tibble() %>% 
        filter(
          type=="disposal", 
          year==2014, 
          state_id == state_id_given),
      by= c("state_id")) %>% 
    group_by(state_id) %>%  
    mutate(
      total_pop = sum(pop_city), 
      weight = tons * pop_city/total_pop) %>% 
    expand_grid(
      composting_all_facilities %>% 
        filter(state_id==state_id_given) %>%
        select(composting_facility, long, lat) %>% 
        rename(
          long_pro = long, 
          lat_pro = lat
        )) %>% 
    mutate(
      lat_gen = lat_gen*pi/180,
      long_gen = long_gen*pi/180,
      lat_pro = lat_pro*pi/180,
      long_pro = long_pro*pi/180, 
      delta_lat = lat_gen-lat_pro, 
      delta_long = long_gen - long_pro, 
      a = sin(delta_lat / 2)^2 + cos(lat_gen) * cos(lat_pro) * sin(delta_long / 2)^2, 
      c = 2 * atan2(sqrt(a), sqrt(1 - a)), 
      distance = 3959*c
    ) %>% 
    select(year, state_id, weight, distance) %>% 
    group_by(weight) %>% 
    summarise(
      min_distance = min(distance, na.rm=TRUE)) %>% 
    ungroup %>% 
    filter(!is.na(weight)) %>%
    filter(min_distance < 100) %>% 
    mutate(
      total_tons = sum(weight), 
      distance_weighted = weight*min_distance/total_tons) %>% 
    summarise(dis=sum(distance_weighted)) %>% 
    mutate(state_id= state_id_given, type="state")
}
  
min_distance_generators <- function (state_id_given) #using store-level waste for Vermont and Massachusetts 
{
  if (state_id_given=="MA")
  {
    res <- 
      food_generators_MA %>% 
      filter(!Type %in% c("F", "W")) %>% 
      as_tibble %>% 
      mutate(Long=-Long) %>% 
      select(DEP_Code, tons, Lat, Long) %>% 
      expand_grid(
        composting_all_facilities %>% 
          filter(state_id==state_id_given) %>%
          select(composting_facility, long, lat) %>% 
          rename(
            long_pro = long, 
            lat_pro = lat
          )) %>% 
      rename(
        lat_gen = Lat, 
        long_gen = Long
      ) %>% 
      mutate(
        lat_gen = lat_gen*pi/180,
        long_gen = long_gen*pi/180,
        lat_pro = lat_pro*pi/180,
        long_pro = long_pro*pi/180, 
        delta_lat = lat_gen-lat_pro, 
        delta_long = long_gen - long_pro, 
        a = sin(delta_lat / 2)^2 + cos(lat_gen) * cos(lat_pro) * sin(delta_long / 2)^2, 
        c = 2 * atan2(sqrt(a), sqrt(1 - a)), 
        distance = 3959*c
      ) %>% 
      group_by(DEP_Code, tons) %>% 
      summarise(min_distance = min(distance)) %>% 
      ungroup %>% 
      filter(!is.na(tons)) %>% 
      mutate(
        total_tons = sum(tons), 
        distance_weighted = tons*min_distance/total_tons) %>% 
      summarise(dis=sum(distance_weighted)) %>% pluck("dis")}
  if(state_id_given=="VT")
  {
    res <- food_generators_VT %>% 
      as_tibble %>% 
      mutate(tons = TonsPerWeek*52, town_name = str_to_lower(Town), id= seq(1:n())) %>% 
      left_join(
        towns_coordinates_VT %>% as_tibble %>% mutate(town_name = str_to_lower(town_name)), 
        by = "town_name"
      ) %>% select(tons, lat_gen, long_gen, id) %>% filter(!is.na(lat_gen)) %>% 
      expand_grid(
        composting_all_facilities %>% 
          filter(state_id=="VT") %>%
          select(composting_facility, long, lat) %>% 
          rename(
            long_pro = long, 
            lat_pro = lat
          )) %>% 
      mutate(
        lat_gen = lat_gen*pi/180,
        long_gen = long_gen*pi/180,
        lat_pro = lat_pro*pi/180,
        long_pro = long_pro*pi/180, 
        delta_lat = lat_gen-lat_pro, 
        delta_long = long_gen - long_pro, 
        a = sin(delta_lat / 2)^2 + cos(lat_gen) * cos(lat_pro) * sin(delta_long / 2)^2, 
        c = 2 * atan2(sqrt(a), sqrt(1 - a)), 
        distance = 3959*c
      ) %>% 
      group_by(id, tons) %>% 
      summarise(min_distance = min(distance, na.rm=TRUE)) %>% 
      ungroup %>% filter(!is.na(tons)) %>% 
      mutate(
        total_tons = sum(tons), 
        distance_weighted = tons*min_distance/total_tons) %>% 
      summarise(dis = sum(distance_weighted)) %>% 
      pluck("dis")
  }
  res
  
}

#### Results ####

st <- # states for which i want to check the minimum distance
  composting_all_facilities %>% 
  summarise(state_name=unique(state_name)) %>% 
  left_join(
    tibble(
      state_name = state.name, 
      state_id = state.abb
    ), by = c("state_name")
  ) %>% pluck("state_id")

min_distance_res <-
  lapply(st, min_distance_cities_county) %>% bind_rows() %>% 
  rbind(
    lapply(st, min_distance_cities_state) %>% bind_rows() 
  )

min_dist_MA <- min_distance_generators("MA")
min_dist_VT <- min_distance_generators("VT")
  

infrastructure_plot_data <-
  min_distance_res %>% 
  pivot_wider(names_from = type, values_from = dis) %>% 
  filter(state+counties!=0) %>% 
  mutate(generators = 0) %>% 
  mutate(
    generators = ifelse(state_id=="VT", min_dist_VT, generators), 
    generators = ifelse(state_id=="MA", min_dist_MA, generators) 
  ) %>% 
  group_by(state_id) %>% 
  mutate(
    final = state,
    final = ifelse(counties!=0, counties, final) 
    #final = ifelse(generators!=0, generators, final), 
  ) %>%
  rename(
    min_distance = final
  ) %>% 
  left_join(
    state_area, by = c("state_id")
  ) %>% 
  left_join(
    composting_all_facilities %>% 
      group_by(state_id) %>% 
      summarise(
        n= n()
      ), 
    by = c("state_id")) %>% 
  mutate(
    density = n/sq_miles*1000, 
    ind = ifelse(state_id %in% c(all_treated), "Ban", "No Ban"), 
    ind = ifelse(state_id %in% c("WA", "NJ", "NY", "MD"), "Future Ban",ind), 
    state_id_label = ifelse(ind!="No Ban", state_id, NA)
  ) %>% 
  left_join(
    composting_capacities %>% 
      left_join(
        power2 %>% 
          filter(year==2013, type=="disposal") %>% 
          group_by(state_id) %>% 
          summarise(total_tons = sum(tons)), 
        by = c("state_id")
      ) %>% 
      mutate(cap_per = 100*capacity/total_tons) %>% 
      select(-total_tons),
    by = c("state_id")
  ) %>% 
  mutate(min_distance = 1/min_distance)

  
#### Infrastructure plot #### 

composting_facilities_map <- 
  map_data("state") %>% 
  left_join(
    tibble(
      region = state.name %>% str_to_lower(), 
      state_id = state.abb
    ), 
    by = c("region")
  ) %>% 
  mutate(
    ind=ifelse(state_id %in% data_states, "Waste data", "No data"), 
    ind2 = ifelse(state_id %in% all_treated, "Ban state", ind), 
    ind2 = factor (ind2, levels=c("Waste data", "No data", "Ban state"))
  ) %>% 
  ggplot() + 
  geom_polygon(
    aes(x = long, y = lat, group = group, fill = ind2),
    linewidth= 0.1, 
    color = ut_colors[5]
  )+
  scale_fill_manual(
    values = 
      c("No data" = "white", 
        "Ban state" = "#2E8B5750",
        "Waste data" = "#9CADB780") # 6450C820 would be more thansparent thatn 6450C880
  )+
  labs (x="", y ="", fill = "")+
  
  ggnewscale::new_scale_color()+
  geom_point(
    data = 
      composting_all_facilities %>% 
      select(long, lat),
    aes(x=long, y= lat, color = ""), 
    size =1
    #color =
  )+
  #scale_color_manual(values = rgb(100, 80, 200, maxColorValue = 255))+ (this is the same # 6450C8 )
  scale_color_manual(values = ut_colors[4])+
  labs(color="Facilities")+
  theme(
    panel.border = element_blank(),
    legend.position = "top", 
    panel.background = element_blank(),
    axis.ticks = element_blank(), 
    axis.text=element_blank(),
    text = element_text(family="Helvetica", size = 16, color = ut_colors[4]), 
    legend.text = element_text(family="Helvetica", size = 16, color = ut_colors[4]), 
    strip.background = element_blank(), 
    panel.spacing = unit(2, "lines") , 
    legend.key = element_blank(), 
    legend.text.align = 0.5
    #legend.key.width = unit(2.5, "cm"), 
    #legend.key.height = unit(.25, "cm")
  ) 



ggsave(
  composting_facilities_map, filename = "composting_facilities_map.pdf", device = cairo_pdf,
  path= figure_path,
  width = 12, height = 7, units = "in")



infrastructure_plot_2 <-
  infrastructure_plot_data %>% 
  pivot_longer(
    cols=c("density", "cap_per", "min_distance"), 
    names_to = "metrics", 
    values_to = "metrics_values"
  ) %>% 
  mutate(
    metrics = ifelse(metrics=="density", "Coverage (# per '000 sq. miles)", metrics), 
    metrics = ifelse(metrics=="cap_per","Capacity (% of 2013 disposal)", metrics), 
    metrics = ifelse(metrics=="min_distance","Distance density (facilities / mile)", metrics)
  ) %>% 
  mutate(
    metrics = factor(metrics, levels = c("Distance density (facilities / mile)", "Coverage (# per '000 sq. miles)", "Capacity (% of 2013 disposal)"))
  ) %>% 
  mutate(
    state_id_label = ifelse(state_id %in% c("MA", "CT", "VT", "CA", "RI", "NJ", "MD", "GA", "FL", "IA", "NC"), state_id, NA)
  ) 


levels(infrastructure_plot_2$metrics) <- c(
  "Distance density (facilities / mile)" = expression(paste("Distance density ", paste(group("(", "distance to nearest facility", ")")^{-1}))),
  "Coverage (# per '000 sq. miles)" = expression(paste("Coverage (# per '000 sq. miles)")),
  "Capacity (% of 2013 disposal)" = expression(paste("Capacity (% of 2013 disposal)"))
)


infrastructure_plot_2 <-
  infrastructure_plot_2 %>% 
  ggplot()+
  aes(x=metrics_values, label = state_id, color = ind)+
  #scale_color_manual(values = c(ut_colors[4], "#33494880", "#9CADB780"))+
  scale_color_manual(values = c(ut_colors[4], ut_colors[4], ut_colors[4]))+
  labs (x="", y = "", color = "")+
  geom_point(aes(y=0))+
  geom_text(aes(y=0), show.legend = FALSE, vjust=-0.4, check_overlap = TRUE)+
  facet_wrap(vars(metrics), scales="free", nrow=3, strip.position = "bottom", labeller=label_parsed)+
  scale_y_continuous(limits = c(-0.1,0.1))+
  theme(
    legend.position = "none",
    strip.background = element_rect(color = "white", fill = "white"),
    panel.grid.major.x = element_blank() ,
    panel.grid.major.y = element_blank(),
    panel.background = element_blank(), 
    text = element_text(family = "Helvetica",size = 20, color= ut_colors[4]), 
    strip.placement = "outside",
    strip.text = element_text(family = "Helvetica",size = 14, color= ut_colors[4]), 
    panel.spacing = unit(1.5, "lines"), 
    axis.text.y=element_blank(),
    axis.ticks.y=element_blank(),
    legend.key = element_blank(), 
    axis.text.x = element_text(family = "Helvetica",size = 14, color= ut_colors[4])
  )


infrastructure_plot_2 <-
  infrastructure_plot_2+
  ggh4x::facetted_pos_scales(x = list(
    metrics == expression(paste("Capacity (% of 2013 disposal)")) ~ scale_x_continuous(breaks = c(0.0, 2.5, 5.0, 7.5, 10.0,12.5), limits = c(0.0,12.5)),
    metrics == expression(paste("Coverage (# per '000 sq. miles)")) ~ scale_x_continuous(breaks = c(0.0, 1.2, 2.4, 3.6,4.8,6.0), limits = c(0.0,6.0)),
    metrics == expression(paste("Distance density ", paste(group("(", "distance to nearest facility", ")")^{-1}))) ~ scale_x_continuous(breaks = c(0.00, 0.04, 0.08, 0.12,0.16,0.20), limits = c(0.0, 0.20))
  ))


map_and_densities <-  ggpubr::ggarrange(composting_facilities_map, infrastructure_plot_2, nrow=1, widths = c(2, 1))

ggsave(
  map_and_densities, filename = "map_and_densities.pdf", device = cairo_pdf,
  path= figure_path,
  width = 17, height = 6.5, units = "in")


ggsave(
  infrastructure_plot_2, filename = "densities_for_pres.pdf", device = cairo_pdf,
  path= figure_path,
  width = 8, height = 7, units = "in")


#MA is % higher than the second highest, Vermont: 
(infrastructure_plot_data %>% filter(state_id=="MA") %>% pluck("min_distance") - infrastructure_plot_data %>% filter(state_id=="VT") %>% pluck("min_distance"))/ infrastructure_plot_data %>% filter(state_id=="VT") %>% pluck("min_distance")

(infrastructure_plot_data %>% filter(state_id=="MA") %>% pluck("min_distance") - infrastructure_plot_data %>% ungroup %>% summarise(m=mean(min_distance)) %>% pluck("m"))/ infrastructure_plot_data %>% ungroup %>% summarise(m=mean(min_distance)) %>% pluck("m")

#MI is % less than the Massachusetts: 
(infrastructure_plot_data %>% filter(state_id=="MI") %>% pluck("min_distance") - infrastructure_plot_data %>% filter(state_id=="MA") %>% pluck("min_distance"))/ infrastructure_plot_data %>% filter(state_id=="MA") %>% pluck("min_distance")

(infrastructure_plot_data %>% filter(state_id=="MI") %>% pluck("min_distance") - infrastructure_plot_data %>% ungroup %>% summarise(m=mean(min_distance)) %>% pluck("m"))/ infrastructure_plot_data %>% ungroup %>% summarise(m=mean(min_distance)) %>% pluck("m")




#### Mechanism plot ####

effect = 
  c(ca_effect,
    ct_effect,
    -ma_effect,
    ri_effect,
    vt_effect
  )


##################### Enforcement ###############################
enforcement_counties <- 
  c("alamedaCA", "butteCA", "contra costaCA", "el doradoCA", "humboldtCA", "kernCA", "kingsCA", "lakeCA", 
    "maderaCA", "mendocinoCA", "montereyCA", "napaCA", "nevadaCA", "riversideCA", "sacramentoCA", "san diegoCA", 
    "san joaquinCA", "san luis obispoCA", "santa barbaraCA", "santa claraCA", "santa cruzCA", "shastaCA", "solanoCA", 
    "sonomaCA", "stanislausCA", "sutterCA", "venturaCA", "yoloCA", "los angelesCA", "imperialCA", "orangeCA", "sacramentoCA", "tulareCA")

# Massachusetts generators
ma_gen <- food_generators_MA %>% as_tibble() %>% filter(tons>52|(Type%in%c("W", "F") & is.na(tons))) %>% 
  summarise(n = n()) %>% pluck("n")
vt_gen <- food_generators_VT %>% as_tibble() %>% mutate(tons = TonsPerWeek*52) %>% filter(tons>52| is.na(tons)) %>% 
  summarise(n = n()) %>% pluck("n")
# Alameda generators 
al_gen <- 7000*0.19 # from Alameda report (7000 total generators, 0.19 fraction of organics related violations)
# California generators
ca_gen <- 
  population %>% filter(year==2019, state_id=="CA") %>% 
  mutate(county_id = paste0(county_name, state_id)) %>% 
  filter(county_id %in% enforcement_counties) %>% 
  filter(!county_id %in% c("san diegoCA")) %>% 
  mutate(total_pop = sum(pop)) %>% 
  filter(county_id=="alamedaCA") %>% 
  mutate(generators = al_gen/pop*total_pop) %>% 
  pluck("generators")
  

fines_al_food <- 2400*0.19/5 #685*0.19 #2400 is the total citations that they have had, 0.19 the fraction of organic, 5 the total years for data
#food waste actions = total fines in 19-20 * fraction of organics violations OR total fines * fraction of organics / 5
monetary_fines_alameda_food <- fines_al_food*100
monetary_fines_alameda_food/ca_gen
monetary_fines_alameda_food/al_gen


monetary_fines_massachusetts_food <- 31544 # from FOIA
monetary_fines_massachusetts_food/ma_gen/(2021-2016+1)

3000/685*2400*0.19/al_gen/5

#total inspections in alameda: 3000/685*2400


enforcement <- 
  c(
    (3000*0.19+36/0.19)/ ca_gen, 
    0,
    #(45000/(2023-2016+1)*(2020-2016+1))/ ma_gen, 
    45000/(2023-2014+1)/ ma_gen*(95/933), # from FOIA 95 is the total numvber of organics citations/violations and 933 is the toatl
    0,
    (432)/vt_gen/(2020-2014+1) # from FOIA, 432 total inspections
  )

# MA's enforcement xx % higher than the second best

(45000/(2023-2014+1)/ ma_gen*(95/933)-(432)/vt_gen/(2020-2014+1))/((432)/vt_gen/(2020-2014+1))




mech1 <- 
  tibble(
    effect, 
    state_id = c("CA", "CT", "MA", "RI", "VT"), 
    complexity_grade = c(94,98,49,84,139), 
    enforcement
  ) %>% 
  mutate(
    simplicity_grade = 1/complexity_grade
  ) %>% 
  left_join(
    bt_with_power_data %>% select(state_id, power_high, power_low), 
    by=c("state_id")
  ) %>% 
  left_join(
    infrastructure_plot_data %>% 
      #filter(metrics=="Average min. distance (miles)") %>% 
      select(state_id, min_distance)
  ) %>%
  mutate(
    effect=effect,
    density = min_distance %>% round(digits=2), 
  ) %>% 
  select(-complexity_grade) %>% 
  select(-min_distance) %>% 
  rename(
    `Affordability \n(facilities / mile)` = density, 
    `Simplicity \n(1 / (#operators + #operands))`=simplicity_grade, 
    `Enforcement \n(inspections / generator-year)` = enforcement 
  ) %>% 
  pivot_longer(
    #cols =c("Density (facility / miles)", "Simplicity (1 / Length)", "Inspections (inspections / generator-year)"), 
    cols =c("Affordability \n(facilities / mile)", "Simplicity \n(1 / (#operators + #operands))", "Enforcement \n(inspections / generator-year)"), 
    names_to = "mechanism", 
    values_to = "values") %>% 
  left_join(
    tibble(
      state.name, 
      state_id = state.abb
    ), by = c("state_id")) %>% 
  mutate(
    mechanism = factor(
      mechanism, 
      levels = c("Affordability \n(facilities / mile)", 
                 "Simplicity \n(1 / (#operators + #operands))", 
                 "Enforcement \n(inspections / generator-year)"))
  )


  
#levels(mech1$mechanism) <-  c("Affordability\n(facility / miles)", "Simplicity\n(1 / Length)", "Enforcement\n(inspections / generators)")
  
levels(mech1$mechanism) <- c(
  "Affordability (facilities / mile)" = expression(atop(Affordability, paste(group("(", "distance to nearest facility", ")")^{-1}))),
  "Simplicity Complexity^-1 = (operators + operands)^-1" = expression(atop(Simplicity, paste(group("(", "operators + operands of bans' statutes", ")")^{-1}))),  # Adjusted for double row with Complexity^-1 in the second row
  "Enforcement (inspections / generator-year)" = expression(atop(Enforcement, "(inspections / generator-year)"))
)



mech1_plot <- 
  mech1 %>%
  mutate(ind = ifelse(state_id=="MA", "Significant", "Nil"), 
         ind = factor(ind, levels = c("Significant", "Nil"))) %>% 
  ggplot(
    aes(y=effect, x = values, label=state_id, color = state.name) 
  )+
  geom_point(aes(color = ind), size=2, show.legend = FALSE)+
  geom_line(aes(color = ind, y=-100), linewidth = 1, show.legend = TRUE) +
  geom_text(
    aes(label = state_id,
        color = ind, 
        vjust = ifelse(state_id=="CA", -0.9, 0), 
        hjust = ifelse(state_id=="CA", +0.5, -0.3)
    ),
    size = 3.2,
    family = "Helvetica", show.legend = FALSE, 
    fontface = "bold", 
  ) +
  scale_color_manual(
    values = c(
      "Significant" = ut_colors[4], 
      "Nil" = ut_colors[5]
    )
  )+
  facet_wrap(vars(mechanism), scales= "free_x", strip.position = "bottom", labeller=label_parsed)+
  labs(x="", y="", color="Average treatment effect:")+
  geom_hline(yintercept = 0, linetype = "dotted", color = ut_colors[5]) +
  labs(y="Average treatment effect (%)")+
  scale_y_continuous(limits = c(-8,2.5), breaks= seq(-8,3, by =2))+
  theme(
    legend.position = "top",
    strip.background = element_rect(color = "white", fill = "white"),
    panel.grid.major.x = element_blank() ,
    panel.grid.major.y = element_blank(),
    panel.background = element_blank(), 
    text = element_text(family = "Helvetica",size = 11, color= ut_colors[4]), 
    strip.placement = "outside",
    strip.text.y = element_text( hjust = 0.5, family = "Helvetica",size = 11, color = ut_colors[4]),
    panel.spacing = unit(2, "cm"), 
    legend.key = element_blank(),
    legend.text = element_text(family = "Helvetica",size = 11, color= ut_colors[4]), 
    legend.title = element_text(family = "Helvetica",size = 11, color= ut_colors[4]), 
  )



mech1_plot <-
  mech1_plot+
  ggh4x::facetted_pos_scales(x = list(
    mechanism == expression(atop(Affordability, paste(group("(", "distance to nearest facility", ")")^{-1}))) ~ scale_x_continuous(breaks = c(0.00, 0.05, 0.10, 0.15, 0.20,0.25), limits = c(-0.01,0.26)),
    mechanism == expression(atop(Simplicity, paste(group("(", "operators + operands of bans' statutes", ")")^{-1}))) ~scale_x_continuous(breaks = c(0, 0.005, .01, 0.015, 0.020,0.025), limits = c(0,0.026)),
    mechanism == expression(atop(Enforcement, "(inspections / generator-year)")) ~ scale_x_continuous(breaks = c(0.00, 0.04, 0.08, 0.12, 0.16, 0.20), limits = c(0,0.20))
  ))


ggsave(
  mech1_plot, filename = "effects_mechanismalt3.pdf", device = cairo_pdf,
  path= figure_path,
  width = 11, height = 4, units = "in")




mech1_plot <-
  mech1_plot+
  ggh4x::facetted_pos_scales(y = list(
    mechanism == "Affordability (1 / miles)" ~ scale_y_continuous(breaks = c(0.00, 0.05, 0.10, 0.15, 0.20,0.25), limits = c(-0.01,0.26)),
    mechanism == "Simplicity (1 / volume)" ~ scale_y_continuous(breaks = c(0, 0.5, 1, 1.5, 2.0,2.5), limits = c(-0.1,2.6)),
    mechanism == "Enforcement (inspections / generators)" ~ scale_y_continuous(breaks = c(0, 0.2, 0.4, 0.6,0.8,1), limits = c(-0.1/2.6,1))
  ))



ggsave(
  mech1_plot, filename = "effects_mechanism.pdf", device = cairo_pdf,
  path= figure_path,
  width = 5, height = 8, units = "in")




effects_mechanism <- 
  ggpubr::ggarrange(
  mech1, mech2, nrow=2, heights = c(2, 1))


ggsave(
  effects_mechanism, filename = "effects_mechanism.pdf", device = cairo_pdf,
  path= figure_path,
  width = 9, height = 7, units = "in")


############# Simplicity ################

summaries_complexity_claude <- read.csv("C:/Users/fa24575/Dropbox/Organic Waste Bans/06. Post SYP/03.2.Complexity/complexity_claude.csv")
set.seed(4)
epsilon <- rnorm(750, 0, 0.001) # to resolve same rank

complexity_rank <- 
  summaries_complexity_claude %>% 
  pivot_longer(
    cols = c("CA", "CT", "MA", "VT", "RI"), 
    names_to = "state_id", 
    values_to = "grade"
  ) %>% 
  as_tibble() %>% 
  mutate(
    grade_per = grade + epsilon
  ) %>% 
  group_by(iteration) %>%
  mutate(
    rank = rank(grade_per)) %>% 
  mutate(grade = 10- grade) %>%
  group_by(rank, state_id) %>% 
  mutate(n=n()) %>% 
  group_by (rank) %>% 
  mutate(states_per_rank = n_distinct(state_id)) %>% 
  ungroup %>% 
  mutate(percentage = n/n()*5) %>% 
  mutate(
    hjust_value= case_when(
      state_id == "CA" & rank %in%c(2,5)~ 3.2,
      state_id == "CA" & rank %in%c(3,4)~ 2.9,
      
      state_id == "RI" & rank %in%c(2,5)~ -1.3,
      state_id == "RI" & rank %in%c(3,4)~ -0.5,
      state_id == "RI" & rank %in%c(1)~ 0.5,
      
      state_id == "CT" & rank %in%c(2,5)~ 1.8,
      state_id == "CT" & rank %in%c(3,4)~ 1.5,
      state_id == "CT" & rank %in%c(1)~ 2.5,
      
      state_id == "MA" & rank %in%c(2,5)~ 0.5,
      state_id == "MA" & rank %in%c(1)~ +2.5,
      
      state_id == "VT" & rank %in%c(2,5)~ -2.3,
      state_id == "VT" & rank %in%c(3,4)~ -2.1,
      state_id == "VT" & rank %in%c(1)~ -1.8,
      TRUE ~ 1.5
    )
  ) %>% 
  ggplot(aes(x=rank,y=100*percentage, fill=state_id))+
  #geom_col(position=position_dodge(width = 0.9 / max_groups_per_rank), width = 0.9 / max_groups_per_rank, color="white") +
  geom_col(aes(width = 0.12*states_per_rank), position= "dodge", color="white") +
  labs(fill="")+
  geom_text(
    aes(
      y=100*percentage, 
      label = state_id
      #hjust=hjust_value
    ), 
    position = position_dodge(width=0.12*5),
    vjust = -0.5, 
    color = ut_colors[4], 
    size = 3.5, 
    fontface = "plain", 
    family = "Helvetica"
  )+
  scale_fill_manual(
    values=c(
      "CA" = ut_colors[5],
      "CT" = ut_colors[5],
      "VT" = ut_colors[5],
      "RI" = ut_colors[5],
      "MA" = ut_colors[4]
    )
  ) +
  labs(x="Rank", y = "Fraction of times(%)")+
  scale_y_continuous(limits = c(0, 100), breaks = c(seq(0,100, by = 20)))+
  theme(
    legend.position = "none",
    strip.background = element_rect(color = "white", fill = "white"),
    panel.grid.major.x = element_blank() ,
    panel.grid.major.y = element_blank(),
    panel.background = element_blank(), 
    text = element_text(family = "Helvetica",size = 11, color= ut_colors[4]), 
    strip.placement = "outside",
    strip.text.y = element_text( hjust = 0.5, family = "Helvetica",size = 11, color = ut_colors[4]),
    panel.spacing = unit(2, "cm"), 
    legend.key = element_blank(),
    legend.text = element_text(family = "Helvetica",size = 11, color= ut_colors[4]), 
    legend.title = element_text(family = "Helvetica",size = 11, color= ut_colors[4]), 
  )

ggsave(
  complexity_rank, filename = "complexity_rank.pdf", device = cairo_pdf,
  path= figure_path,
  width = 8, height = 5, units = "in")