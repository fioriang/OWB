ghg_data_path <- "C:/Users/fa24575/Dropbox/Organic Waste Bans/06. Post SYP/06. Emissions/GHG_Data"
facility <- read.csv(paste0(ghg_data_path,"/facility_info.csv"))%>% as_tibble
msw_fac <- read.csv(paste0(ghg_data_path,"/msw_facilities.csv"))%>% as_tibble
gas <- read.csv(paste0(ghg_data_path,"/gas_data.csv")) %>% as_tibble
measurements <-read.csv(paste0(ghg_data_path,"/measure_data.csv")) %>% as_tibble
captured_methane <-read.csv(paste0(ghg_data_path,"/captured_methane.csv")) %>% as_tibble


#Check our data, with EPA's data
data_comparison <-
  power2 %>% 
  as_tibble() %>% 
  filter(
    type %in% c("disposal")
  ) %>% 
  mutate(
    tons = ifelse(state_id=="DE" & year < 2009, NA, tons)
  ) %>% 
  group_by(state_id, year) %>% 
  summarise(
    our_data = sum(tons, na.rm=TRUE)
  ) %>%
  left_join(
    population %>% group_by(state_id, year) %>% summarise(state_pop=sum(pop, na.rm = TRUE)), 
    by = c("state_id", "year")
  ) %>% 
  filter(our_data>1) %>% 
  mutate(our_data = our_data/state_pop) %>% 
  left_join(
    msw_fac %>% as_tibble %>% 
      left_join(
        facility %>% 
          group_by(facility_id) %>% 
          summarise(state=unique(state)), 
        by = c("facility_id")
      )%>% 
      filter(method_used_to_find_qty=="USED SCALES TO WEIGH LOADS BEFORE OFF-LOADING AND EITHER USED SCALES TO WEIGH INDIVIDUAL LOADS AFTER OFF-LOADING OR USED REPRESENTATIVE TARE VEHICLE/CONTAINER WEIGHTS") %>% 
      filter(!is.na(total_waste_disposal_qty_ry)) %>% 
      group_by(waste_disp_reporting_year, facility_id, state) %>% 
      summarise(waste=mean(total_waste_disposal_qty_ry)) %>% 
      group_by(waste_disp_reporting_year, state) %>% 
      summarise(epa_waste = sum(waste)) %>% 
      left_join(
        population %>% group_by(state_id, year) %>% summarise(state_pop=sum(pop, na.rm = TRUE)), 
        by = c("state"="state_id", "waste_disp_reporting_year"="year")
      ) %>% 
      mutate(epa_waste= epa_waste/state_pop) %>% select(-state_pop),
    by = c("state_id"="state", "year"="waste_disp_reporting_year")
  ) %>% 
  pivot_longer(
    cols=c("our_data", "epa_waste"), 
    values_to = "waste"
  ) %>% 
  left_join(
    tibble(
      state_id = state.abb, 
      state_name = state.name
    ), 
    by = c("state_id")
  ) %>% 
  mutate(name = ifelse(name == "epa_waste", "EPA", "Our Data")) %>% 
  filter(year > 1995) %>% 
  ggplot()+
  aes(x=year, y=waste, color=name) +
  geom_line()+
  geom_point()+
  facet_wrap(vars(state_name))+
  labs(x="Year", y = "Disposal (tons per capita)", color="")+
  scale_x_continuous (breaks = c(1997, 2007, 2017), limits = c(1995, 2019),labels = c("'97", "'07", "'17"))+
  scale_y_continuous (limits = c(0.0, 1.8))+
  
  scale_color_manual(values=c(ut_colors[5], ut_colors[4]), na.translate = FALSE)+
  theme(
    legend.position = "top",  # Keep the legend at the top
    legend.box.margin = margin(t = 0, r = 0, b = 0, l = 70),
    strip.background = element_rect(color = "white", fill = "white"),
    panel.grid.major.x = element_blank() ,
    panel.grid.major.y = element_blank(),
    panel.background = element_blank(), 
    text = element_text(family = "Helvetica",size = 16, color= ut_colors[4]), 
    strip.text = element_text(angle = 0, hjust = 1), 
    strip.placement = "outside",
    axis.ticks.y = element_blank(), 
    legend.key = element_rect(colour = NA, fill = NA, size = 16),
    legend.spacing.x = unit(-1, "pt"),
    axis.ticks.x = element_line(size = 0.1), 
    legend.text = element_text(family = "Helvetica",size = 16, color= ut_colors[4])
  )

ggsave(data_comparison, filename = "data_comparison.pdf", device = cairo_pdf,
       path= figure_path,
       width = 14, height = 13, dpi=320, units = "in")



gas_fc <- 
  gas %>% 
  right_join(
    msw_fac %>% as_tibble %>% 
      left_join(
        facility %>% 
          group_by(facility_id) %>% 
          summarise(state=unique(state)), 
        by = c("facility_id")
      )%>% 
      filter(method_used_to_find_qty=="USED SCALES TO WEIGH LOADS BEFORE OFF-LOADING AND EITHER USED SCALES TO WEIGH INDIVIDUAL LOADS AFTER OFF-LOADING OR USED REPRESENTATIVE TARE VEHICLE/CONTAINER WEIGHTS") %>% 
      filter(!is.na(total_waste_disposal_qty_ry)) %>% 
      group_by(waste_disp_reporting_year, facility_id, state) %>% 
      summarise(waste=mean(total_waste_disposal_qty_ry)),
    by =c ("facility_id", "state", "year"="waste_disp_reporting_year")
  ) %>% 
  filter(gas_name %in% c("Methane"), waste>2) %>% #Add Carbon Dioxide here if need to check thos to
  group_by(year, facility_id, state) %>% 
  summarise(
    gas= sum(co2e_emission),
    waste = mean (waste, na.rm=TRUE)) %>% 
  filter(
    year<2019, !is.na(waste)) %>% 
  rename(
    state_id=state
  ) %>% 
  left_join(
    tibble (
      all_treated= c("VT", "MA", "CA", "CT", "RI"),# Never changes
      bans = c(2014, 2014, 2016, 2014, 2016)), 
    by = c("state_id"="all_treated")
  ) %>% 
  left_join(
    regions, by = c("state_id")
  ) %>% 
  mutate(
    treated = ifelse(!is.na(bans) & year>= bans, 1, 0), 
    gas_pt = gas/waste
  ) %>% 
  select(-bans) %>% 
  filter(!is.na(region)) %>% 
  ungroup %>% 
  #filter(gas_pt > quantile(gas_pt, 0.05), gas_pt < quantile(gas_pt, 0.95)) %>% 
  #group_by(facility_id) %>% filter(n()==9) %>% 
  mutate(cluster=paste0(year, region), gas=gas) %>% 
  mutate(
    gas=ifelse(state_id=="RI"& year==2012, gas/2, gas),
    efficiency=gas/waste) %>% 
  left_join(
    captured_methane %>% 
      select(annual_qty_recoverd_ch4_eq_hh4, reporting_year, facility_id),
    by = c("facility_id", "year"="reporting_year")) %>% 
  rename (recovered=annual_qty_recoverd_ch4_eq_hh4) %>% 
  mutate (recovered_pg = recovered/gas)



all<-fixest::feols (log(gas) ~ treated| facility_id+year, gas_fc)
ma <-fixest::feols (log(gas) ~ treated| facility_id+year, gas_fc%>% filter(!state_id %in% c("CA", "RI", "CT", "VT")))
ca <-fixest::feols (log(gas) ~ treated| facility_id+year, gas_fc%>% filter(!state_id %in% c("MA", "RI", "CT", "VT")) %>% mutate(post = ifelse(year >= 2016, 1, 0) ))
ri <-fixest::feols (log(gas) ~ treated| facility_id+year, gas_fc%>% filter(!state_id %in% c("CA", "MA", "CT", "VT")) %>% mutate(post = ifelse(year >= 2016, 1, 0) ))
ct <-fixest::feols (log(gas) ~ treated| facility_id+year, gas_fc%>% filter(!state_id %in% c("CA", "RI", "MA", "VT")))
vt <-fixest::feols (log(gas) ~ treated| facility_id+year, gas_fc%>% filter(!state_id %in% c("CA", "RI", "CT", "MA")))

fixest::etable (all, ma, ca, ri, vt, ct, vcov="twoway")

all<-fixest::feols (gas_pt ~ treated| facility_id+year, gas_fc)
ma <-fixest::feols (gas_pt ~ treated| facility_id+year, gas_fc%>% filter(!state_id %in% c("CA", "RI", "CT", "VT")))
ca <-fixest::feols (gas_pt ~ treated| facility_id+year, gas_fc%>% filter(!state_id %in% c("MA", "RI", "CT", "VT")) %>% mutate(post = ifelse(year >= 2016, 1, 0) ))
ri <-fixest::feols (gas_pt ~ treated| facility_id+year, gas_fc%>% filter(!state_id %in% c("CA", "MA", "CT", "VT")) %>% mutate(post = ifelse(year >= 2016, 1, 0) ))
ct <-fixest::feols (gas_pt ~ treated| facility_id+year, gas_fc%>% filter(!state_id %in% c("CA", "RI", "MA", "VT")))
vt <-fixest::feols (gas_pt ~ treated| facility_id+year, gas_fc%>% filter(!state_id %in% c("CA", "RI", "CT", "MA")))

fixest::etable (all, ma, ca, ri, vt, ct, vcov="twoway")

######## capture rates DID ################
#add winsoring filter:   filter(gas_pt > quantile(gas_pt, 0.05), gas_pt < quantile(gas_pt, 0.95)) for exact results
# Robust if not too
all<-fixest::feols (recovered_pg ~ treated| facility_id+cluster, gas_fc)
ma <-fixest::feols (recovered_pg ~ treated| facility_id+cluster, gas_fc%>% filter(!state_id %in% c("CA", "RI", "CT", "VT")))
ca <-fixest::feols (recovered_pg ~ treated| facility_id+cluster, gas_fc%>% filter(!state_id %in% c("MA", "RI", "CT", "VT")) %>% mutate(post = ifelse(year >= 2016, 1, 0) ))
ri <-fixest::feols (recovered_pg ~ treated| facility_id+cluster, gas_fc%>% filter(!state_id %in% c("CA", "MA", "CT", "VT")) %>% mutate(post = ifelse(year >= 2016, 1, 0) ))
ct <-fixest::feols (recovered_pg ~ treated| facility_id+cluster, gas_fc%>% filter(!state_id %in% c("CA", "RI", "MA", "VT")))
vt <-fixest::feols (recovered_pg ~ treated| facility_id+cluster, gas_fc%>% filter(!state_id %in% c("CA", "RI", "CT", "MA")))

fixest::etable (all, ca, ct, ma, ri, vt, vcov="twoway", tex=TRUE, digits = "r3", digits.stats = "r2")


gas_st <- gas_fc %>% group_by(year, state_id, treated) %>% summarise(waste = sum(waste), gas_per_facility = mean(gas), gas=sum(gas))
gas_st <- 
  gas_st %>% 
  left_join (
    power2 %>% 
      as_tibble() %>% 
      filter(
        type %in% c("disposal")
      ) %>% 
      group_by(state_id, year) %>% 
      summarise(
        fiori_waste = sum(tons, na.rm=TRUE)
      ) %>%   
      left_join(
        population %>% group_by(state_id, year) %>% summarise(state_pop=sum(pop, na.rm = TRUE)), 
        by = c("state_id", "year")
      ) %>% 
      group_by(state_id) %>% 
      summarise(avg_waste_disp = mean(fiori_waste/state_pop))
  ) %>% 
  mutate(
    implied_pop = waste/avg_waste_disp, 
    gas_pc = gas/implied_pop
  ) %>% 
  filter(!state_id %in% c("DE", "NV", "ME", "WA", "WI", "PA")) %>% 
  left_join(
    regions, by = c("state_id")
  ) %>% 
  group_by(state_id) %>% 
  mutate(treatment=max(treated), post = ifelse(year >= 2014, 1, 0)) %>% 
  #mutate(gas=ifelse(state_id=="RI"& year==2012, gas/2, gas)) %>% 
  mutate(efficiency=gas/waste) %>% 
  left_join(
    population %>% group_by(state_id, year) %>% summarise(state_pop=sum(pop, na.rm = TRUE)), 
    by = c("state_id", "year")
  )




all<-fixest::feols (log(gas) ~ treatment + post + treatment*post| region, gas_st)
ma <-fixest::feols (log(gas) ~ treatment + post + treatment*post| region, gas_st%>% filter(!state_id %in% c("CA", "RI", "CT", "VT")))
ca <-fixest::feols (log(gas) ~ treatment + post + treatment*post| region, gas_st%>% filter(!state_id %in% c("MA", "RI", "CT", "VT")) %>% mutate(post = ifelse(year >= 2016, 1, 0) ))
ri <-fixest::feols (log(gas) ~ treatment + post + treatment*post| region, gas_st%>% filter(!state_id %in% c("CA", "MA", "CT", "VT")) %>% mutate(post = ifelse(year >= 2016, 1, 0) ))
ct <-fixest::feols (log(gas) ~ treatment + post + treatment*post| region, gas_st%>% filter(!state_id %in% c("CA", "RI", "MA", "VT")))
vt <-fixest::feols (log(gas) ~ treatment + post + treatment*post| region, gas_st%>% filter(!state_id %in% c("CA", "RI", "CT", "MA")))

fixest::etable (all, ma, ca, ri, vt, ct)

all<-fixest::feols (log(gas) ~ treated| year+state_id, gas_st)
ma <-fixest::feols (log(gas) ~ treated| year+state_id, gas_st%>% filter(!state_id %in% c("CA", "RI", "CT", "VT")))
ca <-fixest::feols (log(gas) ~ treated| year+state_id, gas_st%>% filter(!state_id %in% c("MA", "RI", "CT", "VT")))
ri <-fixest::feols (log(gas) ~ treated| year+state_id, gas_st%>% filter(!state_id %in% c("CA", "MA", "CT", "VT")))
ct <-fixest::feols (log(gas) ~ treated| year+state_id, gas_st%>% filter(!state_id %in% c("CA", "RI", "MA", "VT")))
vt <-fixest::feols (log(gas) ~ treated| year+state_id, gas_st%>% filter(!state_id %in% c("CA", "RI", "CT", "MA")))

fixest::etable (all, ma, ca, ri, vt, ct, vcov="twoway")


all<-fixest::feols (gas_pc ~ treated| year+state_id, gas_st)
ma <-fixest::feols (gas_pc ~ treated| year+state_id, gas_st%>% filter(!state_id %in% c("CA", "RI", "CT", "VT")))
ca <-fixest::feols (gas_pc ~ treated| year+state_id, gas_st%>% filter(!state_id %in% c("MA", "RI", "CT", "VT")))
ri <-fixest::feols (gas_pc ~ treated| year+state_id, gas_st%>% filter(!state_id %in% c("CA", "MA", "CT", "VT")))
ct <-fixest::feols (gas_pc ~ treated| year+state_id, gas_st%>% filter(!state_id %in% c("CA", "RI", "MA", "VT")))
vt <-fixest::feols (gas_pc ~ treated| year+state_id, gas_st%>% filter(!state_id %in% c("CA", "RI", "CT", "MA")))

fixest::etable (all, ma, ca, ri, vt, ct)

all<-fixest::feols (efficiency ~ treated| year+state_id, gas_st)
ma <-fixest::feols (efficiency ~ treated| year+state_id, gas_st%>% filter(!state_id %in% c("CA", "RI", "CT", "VT")))
ca <-fixest::feols (efficiency ~ treated| year+state_id, gas_st%>% filter(!state_id %in% c("MA", "RI", "CT", "VT")))
ri <-fixest::feols (efficiency ~ treated| year+state_id, gas_st%>% filter(!state_id %in% c("CA", "MA", "CT", "VT")))
ct <-fixest::feols (efficiency ~ treated| year+state_id, gas_st%>% filter(!state_id %in% c("CA", "RI", "MA", "VT")))
vt <-fixest::feols (efficiency ~ treated| year+state_id, gas_st%>% filter(!state_id %in% c("CA", "RI", "CT", "MA")))

fixest::etable (all, ma, ca, ri, vt, ct)
fixest::etable (all, ma, ca, ri, vt, ct, vcov="twoway")

all<-fixest::feols (gas_per_facility ~ treated| year+state_id, gas_st)
ma <-fixest::feols (gas_per_facility ~ treated| year+state_id, gas_st%>% filter(!state_id %in% c("CA", "RI", "CT", "VT")))
ca <-fixest::feols (gas_per_facility ~ treated| year+state_id, gas_st%>% filter(!state_id %in% c("MA", "RI", "CT", "VT")))
ri <-fixest::feols (gas_per_facility ~ treated| year+state_id, gas_st%>% filter(!state_id %in% c("CA", "MA", "CT", "VT")))
ct <-fixest::feols (gas_per_facility ~ treated| year+state_id, gas_st%>% filter(!state_id %in% c("CA", "RI", "MA", "VT")))
vt <-fixest::feols (gas_per_facility ~ treated| year+state_id, gas_st%>% filter(!state_id %in% c("CA", "RI", "CT", "MA")))

fixest::etable (all, ma, ca, ri, vt, ct)
fixest::etable (all, ma, ca, ri, vt, ct, vcov="twoway")



do_many_times_gas <- function (i, x, test_ind_end1, test_ind_end2,y_train, y_test, y_att, n_don,sample_size)
{
  #Approach 2- Only Intercept
  samples <- sample(n_don, sample_size)
  x <- rowMeans(x[, samples])
  n <- length(y_train)+ length(y_test) + length(y_att)
  
  intercept <- mean(y_train-x[1:test_ind_end1])
  
  ss_res <- sum((y_train-x[1:test_ind_end1] - intercept)^2) #calculating the in-sample R-squared
  ss_tot <- sum((y_train-mean(y_train))^2)
  r <- 1- ss_res/ss_tot
  MA  <- (intercept + x[(test_ind_end1+1):test_ind_end2] - y_test )/(intercept + x[(test_ind_end1+1):test_ind_end2]) 
  MA <- MA %>% abs %>%  mean
  
  att <- (y_att-x[(test_ind_end2+1):n]-intercept) %>% sum
  cf <- (x[(test_ind_end2+1):n]+intercept) %>% sum
  intercept2 <-  mean(c(y_train, y_test)-x[1:test_ind_end2])
  att2 <- ((y_att-x[(test_ind_end2+1):n]-intercept2) %>% sum)/(x[(test_ind_end2+1):n]+intercept2) %>% sum
  
  c(r, MA, att, cf,intercept,att2, intercept2, c(samples))
}


dt_state<- gas_st %>% mutate(tons_pc=efficiency)

donors <- dt_state %>% filter(!state_id %in% all_treated) %>% ungroup %>% summarise(state_id=unique(state_id)) %>% pluck("state_id")
offset <- 3
dt_state <- dt_state %>% mutate(county_id=state_id)
year_start <- 2010
year_end <- 2018
dt_state_initial <- dt_state
xy_plot_data_function <- function (treated_state, f, seed, treated_state_2)
{
  set.seed(seed)
  # if(treated_state_2 == "MA")
  # {
  #   dt_state <- dt_state_initial %>% group_by(state_id) %>% 
  #     mutate(
  #       tons_pc = tons_pc*1 + 0*ifelse(is.na(lag(tons_pc, n=1, default = NA)), tons_pc, lag(tons_pc, n=1, default = NA)) 
  #     )
  # }else if(treated_state_2 == "VT")
  # {
  #   dt_state <- dt_state_initial %>%group_by(state_id) %>% 
  #     mutate(
  #       lag =lag(tons_pc, n=1, default = NA),
  #       tons_pc = tons_pc*.5 + .5*ifelse(is.na(lag(tons_pc, n=1, default = NA)), tons_pc, lag(tons_pc, n=1, default = NA)) 
  #     )
  # }else if(treated_state_2 == "CA")
  # {
  #   dt_state <- dt_state_initial %>%group_by(state_id) %>% 
  #     mutate(
  #       tons_pc = tons_pc*.75 + .25*ifelse(is.na(lag(tons_pc, n=1, default = NA)), tons_pc, lag(tons_pc, n=1, default = NA)) 
  #     )
  # }else {dt_state <- dt_state_initial} 
  
  
  dt <- dt_state %>% as.data.frame()
  if (treated_state %in% all_treated){ban_year <- bans[which(all_treated == treated_state)]}else{ban_year <- bans[which(all_treated == treated_state_2)]}
  year_end <- ban_year-offset
  treated_location <- treated_state
  don_new <- donors[donors!=treated_state]
  don_new <- donors[!donors %in% all_treated]
  n_don <- length(don_new)
  test_ind_end1 <- year_end - year_start+1
  test_ind_end2 <- ban_year-year_end +test_ind_end1-1
  
  y <- dt[dt$county_id==treated_location, c("tons_pc")] 
  y_train <- y[1:test_ind_end1]
  y_test <-  y[(test_ind_end1+1):test_ind_end2]
  y_att <- y[(test_ind_end2+1):length(y)]
  
  x <- dt[!dt$county_id %in% treated_counties_id,]
  x <- x[x$county_id!=treated_location, c("tons_pc", "county_id")]
  x <- as.matrix(unstack(x, tons_pc ~ county_id)) 
  
  res <- tibble(
    r_sq = 0, 
    mape = 0, 
    att=0, 
    cf=0, 
    att2 =0, 
    intercept2=0,
    intercept = 0, 
    sample_size=0, 
    county_id =0, 
    iterations = 0, 
    ban_year = 0, 
    donor_number=0, 
    chosen_donor=0
  )
  
  sample_size <- samp[f]
  all <- lapply(seq(1:iterations),do_many_times_gas,x, test_ind_end1, test_ind_end2,y_train, y_test, y_att,n_don, sample_size)
  donor_cols <- paste0(rep("V", sample_size), paste0("", c(4:(4+sample_size-1))))
  
  all <- all %>% sapply(c) %>% t
  
  colnames(all) <- c(
    "r_sq",
    "mape", 
    "att",
    "cf",
    "intercept",
    "att2", 
    "intercept2",
    paste0(rep("donor", sample_size), paste0("_", c(1:sample_size)))
  )
  
  
  all <- 
    all %>% 
    as_tibble %>% 
    filter(r_sq > 0) %>% 
    arrange(mape) %>% 
    slice(1:50) %>% 
    mutate(
      sample_size = samp[f], 
      county_id = treated_location, 
      iterations = iterations, 
      ban_year = ban_year, 
      att = att/cf
    ) %>%  
    pivot_longer(
      cols = c(paste0(rep("donor", sample_size), paste0("_", c(1:sample_size)))), 
      names_to = "donor_number", 
      values_to = "chosen_donor"
    ) %>% 
    mutate(
      chosen_donor = colnames(x)[chosen_donor]
    )
  
  attempts <-all %>% filter(donor_number == "donor_1") %>% summarise(n= n()) %>%  pluck("n")
  
  all <- 
    all %>%  
    mutate(
      attempt = rep(1:attempts, each= sample_size)
    )
  
  
  all
  
  
}


f <- 4
seed <- 2
c<-1
sc_data_ma <-xy_plot_data_function("MA", f, seed, "MA")
sc_data_ct <-xy_plot_data_function("CT", f, seed, "CT")
sc_data_ca <-xy_plot_data_function("CA", f, seed, "CA") 
sc_data_ri <-xy_plot_data_function("RI", f, seed, "RI")
sc_data_vt <-xy_plot_data_function("VT", f, seed, "VT")

plot_sc <- function (all){
  all %>% as_tibble() %>% 
    #rename(state_id = county_id)  %>% 
    left_join(dt_state %>% select(state_id, year, tons_pc), by = c("county_id" = "state_id")) %>% 
    select(-iterations) %>% 
    left_join(
      dt_state %>% 
        select(state_id, year, tons_pc, county_id) %>% 
        rename (y=tons_pc, donor_state_id = state_id),
      by = c("chosen_donor"="county_id", "year")) %>% 
    group_by(year, county_id, intercept2, intercept, ban_year, attempt, tons_pc) %>% 
    summarise(y=mean(y)) %>%
    mutate(
      y_0  = y+intercept2
    ) %>% filter(attempt < 5) %>% 
    ggplot(aes(x=year))+geom_line(aes(y = tons_pc), color="black")+geom_line(aes(y=y_0, color=as.factor(attempt)))
}

reg_sc <- function (sc_data)
{
  chosen <- c(sc_data %>% filter(attempt==1) %>% pluck("chosen_donor"), sc_data %>%summarise(county_id=unique(county_id)) %>%  pluck("county_id"))
  
  
  lm1 <- fixest::feols (log(gas) ~ treated| year+state_id, gas_st%>% filter(state_id %in% chosen))
  lm2 <- fixest::feols (gas_pc  ~ treated| year+state_id, gas_st%>% filter(state_id %in% chosen))
  
  fixest::etable(lm1, lm2)
}

att_sc <- function (all){
  all %>% as_tibble() %>% 
    group_by(attempt) %>% 
    summarise(att2=unique(att2))
}

power_gas <- function (i, treated_state_2, f){
  xy_plot_data_function(donors[i], f, seed, treated_state_2) %>% as_tibble() %>% 
    group_by(attempt) %>% 
    summarise(att2=unique(att2)) %>% 
    mutate(treated_location=donors[i], treated_state_2=treated_state_2)
}

power_gas_res_ma <- lapply(1:25, power_gas, "MA",f)
power_gas_res_ct <- lapply(1:25, power_gas, "CT",f)
power_gas_res_ca <- lapply(1:25, power_gas, "CA",f)
power_gas_res_ri <- lapply(1:25, power_gas, "RI",f)
power_gas_res_vt <- lapply(1:25, power_gas, "VT",f)


sc_data <- 
  rbind(
    sc_data_ma, 
    sc_data_ct,
    sc_data_ri, 
    sc_data_ca,
    sc_data_vt
  )  

#write.csv(sc_data, "sc_data_ghg.csv", row.names = FALSE)
sc_data <- read.csv("sc_data_ghg.csv")

actual_effects <- 
  sc_data %>% 
  group_by(county_id) %>% 
  filter(attempt==1)%>% 
  summarise(actual=unique(att2)) %>% 
  rename(state_id=county_id) %>% 
  left_join(
    tibble(
      ban_year = c(2014, 2014, 2016, 2016, 2014), 
      state_id = c("CT", "MA", "CA", "RI", "VT")
    ), by = c("state_id")
  )


mfood <- 
  wcs %>% 
  filter(state_id%in% all_treated) %>% 
  group_by(state_id, generator_category) %>% 
  summarise(
    mfood=mean(food_share)) %>% 
  group_by(state_id) %>% 
  summarise(mfood=sum(mfood)) %>% 
  mutate(mfood=ifelse(state_id=="MA", 0.2045, mfood), mfood=100*mfood)



expected_effects <- 
  sc_data %>% 
  filter(attempt==1) %>% 
  left_join(dt_state %>% select(state_id, year, tons_pc), by = c("county_id" = "state_id")) %>% 
  select(-iterations) %>% 
  left_join(
    dt_state %>% 
      select(state_id, year, tons_pc, county_id) %>% 
      rename (y=tons_pc, donor_state_id = state_id),
    by = c("chosen_donor"="county_id", "year")) %>% 
  group_by(year, county_id, intercept2, intercept, ban_year, attempt, tons_pc) %>% 
  summarise(y=mean(y)) %>%
  mutate(
    y_0  = y+intercept2
  ) %>% 
  left_join(
    disposal_effect_size %>% filter(effect_type== "lower_bound") %>%  select(year, state_id, effect_size), 
    by = c("year", "county_id" = 'state_id')
  ) %>%
  left_join(mfood, by = c("county_id"="state_id")) %>% 
  left_join(
    tibble(
      state_id=c("MA", "CA", "VT"), 
      reg_effect = reg_effect # Vermont's reg expect comes from: Regulators expectations are 17.52% to 21.78% by 2022. 
      #Using our coverage we find that by 2022 exp effect is 15.9%. Then using simple linear approximation and taking the mean (0.0824*21.78/15.9+0.0824*17.52/15.9)/2 we get this number
    ), 
    by = c("county_id"="state_id")
  ) %>% 
  group_by(county_id) %>% 
  mutate(
    effect_size = ifelse(county_id=="CA", effect_size *0.6, effect_size), # because it covers many more materials other than food waste
    reg_effect = ifelse(county_id=="CA", reg_effect *0.6, reg_effect), # because it covers many more materials other than food waste
    prior_to_the_ban = ifelse(year==ban_year-1, tons_pc, 0),
    prior_to_the_ban= ifelse(prior_to_the_ban==0, max(prior_to_the_ban, na.rm=TRUE), prior_to_the_ban),
    effect_size =1- (prior_to_the_ban*100*.58*(mfood-effect_size*100)/mfood+prior_to_the_ban*100*(1-.58))/(prior_to_the_ban*100), 
    reg_effect =NA#1- (prior_to_the_ban*100*.58*(mfood-reg_effect*100)/mfood+prior_to_the_ban*100*(1-.58))/(prior_to_the_ban*100)
    
  ) %>% 
  select(-prior_to_the_ban) %>% 
  group_by(county_id,reg_effect) %>% 
  summarise(expected_effect = mean(effect_size, na.rm=TRUE))

# power_gas_res <- 
#   rbind(
#     power_gas_res_ma %>% bind_rows, 
#     power_gas_res_ca %>% bind_rows, 
#     power_gas_res_ct %>% bind_rows, 
#     power_gas_res_ri %>% bind_rows, 
#     power_gas_res_vt %>% bind_rows
#   )


#write.csv(power_gas_res, "power_gas_res.csv", row.names=FALSE)
power_gas_res <- read.csv("power_gas_res.csv")

power_gas_res_plot <- 
  power_gas_res %>% 
  group_by(treated_state_2, attempt) %>% 
  summarise(
    min = quantile(att2, 0.025), 
    max = quantile(att2, 0.975)) %>%  
  filter(min == max(min)) %>% 
  group_by(treated_state_2) %>% 
  summarise(min = unique(min), max = min(max)) %>% 
  
  left_join(actual_effects, by = c("treated_state_2"="state_id")) %>% 
  rename(state_id=treated_state_2) %>% 
  mutate(
    state_id = factor(state_id, levels = c("CA", "CT", "MA", "RI", "VT"))
  ) %>% 
  left_join(expected_effects, by = c("state_id"="county_id")) %>% 
  mutate(
    state_id = fct_recode(
      state_id,
      "Vermont" = "VT",
      "Rhode Island" = "RI",
      "Massachusetts" = "MA",
      "Connecticut" = "CT",
      "California" = "CA"
    )) %>% 
  ggplot()+
  aes(y= state_id, x= 100*actual, group = 1)+
  
  geom_errorbar(aes(xmin = 100*min, xmax = 100*max, color = "Placebo"), width = 0.2, size=0.5)+
  scale_color_manual(breaks = c("Placebo"), values = c(ut_colors[5]), guide = guide_legend(order = 2, legend.spacing.x=unit(-1, "cm"), byrow=TRUE ))+
  geom_vline(xintercept = 0, lty = "dotted", color = ut_colors[5])+
  labs(y="", x = "", color = "")+
  ggnewscale::new_scale_color()+
  # 
  geom_point(aes(color = "Estimate"),size=1.5)+
  scale_color_manual(breaks = c("Estimate"), values = c(ut_colors[4]),guide = guide_legend(order = 1,  legend.spacing.x=unit(-1, "cm"), byrow=TRUE))+
  labs(y="", x = "", color = "")+
  ggnewscale::new_scale_color()+
  
  geom_errorbar(aes(xmin=-100*expected_effect, xmax=-100*expected_effect, color = "Our Exp."), linewidth=1, width=0.2)+
  scale_color_manual(breaks = c("Our Exp."), values = c("seagreen" ),guide = guide_legend(order = 3,  legend.spacing.x=unit(-1, "cm"), byrow=TRUE))+
  labs(y="", x = "", color = "")+
  ggnewscale::new_scale_color()+
  
  #geom_errorbar(aes(xmin=-100*reg_effect, xmax=-100*reg_effect, color = "Regulators' Exp."), linewidth=1, width=0.2)+
  #scale_color_manual(breaks = c("Regulators' Exp."), values = c("#bad9c6" ),guide = guide_legend(order = 4,  legend.spacing.x=unit(-1, "cm"), byrow=TRUE))+
  #labs(y="", x = "", color = "")+
  
  
  scale_x_continuous(breaks = c(seq(-40, 40, by = 15)), limits = c(-45, 45))+ 
  scale_y_discrete(position= "right", expand=c(0.02,0.15), limits = rev(c("California", "Connecticut", "Massachusetts", "Rhode Island", "Vermont")))+
  labs(x="", title = "Average treatment effect (%)")+
  theme(
    legend.position = "top",  # Keep the legend at the top
    legend.box.margin = margin(t = 0, r = 0, b = 0, l = 70),
    strip.background = element_rect(color = "white", fill = "white"),
    panel.grid.major.x = element_blank() ,
    panel.grid.major.y = element_blank(),
    panel.background = element_blank(), 
    text = element_text(family = "Helvetica",size = 10, color= ut_colors[4]), 
    strip.text = element_text(angle = 0, hjust = 1), 
    strip.placement = "outside",
    axis.ticks.y = element_blank(), 
    legend.key = element_rect(colour = NA, fill = NA, size = 5),
    legend.spacing.x = unit(-1, "pt"),
    axis.ticks.x = element_line(size = 0.1), 
    legend.text = element_text(family = "Helvetica",size = 9, color= ut_colors[4]),
    plot.title = element_text(hjust=0.6, size = 9, color=ut_colors[4])
  )





xy_gas_plot_function <- function (chosen_attempt)
{
  xy_gas_data <- 
    sc_data %>% filter(attempt==chosen_attempt)%>% 
    left_join(dt_state %>% select(state_id, year, tons_pc), by = c("county_id" = "state_id")) %>% 
    select(-iterations) %>% 
    left_join(
      dt_state %>% 
        select(state_id, year, tons_pc, county_id) %>% 
        rename (y=tons_pc, donor_state_id = state_id),
      by = c("chosen_donor"="county_id", "year")) %>% 
    group_by(year, county_id, intercept2, intercept, ban_year, attempt, tons_pc) %>% 
    summarise(y=mean(y)) %>%
    mutate(
      y_0  = y+intercept2
    ) %>% 
    left_join(
      disposal_effect_size %>% filter(effect_type== "lower_bound") %>%  select(year, state_id, effect_size), 
      by = c("year", "county_id" = 'state_id')
    ) %>%
    left_join(mfood, by = c("county_id"="state_id")) %>% 
    left_join(
      tibble(
        state_id=c("MA", "CA", "VT"), 
        reg_effect = reg_effect # Vermont's reg expect comes from: Regulators expectations are 17.52% to 21.78% by 2022. 
        #Using our coverage we find that by 2022 exp effect is 15.9%. Then using simple linear approximation and taking the mean (0.0824*21.78/15.9+0.0824*17.52/15.9)/2 we get this number
      ), 
      by = c("county_id"="state_id")
    ) %>% 
    group_by(county_id) %>% 
    mutate(
      effect_size = ifelse(county_id=="CA", effect_size *0.6, effect_size), # because it covers many more materials other than food waste

      reg_effect = ifelse(county_id=="CA", reg_effect *0.6, reg_effect), # because it covers many more materials other than food waste
      prior_to_the_ban = ifelse(year==ban_year-1, tons_pc, 0),
      prior_to_the_ban= ifelse(prior_to_the_ban==0, max(prior_to_the_ban, na.rm=TRUE), prior_to_the_ban),
      effect_size =1- (prior_to_the_ban*100*.58*(mfood-effect_size*100)/mfood+prior_to_the_ban*100*(1-.58))/(prior_to_the_ban*100), 
      reg_effect =NA#1- (prior_to_the_ban*100*.58*(mfood-reg_effect*100)/mfood+prior_to_the_ban*100*(1-.58))/(prior_to_the_ban*100)
      ) %>% select(-prior_to_the_ban) %>% 
    mutate(
      y_0_effect = ifelse(year>=ban_year, y_0*(1-effect_size), NA), 
      `Regulators' Exp.` = ifelse(!is.na(effect_size), y_0*(1-reg_effect), NA)
    ) %>% 
    rename(
      Synthetic = y_0, 
      Actual = tons_pc, 
      `Our Exp.` = y_0_effect,
      treated_state=county_id
    ) %>% 
    pivot_longer(
      cols = c("Synthetic", "Actual", "Our Exp.", `Regulators' Exp.`), 
      names_to = "location", 
      values_to = "tons_pc") %>% 
    mutate(
      y_first = ifelse(year==2010 & location =="Actual", tons_pc %>% round(2), NA), 
      y_last =ifelse(year==2018 & location =="Actual", tons_pc%>% round(2)*1.0, NA)
    ) %>% 
    left_join(
      tibble (xlab= c(2012, 2016-1, 2016+1), ylab = 0.29, label = c("Train", "Validation", "Evaluation"), treated_state = "CA"), 
      by =c ("treated_state", "year"="xlab")
    ) %>% 
    mutate(
      xlab = ifelse(!is.na(ylab), year, NA)
    )

  
  
  xy_gas_data <- 
    xy_gas_data %>% 
    left_join(
      #for end points to get rid of geom_vline
      xy_gas_data %>% 
        group_by(treated_state, ban_year) %>% 
        mutate(y_min = min(tons_pc, na.rm=TRUE), ymax=max(tons_pc, na.rm=TRUE)) %>% 
        filter(year==ban_year, location %in% c("Actual", "Synthetic", "Our Exp.")) %>% 
        mutate(
          y_end_low = min(tons_pc, na.rm=TRUE),
          y_end_high = max(tons_pc, na.rm=TRUE)
        ) %>% select(treated_state, y_end_low, y_min, y_end_high, ymax) %>% 
        mutate(
          y_end_low = ifelse(y_min < y_end_low-0.02, y_end_low-0.04, y_min), 
          y_end_high = ifelse(ymax <y_end_high+0.03, y_end_low+0.13, y_end_high+0.05)
        ) %>% 
        group_by(treated_state, ban_year) %>% 
        summarise(
          y_end_low = unique(y_end_low), 
          y_end_high = unique(y_end_high)
        ) %>% 
        mutate(
          location= "Actual"),
      by = c("year"="ban_year", "treated_state", "location")
    ) %>% 
    left_join(
      #for end points to get rid of geom_vline
      xy_gas_data %>% 
        group_by(treated_state, ban_year) %>% 
        mutate(y_min = min(tons_pc, na.rm=TRUE), ymax=max(tons_pc, na.rm=TRUE)) %>% 
        filter(year==ban_year-3, location %in% c("Actual", "Synthetic")) %>% 
        mutate(
          y_end_low = min(tons_pc, na.rm=TRUE),
          y_end_high = max(tons_pc, na.rm=TRUE)
        ) %>% select(treated_state, y_end_low, y_min, y_end_high, ymax) %>% 
        mutate(
          y_end_low = ifelse(y_min < y_end_low-0.02, y_end_low-0.04, y_min), 
          y_end_high = ifelse(ymax <y_end_high+0.03, y_end_low+0.13, y_end_high+0.05)
        ) %>% 
        group_by(treated_state, ban_year) %>% 
        summarise(
          year=ban_year-3,
          y_end_low_2 = unique(y_end_low), 
          y_end_high_2 = unique(y_end_high)
        ) %>% 
        mutate(
          location= "Actual"
        )%>% 
        ungroup %>% 
        select(-ban_year), 
      by = c("year", "treated_state", "location")
    )
  
  xy_gas_data %>%  
    ggplot(
      aes(x=year, y=tons_pc, color =location, linetype= location, size=location)
    )+
    geom_segment(
      aes(x=ban_year, xend=ban_year, y=y_end_low, yend  = y_end_high),
      linetype = "dotted", linewidth = 0.2, color = ut_colors[5])+
    geom_segment(
      aes(x=ban_year-2, xend=ban_year-2, y=y_end_low_2, yend  = y_end_high_2),
      linetype = "dotted", linewidth = 0.2, color = ut_colors[5])+
    
    geom_line()+
    facet_grid(
      rows=vars(factor(treated_state, levels = c("CA", "CT", "MA", "RI", "VT"))),
      scales="free_y"
    )+
    geom_point(data = subset(xy_gas_data, location %in% c("Actual", "Synthetic")), size = .25)+
    
    geom_text(aes(x=xlab, y=ylab-0.01, label = label), color=ut_colors[5], size=2.5, family="Helvetica")+
    geom_text(aes(x=2009.5, y = y_first%>% as.numeric, label=scales::number(y_first, accuracy = 0.01) ), color=rgb(90,90,90, maxColorValue = 255), size=3, family="Helvetica")+
    geom_text(aes(x=2018.5, y = y_last %>% as.numeric, label=scales::number(y_last, accuracy = 0.01) ), color=rgb(90,90,90, maxColorValue = 255), size=3, family="Helvetica")+
    
    #scale_color_manual(breaks= c("Actual", "Synthetic", "Our Exp.", "Regulators' Exp."), values = c(ut_colors[4],ut_colors[5],"seagreen", "#bad9c6"), name = "")+
    #scale_linetype_manual(breaks= c("Actual", "Synthetic", "Our Exp.", "Regulators' Exp."), values = c("solid", "solid", "solid", "solid"), name = "")+
    #scale_size_manual(breaks= c("Actual", "Synthetic", "Our Exp.", "Regulators' Exp."), values = c(0.5, 0.5, 1.0, 1.0), name = "")+
    
    scale_color_manual(breaks= c("Actual", "Synthetic", "Our Exp."), values = c(ut_colors[4],ut_colors[5],"seagreen", "#bad9c6"), name = "")+
    scale_linetype_manual(breaks= c("Actual", "Synthetic", "Our Exp."), values = c("solid", "solid", "solid", "solid"), name = "")+
    scale_size_manual(breaks= c("Actual", "Synthetic", "Our Exp."), values = c(0.5, 0.5, 1.0, 1.0), name = "")+
    scale_x_continuous(breaks=c(seq(2010, 2018, 2)), limits=c(2009, 2019), expand = c(0,0))+
    scale_y_continuous(expand = c(0.03,0.02))+
    labs(y="", x= "")+
    theme_classic()+
    theme(
      legend.position = "top",
      strip.background = element_rect(color = "white", fill = "white"),
      panel.grid.major.x = element_blank() ,
      panel.grid.major.y = element_blank(), 
      text = element_text(family = "Helvetica",size = 9, color= ut_colors[4]), 
      axis.line.x = element_blank(),
      axis.line.y = element_blank(), 
      axis.ticks.y = element_blank(), 
      axis.text.y= element_blank(),
      axis.ticks.x = element_line(size = 0.1), 
      legend.text = element_text(family = "Helvetica", color = ut_colors[4],size = 9),
      panel.spacing = unit(1, "cm")
    )
}


xy_gas_plot <- xy_gas_plot_function(1)
xy_gas_plot <- 
  xy_gas_plot+
  labs(x="", y = "", title = "Emissions (tons per landfilled ton)") + 
  theme(
    strip.text = element_blank(),
    plot.title = element_text(family = "Helvetica", color = ut_colors[4],size = 9, hjust=0.5))

  
xy_and_power_gas <-
  ggpubr::ggarrange(
    xy_gas_plot,
    power_gas_res_plot,
    heights = c(0.5, 1))

xy_and_power_gas

ggsave(
  xy_and_power_gas, filename = "xy_and_power_gas.pdf", device = cairo_pdf,
  path= figure_path,
  width = 8, height = 7, units = "in")


# reg_sc(sc_data_ct)
# reg_sc(sc_data_ma)
# reg_sc(sc_data_ca)
# reg_sc(sc_data_ri)
# reg_sc(sc_data_vt)

plot_sc(sc_data_ct)
plot_sc(sc_data_ma)
plot_sc(sc_data_ca)
plot_sc(sc_data_ri)
plot_sc(sc_data_vt)