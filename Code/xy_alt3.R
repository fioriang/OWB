############### File description ###########

# In this code there are xx parts 

# Part 1: Loading the data, paths, pre-processing and functions 
# Part 2: Running the code necessary for the left side of Fig.2 (that creates the time series of actual and synthetic disposal)
# Part 3: Running the code necessary for the right side of Fig.2 (that creates the placebo CI and plots the actual effect)
# Part 4: Creating the plot for the city-level bans (Fig. S7)
# Part 5: Saving the values (this is needed for overleaf. basically all the values, ATTs, p-values, reject thresholds, errors etc are saved in txt files and reported directly on overleaf)
# Part 6: Monte Carlo simulations 

#### Libraries######### 
library(tidyverse)
library(ggplot2)
library(ggnewscale)
#library(data.table) 

##### Basic pre-processing functions ####

pre_processing_dt <- function(power2)
{
  year_start <- 2006
  year_cutoff <- 2018
  year_end <- 2011
  
  dt <- power2
  
  
  dt_ca <- 
    dt %>% #filter(county_name!="calaveras") %>% 
    filter(
      year >= year_start, 
      year <= year_cutoff, 
      type %in% c("disposal", "msw_disposed"),
      state_id=="CA"
    ) %>% 
    group_by(county_name) %>% 
    mutate(
      tons_high = quantile(tons, 0.975), 
      tons_low = quantile(tons, 0.025), 
      tons = ifelse(tons> tons_high, tons_high, tons), 
      tons = ifelse(tons< tons_low, tons_low, tons)
    ) %>% 
    select(-tons_high) %>% select(-tons_low) %>% ungroup
  
  
  
  dt <- 
    dt %>% 
    as_tibble() %>% 
    filter(state_id!="CA") %>% 
    rbind(
      dt_ca
    ) %>% 
    filter(
      year >= year_start, 
      year <= year_cutoff, 
      type %in% c("disposal", "msw_disposed")
    ) %>% 
    group_by (year, state_id, county_name) %>% 
    summarise(tons = sum(tons)) %>% 
    left_join (
      population %>% group_by(state_id, year) %>% summarise(state_pop = sum(pop)), 
      by = c("state_id", "year")
    ) %>%
    left_join (
      population,
      by = c("state_id", "county_name", "year")
    ) %>% 
    mutate(
      tons_pc = ifelse(is.na(county_name),tons/state_pop, tons/pop), 
      county_id = ifelse(is.na(county_name), state_id, paste0(county_name, state_id) )
    ) %>% 
    filter(
      tons_pc > 0.2
    )
  
  #Exclude missing values and small values
  exc<- 
    as.data.frame(
      dt %>% 
        group_by(county_id) %>% 
        #filter(state_id != "CT", state_id != "RI") %>% 
        summarize(
          m = mean(tons_pc), 
          min_year = min(year), 
          min_tons = min(tons_pc), 
          max_tons = max(tons_pc)) %>%
        filter(
          m < 0.3 | min_year > year_start)#|max_tons>2.5)
    ) %>% as_tibble()
  
  dt <- dt[!(dt$county_id %in% exc$county_id),]
  dt <- dt[dt$year >=year_start  & dt$year <=year_cutoff,]
  exc<- 
    as.data.frame(
      dt %>% 
        group_by(county_id) %>% 
        summarize(n=  n()) %>% 
        filter(
          n < (year_cutoff- year_start+1)
        )
    )
  dt <- dt[!(dt$county_id %in% exc$county_id),]
  
  # Percentage change in disposal tons
  f <- 
    as.data.frame(
      dt %>% 
        filter(
          #state_id != "RI", state_id!="CA", 
          !is.na(county_name)) %>% 
        group_by(county_id) %>% 
        mutate(
          percentage = (tons-dplyr::lag(tons, n = 1, default = NA))/dplyr::lag(tons, n = 1, default = NA),
          percentage_1 = (tons-dplyr::lead(tons, n = 1, default = NA))/dplyr::lead(tons, n = 1, default = NA)
        )
    )
  
  f <- f[!is.na(f$percentage),]
  threshold <- 0.42
  exclude2 <- unique(f$county_id[#f$percentage > threshold | f$percentage < -threshold 
    f$percentage_1 > threshold | f$percentage_1 < -threshold])
  #dt %>% filter(county_id %in% exclude2, year == 2014) %>% ungroup %>% summarise(sum(tons))/
  #dt %>% filter(year == 2014) %>% ungroup %>% summarise(sum(tons))
  
  
  dt <- dt[!dt$county_id %in% exclude2,]
  
  rm(f, exc, exclude2)
  
  dt_initial <- dt
  return(dt_initial)
}

pre_processing_dt_state <- function (power2)
{
  
  year_start <- 2006
  year_cutoff <- 2018
  
  dt_ca <- 
    power2 %>% 
    filter(
      year >= year_start, 
      year <= year_cutoff, 
      type %in% c("disposal", "msw_disposed"),
      state_id=="CA" | state_id=="FL"
    ) %>% 
    group_by(county_name, state_id) %>% 
    mutate(
      tons_high = sort(tons, decreasing=TRUE)[2], # this takes the 2nd largest and 2nd smallest values
      tons_low = sort(tons)[2],
      tons= ifelse(tons>= tons_high, tons_high, tons),
      tons = ifelse(tons<= tons_low, tons_low, tons)
    ) %>% 
    select(-tons_high) %>% select(-tons_low) %>% ungroup
  
  
  dt_state <- 
    rbind(
      power2 %>% filter(!state_id %in% c("CA", "FL")), 
      dt_ca) %>% 
    mutate(county_id=paste0(county_name, state_id)) %>% 
    group_by (year, state_id, type) %>% 
    summarise(tons = sum(tons))%>% 
    filter(
      year >= year_start, 
      year <= year_cutoff,
      type %in% c("disposal", "msw_disposed")
    ) %>%
    group_by(state_id) %>% 
    left_join (
      population %>% group_by(state_id, year) %>% summarise(state_pop = sum(pop)), 
      by = c("state_id", "year")
    ) %>%
    group_by(state_id, year) %>% 
    mutate(
      tons_pc = (tons/state_pop), 
      county_id = state_id
    ) %>% 
    group_by(state_id) %>% 
    mutate(n=n()) %>% 
    filter(n == year_cutoff - year_start+1) %>% 
    select(-n) %>% 
    ungroup() 
  
  # dt_state <- 
  #   dt_state %>% 
  #   group_by(state_id) %>% 
  #   mutate(
  #     lag = ifelse(is.na(lag(tons_pc, n=1, default = NA)), tons_pc, lag(tons_pc, n=1, default = NA)), 
  #     tons_pc = 100*(tons_pc - lag)/lag
  #   ) 
  
  dt_state_initial <- dt_state
  return(dt_state_initial)
  
}


pre_processing_dt_sf <- function(power2)
{
  year_start <- 1996
  year_cutoff <- 2014
  
  dt <- power2
  
  
  dt_ca <- 
    dt %>% #filter(county_name!="calaveras") %>% 
    filter(
      year >= year_start, 
      year <= year_cutoff, 
      type %in% c("disposal", "msw_disposed"),
      state_id=="CA"
    ) %>% 
    group_by(county_name) %>% 
    mutate(
      tons_high = quantile(tons, 0.975), 
      tons_low = quantile(tons, 0.025), 
      tons = ifelse(tons> tons_high, tons_high, tons), 
      tons = ifelse(tons< tons_low, tons_low, tons)
    ) %>% 
    select(-tons_high) %>% select(-tons_low) %>% ungroup
  
  
  
  dt <- 
    dt %>% 
    as_tibble() %>% 
    filter(state_id!="CA") %>% 
    rbind(
      dt_ca
    ) %>% 
    filter(
      year >= year_start, 
      year <= year_cutoff, 
      type %in% c("disposal", "msw_disposed")
    ) %>% 
    group_by (year, state_id, county_name) %>% 
    summarise(tons = sum(tons)) %>% 
    left_join (
      population %>% group_by(state_id, year) %>% summarise(state_pop = sum(pop)), 
      by = c("state_id", "year")
    ) %>%
    left_join (
      population,
      by = c("state_id", "county_name", "year")
    ) %>% 
    mutate(
      tons_pc = ifelse(is.na(county_name),tons/state_pop, tons/pop), 
      county_id = ifelse(is.na(county_name), state_id, paste0(county_name, state_id) )
    ) %>% 
    filter(
      tons_pc > 0.2
    )
  
  #Exclude missing values and small values
  exc<- 
    as.data.frame(
      dt %>% 
        group_by(county_id) %>% 
        #filter(state_id != "CT", state_id != "RI") %>% 
        summarize(
          m = mean(tons_pc), 
          min_year = min(year), 
          min_tons = min(tons_pc), 
          max_tons = max(tons_pc)) %>%
        filter(
          m < 0.3 | min_year > year_start)#|max_tons>2.5)
    ) %>% as_tibble()
  
  dt <- dt[!(dt$county_id %in% exc$county_id),]
  dt <- dt[dt$year >=year_start  & dt$year <=year_cutoff,]
  exc<- 
    as.data.frame(
      dt %>% 
        group_by(county_id) %>% 
        summarize(n=  n()) %>% 
        filter(
          n < (year_cutoff- year_start+1)
        )
    )
  dt <- dt[!(dt$county_id %in% exc$county_id),]
  
  # Percentage change in disposal tons
  f <- 
    as.data.frame(
      dt %>% 
        filter(
          #state_id != "RI", state_id!="CA", 
          !is.na(county_name)) %>% 
        group_by(county_id) %>% 
        mutate(
          percentage = (tons-dplyr::lag(tons, n = 1, default = NA))/dplyr::lag(tons, n = 1, default = NA),
          percentage_1 = (tons-dplyr::lead(tons, n = 1, default = NA))/dplyr::lead(tons, n = 1, default = NA)
        )
    )
  
  f <- f[!is.na(f$percentage),]
  threshold <- 0.42
  exclude2 <- unique(f$county_id[#f$percentage > threshold | f$percentage < -threshold 
    f$percentage_1 > threshold | f$percentage_1 < -threshold])
  #dt %>% filter(county_id %in% exclude2, year == 2014) %>% ungroup %>% summarise(sum(tons))/
  #dt %>% filter(year == 2014) %>% ungroup %>% summarise(sum(tons))
  
  
  dt <- dt[!dt$county_id %in% exclude2,]
  
  rm(f, exc, exclude2)
  
  dt_initial <- dt
  return(dt_initial)
}

######### Paths ######### 
mypathname <-"/Users/fian4421/Library/CloudStorage/Dropbox/Organic Waste Bans"
#mypathname = '/Users/robertsanders/Dropbox/Desktop/A UC San Diego/Research/Active Research/Organic Waste Bans/'



municipal_path <- paste0(mypathname, "/03.1. Municipal Data")
state_data_path <- paste0(mypathname,"/03. State_Data")
base_path <- paste0(mypathname,"/06. Post SYP/00. Code/")
figure_path <- "/Users/fian4421/Library/CloudStorage/Dropbox/Apps/Overleaf/Organic Waste Bans/Figures/Corrections"
######### Other data that are needed ######### 
all_treated <- c("VT", "MA", "CA", "CT", "RI")# Never changes
bans <- c(2014, 2014, 2016, 2014, 2016)
power2 <- read.csv(file=paste0(base_path,"power2_impexp_final_20sep.csv")) 
disposal_effect_size2 <- read.csv("disposal_effect_size2.csv") %>% as_tibble() #needed to caclulate the expected effects
population <- read.csv(paste0(state_data_path,"/00. Controls/Population/population.csv"))
population <- cbind(population[1:2], stack(population[3:31]))
colnames(population)<- c("state_id", "county_name", "pop", "year")
population$year <- substring(population$year, 2) %>% as.integer
population$pop <- as.numeric(population$pop)
population$county_name[population$county_name=="doña ana"] <- "dona ana"
population <- population[population$state_id!="AK" & population$state_id!="co" & population$state_id!="ia",] # contiguous states, DC is considered a contiguous state

ut_colors <- c(#used in plots
  rgb(132, 59,14, max=255), # dark orange
  rgb(255, 127, 21, max=255), # bright orange
  rgb(191,87,0, max=255), # ut orange
  rgb(51,73,72, max=255), # dark grey
  rgb(156, 173, 183, max=255), #light grey
  rgb(191,87,0,alpha=50, max=255))# ut orange

######### Municipal data ######### 
# The data for Seattle and for Boulder are loaded seperately below, we need this for Fig. S7 (not needed for Fig. 2 but the way the functions are set up is needed to run)
year_start = 2006
year_end = 2018
population_seattle <- read.csv(paste0(municipal_path,"/Seattle, WA/import_R/population_seattle.csv"))
composting <- read.csv(paste0(municipal_path,"/Seattle, WA/import_R/seattle_composting.csv"))
disposal <- read.csv(paste0(municipal_path,"/Seattle, WA/import_R/seattle_disposal.csv"))

seattle <- 
  disposal %>%
  rename(
    total = Landfill,
    year = Year, 
    month = Month
  ) %>% 
  pivot_longer(
    cols = c("Self_Haul", "Residential", "Commercial"), 
    names_to = "generator", 
    values_to = "tons"
  ) %>%  
  mutate(
    type = "disposal", 
    generator = str_to_lower(generator)
  ) %>% 
  select(
    year, month, generator, tons, type
  ) %>% 
  rbind(
    composting %>%
      pivot_longer(
        cols = c("self_haul", "residential", "commercial"), 
        names_to = "generator", 
        values_to = "tons"
      ) %>%  
      mutate(
        type = "composting"
      )
  ) %>%  
  group_by(year, type) %>%  
  summarise(
    tons = sum(tons, na.rm = TRUE)
  ) %>% 
  left_join(
    population_seattle %>%  
      rename (pop=population) %>% 
      select (year, pop), 
    by = c("year")
  ) %>%  
  mutate (
    tons_pc = tons/pop,
    state_id = "M1", 
    county_name = "seattle", 
    county_id = paste(county_name, state_id, sep = ""), 
  ) 

waste <- read.csv(paste0(municipal_path,"/Boulder, CO/import_R/boulder_waste.csv"))
boulder_population <- read.csv(paste0(municipal_path,"/Boulder, CO/import_R/boulder_population.csv"))

colnames(waste) <- c("year", "sector", "disposal", "recycling", "organics","reuse", "total", "diversion", "notes")

boulder <- 
  waste %>% 
  filter(
    year >= 2006, 
  ) %>%  
  pivot_longer(
    cols = c("disposal", "recycling", "organics", "reuse", "total"), 
    names_to = "type", 
    values_to = "tons"
  ) %>%  
  select(
    year, type, tons
  ) %>% 
  group_by(year, type) %>% 
  summarise(
    tons = sum(tons, na.rm = TRUE)
  ) %>% 
  left_join(
    boulder_population %>%  
      rename(pop=population),
    by = c("year")) %>%  
  mutate (
    tons_pc = tons/pop, 
    state_id = "M2", 
    county_name = "boulder", 
    county_id = paste(county_name, state_id, sep = ""), 
    type = ifelse(type == "organics", "composting", type)
  )

## Transforming the muncipal data

dt_initial <- pre_processing_dt(power2)

dt_municipal <- 
  dt_initial %>%
  select(year, state_id, county_id, tons_pc, tons) %>%
  rbind(
    seattle %>%  
      filter(
        type == "disposal", 
        year >= year_start, 
        year <= 2018
      ) %>%  
      select(year, state_id, county_id, tons_pc, tons), 
    boulder %>%  
      filter(
        type == "disposal", 
        year >= year_start, 
        year <= 2018
      ) %>%  
      select(year, state_id, county_id, tons_pc, tons)
  ) %>% 
  # filter(
  #   !state_id %in% c("WA", "CO")
  # ) %>%
  as.data.frame

dt_municipal_initial <- dt_municipal

year_start = 2006
year_cutoff = 2018
pooled_ban_year = 2015
offset=3
iterations = 100000
ey = 2018
#vt_p <-48098/power2 %>% filter(year%in%c(2014), type=="disposal", state_id=="VT") %>% summarise(tons=mean(tons)) %>% pluck("tons")
#vt_exp_dec <-(vt_p*0.463/0.922+vt_p*0.665/0.922+vt_p*0.873/0.922+vt_p*0.922/0.922*2)/5
vt_exp_effect <- disposal_effect_size2 %>% filter(state_id=="VT", year<2019) %>% summarise(m=mean(effect_size)) %>% pluck("m")
reg_effect <- c(0.098, 0.185, 0.6*0.316/0.214*vt_exp_effect)



######### Functions######### 

power_state_fun2 <- function(plac, treated_state)
{
  #### 
  # This function creates the placebo intervals
  # In this function, plac (the input) is the result of the placebo runs (i.e., the ATT, the mape etc)
  
  # For the state-level specification we use the SC with the lowest MAPE and create the placebo intervals (the output of the function)
  
  ####
  
  spec4 <- 
    plac %>% 
    bind_rows() %>% 
    filter(sample_size!=0) %>% 
    mutate(county_id = as.character(county_id)) %>% 
    group_by(sample_size, ban_year, county_id ) %>% 
    filter(mape==min(mape)) %>% #choose the SC with the minimum MAPE
    summarise(att = mean(att)) %>% # if there are mulptiple SC wth the same min MAPE take the mean of their ATTs
    group_by(sample_size, ban_year) %>%     
    summarise(
      att_min = sort(att)[2], #because our sample is not super large the quantile 0.025 averages across observations and gives us an att that does not exist. We could choose either the largest or the 2nd largest att as our quantile. Given that the round(0.025*(22)+1) gives us 2 we choose the 2nd observation (that is because 22 is the sample size)
      att_max = sort(att, decreasing = TRUE)[2],
      att_median = mean(att)
    )
  
  
  specs <-
    rbind(
      spec4 %>%  mutate (specification = "State") 
    ) %>% mutate(
      year = ban_year, 
      treated_state = treated_state
    )
  
  return(specs)
  
}

pool_function <-function(i, data, donors, r_threshold,mape_threshold,n, seed)
{
  
  # data =
  # data %>% filter(sample_size!=0) %>% 
  # group_by(sample_size, county_id, ban_year) %>% 
  # filter(mape==min(mape)) %>% 
  # summarise(
  #   att = mean(att),
  #   r_sq = mean(r_sq),
  #   mape = mean(mape), 
  #   cf = mean(cf)
  # )
  
  
  set.seed(seed)
  donors <- tibble(
    county_id =donors, 
    num_id = 1:length(donors)
  )
  
  pooled <- 
    data %>%
    left_join(
      donors, 
      by = c("county_id")
    ) %>% 
    group_by(county_id) %>% 
    mutate(
      pools = sample(nrow(donors),n) %>% list(), .groups = "drop"
    )%>% 
    ungroup #choose at random five states and consider them treated
  
  
  pooled %>%
    mutate(
      num_id = num_id %>% as.integer
    ) %>% 
    filter(
      num_id %in% (
        pooled%>% 
          filter(num_id==i) %>% 
          slice(1) %>% 
          pluck("pools") %>% 
          unlist
      )
    ) %>% 
    group_by(sample_size, ban_year, county_id) %>%
    filter(mape==min(mape)) %>% # of these five states choose the SC that has the min mape
    summarise(
      att=mean(att), # the mean serves two purposes: 1) if there's two SC with the same MAPE take the avg of the two 2) because of the way that the dataset is structured for each chosen donor in the SC there's one line (where the ATT, R_sq and MAPE are the same ofc) to collapse these lines we use the mean 
      r_sq=mean(r_sq), 
      mape=mean(mape), 
      cf= mean(cf), 
      .groups = "drop") %>% 
    group_by(sample_size, ban_year) %>% 
    summarise(
      att = sum(att*cf)/sum(cf), #and finally, to find the ATT of the aggregate case, take the mean of the five chosen states. 
      mape = mean(mape), 
      r_sq = mean(r_sq), 
      i, 
      .groups = "drop"
    ) %>% 
    ungroup
}

disposal_spec_states_function <- function(file_name)
{
  power_state_plac_data <- read.csv(filename)
  power_state1 <- power_state_fun2(power_state_plac_data %>% filter(treated_state=="MA"), "MA") 
  power_state2 <- power_state_fun2(power_state_plac_data %>% filter(treated_state=="CA"), "CA")
  power_state3 <- power_state_fun2(power_state_plac_data %>% filter(treated_state=="CT"), "CT")
  power_state4 <- power_state_fun2(power_state_plac_data %>% filter(treated_state=="RI"), "RI")
  power_state5 <- power_state_fun2(power_state_plac_data %>% filter(treated_state=="VT"), "VT")
  power_state_plac6 <- power_state_plac_data %>% filter(treated_state=="All")
  pool_estimates <- lapply(seq(1:length(donors_state)), pool_function, data = power_state_plac6 %>% bind_rows(), donors = donors_state , 
                           r_threshold = 0, mape_threshold = Inf, n =5, seed=5)
  power_state6 <- 
    pool_estimates %>% 
    bind_rows %>%  
    group_by(sample_size, ban_year) %>% 
    summarise(
      att_min = sort(att)[2],
      att_max = sort(att, decreasing = TRUE)[2],
      att_median = median(att)
    ) %>% mutate(
      specification = "State Pooled", year = 2015, treated_state="All"
    )
  
  rbind(
    power_state1%>%  bind_rows(),
    power_state2%>%  bind_rows(),
    power_state3%>%  bind_rows(),
    power_state4%>%  bind_rows(),
    power_state5%>%  bind_rows(),
    power_state6%>%  bind_rows()
  ) %>% 
    group_by(treated_state) %>% 
    filter(att_min == max(att_min))
  
  
}


do_many_times_v3_with_inter <- function (i, x, test_ind_end1, test_ind_end2,
                                          y_train, y_test, y_att, n_don, sample_size)
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
  
} # this is the function that creates the SC

#state-level data:
dt_state_initial <- pre_processing_dt_state(power2) #run the function from the placebo_all.RMD
treated_counties_id <- unique(dt_state_initial$county_id[dt_state_initial$state_id%in% all_treated])
donors_state <- unique(dt_state_initial$county_id[!(dt_state_initial$state_id%in% all_treated)])
donors <- donors_state

xy_plot_data_function <- function (treated_state, f, seed)#this function creates the SC and returns the disposal per capita of the actual and the synthetic state for states
{
  set.seed(seed)
  ####
  # we recenter the time series of the treated states based on when the ban went into effect, i.e., for VT it went into effect in July so we recenter the time series so it starts in July
  ####
  ban_year <- bans[which(all_treated == treated_state)]
  dt_state <- dt_state_initial
  
  dt <- dt_state %>% as.data.frame()
  year_end <- ban_year-offset                           
  treated_location <- treated_state                     #treated state
  don_new <- donors[donors!=treated_location]           #potential donors
  n_don <- length(don_new)                              #number of potential donors
  test_ind_end1 <- year_end - year_start+1              #end of training period 
  test_ind_end2 <- ban_year-year_end-1 +test_ind_end1   #end of validation period 
  
  y <- dt[dt$county_id==treated_location, c("tons_pc")] #actual disposal per capita of the treated state  
  y_train <- y[1:test_ind_end1]                         #disposal series during training period
  y_test <-  y[(test_ind_end1+1):test_ind_end2]         #disposal series during validation period
  y_att <- y[(test_ind_end2+1):length(y)]               #disposal series during the post-ban period
  
  x <- dt[!dt$county_id %in% treated_counties_id,]      #actual disposal per capita of the potential donors
  x <- x[x$county_id!=treated_location, c("tons_pc", "county_id")]
  x <- as.matrix(unstack(x, tons_pc ~ county_id)) 
  
  #initialize the results
  
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
  iterations=100000
  if(sample_size==3){iterations = 20000}
  all <- lapply(seq(1:iterations),do_many_times_v3_with_inter,x, test_ind_end1, test_ind_end2,y_train, y_test, y_att,n_don, sample_size)
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
    #filter(r_sq > 0) %>% 
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
  
  
  all %>% 
    #rename(state_id = county_id)  %>% 
    left_join(dt %>% select(state_id, year, tons_pc), by = c("county_id" = "state_id")) %>% 
    select(-iterations) %>% 
    left_join(
      dt %>% 
        select(state_id, year, tons_pc, county_id) %>% 
        rename (y=tons_pc, donor_state_id = state_id),
      by = c("chosen_donor"="county_id", "year")) %>% 
    group_by(year, county_id, intercept2, intercept, ban_year, attempt, tons_pc) %>% 
    summarise(y=mean(y)) %>%
    mutate(
      y_0  = y+intercept2
    ) %>% 
    ungroup %>% 
    left_join(
      disposal_effect_size2 %>%  select(year, state_id, effect_size), 
      by = c("year", "county_id" = 'state_id')
    ) %>% 
    mutate(
      treated_state = treated_state, 
      #y_0_effect = ifelse(year>=ban_year, y_0*(1-effect), ifelse(year == ban_year-1, y_0, NA))
      y_0_effect = ifelse(year>=ban_year, y_0*(1-effect_size), NA)
    )
}

xy_plot_data_uncentered_function <- function (treated_state, f, seed)#this function creates the SC and returns the disposal per capita of the actual and the synthetic state for states
{
  set.seed(seed)
  ####
  # we recenter the time series of the treated states based on when the ban went into effect, i.e., for VT it went into effect in July so we recenter the time series so it starts in July
  ####
  dt_state <- dt_state_initial
  
  dt <- dt_state %>% as.data.frame()
  ban_year <- 2015#bans[which(all_treated == treated_state)]
  year_end <- ban_year-offset                           
  treated_location <- treated_state                     #treated state
  don_new <- donors[donors!=treated_location]           #potential donors
  n_don <- length(don_new)                              #number of potential donors
  test_ind_end1 <- year_end - year_start+1              #end of training period 
  test_ind_end2 <- ban_year-year_end-1 +test_ind_end1   #end of validation period 
  
  y <- dt[dt$county_id==treated_location, c("tons_pc")] #actual disposal per capita of the treated state  
  y_train <- y[1:test_ind_end1]                         #disposal series during training period
  y_test <-  y[(test_ind_end1+1):test_ind_end2]         #disposal series during validation period
  y_att <- y[(test_ind_end2+1):length(y)]               #disposal series during the post-ban period
  
  x <- dt[!dt$county_id %in% treated_counties_id,]      #actual disposal per capita of the potential donors
  x <- x[x$county_id!=treated_location, c("tons_pc", "county_id")]
  x <- as.matrix(unstack(x, tons_pc ~ county_id)) 
  
  #initialize the results
  
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
  all <- lapply(seq(1:iterations),do_many_times_v3_with_inter,x, test_ind_end1, test_ind_end2,y_train, y_test, y_att,n_don, sample_size)
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
  
  # if(treated_location%in% c("VT", "CA", "MA", "CT")) # we do not include RI here because we are unable to find a SC that has r_sq>0
  # {
  #  all <-
  #    all %>%
  #    as_tibble %>%
  #    filter(r_sq > 0)
  # }
  
  all <- 
    all %>% 
    as_tibble %>% 
    #filter(r_sq > 0) %>% 
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
  
  
  all %>% 
    #rename(state_id = county_id)  %>% 
    left_join(dt %>% select(state_id, year, tons_pc), by = c("county_id" = "state_id")) %>% 
    select(-iterations) %>% 
    left_join(
      dt %>% 
        select(state_id, year, tons_pc, county_id) %>% 
        rename (y=tons_pc, donor_state_id = state_id),
      by = c("chosen_donor"="county_id", "year")) %>% 
    group_by(year, county_id, intercept2, intercept, ban_year, attempt, tons_pc) %>% 
    summarise(y=mean(y)) %>%
    mutate(
      y_0  = y+intercept2
    ) %>% 
    ungroup %>% 
    left_join(
      disposal_effect_size2 %>%  select(year, state_id, effect_size), 
      by = c("year", "county_id" = 'state_id')
    ) %>% 
    mutate(
      treated_state = treated_state, 
      #y_0_effect = ifelse(year>=ban_year, y_0*(1-effect), ifelse(year == ban_year-1, y_0, NA))
      y_0_effect = ifelse(year>=ban_year, y_0*(1-effect_size), NA)
    )
}

xy_plot_data_function_donors <- function (treated_state, f, seed) #what are the states that comprise the synthetic control
{
  set.seed(seed)
  ban_year <- bans[which(all_treated == treated_state)]
  dt_state <- dt_state_initial
  
  dt <- dt_state %>% as.data.frame()
  year_end <- ban_year-offset
  treated_location <- treated_state
  don_new <- donors[donors!=treated_location]
  n_don <- length(don_new)
  test_ind_end1 <- year_end - year_start+1
  test_ind_end2 <- ban_year-year_end-1 +test_ind_end1
  
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
  all <- lapply(seq(1:iterations),do_many_times_v3_with_inter,x, test_ind_end1, test_ind_end2,y_train, y_test, y_att,n_don, sample_size)
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
    #filter(r_sq > 0) %>% 
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
  
  
  all %>% 
    #rename(state_id = county_id)  %>% 
    left_join(dt %>% select(state_id, year, tons_pc), by = c("county_id" = "state_id")) %>% 
    select(-iterations) %>% 
    left_join(
      dt %>% 
        select(state_id, year, tons_pc, county_id) %>% 
        rename (y=tons_pc, donor_state_id = state_id),
      by = c("chosen_donor"="county_id", "year")) %>% 
    group_by(year, county_id, intercept2, intercept, ban_year, attempt, tons_pc) %>% 
    filter(attempt==1, year==2015) %>% select(donor_state_id)
}

#City-level data:
all_treated <- c("VT", "MA", "CA", "CT", "RI", "M1", "M2", "M3")# Never changes
dt_initial <- dt_municipal_initial #for this to run go on placebo_all.RMD file and run the "Importing data" and "Transforming data" subsections from "Municipal" section
donors_cities <- unique(dt_initial$county_id[!(dt_initial$state_id%in% all_treated)])
treated_counties_id_cities <- unique(dt_initial$county_id[dt_initial$state_id%in% all_treated]) #make sure that it includes seattle and boulder
samp= c(3:10)
bans <- c(2014, 2014, 2016, 2014, 2016, 2015, 2016, 2009)
year_start = 2006
year_cutoff = 2018
municipal_effect <- read.csv("municipal_effect.csv")

xy_plot_data_function_cities <- function (k, f, seed) #this function creates the SC and returns the disposal per capita of the actual and the synthetic state for cities
{
  set.seed(seed)
  dt <- dt_initial %>% as.data.frame()
  treated_state <- str_sub(treated_counties_id_cities[k],start=-2)
  ban_year <- bans[which(all_treated == treated_state)]
  year_end <- ban_year-offset
  treated_location <- treated_counties_id_cities[k]
  
  don_new <- donors_cities[donors_cities!=treated_location]
  n_don <- length(don_new)
  test_ind_end1 <- year_end - year_start+1
  test_ind_end2 <- ban_year-year_end-1 +test_ind_end1
  
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
  all <- lapply(seq(1:50000),do_many_times_v3_with_inter,x, test_ind_end1, test_ind_end2,y_train, y_test, y_att,n_don, sample_size)
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
  
  if(treated_location%in% c("VT", "CA", "MA"))
  {
    all <-
      all %>%
      as_tibble %>%
      filter(r_sq > 0)
  }
  
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
  
  effect_size = municipal_effect %>%  filter(state_id == treated_state, type == "disposal") %>% mutate(effect=-effect) %>%  pluck("effect")
  
  all %>%     
    mutate(
      chosen_donor = str_replace_all(chosen_donor, "\\.", "")
    ) %>%  
    left_join(
      dt %>%        
        mutate(
          county_id = str_replace_all(county_id, "\\.", ""), 
          county_id = str_replace_all(county_id, "-", ""), 
          county_id = str_replace_all(county_id, " ", "")
        ) %>%  
        select(county_id, year, tons_pc), by = c("county_id")) %>% 
    select(-iterations) %>% 
    left_join(
      dt %>% 
        select(state_id, year, tons_pc, county_id) %>% 
        mutate(
          county_id = str_replace_all(county_id, "\\.", ""), 
          county_id = str_replace_all(county_id, "-", ""), 
          county_id = str_replace_all(county_id, " ", "")
        ) %>% 
        rename (y=tons_pc),
      by = c("chosen_donor"="county_id", "year")) %>% 
    group_by(year, county_id, intercept2, intercept, ban_year, attempt, tons_pc) %>% 
    summarise(y=mean(y)) %>%
    mutate(
      y_0  = y+intercept2
    ) %>% #filter(attempt %in% c(1,7,19)) %>% 
    ungroup %>%  
    mutate(
      treated_state = treated_state, 
      effect_size = effect_size,
      #y_0_effect = ifelse(year>=ban_year, y_0*(1-effect), ifelse(year == ban_year-1, y_0, NA))
      y_0_effect = ifelse(year>=ban_year, y_0*(1-effect_size), NA)
    )
}

xy_plot_data_function_sf <- function (f, seed) #this function creates the SC and returns the disposal per capita of the actual and the synthetic state for SF
{
  set.seed(seed)
  year_start <- 1996
  year_cutoff <- 2014
  year_end <- 2011
  
  dt <- 
    power2 %>% pre_processing_dt_sf %>% 
    filter(state_id=="CA") %>% 
    mutate(
      county_id = ifelse(is.na(county_name), state_id, paste0(county_name, state_id)),
      county_id = ifelse(county_id=="san franciscoCA", "san franciscoM3", county_id),
      state_id = ifelse(county_id == "san franciscoM3", "M3", "other")
    )
  
  # the implementation date of SF's ban is on Oct 2009
  dt <- 
    dt %>%
    group_by(county_id) %>% 
    mutate(
      tons_pc = tons_pc*0.75 + .25*ifelse(is.na(lead(tons_pc, n=1, default = NA)), tons_pc, lead(tons_pc, n=1, default = NA)) 
    )
  
  # Treatment 
  dt <- dt %>%  as.data.frame()
  treated_counties_id <- unique(dt$county_id[dt$state_id%in% all_treated])
  donors <- unique(dt$county_id[!(dt$state_id%in% all_treated)])
  treated_state = "M3"
  ban_year <- bans[which(all_treated == treated_state)]

  year_end <- ban_year-offset
  treated_location <- "san franciscoM3"
  
  don_new <- donors[donors!=treated_location]
  n_don <- length(don_new)
  test_ind_end1 <- year_end - year_start+1
  test_ind_end2 <- ban_year-year_end-1 +test_ind_end1
  
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
  all <- lapply(seq(1:iterations),do_many_times_v3_with_inter,x, test_ind_end1, test_ind_end2,y_train, y_test, y_att,n_don, sample_size)
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
  
  if(treated_location%in% c("VT", "CA", "MA"))
  {
    all <-
      all %>%
      as_tibble %>%
      filter(r_sq > 0)
  }
  
  all <- 
    all %>% 
    as_tibble %>% 
    #filter(r_sq > 0) %>% 
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
  
  effect_size = municipal_effect %>%  filter(state_id == treated_state, type == "disposal") %>% mutate(effect=-effect) %>%  pluck("effect")
  
  all %>%     
    mutate(
      chosen_donor = str_replace_all(chosen_donor, "\\.", ""), 
      county_id = str_replace_all(county_id, " ", "")
    ) %>%
    left_join(
      dt %>%        
        mutate(
          county_id = str_replace_all(county_id, "\\.", ""), 
          county_id = str_replace_all(county_id, "-", ""), 
          county_id = str_replace_all(county_id, " ", "")
        ) %>%  
        select(county_id, year, tons_pc), by = c("county_id")) %>% 
    select(-iterations) %>% 
    left_join(
      dt %>% 
        select(state_id, year, tons_pc, county_id) %>% 
        mutate(
          county_id = str_replace_all(county_id, "\\.", ""), 
          county_id = str_replace_all(county_id, "-", ""), 
          county_id = str_replace_all(county_id, " ", "")
        ) %>% 
        rename (y=tons_pc),
      by = c("chosen_donor"="county_id", "year")) %>% 
    group_by(year, county_id, intercept2, intercept, ban_year, attempt, tons_pc) %>% 
    summarise(y=mean(y)) %>%
    mutate(
      y_0  = y+intercept2
    ) %>% #filter(attempt %in% c(1,7,19)) %>% 
    ungroup %>%  
    mutate(
      treated_state = treated_state, 
      effect_size = effect_size,
      #y_0_effect = ifelse(year>=ban_year, y_0*(1-effect), ifelse(year == ban_year-1, y_0, NA))
      y_0_effect = ifelse(year>=ban_year, y_0*(1-effect_size), NA)
    )
  
  
}

xy_plot_fun <- function (i) #this fn takes as input the xy_plot_data output and creates the plot (Fig.2 left panel) for the five states, the aggregate case and the two cities (seattle and boulder)
{
  bans = c()
  reg_expect <- 
    tibble(
      state_id=c("MA", "CA", "VT"), 
      reg_effect = reg_effect
    )
  
  all <-  ## aggregate case, use the uncentered
    #xy_plot_data %>% 
    xy_plot_data_uncentered %>% 
    filter(attempt ==i, treated_state %in% c("CA", "CT","MA", "VT", "RI")) %>% 
    group_by(year, attempt) %>% 
    summarise(
      tons_pc = mean(tons_pc), 
      y=mean(y), 
      y_0 = mean(y_0), 
    ) %>%
    left_join(
      disposal_effect_size2 %>%
        select(year, state_id, effect_size) %>% 
        group_by(year) %>% 
        summarise(effect_size=mean(effect_size, na.rm=TRUE)), 
      by = "year"
    ) %>% 
    mutate(
      y_0_effect = y_0*(1-effect_size),
      ban_year = pooled_ban_year, 
      treated_state= "All", 
      y_0_effect = ifelse(year<ban_year, NA, y_0_effect)
    ) %>% 
    select(
      year, treated_state, ban_year, attempt, tons_pc, y, y_0, y_0_effect
    )
  
  all_plot_data <- 
    xy_plot_data %>% 
    filter(attempt==i, treated_state!="San Francisco, CA") %>% 
    rename(
      Synthetic = y_0, 
      Actual = tons_pc, 
      `Our Exp.` = y_0_effect
    ) %>% 
    left_join(reg_expect, by = c("county_id"="state_id")) %>% 
    mutate(
      `Regulators' Exp.` = ifelse(!is.na(effect_size), Synthetic*(1-reg_effect), NA)
    ) %>% 
    pivot_longer(
      cols = c("Synthetic", "Actual", "Our Exp.", `Regulators' Exp.`), 
      names_to = "location", 
      values_to = "tons_pc") %>% select(year, attempt, location, tons_pc, treated_state, ban_year) %>% 
    rbind(
      all%>% 
        rename(
          Synthetic = y_0, 
          Actual = tons_pc, 
          `Our Exp.` = y_0_effect
        ) %>% 
        pivot_longer(
          cols = c("Synthetic", "Actual", "Our Exp."), 
          names_to = "location", 
          values_to = "tons_pc") %>% select(year, attempt, location, tons_pc, treated_state, ban_year)
    ) %>% 
    left_join(
      rbind(
        xy_plot_data%>% 
          filter(attempt==i) %>%  
          select(year, treated_state, ban_year, attempt, tons_pc, y, y_0, y_0_effect), 
        all) %>% 
        group_by(treated_state) %>% 
        filter(year > ban_year-3, year < ban_year) %>%
        mutate(
          year=ceiling(round(mean(year), 0)), # i duplicate so i join and then also keep the xlab_mae
          xlab_mae=mean(year)+0.5,  
          xlab_mae=ifelse(treated_state%in%c("Boulder, CO"), xlab_mae+0.3, xlab_mae),
          ylab_mae=ifelse(treated_state%in%c("All", "CA", "CT"), mean(tons_pc)-0.05, mean(tons_pc)-0.06), 
          ylab_mae=ifelse(treated_state%in%c("Seattle, WA"), ylab_mae-0.01, ylab_mae), 
          ylab_mae=ifelse(treated_state%in%c("CA"), ylab_mae+0.01, ylab_mae), 
          ylab_mae=ifelse(treated_state%in%c("RI"), ylab_mae-0.01, ylab_mae), 
          xlab_mae=ifelse(treated_state%in%c("Seattle, WA"), xlab_mae-2.5, xlab_mae), 
          xlab_mae=ifelse(treated_state%in%c("All"), xlab_mae-3.5, xlab_mae), 
          ) %>% 
        group_by(treated_state, xlab_mae, ylab_mae, year) %>% 
        summarise(
          MAE = abs((tons_pc-y_0)/tons_pc) %>%{.*100} %>%  mean %>% round(2)) %>% 
        mutate(
          MAE = scales::number(MAE, accuracy = 0.01)
        ), 
      by = c("treated_state", "year")
    ) %>% 
    mutate(
      treated_state = factor(treated_state, levels = c("All", "CA", "CT", "MA", "RI", "VT", "Seattle, WA", "Boulder, CO", "San Francisco, CA"))
    ) %>% 
    left_join(
      tibble (xlab= c(2010, pooled_ban_year-2, pooled_ban_year+2), ylab = 0.85, label = c("Training", "Validation", "Evaluation"), treated_state = "All"), 
      by =c ("treated_state", "year"="xlab")
    ) %>% 
    mutate(
      xlab = ifelse(!is.na(ylab), year, NA),
      xlab = ifelse(!is.na(ylab) & year==pooled_ban_year-2, xlab+0.5, xlab), 
      MAE = ifelse(treated_state%in%c("All", "Seattle, WA"), paste0("Mean absolute percentage error (%): ", MAE), MAE), 
      y_first = ifelse(year==2006 & location =="Actual", tons_pc %>% round(2), NA), 
      y_last =ifelse(year==2018 & location =="Actual", tons_pc%>% round(2)*1.0, NA)
    ) 
  
  
  all_plot_data <- 
    all_plot_data %>% 
    left_join(
      #for end points to get rid of geom_vline
      all_plot_data %>% 
        group_by(treated_state, ban_year) %>% 
        mutate(y_min = min(tons_pc, na.rm=TRUE), ymax=max(tons_pc, na.rm=TRUE)) %>% 
        filter(year==ban_year, location %in% c("Actual", "Synthetic")) %>% 
        mutate(
          y_end_low = min(tons_pc, na.rm=TRUE),
          y_end_high = max(tons_pc, na.rm=TRUE)
        ) %>% select(treated_state, y_end_low, y_min, y_end_high, ymax) %>% 
        mutate(
          y_end_low = ifelse(y_min < y_end_low-0.02, y_end_low-0.02, y_min), 
          y_end_high = ifelse(ymax <y_end_high+0.03, y_end_low+0.13, y_end_high+0.05)
        ) %>% 
        group_by(treated_state, ban_year) %>% 
        summarise(
          y_end_low = unique(y_end_low), 
          y_end_high = unique(y_end_high)
        ) %>% 
        mutate(
          location= "Actual", 
          y_end_low = ifelse(location=="Boulder, CO", y_end_low-0.4, y_end_low), 
          y_end_high = ifelse(location=="Boulder, CO", y_end_high+0.35, y_end_high)), 
      by = c("year"="ban_year", "treated_state", "location")
    ) %>% 
    left_join(
      #for end points to get rid of geom_vline
      all_plot_data %>% 
        group_by(treated_state, ban_year) %>% 
        mutate(y_min = min(tons_pc, na.rm=TRUE), ymax=max(tons_pc, na.rm=TRUE)) %>% 
        filter(year==ban_year-3, location %in% c("Actual", "Synthetic")) %>% 
        mutate(
          y_end_low = min(tons_pc, na.rm=TRUE),
          y_end_high = max(tons_pc, na.rm=TRUE)
        ) %>% select(treated_state, y_end_low, y_min, y_end_high, ymax) %>% 
        mutate(
          y_end_low = ifelse(y_min < y_end_low-0.02, y_end_low-0.02, y_min), 
          y_end_high = ifelse(ymax <y_end_high+0.03, y_end_low+0.13, y_end_high+0.05)
        ) %>% 
        group_by(treated_state, ban_year) %>% 
        summarise(
          year=ban_year-3,
          y_end_low_2 = unique(y_end_low), 
          y_end_high_2 = unique(y_end_high)
        ) %>% 
        mutate(
          location= "Actual", 
          y_end_low_2 = ifelse(location=="Boulder, CO", y_end_low_2-0.35, y_end_low_2),
          y_end_high_2 = ifelse(location=="Boulder, CO", y_end_high_2+0.35, y_end_high_2)
        )%>% 
        ungroup %>% 
        select(-ban_year), 
      by = c("year", "treated_state", "location")
    )
  
  p <- 
    all_plot_data %>% 
    ggplot(
      aes(x=year, y=tons_pc, color =location, linetype= location, size=location)
    )+
    geom_segment(
      aes(x=ban_year, xend=ban_year, y=y_end_low, yend  = y_end_high),
      linetype = "dotted", linewidth = 0.2, color = ut_colors[4])+
    geom_segment(
      aes(x=ban_year-3, xend=ban_year-3, y=y_end_low_2, yend  = y_end_high_2),
      linetype = "dotted", linewidth = 0.2, color = ut_colors[4])+
    
    geom_line()+
    facet_grid(
      rows=vars(factor(treated_state, levels = c("All", "CA", "CT", "MA", "RI", "VT", "Seattle, WA", "Boulder, CO", "San Francisco, CA"))),
      scales="free_y"
    )+
    
    geom_text(aes(x=xlab, y=ylab-0.01, label = label), color=ut_colors[5], size=3, family="Helvetica")+
    ####geom_text(aes(x=2005.5, y = y_first, label=y_first ), color=rgb(90,90,90, maxColorValue = 255), size=3, family="Helvetica")+
    ####geom_text(aes(x=2018.5, y = y_last %>% as.numeric, label=scales::number(y_last, accuracy = 0.01) ), color=rgb(90,90,90, maxColorValue = 255), size=3, family="Helvetica")+
    
    #geom_text(aes(x=xlab_mae,y=ylab_mae, label = MAE), color ="#857d95", size=3, family="Helvetica")+
    geom_text(aes(x=xlab_mae,y=ylab_mae, label = MAE), color =ut_colors[5], size=3, family="Helvetica")+
    
    scale_color_manual(breaks= c("Actual", "Synthetic", "Our Exp.", "Regulators' Exp."), values = c(ut_colors[4],ut_colors[5],"seagreen", "#bad9c6"), name = "")+
    scale_linetype_manual(breaks= c("Actual", "Synthetic", "Our Exp.", "Regulators' Exp."), values = c("solid", "solid", "solid", "solid"), name = "")+
    scale_size_manual(breaks= c("Actual", "Synthetic", "Our Exp.", "Regulators' Exp."), values = c(0.5, 0.5, 1.0, 1.0), name = "")+
    scale_x_continuous(breaks=c(seq(2006, 2018, 2)), limits=c(2005, 2019), expand = c(0,0))+
    ####scale_y_continuous(expand = c(0.05,0.05))+
    scale_y_continuous(expand = c(0.05,0.05), breaks = scales::breaks_pretty(3))+
    
    labs(y="", x= "")+
    theme_classic()+
    theme(
      legend.position = "top",
      strip.background = element_rect(color = "white", fill = "white"),
      panel.grid.major.x = element_blank() ,
      panel.grid.major.y = element_blank(), 
      text = element_text(family = "Helvetica",size = 10, color= ut_colors[4]), 
      axis.line.x = element_blank(),
      axis.line.y = element_blank(), 
      #axis.ticks.y = element_blank(), 
      #axis.text.y= element_blank(),
      axis.ticks.x = element_line(size = 0.1), 
      legend.text = element_text(family = "Helvetica", color = ut_colors[4],size = 10),
      panel.spacing = unit(1, "cm")
    )
  
  
  return(p)
  
}

xy_plot_fun_sf <- function (i) #same as above but for SF
{
  reg_expect <- 
    tibble(
      state_id=c("MA", "CA", "VT"), 
      reg_effect = reg_effect
    )
  
  all <-  
    xy_plot_data %>% filter(attempt ==i, treated_state %in% c("CA", "CT","MA", "VT", "RI")) %>% 
    group_by(year, attempt) %>% 
    summarise(
      tons_pc = mean(tons_pc), 
      y=mean(y), 
      y_0 = mean(y_0), 
    ) %>%
    left_join(
      disposal_effect_size2 %>%
        select(year, state_id, effect_size) %>% 
        group_by(year) %>% 
        summarise(effect_size=mean(effect_size, na.rm=TRUE)), 
      by = "year"
    ) %>% 
    mutate(
      y_0_effect = y_0*(1-effect_size),
      ban_year = 2014, 
      treated_state= "All"
    ) %>% 
    select(
      year, treated_state, ban_year, attempt, tons_pc, y, y_0, y_0_effect
    )
  
  all_plot_data <- 
    xy_plot_data %>% 
    filter(attempt==i) %>% 
    rename(
      Synthetic = y_0, 
      Actual = tons_pc, 
      `Our Exp.` = y_0_effect
    ) %>%
    left_join(reg_expect, by = c("county_id"="state_id")) %>% 
    mutate(
      `Regulators' Exp.` = ifelse(!is.na(effect_size), Synthetic*(1-reg_effect), NA)
    ) %>% 
    pivot_longer(
      cols = c("Synthetic", "Actual", "Our Exp.", `Regulators' Exp.`), 
      names_to = "location", 
      values_to = "tons_pc") %>% select(year, attempt, location, tons_pc, treated_state, ban_year) %>% 
    rbind(
      all%>% 
        rename(
          Synthetic = y_0, 
          Actual = tons_pc, 
          `Our Exp.` = y_0_effect
        ) %>% 
        pivot_longer(
          cols = c("Synthetic", "Actual", "Our Exp."), 
          names_to = "location", 
          values_to = "tons_pc") %>% select(year, attempt, location, tons_pc, treated_state, ban_year)
    ) %>%
    left_join(
      rbind(xy_plot_data%>% select(year, treated_state, ban_year, attempt, tons_pc, y, y_0, y_0_effect), all) %>% 
        group_by(treated_state) %>% 
        filter(year >= ban_year-3, year < ban_year, attempt==i) %>%
        mutate(
          year=mean(year), # i duplicate so i join and then also keep the xlab_mae
          xlab_mae=mean(year)+0.5,  
          xlab_mae=ifelse(treated_state%in%c("Boulder, CO"), xlab_mae+0.3, xlab_mae),
          ylab_mae=ifelse(treated_state%in%c("All", "CA", "CT"), mean(tons_pc)-0.05, mean(tons_pc)-0.06), 
          ylab_mae=ifelse(treated_state%in%c("Seattle, WA"), ylab_mae-0.01, ylab_mae), 
          #ylab_mae=ifelse(treated_state%in%c("CT"), ylab_mae+0.01, ylab_mae), 
          ylab_mae=ifelse(treated_state%in%c("CA"), ylab_mae+0.01, ylab_mae), 
          ylab_mae=ifelse(treated_state%in%c("San Francisco, CA"), ylab_mae-0.07, ylab_mae), 
          
          xlab_mae=ifelse(treated_state%in%c("All"), xlab_mae, xlab_mae), 
          xlab_mae=ifelse(treated_state%in%c("CA"), xlab_mae+0.2, xlab_mae)) %>% 
        group_by(treated_state, xlab_mae, ylab_mae, year) %>% 
        summarise(MAE = abs((tons_pc-y_0)/tons_pc) %>% {.*100} %>%  mean %>% round(2)) %>% 
        mutate(MAE= scales::number(MAE, accuracy = 0.01)), 
      by = c("treated_state", "year")
    ) %>% 
    mutate(
      treated_state = factor(treated_state, levels = c("All", "CA", "CT", "MA", "RI", "VT", "Seattle, WA", "Boulder, CO", "San Francisco, CA"))
    ) %>% 
    left_join(
      tibble (xlab= c(2009, 2012, 2016), ylab = 0.85, label = c("Training", "Validation", "Evaluation"), treated_state = "All"), 
      by =c ("treated_state", "year"="xlab")
    ) %>% 
    mutate(
      xlab = ifelse(!is.na(ylab), year, NA),
      xlab = ifelse(!is.na(ylab) & year==2012, 2012.5, xlab), 
      MAE = ifelse(treated_state=="All", paste0("MAPE (%): ", MAE), MAE), 
      y_first = ifelse(year==2006 & location =="Actual", tons_pc %>% round(2), NA), 
      y_last =ifelse(year==2018 & location =="Actual", tons_pc%>% round(2)*1.0, NA), 
      y_first = ifelse(treated_state== "San Francisco, CA"& year==1996 & location =="Actual", tons_pc %>% round(2), NA), 
      y_last = ifelse(treated_state== "San Francisco, CA"& year==2014 & location =="Actual", tons_pc %>% round(2), NA)
    ) 
  
  
  all_plot_data <- 
    all_plot_data %>% 
    left_join(
      #for end points to get rid of geom_vline
      all_plot_data %>% 
        group_by(treated_state, ban_year) %>% 
        mutate(y_min = min(tons_pc, na.rm=TRUE), ymax=max(tons_pc, na.rm=TRUE)) %>% 
        filter(year==ban_year, location %in% c("Actual", "Synthetic")) %>% 
        mutate(
          y_end_low = min(tons_pc, na.rm=TRUE),
          y_end_high = max(tons_pc, na.rm=TRUE)
        ) %>% select(treated_state, y_end_low, y_min, y_end_high, ymax) %>% 
        mutate(
          y_end_low = ifelse(y_min < y_end_low-0.02, y_end_low-0.02, y_min), 
          y_end_high = ifelse(ymax <y_end_high+0.03, y_end_low+0.13, y_end_high+0.05)
        ) %>% 
        group_by(treated_state, ban_year) %>% 
        summarise(
          y_end_low = unique(y_end_low), 
          y_end_high = unique(y_end_high)
        ) %>% 
        mutate(
          location= "Actual", 
          y_end_low = ifelse(location=="Boulder, CO", y_end_low-0.4, y_end_low), 
          y_end_high = ifelse(location=="Boulder, CO", y_end_high+0.35, y_end_high)), 
      by = c("year"="ban_year", "treated_state", "location")
    ) %>% 
    left_join(
      #for end points to get rid of geom_vline
      all_plot_data %>% 
        group_by(treated_state, ban_year) %>% 
        mutate(y_min = min(tons_pc, na.rm=TRUE), ymax=max(tons_pc, na.rm=TRUE)) %>% 
        filter(year==ban_year-3, location %in% c("Actual", "Synthetic")) %>% 
        mutate(
          y_end_low = min(tons_pc, na.rm=TRUE),
          y_end_high = max(tons_pc, na.rm=TRUE)
        ) %>% select(treated_state, y_end_low, y_min, y_end_high, ymax) %>% 
        mutate(
          y_end_low = ifelse(y_min < y_end_low-0.02, y_end_low-0.02, y_min), 
          y_end_high = ifelse(ymax <y_end_high+0.03, y_end_low+0.13, y_end_high+0.05)
        ) %>% 
        group_by(treated_state, ban_year) %>% 
        summarise(
          year=ban_year-3,
          y_end_low_2 = unique(y_end_low), 
          y_end_high_2 = unique(y_end_high)
        ) %>% 
        mutate(
          location= "Actual", 
          y_end_low_2 = ifelse(location=="Boulder, CO", y_end_low_2-0.35, y_end_low_2),
          y_end_high_2 = ifelse(location=="Boulder, CO", y_end_high_2+0.35, y_end_high_2)
        )%>% 
        ungroup %>% 
        select(-ban_year), 
      by = c("year", "treated_state", "location")
    )
  
  p <- 
    all_plot_data %>% filter(treated_state == "San Francisco, CA") %>% 
    ggplot(
      aes(x=year, y=tons_pc, color =location, linetype= location, size=location)
    )+
    geom_segment(
      aes(x=ban_year, xend=ban_year, y=y_end_low, yend  = y_end_high),
      linetype = "dotted", linewidth = 0.2, color = ut_colors[4])+
    geom_segment(
      aes(x=ban_year-3, xend=ban_year-3, y=y_end_low_2, yend  = y_end_high_2),
      linetype = "dotted", linewidth = 0.2, color = ut_colors[4])+
    
    geom_line()+
    geom_text(aes(x=xlab, y=ylab-0.01, label = label), color=ut_colors[5], size=3, family="Helvetica")+
    geom_text(aes(x=1995.2, y = y_first, label=y_first ), color=rgb(90,90,90, maxColorValue = 255), size=3, family="Helvetica")+
    geom_text(aes(x=2014.8, y = y_last %>% as.numeric, label=scales::number(y_last, accuracy = 0.01) ), color=rgb(90,90,90, maxColorValue = 255), size=3, family="Helvetica")+
    
    geom_text(aes(x=xlab_mae,y=ylab_mae, label = MAE), color =ut_colors[5], size=3, family="Helvetica")+
    scale_color_manual(breaks= c("Actual", "Synthetic", "Our Exp.", "Regulators' Exp."), values = c(ut_colors[4],ut_colors[5],"seagreen", "#bad9c6"), name = "")+
    scale_linetype_manual(breaks= c("Actual", "Synthetic", "Our Exp.", "Regulators' Exp."), values = c("solid", "solid", "solid", "solid"), name = "")+
    scale_size_manual(breaks= c("Actual", "Synthetic", "Our Exp.", "Regulators' Exp."), values = c(0.5, 0.5, 1.0, 1.0), name = "")+
    scale_x_continuous(breaks=c(seq(1996, 2014, 2)), limits=c(1994.7, 2015.3), expand = c(0,0))+
    labs(y="", x= "")+
    theme_classic()+
    theme(
      legend.position = "none",
      strip.background = element_rect(color = "white", fill = "white"),
      panel.grid.major.x = element_blank() ,
      panel.grid.major.y = element_blank(), 
      text = element_text(family = "Helvetica",size = 10, color= ut_colors[4]), 
      axis.line.x = element_blank(),
      axis.line.y = element_blank(), 
      axis.ticks.y = element_blank(), 
      axis.text.y= element_blank(),
      axis.ticks.x = element_line(size = 0.1), 
      legend.text = element_text(family = "Helvetica", color = ut_colors[4],size = 10),
      panel.spacing = unit(1.1, "cm")
    )
}

xy_plot_fun_cities <- function (i) #same as above but for the Seattle and Boulder. The cities are also included in 
{
  ###
  # These cities are also included in the xy_plot_fun, but the following will allow to have the fig. i prefer (i.e., no axis on the left)
  ###
  
  reg_expect <- 
    tibble(
      state_id=c("MA", "CA", "VT"), 
      reg_effect = reg_effect
    )
  
  all <-  
    xy_plot_data %>% filter(attempt ==i, treated_state %in% c("CA", "CT","MA", "VT", "RI")) %>% 
    group_by(year, attempt) %>% 
    summarise(
      tons_pc = mean(tons_pc), 
      y=mean(y), 
      y_0 = mean(y_0), 
    ) %>%
    left_join(
      disposal_effect_size2 %>%
        select(year, state_id, effect_size) %>% 
        group_by(year) %>% 
        summarise(effect_size=mean(effect_size, na.rm=TRUE)), 
      by = "year"
    ) %>% 
    mutate(
      y_0_effect = y_0*(1-effect_size),
      ban_year = 2014, 
      treated_state= "All"
    ) %>% 
    select(
      year, treated_state, ban_year, attempt, tons_pc, y, y_0, y_0_effect
    )
  
  all_plot_data <- 
    xy_plot_data %>% 
    filter(attempt==i) %>% 
    rename(
      Synthetic = y_0, 
      Actual = tons_pc, 
      `Our Exp.` = y_0_effect
    ) %>%
    left_join(reg_expect, by = c("county_id"="state_id")) %>% 
    mutate(
      `Regulators' Exp.` = ifelse(!is.na(effect_size), Synthetic*(1-reg_effect), NA)
    ) %>% 
    pivot_longer(
      cols = c("Synthetic", "Actual", "Our Exp.", `Regulators' Exp.`), 
      names_to = "location", 
      values_to = "tons_pc") %>% select(year, attempt, location, tons_pc, treated_state, ban_year) %>% 
    rbind(
      all%>% 
        rename(
          Synthetic = y_0, 
          Actual = tons_pc, 
          `Our Exp.` = y_0_effect
        ) %>% 
        pivot_longer(
          cols = c("Synthetic", "Actual", "Our Exp."), 
          names_to = "location", 
          values_to = "tons_pc") %>% select(year, attempt, location, tons_pc, treated_state, ban_year)
    ) %>%
    left_join(
      rbind(
        xy_plot_data%>% select(year, treated_state, ban_year, attempt, tons_pc, y, y_0, y_0_effect), 
        all
        ) %>% 
        group_by(treated_state) %>% 
        filter(year > ban_year-3, year < ban_year, attempt==i) %>%
        mutate(
          year=ceiling(round(mean(year), 0)), # i duplicate so i join and then also keep the xlab_mae
          xlab_mae=mean(year)+0.5,  
          xlab_mae=ifelse(treated_state%in%c("Boulder, CO"), xlab_mae+0.3, xlab_mae),
          ylab_mae=ifelse(treated_state%in%c("All", "CA", "CT"), mean(tons_pc)-0.05, mean(tons_pc)-0.06), 
          ylab_mae=ifelse(treated_state%in%c("Seattle, WA"), ylab_mae-0.01, ylab_mae), 
          #ylab_mae=ifelse(treated_state%in%c("CT"), ylab_mae+0.01, ylab_mae), 
          ylab_mae=ifelse(treated_state%in%c("CA"), ylab_mae+0.01, ylab_mae), 
          ylab_mae=ifelse(treated_state%in%c("San Francisco, CA"), ylab_mae-0.07, ylab_mae), 
          
          xlab_mae=ifelse(treated_state%in%c("All", "Seattle, WA"), xlab_mae-2.5, xlab_mae), 
          xlab_mae=ifelse(treated_state%in%c("CA"), xlab_mae+0.2, xlab_mae)) %>% 
        group_by(treated_state, xlab_mae, ylab_mae, year) %>% 
        summarise(MAE = abs((tons_pc-y_0)/tons_pc) %>% {.*100} %>%  mean %>% round(2)) %>% 
        mutate(MAE= scales::number(MAE, accuracy = 0.01)), 
      by = c("treated_state", "year")
    ) %>% 
    mutate(
      treated_state = factor(treated_state, levels = c("All", "CA", "CT", "MA", "RI", "VT", "Seattle, WA", "Boulder, CO", "San Francisco, CA"))
    ) %>% 
    left_join(
      tibble (xlab= c(2009, 2012, 2016), ylab = 0.85, label = c("Training", "Validation", "Evaluation"), treated_state = "All"), 
      by =c ("treated_state", "year"="xlab")
    ) %>% 
    mutate(
      xlab = ifelse(!is.na(ylab), year, NA),
      xlab = ifelse(!is.na(ylab) & year==2012, 2012.5, xlab), 
      MAE = ifelse(treated_state%in%c("All", "Seattle, WA"), paste0("Mean absolute percentage error (%): ", MAE), MAE), 
      y_first = ifelse(year==2006 & location =="Actual", tons_pc %>% round(2), NA), 
      y_last =ifelse(year==2018 & location =="Actual", tons_pc%>% round(2)*1.0, NA), 
      y_first = ifelse(treated_state== "San Francisco, CA"& year==1996 & location =="Actual", tons_pc %>% round(2), y_first), 
      y_last = ifelse(treated_state== "San Francisco, CA"& year==2014 & location =="Actual", tons_pc %>% round(2), y_last)
    ) 
  
  
  all_plot_data <- 
    all_plot_data %>% 
    left_join(
      #for end points to get rid of geom_vline
      all_plot_data %>% 
        group_by(treated_state, ban_year) %>% 
        mutate(y_min = min(tons_pc, na.rm=TRUE), ymax=max(tons_pc, na.rm=TRUE)) %>% 
        filter(year==ban_year, location %in% c("Actual", "Synthetic")) %>% 
        mutate(
          y_end_low = min(tons_pc, na.rm=TRUE),
          y_end_high = max(tons_pc, na.rm=TRUE)
        ) %>% select(treated_state, y_end_low, y_min, y_end_high, ymax) %>% 
        mutate(
          y_end_low = ifelse(y_min < y_end_low-0.02, y_end_low-0.02, y_min), 
          y_end_high = ifelse(ymax <y_end_high+0.03, y_end_low+0.13, y_end_high+0.05)
        ) %>% 
        group_by(treated_state, ban_year) %>% 
        summarise(
          y_end_low = unique(y_end_low), 
          y_end_high = unique(y_end_high)
        ) %>% 
        mutate(
          location= "Actual", 
          y_end_low = ifelse(location=="Boulder, CO", y_end_low-0.4, y_end_low), 
          y_end_high = ifelse(location=="Boulder, CO", y_end_high+0.35, y_end_high)), 
      by = c("year"="ban_year", "treated_state", "location")
    ) %>% 
    left_join(
      #for end points to get rid of geom_vline
      all_plot_data %>% 
        group_by(treated_state, ban_year) %>% 
        mutate(y_min = min(tons_pc, na.rm=TRUE), ymax=max(tons_pc, na.rm=TRUE)) %>% 
        filter(year==ban_year-3, location %in% c("Actual", "Synthetic")) %>% 
        mutate(
          y_end_low = min(tons_pc, na.rm=TRUE),
          y_end_high = max(tons_pc, na.rm=TRUE)
        ) %>% select(treated_state, y_end_low, y_min, y_end_high, ymax) %>% 
        mutate(
          y_end_low = ifelse(y_min < y_end_low-0.02, y_end_low-0.02, y_min), 
          y_end_high = ifelse(ymax <y_end_high+0.03, y_end_low+0.13, y_end_high+0.05)
        ) %>% 
        group_by(treated_state, ban_year) %>% 
        summarise(
          year=ban_year-3,
          y_end_low_2 = unique(y_end_low), 
          y_end_high_2 = unique(y_end_high)
        ) %>% 
        mutate(
          location= "Actual", 
          y_end_low_2 = ifelse(location=="Boulder, CO", y_end_low_2-0.35, y_end_low_2),
          y_end_high_2 = ifelse(location=="Boulder, CO", y_end_high_2+0.35, y_end_high_2)
        )%>% 
        ungroup %>% 
        select(-ban_year), 
      by = c("year", "treated_state", "location")
    )
  
  p <- 
    all_plot_data %>% 
    filter(location != "Regulators' Exp.") %>% 
    filter(treated_state %in% c("Seattle, WA", "Boulder, CO")) %>% 
    ggplot(
      aes(x=year, y=tons_pc, color =location, linetype= location, size=location)
    )+
    geom_segment(
      aes(x=ban_year, xend=ban_year, y=y_end_low, yend  = y_end_high),
      linetype = "dotted", linewidth = 0.2, color = ut_colors[4])+
    geom_segment(
      aes(x=ban_year-3, xend=ban_year-3, y=y_end_low_2, yend  = y_end_high_2),
      linetype = "dotted", linewidth = 0.2, color = ut_colors[4])+
    
    geom_line()+
    facet_grid(
      rows=vars(factor(treated_state, levels = c("All", "CA", "CT", "MA", "RI", "VT", "Seattle, WA", "Boulder, CO", "San Francisco, CA"))),
      scales="free_y"
    )+
    geom_text(aes(x=xlab, y=ylab-0.01, label = label), color=ut_colors[5], size=3, family="Helvetica")+
    geom_text(aes(x=2005.5, y = y_first, label=y_first ), color=ut_colors[4], size=3, family="Helvetica")+
    geom_text(aes(x=2018.5, y = y_last %>% as.numeric, label=scales::number(y_last, accuracy = 0.01) ), color=ut_colors[4], size=3, family="Helvetica")+
    
    geom_text(aes(x=xlab_mae,y=ylab_mae, label = MAE), color =ut_colors[5], size=3, family="Helvetica")+
    scale_color_manual(breaks= c("Actual", "Synthetic", "Our Exp."), values = c(ut_colors[4],ut_colors[5],"seagreen"), name = "")+
    scale_linetype_manual(breaks= c("Actual", "Synthetic", "Our Exp."), values = c("solid", "solid", "solid"), name = "")+
    scale_size_manual(breaks= c("Actual", "Synthetic", "Our Exp."), values = c(0.5, 0.5, 1.0), name = "")+
    scale_y_continuous(expand = c(0.05,0.05))+
    scale_x_continuous(breaks=c(seq(2006, 2018, 2)), limits=c(2005, 2019), expand = c(0,0))+
    labs(y="", x= "")+
    theme_classic()+
    theme(
      legend.position = "top",
      strip.background = element_rect(color = "white", fill = "white"),
      panel.grid.major.x = element_blank() ,
      panel.grid.major.y = element_blank(), 
      text = element_text(family = "Helvetica",size = 10, color= ut_colors[4]), 
      axis.line.x = element_blank(),
      axis.line.y = element_blank(), 
      axis.ticks.y = element_blank(), 
      axis.text.y= element_blank(),
      axis.ticks.x = element_line(size = 0.1), 
      legend.text = element_text(family = "Helvetica", color = ut_colors[4],size = 10),
      panel.spacing = unit(1.1, "cm")
    )
  
  return(p)
}


######## Fig.2: left panel ##############
filename = "power_state_plac_2025_seed_5_alt3.csv"
chosen_sample_size = disposal_spec_states_function(filename)

chosen_sample_size_CA <- chosen_sample_size %>% filter(treated_state=="CA") %>% pluck("sample_size")-2
chosen_sample_size_CT <- chosen_sample_size %>% filter(treated_state=="CT") %>% pluck("sample_size")-2
chosen_sample_size_MA <- chosen_sample_size %>% filter(treated_state=="MA") %>% pluck("sample_size")-2
chosen_sample_size_RI <- chosen_sample_size %>% filter(treated_state=="RI") %>% pluck("sample_size")-2
chosen_sample_size_VT <- chosen_sample_size %>% filter(treated_state=="VT") %>% pluck("sample_size")-2
pooled_sample_size <- chosen_sample_size %>% filter(treated_state=="All") %>% pluck("sample_size")-2

seattle_number=which(treated_counties_id_cities=="seattleM1")
boulder_number=which(treated_counties_id_cities=="boulderM2")

chosen_sample_size_counties <- 
  read.csv(file=paste0(base_path,"power_county.csv")) %>% as_tibble() %>% group_by(treated_state) %>% filter(att_min == max(att_min))

chosen_sample_size_boulder <- chosen_sample_size_counties %>% filter(treated_state == "RI") %>% pluck("sample_size")-2
chosen_sample_size_seattle <- chosen_sample_size_counties %>% filter(treated_state == "M1") %>% pluck("sample_size")-2

set.seed(2)

xy_plot_data <-
  rbind( # we choose the sample size from the power calculations, see placebo_all.RMD or power_state.csv
    xy_plot_data_function("CA",chosen_sample_size_CA,1), # f is the chosen sample size -2
    xy_plot_data_function("CT",chosen_sample_size_CT,1),
    xy_plot_data_function("MA",chosen_sample_size_MA,1),
    xy_plot_data_function("RI",chosen_sample_size_RI,1),
    xy_plot_data_function("VT",chosen_sample_size_VT,1),
    
    xy_plot_data_function_cities(seattle_number, chosen_sample_size_seattle, 5) %>% mutate(treated_state = "Seattle, WA") %>% select(year, county_id, intercept2, intercept, ban_year,attempt, tons_pc, y, y_0, effect_size, treated_state, y_0_effect), 
    xy_plot_data_function_cities(boulder_number, chosen_sample_size_boulder, 5) %>% mutate(treated_state = "Boulder, CO") %>% select(year, county_id, intercept2, intercept, ban_year,attempt, tons_pc, y, y_0, effect_size, treated_state, y_0_effect),
    xy_plot_data_function_sf(2,1 )  %>% mutate(treated_state = "San Francisco, CA") %>% select(year, county_id, intercept2, intercept, ban_year,attempt, tons_pc, y, y_0, effect_size, treated_state, y_0_effect)
    
  )
# 

pooled_seed=1
xy_plot_data_uncentered <-
  rbind( # we choose the sample size from the power calculations, see placebo_all.RMD or power_state.csv
    xy_plot_data_uncentered_function("CA",pooled_sample_size,pooled_seed), # f is the chosen sample size -2
    xy_plot_data_uncentered_function("CT",pooled_sample_size,pooled_seed),
    xy_plot_data_uncentered_function("MA",pooled_sample_size,pooled_seed),
    xy_plot_data_uncentered_function("RI",pooled_sample_size,pooled_seed),
    xy_plot_data_uncentered_function("VT",pooled_sample_size,pooled_seed)
  )

# 
#   # get the donors of each treated states
# donors_CA <- xy_plot_data_function_donors("CA",chosen_sample_size_CA,1) # f is the chosen sample size -2
# donors_CT <- xy_plot_data_function_donors("CT",chosen_sample_size_CT,1)
# donors_MA <- xy_plot_data_function_donors("MA",chosen_sample_size_MA,1)
# donors_RI <- xy_plot_data_function_donors("RI",chosen_sample_size_RI,1)
# donors_VT <- xy_plot_data_function_donors("VT",chosen_sample_size_VT,1)

# this is to save and load the datasets. for exact reproduction of the paper's figures please load the "xy_plot_data.csv"
# 
# xy_plot_data <- write.csv(xy_plot_data, "xy_plot_data.csv", row.names = FALSE)
#xy_plot_data <- read.csv("xy_plot_data.csv")


xy_plot <- xy_plot_fun(1)
xy_plot <-
  xy_plot+
  labs(x="Disposal (tons per capita)", y = "", title = "")+#"Disposal (tons per capita)") + 
  theme(
    strip.text = element_blank(),
    plot.title = element_text(family = "Helvetica", color = ut_colors[4],size = 10, hjust=0.5))

xy_plot_sf <- xy_plot_fun_sf(1)
xy_plot_cities <- xy_plot_fun_cities(1)+
  labs(x="", y = "", title = "Disposal (tons per capita)") + 
  theme(
    strip.text = element_blank(),
    plot.title = element_text(family = "Helvetica", color = ut_colors[4],size = 10, hjust=0.5))


######## Fig.2: right panel ##############

disposal_spec <- rbind( #power_results from placebo_all.RMD
  read.csv(file=paste0(base_path,"power_county.csv")), # power_county for Seattle and Boulder  
  chosen_sample_size, # this replaces the power_state.csv in the previous version (this is better because we have dynamically calucalted this, instead of loading manually a csv)
  read.csv(file=paste0(base_path,"sf_power.csv")))%>% #sf_power for SF
  filter((treated_state=="All" & specification == "State Pooled")| treated_state!="All") 



disposal_effect <- disposal_effect_size2%>% rename(effect = effect_size)

bt_with_power_data <- 
  disposal_spec %>% 
  rename(
    power_low = att_min, 
    power_high = att_max
  ) %>% select(-att_median)%>%  
  mutate (power_low = 100*power_low, power_high=100*power_high) %>% 
  group_by(specification, treated_state, ban_year) %>% 
  filter(power_low == max(power_low)) %>%
  ungroup %>% 
  filter(specification == "State") %>%
  rbind(
    disposal_spec %>%  
      mutate(type = "disposal") %>%  
      filter(
        treated_state %in% c("M1", "RI", "M3"), 
        specification == "County", 
        sample_size!=0) %>% 
      rename(
        power_low = att_min, 
        power_high = att_max
      ) %>% select(-att_median)%>% select(-type) %>%  mutate (power_low = 100*power_low, power_high=100*power_high) %>% 
      group_by(specification, treated_state, ban_year) %>% filter(power_low == max(power_low)) %>%
      ungroup %>% 
      mutate(
        treated_state = ifelse(treated_state=="RI","boulderM2", treated_state),
        treated_state = ifelse(treated_state=="M1","seattleM1", treated_state),
        treated_state = ifelse(treated_state=="M3","san franciscoM3", treated_state)) # i use 2016 power for boulder
  ) %>% 
  left_join(
    xy_plot_data %>% 
      mutate(county_id=ifelse(county_id == "sanfranciscoM3", "san franciscoM3", county_id)) %>% 
      filter(attempt==1) %>% 
      group_by(county_id) %>% 
      filter(year>=ban_year) %>% 
      summarise(actual_treatment_effect = sum(tons_pc - y_0)/sum(y_0)), 
    by = c("treated_state"= "county_id")
  ) %>% 
  rbind(
    # For the aggregate case
    disposal_spec %>% 
      rename(
        power_low = att_min, 
        power_high = att_max
      ) %>% 
      select(-att_median)%>%  
      mutate (power_low = 100*power_low, power_high=100*power_high) %>% 
      group_by(specification, treated_state, ban_year) %>% 
      #filter(sample_size==3) %>% 
      filter(power_low == max(power_low)) %>% #manually choosing 3 because it has super close power to 4 but power_high is much smaller
      ungroup %>% 
      filter(specification == "State Pooled", ban_year == 2015, treated_state=="All") %>% 
      mutate(treated_state="All") %>% 
      left_join(
        #xy_plot_data  %>% 
        xy_plot_data_uncentered %>% 
          filter(attempt==1, treated_state %in% c("CA","CT", "VT", "MA", "RI")) %>% 
          group_by(year, attempt) %>% 
          summarise(
            Synthetic = mean(y_0), 
            Actual = mean(tons_pc)) %>% 
          ungroup %>%  filter(year>=pooled_ban_year) %>% 
          summarise(actual_treatment_effect=sum(Actual-Synthetic)/sum(Synthetic), county_id="All") %>% 
          select(county_id, actual_treatment_effect), 
        by = c("treated_state"= "county_id")
      )
  ) %>% 
  left_join(
    rbind(
      disposal_effect %>% 
        filter(
          year <= ey
        ) %>% 
        #group_by (state_id, effect_type) %>% 
        group_by(state_id) %>% 
        summarise(mean_effect = mean(effect)) %>% 
        ungroup %>%
        mutate(mean_all_effect = mean(mean_effect)) %>%  
        rbind(
          tibble (
            state_id = rep("All",1), 
            #effect_type = c("fiori_estimates", "lower_bound"), 
            mean_effect = NA, 
            mean_all_effect= NA)) %>%  
        mutate(mean_effect = ifelse(is.na(mean_effect), mean(mean_all_effect, na.rm=TRUE),mean_effect)) %>%  
        ungroup %>%  select(-mean_all_effect), #%>% 
      #filter(effect_type=="lower_bound") %>% 
      #select(-effect_type),
      
      municipal_effect %>% filter(type=="disposal") %>% select(-type) %>% 
        rename(mean_effect= effect) %>% 
        mutate(
          state_id = ifelse(state_id=="M2","boulderM2", state_id),
          state_id = ifelse(state_id=="M1","seattleM1", state_id),
          state_id = ifelse(state_id=="M3","san franciscoM3", state_id), 
          mean_effect = -mean_effect)
    ),
    by = c("treated_state"= "state_id")
  )  %>% 
  left_join(
    tibble(
      state_id=c("MA", "CA", "VT"), 
      reg_effect = reg_effect # Vermont's reg expect comes from: Regulators expectations are 17.52% to 21.78% by 2022. 
      #Using our coverage we find that by 2022 exp effect is 15.9%. Then using simple linear approximation and taking the mean (0.0824*21.78/15.9+0.0824*17.52/15.9)/2 we get this number
    ), 
    by = c("treated_state"="state_id")
  ) %>% 
  mutate(
    treated_state = ifelse(treated_state=="boulderM2","Boulder, CO", treated_state),
    treated_state = ifelse(treated_state=="seattleM1","Seattle, WA", treated_state),
    treated_state = ifelse(treated_state=="san franciscoM3","San Francisco, CA", treated_state), 
    treated_state = ifelse(treated_state=="All","All States", treated_state),
    treated_state=factor(treated_state, levels=c("San Francisco, CA","Boulder, CO","Seattle, WA","VT", "RI", "MA", "CT", "CA", "All States"))
  ) %>%  rename(state_id=treated_state) 







bt_with_power_2 <- 
  bt_with_power_data %>% 
  mutate(
    state_id = fct_recode(
      state_id,
      "San Francisco, CA" = "San Francisco, CA", 
      "Boulder, CO" = "Boulder, CO",
      "Seattle, WA" = "Seattle, WA",
      "Vermont" = "VT",
      "Rhode Island" = "RI",
      "Massachusetts" = "MA",
      "Connecticut" = "CT",
      "California" = "CA",
      "All States" = "All States"
    )) %>% 
  ggplot() +
  aes(y= state_id, x= 100*actual_treatment_effect, group = 1)+
  # All geoms with their color aesthetics
  geom_errorbar(aes(xmin = power_low, xmax = power_high, color = "Placebo"), 
                width = 0.2, size = 0.5) +
  geom_point(aes(color = "Estimate"), size = 1.5) +
  geom_errorbar(aes(xmin = -100*mean_effect, xmax = -100*mean_effect, color = "Our Exp."), 
                linewidth = 1, width = 0.2) +
  geom_errorbar(aes(xmin = -100*reg_effect, xmax = -100*reg_effect, color = "Regulators' Exp."), 
                linewidth = 1, width = 0.2) +
  
  # Single color scale for all elements
  scale_color_manual(
    name = "",
    values = c(
      "Placebo" = ut_colors[5],
      "Estimate" = ut_colors[4], 
      "Our Exp." = "seagreen",
      "Regulators' Exp." = "#bad9c6"
    ),
    breaks = c("Estimate", "Placebo", "Our Exp.", "Regulators' Exp."),
    guide = guide_legend(order = 1)
  ) +
  
  # Non-aesthetic elements
  geom_vline(xintercept = 0, lty = "dotted", color = ut_colors[5]) +
  
  labs(y = "", x = "", color = "")+
  
  #scale_x_continuous(breaks = c(seq(-25, 25, by = 5)), limits = c(-30, 25))+ 
  scale_x_continuous(breaks = c(seq(-35, 35, by = 10)), limits = c(-40, 40))+ 
  scale_y_discrete(position= "left", expand=c(0.02,0.01))+
    theme(
      legend.position = "top",  # Keep the legend at the top
      legend.justification = c(1, 2),
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
      legend.text = element_text(family = "Helvetica",size = 10, color= ut_colors[4])
    )
  



# bt_with_power_2 <- 
#   bt_with_power_2+ theme(
#   strip.placement = "outside",
#   strip.text.y = element_text(angle = 0)
# )

#xy_and_power <-ggpubr::ggarrange(xy_plot+labs(x="Year", y = "Tons Per Capita") + theme(strip.text = element_blank()), bt_with_power)
xy_and_power_2 <-
  ggpubr::ggarrange(
    xy_plot %+% 
      subset(
        xy_plot$data, !treated_state %in% c("Boulder, CO", "Seattle, WA"))+
      geom_point(data = subset(xy_plot$data %>% filter(!treated_state %in%  c("Boulder, CO", "Seattle, WA")), location %in% c("Actual", "Synthetic"))) +
      labs(x="Ban calendar year", y = "Disposal (tons per capita)")+
      theme(axis.text = element_text(family = "Helvetica",size = 10, color= ut_colors[4])), 
    
    bt_with_power_2 %+% subset(bt_with_power_2$data, !state_id %in% c("San Francisco, CA", "Boulder, CO", "Seattle, WA")) +
      labs(x="Ban effect on disposal (%)", title = "")+
      scale_x_continuous(limits=c(-20, 20), breaks = c(seq(-15, 15, by =5)))+
      theme (
        plot.title = element_text(hjust=0.6, size = 10, color=ut_colors[4]), 
        axis.text = element_text(family = "Helvetica",size = 10, color= ut_colors[4])), 
    labels = c("A","B"),
    label.x = c(0, 0.1),
    label.y = c(.98, 0.98),
    heights = c(0.5, 1), 
    widths = c(1, 1.2))

xy_and_power_2


# ggsave(
#   xy_and_power_2, filename = "xy_and_power_2025.pdf", device = cairo_pdf,
#   path= figure_path,
#   width = 10, height = 9, units = "in")



# 
# ggsave(xy_and_power, filename = "xy_and_power.pdf", device = cairo_pdf,
#        path= figure_path,
#        width = 7, height = 7, units = "in")

##### Fig.2: all together #####
bt_with_power_data
# 
ggsave(
  xy_and_power_2, filename = "xy_and_power_2025_alt3.pdf", device = cairo_pdf,
  path= figure_path,
  width = 10, height = 9, units = "in")
