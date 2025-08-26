######### 
# This is the exact same as placebo_all.R with one key difference: instead of creating ban-calendar years, we shift the ban's implementation date to the closest calendar year, 
# that is for CA (actual month of implementation 04/2016) --> 2016, for VT (actual month of implementation 07/2014) --> 2015, for MA (actual month of implementation 10/2014) --> 2015
#########

library(tidyverse)
library(extrafont)

mypathname <-"/Users/fian4421/Library/CloudStorage/Dropbox/Organic Waste Bans"
municipal_path <- paste0(mypathname, "/03.1. Municipal Data")
state_data_path <- paste0(mypathname,"/03. State_Data")
base_path <- paste0(mypathname,"/06. Post SYP/00. Code/")
figure_path <- "/Users/fian4421/Library/CloudStorage/Dropbox/Apps/Overleaf/Organic Waste Bans/Figures/Corrections"
loadfonts(device = "win")
ut_colors <- c(
  rgb(132, 59,14, max=255), # dark orange
  rgb(255, 127, 21, max=255), # bright orange
  rgb(191,87,0, max=255), # ut orange
  rgb(51,73,72, max=255), # dark grey
  rgb(156, 173, 183, max=255), #light grey
  rgb(191,87,0,alpha=50, max=255))# ut orange

# Population
population <- read.csv(paste0(state_data_path,"/00. Controls/Population/population.csv"))
population <- cbind(population[1:2], stack(population[3:31]))
colnames(population)<- c("state_id", "county_name", "pop", "year")
population$year <- substring(population$year, 2) %>% as.integer
population$pop <- as.numeric(population$pop)
population$county_name[population$county_name=="doña ana"] <- "dona ana"
population <- population[population$state_id!="AK" & population$state_id!="co" & population$state_id!="ia",] # contiguous states, DC is considered a contiguous state

population_2020 <- read.csv(paste0(state_data_path,"/00. Controls/Population/population_2020.csv"))
population_2020 <- population_2020[population_2020$state_id !="DC",]
population_2020$county_name[population_2020$county_name=="doña ana"] <- "dona ana"
population <- rbind(population, population_2020)
rm(population_2020)


# Waste Data
#power2 <- read.csv("power2_2.csv")
power2 <- read.csv(file=paste0(base_path,"power2_impexp.csv"))
all_treated <- c("VT", "MA", "CA", "CT", "RI")# Never changes
bans <- c(2014, 2014, 2016, 2014, 2016)
#bans <- c(2014, 2014, 2016, 2014, 2016)
bans_passage <- c(2012, 2013, 2014, 2011, 2014) #passage dates
year_start <- 2006
year_end <- 2018




##### Pre-processing function ####

pre_processing_dt_state <- function (power2)
{
  
  year_start <- 2006
  year_cutoff <- 2018
  dt_state <- 
    power2 %>% 
    mutate(county_id=paste0(county_name, state_id)) %>% 
    #filter(!county_id%in%c(rural)) %>% 
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

##### Functions ####
do_many_times_v3 <- function (i, x, test_ind_end1, test_ind_end2,y_train, y_test, y_att, n_don,sample_size)
{
  #Approach 2- Only Intercept
  samples <- sample(n_don, sample_size)
  x <- rowMeans(x[, samples]) # This is for sample size > 1
  #x=x[, samples] # This is for sample size equal to 1
  n <- length(y_train)+ length(y_test) + length(y_att)
  
  intercept <- mean(y_train-x[1:test_ind_end1])
  
  ss_res <- sum((y_train-x[1:test_ind_end1] - intercept)^2) #calculating the in-sample R-squared
  ss_tot <- sum((y_train-mean(y_train))^2)
  r <- 1- ss_res/ss_tot
  MA  <- (intercept + x[(test_ind_end1+1):test_ind_end2] - y_test )/(intercept + x[(test_ind_end1+1):test_ind_end2]) 
  MA <- MA %>% abs %>%  mean
  
  att <- (y_att-x[(test_ind_end2+1):n]-intercept) %>% sum
  cf <- (x[(test_ind_end2+1):n]+intercept) %>% sum
  c(r, MA, att, cf, c(samples))
  intercept2 <-  mean(c(y_train, y_test)-x[1:test_ind_end2])
  att <- (y_att-x[(test_ind_end2+1):n]-intercept2) %>% sum
  cf <- (x[(test_ind_end2+1):n]+intercept2) %>% sum
  c(r, MA, att, cf, c(samples))
  
  
  # 
  # samples <- sample(n_don, sample_size)
  # x <- rowMeans(x[, samples])
  # n <- length(y_train)+ length(y_test) + length(y_att)
  # 
  # x_train <- x[1:test_ind_end1]
  # x_test <-  x[(test_ind_end1+1):test_ind_end2]
  # x_att <- x[(test_ind_end2+1):n]
  # 
  # coef <- sum((x_train - mean(x_train)) * (y_train - mean(y_train))) / sum((x_train - mean(x_train))^2)
  # intercept <- mean(y_train)-coef * mean(x_train)
  # 
  # MA <- mean(abs((intercept + x_test * coef - y_test) / y_test))
  # #att <- sum(intercept + x_att * coef - y_att)
  # cf <-(intercept + x_att * coef ) %>% sum
  # att <- (y_att - intercept - x_att * coef) %>% sum
  # #ss_res <- sum((y_train - intercept - x_train * coef)^2)
  # #ss_tot <- sum((y_train - mean(y_train))^2)
  # r <- 1 - sum((y_train - intercept - x_train * coef)^2) /  sum((y_train - mean(y_train))^2)
  # c(r, MA, att, cf, c(samples))
  
}

in_sample_R2_v2 <- function (k, dt, donors,iterations_scale, option, ban_year, offset, samp, seed)
{
  set.seed(seed)
  ###
  # This function creates the SC using the identification method we describe in the paper 
  # It returns for each sample_size the 100 best SC (best means that these SCs have the lowest MAPE)
  ###
  
  treated_location <- donors[k] #the placebo state
  treated_counties_id <- all_treated #c(all_treated, treated_location)#[which(all_treated != treated_state)]
  year_end <- ban_year-offset # the start of the validation period
  state_treated <- ifelse( #the placebo state, this serves the case where the treated location is a county and thus we want to exclude all the counties in the same state
    str_length(treated_location)>2, 
    substr(treated_location, str_length(treated_location)-1, str_length(treated_location)), 
    treated_location)
  
  don_new <- donors[donors!=treated_location] #the donor pool
  don_new <- #the donor pool, this serves the case where the treated location is a county and thus we want to exclude all the counties in the same state
    don_new %>% 
    as_tibble %>% 
    mutate(
      state = ifelse( 
        str_length(value)>2, 
        substr(value, str_length(value)-1, str_length(value)), 
        value)
    ) %>% 
    filter(
      state != state_treated
    ) %>% as.data.frame()
  
  don_new <- don_new[,"value"]
  
  n_don <- length(don_new)                              #number of potential donors
  test_ind_end1 <- year_end - year_start+1              #end of training period 
  test_ind_end2 <- ban_year-year_end-1 +test_ind_end1   #end of validation period 
  
  y <- dt[dt$county_id==treated_location, c("tons_pc")] #actual disposal per capita of the treated state  
  y_train <- y[1:test_ind_end1]                         #disposal series during training period
  y_test <-  y[(test_ind_end1+1):test_ind_end2]         #disposal series during validation period
  y_att <- y[(test_ind_end2+1):length(y)]               #disposal series during the post-ban period
  
  x <- dt[dt$county_id %in% don_new,]      #actual disposal per capita of the potential donors
  x <- as.matrix(unstack(x, tons_pc ~ county_id)) 
  
  
  res <- tibble( #initializing results table
    r_sq = 0, 
    mape = 0, 
    att=0, 
    cf=0, 
    sample_size=0, 
    county_id ="", 
    iterations = 0, 
    ban_year = 0, 
    donor_number="", 
    chosen_donor=0
  )
  
  for ( f in 1:length(samp)) #for each potential value of |S| (in our case |S| can be between 3 and 10)
  {
    sample_size <- samp[f]
    iterations <- iterations_scale #min(iterations_scale* sample_size, 15000)
    
    all <- lapply(seq(1:iterations),do_many_times_v3,x, test_ind_end1, test_ind_end2,y_train, y_test, y_att,n_don, sample_size)
    all <- all %>% sapply(c) %>% t
    
    colnames(all) <- c(
      "r_sq",
      "mape", 
      "att",
      "cf",
      paste0(rep("donor", sample_size), paste0("_", c(1:sample_size)))
    )
    
    all <- 
      all %>% 
      as_tibble %>%
      filter(r_sq>0) %>% 
      arrange(mape) %>% 
      slice(1:50) %>% # keep the 100 that have the lowest MAPE
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
      )
    
    res <- rbind(res, all)
  }
  res %>% 
    as_tibble 
}

power_state_plac <- function(treated_state, dt_state_initial, seed)
{
  ###
  # This function creates the re-centered dataset for each ban 
  # and returns the best 100 SC for each sample size 
  ###
  ban_year <- bans[which(all_treated == treated_state)]#the ban year of the treated state
  set.seed(seed)
  ####
  # we recenter the time series of the treated states based on when the ban went into effect, i.e., for VT it went into effect in July so we recenter the time series so it starts in July
  ####
  if(treated_state == "MA" | treated_state == "VT"){ban_year = ban_year+1}
  dt_state <- dt_state_initial
  #all_treated <- all_treated[which(all_treated != treated_state)]
  
  dt_state <- dt_state %>%  as.data.frame()
  treated_counties_id <- unique(dt_state$county_id[dt_state$state_id%in% all_treated])
  dt_state <- dt_state[!(dt_state$state_id %in% all_treated),] %>% as.data.frame
  donors_state <- unique(dt_state$county_id)#[!(dt_state$state_id%in% all_treated)])
  
  
  ###
  # apply function in_sample_R2_v2 to all the non-treated states
  # Function in_sample_R2_v2 essentially creates the 100000 SC and returns 
  # the 100 best SCs. 
  ###
  
  plac <- lapply(seq(1:length(donors_state)),in_sample_R2_v2,dt_state, donors=donors_state, iterations_scale=100000, option="V2", ban_year=ban_year, offset=3, samp =samp, seed)
  
  return(plac)
  
}

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
      pools = sample(nrow(donors),n) %>% list()
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
      cf= mean(cf)) %>% 
    group_by(sample_size, ban_year) %>% 
    summarise(
      att = sum(att*cf)/sum(cf), #and finally, to find the ATT of the aggregate case, take the mean of the five chosen states. 
      mape = mean(mape), 
      r_sq = mean(r_sq), 
      i
    ) %>% 
    ungroup
}


#### Power ####

dt_state_initial <- pre_processing_dt_state(power2) #state-level disposal
treated_counties_id <- unique(dt_state_initial$county_id[dt_state_initial$state_id%in% all_treated]) # treated states id 
donors_state <- unique(dt_state_initial$county_id[!(dt_state_initial$state_id%in% all_treated)]) # all donor states
all_treated <- c("VT", "MA", "CA", "CT", "RI", "All")# Never changes
bans <- c(2014, 2014, 2016, 2014, 2016, 2015) #we assume that the aggregate ban is implemented in 2015
samp=seq(3,10) # possible values of |S|

seed=1
for (seed in 1:1)
{
  power_state_plac1 <- power_state_plac("MA", dt_state_initial, seed) # SC outcomes for MA's ban
  power_state_plac2 <- power_state_plac("CA", dt_state_initial, seed) # SC outcomes for CA's ban
  power_state_plac3 <- power_state_plac("CT", dt_state_initial, seed)# SC outcomes for CT's ban
  power_state_plac4 <- power_state_plac("RI", dt_state_initial, seed)# SC outcomes for RI's ban
  power_state_plac5 <- power_state_plac("VT", dt_state_initial, seed)# SC outcomes for VT's ban
  power_state_plac6 <- power_state_plac("All", dt_state_initial,seed) #needed for the aggregate case
  
  
  write.csv(
    rbind(
      power_state_plac1 %>% bind_rows %>% mutate(treated_state="MA"),
      power_state_plac2 %>% bind_rows %>% mutate(treated_state="CA"),
      power_state_plac3 %>% bind_rows %>% mutate(treated_state="CT"),
      power_state_plac4 %>% bind_rows %>% mutate(treated_state="RI"),
      power_state_plac5 %>% bind_rows %>% mutate(treated_state="VT"),
      power_state_plac6 %>% bind_rows %>% mutate(treated_state="All")
    ),
    paste0("power_state_plac_2025_seed_", seed, "_alt2.csv"), row.names=FALSE
  )
  
}




