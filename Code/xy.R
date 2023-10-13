
do_many_times_v3_with_inter <- function (i, x, test_ind_end1, test_ind_end2,y_train, y_test, y_att, n_don,sample_size)
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

disposal_effect_size <- read.csv("disposal_effect_size.csv")
year_start = 2006
year_cutoff = 2018
pooled_ban_year = 2015


dt_state_initial <- pre_processing_dt_state(power2)
#dt_state_initial<- dt_state_initial %>% mutate(tons_pc = ifelse(state_id=="CT", tons_pc+0.15, tons_pc))
treated_counties_id <- unique(dt_state_initial$county_id[dt_state_initial$state_id%in% all_treated])
donors_state <- unique(dt_state_initial$county_id[!(dt_state_initial$state_id%in% all_treated)])
donors <- donors_state
xy_plot_data_function <- function (treated_state, f, seed)
{
  set.seed(seed)
  if(treated_state == "MA")
  {
    dt_state <- dt_state_initial %>% group_by(state_id) %>% 
      mutate(
        tons_pc = tons_pc*0.25 + .75*ifelse(is.na(lag(tons_pc, n=1, default = NA)), tons_pc, lag(tons_pc, n=1, default = NA)) 
      )
  }else if(treated_state == "VT")
  {
    dt_state <- dt_state_initial %>%group_by(state_id) %>% 
      mutate(
        lag =lag(tons_pc, n=1, default = NA),
        tons_pc = tons_pc*.5 + .5*ifelse(is.na(lag(tons_pc, n=1, default = NA)), tons_pc, lag(tons_pc, n=1, default = NA)) 
      )
  }else if(treated_state == "CA")
  {
    dt_state <- dt_state_initial %>%group_by(state_id) %>% 
      mutate(
        tons_pc = tons_pc*.75 + .25*ifelse(is.na(lag(tons_pc, n=1, default = NA)), tons_pc, lag(tons_pc, n=1, default = NA)) 
      )
  }else {dt_state <- dt_state_initial} 
  
  dt <- dt_state %>% as.data.frame()
  ban_year <- bans[which(all_treated == treated_state)]
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
    ) %>% #filter(attempt %in% c(1,7,19)) %>% 
    ungroup %>% 
    left_join(
      disposal_effect_size %>% filter(effect_type== "lower_bound") %>%  select(year, state_id, effect_size), 
      by = c("year", "county_id" = 'state_id')
    ) %>% 
    mutate(
      treated_state = treated_state, 
      #y_0_effect = ifelse(year>=ban_year, y_0*(1-effect), ifelse(year == ban_year-1, y_0, NA))
      y_0_effect = ifelse(year>=ban_year, y_0*(1-effect_size), NA)
    )
}

dt_initial <- dt_municipal_initial
donors_cities <- unique(dt_initial$county_id[!(dt_initial$state_id%in% all_treated)])
treated_counties_id_cities <- unique(dt_initial$county_id[dt_initial$state_id%in% all_treated])
samp= c(3:10)
all_treated <- c("VT", "MA", "CA", "CT", "RI", "M1", "M2", "M3")# Never changes
bans <- c(2014, 2014, 2016, 2014, 2016, 2015, 2016, 2009)
year_start = 2006
year_cutoff = 2018
municipal_effect <- read.csv("municipal_effect.csv")


xy_plot_data_function_cities <- function (k, f)
{
  
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

xy_plot_data_function_sf <- function (f)
{
  year_start <- 1996
  year_cutoff <- 2014
  year_end <- 2011
  
  dt <- 
    power2 %>% filter(state_id=="CA") %>% 
    filter(
      year >= year_start, 
      year <= year_cutoff, 
      state_id != "OH",
      type %in% c("disposal", "msw_disposed")
    ) %>% 
    mutate(
      tons = ifelse(county_name=="butte" & state_id == "CA" & year == 2019, 181914, tons), #I think one digit was wron original is: 1819143
      tons = ifelse(county_name=="colusa" & state_id == "CA" & year == 1995, 13375, tons), # Use next year's tonnage 13375
      tons = ifelse(county_name=="glenn" & state_id == "CA" & year == 2019, 27394, tons), # First quarter tonnage is 60,200 --> i think it should have been 6,020 
      tons = ifelse(county_name=="tehama" & state_id == "CA" & year == 2011, (67376+41921)/2, tons), # Linear extrapolation between 2010 and 2012. The original is more than 3 times as high. 
      tons = ifelse(county_name=="trinity" & state_id == "CA" & year == 2017, 8669, tons), # Fourth quarter tonnage is 21,493 which is unusually high=> I think it should have been 2,149
      tons = ifelse(county_name=="modoc" & state_id == "CA" & year == 2019, tons-11537, tons), # Unusally high 
      tons = ifelse(county_name=="mariposa" & state_id == "CA" & year == 2017, 14559, tons), #3rd quarter is extremely high
      tons = ifelse(county_name=="mariposa" & state_id == "CA" & year == 2019, 13500, tons), #2nd quarter is extremely high
      tons = ifelse(county_name=="del norte" & state_id == "CA" & year == 2019, tons+ 4883*2, tons), #missing 3rd and 4th quarter
      tons = ifelse(county_name=="mono" & state_id == "CA" & year == 1995, 22024, tons), #linear interpolation from years 1996 1997
      tons = ifelse(county_name=="sonoma" & state_id == "CA" & year == 2017, tons-495370, tons), #4th quarter is 4times as much as the previous- use the third
      tons = ifelse(county_name=="sonoma" & state_id == "CA" & year == 2018, tons-662089, tons), #1st quarter is 5times as much as the next- use the second
      tons = ifelse(county_name=="shasta" & state_id == "CA" & year == 2018, 198899, tons), #use the average between the previous and the next year
      tons = ifelse(county_name=="sierra" & state_id == "CA" & year == 2019, tons+1371, tons), #3rd and 4th quarter missing (adding the 2018 ones)
      tons = ifelse(county_name=="nevada" & state_id == "CA" & year == 2012, 68165, tons), #2012 has unusually low tonnage
      tons = ifelse(county_name=="calaveras" & state_id == "CA" & year == 2015, (31741.23+42755.94)/2, tons), #use the average between the previous and the next year
      tons = ifelse(county_name=="san benito" & state_id == "CA" & year %in% c(1995, 1996, 1997), 56821.52, tons), #use the 1998 for all the years between 95 and 98 because its like half
      tons = ifelse(county_name=="siskiyou" & state_id == "CA" & year == 2014, (27811.48+35305.71)/2, tons) #use the average between the previous and the next year
    )%>%  
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
      county_id = ifelse(is.na(county_name), state_id, paste0(county_name, state_id)),
      county_id = ifelse(county_id=="san franciscoCA", "san franciscoM3", county_id),
      state_id = ifelse(county_id == "san franciscoM3", "M3", "other")
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

xy_plot_fun <- function (i)
{
  
  reg_expect <- 
    tibble(
      state_id=c("MA", "CA", "VT"), 
      reg_effect = c(0.098, 0.185, 0.1752)
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
      disposal_effect_size %>%
        filter(effect_type== "lower_bound") %>%  
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
      Expected = y_0_effect
    ) %>% 
    left_join(reg_expect, by = c("county_id"="state_id")) %>% 
    mutate(
      `Regulators' Exp.` = ifelse(!is.na(effect_size), Synthetic*(1-reg_effect), NA)
    ) %>% 
    pivot_longer(
      cols = c("Synthetic", "Actual", "Expected", `Regulators' Exp.`), 
      names_to = "location", 
      values_to = "tons_pc") %>% select(year, attempt, location, tons_pc, treated_state, ban_year) %>% 
    rbind(
      all%>% 
        rename(
          Synthetic = y_0, 
          Actual = tons_pc, 
          Expected = y_0_effect
        ) %>% 
        pivot_longer(
          cols = c("Synthetic", "Actual", "Expected"), 
          names_to = "location", 
          values_to = "tons_pc") %>% select(year, attempt, location, tons_pc, treated_state, ban_year)
    ) %>% 
    left_join(
      rbind(xy_plot_data%>% select(year, treated_state, ban_year, attempt, tons_pc, y, y_0, y_0_effect), all) %>% 
        group_by(treated_state) %>% 
        filter(year >= ban_year-3, year < ban_year) %>%
        mutate(
          year=mean(year), # i duplicate so i join and then also keep the xlab_mae
          xlab_mae=mean(year)+0.5,  
          xlab_mae=ifelse(treated_state%in%c("Boulder, CO"), xlab_mae+0.3, xlab_mae),
          ylab_mae=ifelse(treated_state%in%c("All", "CA", "CT"), mean(tons_pc)-0.05, mean(tons_pc)-0.06), 
          ylab_mae=ifelse(treated_state%in%c("Seattle, WA"), ylab_mae-0.01, ylab_mae), 
          #ylab_mae=ifelse(treated_state%in%c("CT"), ylab_mae+0.01, ylab_mae), 
          ylab_mae=ifelse(treated_state%in%c("All"), ylab_mae+0.01,ylab_mae), 
          ylab_mae=ifelse(treated_state%in%c("CA"), ylab_mae+0.01, ylab_mae), 
          ylab_mae=ifelse(treated_state%in%c("RI"), ylab_mae-0.01, ylab_mae), 
          xlab_mae=ifelse(treated_state%in%c("All"), xlab_mae, xlab_mae), 
          xlab_mae=ifelse(treated_state%in%c("CA"), xlab_mae+0.2, xlab_mae)) %>% 
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
      tibble (xlab= c(2010, pooled_ban_year-2, pooled_ban_year+2), ylab = 0.85, label = c("Train", "Validation", "Evaluation"), treated_state = "All"), 
      by =c ("treated_state", "year"="xlab")
    ) %>% 
    mutate(
      xlab = ifelse(!is.na(ylab), year, NA),
      xlab = ifelse(!is.na(ylab) & year==pooled_ban_year-2, xlab+0.5, xlab), 
      MAE = ifelse(treated_state=="All", paste0("MAPE (%): ", MAE), MAE), 
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
      linetype = "dotted", linewidth = 0.2, color = ut_colors[5])+
    geom_segment(
      aes(x=ban_year-3, xend=ban_year-3, y=y_end_low_2, yend  = y_end_high_2),
      linetype = "dotted", linewidth = 0.2, color = ut_colors[5])+
    
    geom_line()+
    facet_grid(
      rows=vars(factor(treated_state, levels = c("All", "CA", "CT", "MA", "RI", "VT", "Seattle, WA", "Boulder, CO", "San Francisco, CA"))),
      scales="free_y"
    )+
    geom_text(aes(x=xlab, y=ylab-0.01, label = label), color=ut_colors[5], size=3, family="Helvetica")+
    geom_text(aes(x=2005.5, y = y_first, label=y_first ), color=rgb(90,90,90, maxColorValue = 255), size=3, family="Helvetica")+
    geom_text(aes(x=2018.5, y = y_last %>% as.numeric, label=scales::number(y_last, accuracy = 0.01) ), color=rgb(90,90,90, maxColorValue = 255), size=3, family="Helvetica")+
    
    #geom_text(aes(x=xlab_mae,y=ylab_mae, label = MAE), color ="#857d95", size=3, family="Helvetica")+
    geom_text(aes(x=xlab_mae,y=ylab_mae, label = MAE), color =ut_colors[5], size=3, family="Helvetica")+
    
    scale_color_manual(breaks= c("Actual", "Synthetic", "Expected", "Regulators' Exp."), values = c(ut_colors[4],ut_colors[5],"seagreen", "#bad9c6"), name = "")+
    scale_linetype_manual(breaks= c("Actual", "Synthetic", "Expected", "Regulators' Exp."), values = c("solid", "solid", "solid", "solid"), name = "")+
    scale_size_manual(breaks= c("Actual", "Synthetic", "Expected", "Regulators' Exp."), values = c(0.5, 0.5, 1.0, 1.0), name = "")+
    scale_x_continuous(breaks=c(seq(2006, 2018, 2)), limits=c(2005, 2019), expand = c(0,0))+
    scale_y_continuous(expand = c(0.05,0.05))+
    
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
      panel.spacing = unit(1, "cm")
    )
  

  return(p)

}

xy_plot_fun_sf <- function (i)
{
  reg_expect <- 
    tibble(
      state_id=c("MA", "CA", "VT"), 
      reg_effect = c(0.098, 0.185, 0.1752)
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
      disposal_effect_size %>% 
        filter(effect_type== "lower_bound") %>%  
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
      Expected = y_0_effect
    ) %>%
    left_join(reg_expect, by = c("county_id"="state_id")) %>% 
    mutate(
      `Regulators' Exp.` = ifelse(!is.na(effect_size), Synthetic*(1-reg_effect), NA)
    ) %>% 
    pivot_longer(
      cols = c("Synthetic", "Actual", "Expected", `Regulators' Exp.`), 
      names_to = "location", 
      values_to = "tons_pc") %>% select(year, attempt, location, tons_pc, treated_state, ban_year) %>% 
    rbind(
      all%>% 
        rename(
          Synthetic = y_0, 
          Actual = tons_pc, 
          Expected = y_0_effect
        ) %>% 
        pivot_longer(
          cols = c("Synthetic", "Actual", "Expected"), 
          names_to = "location", 
          values_to = "tons_pc") %>% select(year, attempt, location, tons_pc, treated_state, ban_year)
    ) %>%
    left_join(
      rbind(xy_plot_data%>% select(year, treated_state, ban_year, attempt, tons_pc, y, y_0, y_0_effect), all) %>% 
        group_by(treated_state) %>% 
        filter(year >= ban_year-3, year < ban_year) %>%
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
      tibble (xlab= c(2009, 2012, 2016), ylab = 0.85, label = c("Train", "Validation", "Evaluation"), treated_state = "All"), 
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
      linetype = "dotted", linewidth = 0.2, color = ut_colors[5])+
    geom_segment(
      aes(x=ban_year-3, xend=ban_year-3, y=y_end_low_2, yend  = y_end_high_2),
      linetype = "dotted", linewidth = 0.2, color = ut_colors[5])+
    
    geom_line()+
    geom_text(aes(x=xlab, y=ylab-0.01, label = label), color=ut_colors[5], size=3, family="Helvetica")+
    geom_text(aes(x=1995.2, y = y_first, label=y_first ), color=rgb(90,90,90, maxColorValue = 255), size=3, family="Helvetica")+
    geom_text(aes(x=2014.8, y = y_last %>% as.numeric, label=scales::number(y_last, accuracy = 0.01) ), color=rgb(90,90,90, maxColorValue = 255), size=3, family="Helvetica")+
    
    geom_text(aes(x=xlab_mae,y=ylab_mae, label = MAE), color =ut_colors[5], size=3, family="Helvetica")+
    scale_color_manual(breaks= c("Actual", "Synthetic", "Expected", "Regulators' Exp."), values = c(ut_colors[4],ut_colors[5],"seagreen", "#bad9c6"), name = "")+
    scale_linetype_manual(breaks= c("Actual", "Synthetic", "Expected", "Regulators' Exp."), values = c("solid", "solid", "solid", "solid"), name = "")+
    scale_size_manual(breaks= c("Actual", "Synthetic", "Expected", "Regulators' Exp."), values = c(0.5, 0.5, 1.0, 1.0), name = "")+
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

set.seed(1)
xy_plot_data <-
  rbind(
    xy_plot_data_function("CA",2,2),
    xy_plot_data_function("CT",6,8),
    xy_plot_data_function("MA",5,7),
    xy_plot_data_function("RI",1,1),
    xy_plot_data_function("VT",5,1),
    xy_plot_data_function_cities(66, 1) %>% mutate(treated_state = "Seattle, WA") %>% select(year, county_id, intercept2, intercept, ban_year,attempt, tons_pc, y, y_0, effect_size, treated_state, y_0_effect) ,
    xy_plot_data_function_cities(67, 1) %>% mutate(treated_state = "Boulder, CO") %>% select(year, county_id, intercept2, intercept, ban_year,attempt, tons_pc, y, y_0, effect_size, treated_state, y_0_effect),
    xy_plot_data_function_sf(1)  %>% mutate(treated_state = "San Francisco, CA") %>% select(year, county_id, intercept2, intercept, ban_year,attempt, tons_pc, y, y_0, effect_size, treated_state, y_0_effect)
  )


# xy_plot_data <- xy_plot_data %>% filter(treated_state!="Seattle, WA")
# 
# xy_plot_data <- xy_plot_data %>%
#     rbind(
#     xy_plot_data_function_cities(66, 1) %>% mutate(treated_state = "Seattle, WA") %>% select(year, county_id, intercept2, intercept, ban_year,attempt, tons_pc, y, y_0, effect_size, treated_state, y_0_effect)
#   )
# 

# xy_plot_data <- xy_plot_data %>% filter(treated_state!="MA")
# 
# xy_plot_data <-
#   xy_plot_data %>%
#   rbind(
#     xy_plot_data_function("MA",5,19)
#   )

xy_plot_fun(1)

xy_plot <- xy_plot_fun(1)
xy_plot <-
  xy_plot+
  labs(x="", y = "", title = "Disposal (tons per capita)") + 
  theme(
    strip.text = element_blank(),
    plot.title = element_text(family = "Helvetica", color = ut_colors[4],size = 10, hjust=0.5))

xy_plot
xy_plot_sf <- xy_plot_fun_sf(1)

# xy_plot <-
#   ggpubr::ggarrange(
#   xy_plot+
#     labs(x="", y = "", title = "Annual waste (tons per capita)") + 
#     theme(
#       strip.text = element_blank(), 
#       plot.title = element_text(hjust = 0.5, size = 10, family= "Helvetica")),
#   xy_plot_sf+theme(strip.text = element_blank()),
#   heights= c(9.5, 1), nrow=2)
# xy_plot

# 
# ggsave(xy_plot, filename = "xy_plot.pdf", device = cairo_pdf,
#        path= figure_path,
#        width = 7, height = 7, units = "in")


### Science Plot

disposal_spec <- rbind(
  read.csv("power_county.csv"), 
  read.csv("power_state.csv"), 
  read.csv("sf_power.csv"))%>% 
  filter((treated_state=="VT" & sample_size < 9)|treated_state!="VT") %>% 
  filter((treated_state=="All" & specification == "State Pooled")| treated_state!="All")


disposal_effect <-read.csv("disposal_effect_size.csv")  %>% rename(effect = effect_size)

bt_with_power_data <- 
  disposal_spec %>% 
  rename(
    power_low = att_min, 
    power_high = att_max
  ) %>% select(-att_median)%>%  mutate (power_low = 100*power_low, power_high=100*power_high) %>% 
  group_by(specification, treated_state, ban_year) %>% filter(power_low == max(power_low)) %>%
  ungroup %>% 
  filter(specification == "State") %>%
  rbind(
      disposal_spec %>%  mutate(type = "disposal") %>%  
      filter(treated_state %in% c("M1", "CA", "M3"), specification == "County", sample_size!=0) %>% 
      rename(
        power_low = att_min, 
        power_high = att_max
      ) %>% select(-att_median)%>% select(-type) %>%  mutate (power_low = 100*power_low, power_high=100*power_high) %>% 
      group_by(specification, treated_state, ban_year) %>% filter(power_low == max(power_low)) %>%
      ungroup %>% 
        mutate(
          treated_state = ifelse(treated_state=="CA","boulderM2", treated_state),
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
    # All
    disposal_spec %>% 
      rename(
        power_low = att_min, 
        power_high = att_max
      ) %>% select(-att_median)%>%  mutate (power_low = 100*power_low, power_high=100*power_high) %>% 
      group_by(specification, treated_state, ban_year) %>% filter(sample_size==3) %>% #filter(power_low == max(power_low)) %>% manually choosing 3 because it has super close power to 4 but power_high is much smaller
      ungroup %>% 
      filter(specification == "State Pooled", ban_year == 2015, treated_state=="All") %>% 
      mutate(treated_state="All") %>% 
      left_join(
        xy_plot_data %>% filter(attempt==1, treated_state %in% c("CA","CT", "VT", "MA", "RI")) %>% 
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
        group_by (state_id, effect_type) %>% 
        summarise(mean_effect = mean(effect)) %>% 
        ungroup %>%
        group_by(effect_type) %>%  
        mutate(mean_all_effect = mean(mean_effect)) %>%  
        rbind(
          tibble (
            state_id = rep("All",2), 
            effect_type = c("fiori_estimates", "lower_bound"), 
            mean_effect = NA, 
            mean_all_effect= NA)) %>%  
        mutate(mean_effect = ifelse(is.na(mean_effect), mean(mean_all_effect, na.rm=TRUE),mean_effect)) %>%  
        ungroup %>%  select(-mean_all_effect) %>% 
        filter(effect_type=="lower_bound") %>% 
        select(-effect_type),
      
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
      reg_effect = c(0.098, 0.185, 0.1752)
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
  bt_with_power_data %>% #filter(state_id!="San Francisco, CA") %>% 
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
    ))%>% 
  ggplot()+
  aes(y= state_id, x= 100*actual_treatment_effect, group = 1)+
  #geom_line(color = col)+
  #facet_wrap(vars(rows=vars(factor(state_id, levels = c("All States", "California", "Connecticut", "MA", "RI", "VT", "Seattle, WA", "Boulder, CO", "San Francisco, CA")))), ncol=1)+
  #facet_grid(
  #  rows=vars(factor(state_id, levels = c("All States", "California", "Connecticut", "Massachusetts", "Rhode Island", "Vermont", "Seattle, WA", "Boulder, CO", "San Francisco, CA"))))+
  geom_errorbar(aes(xmin = power_low, xmax = power_high, color = "Placebo"), width = 0.2, size=0.5)+
  scale_color_manual(breaks = c("Placebo"), values = c(ut_colors[5]), guide = guide_legend(order = 2, legend.spacing.x=unit(-1, "cm"), byrow=TRUE ))+
  geom_vline(xintercept = 0, lty = "dotted", color = ut_colors[5])+
  labs(y="", x = "", color = "")+
  ggnewscale::new_scale_color()+
  # 
  #geom_text(aes(x=15, y=as.numeric(state_id)+0.2, label=paste0("[",mae_low, " , ", mae_high, "]")), family = "Helvetica", size=3)+
  geom_point(aes(color = "Estimate"),size=1.5)+
  scale_color_manual(breaks = c("Estimate"), values = c(ut_colors[4]),guide = guide_legend(order = 1,  legend.spacing.x=unit(-1, "cm"), byrow=TRUE))+
  labs(y="", x = "", color = "")+
  ggnewscale::new_scale_color()+

  geom_errorbar(aes(xmin=-100*mean_effect, xmax=-100*mean_effect, color = "Expected"), linewidth=1, width=0.2)+
  scale_color_manual(breaks = c("Expected"), values = c("seagreen" ),guide = guide_legend(order = 3,  legend.spacing.x=unit(-1, "cm"), byrow=TRUE))+
  labs(y="", x = "", color = "")+
  ggnewscale::new_scale_color()+
  
  geom_errorbar(aes(xmin=-100*reg_effect, xmax=-100*reg_effect, color = "Regulators' Exp."), linewidth=1, width=0.2)+
  scale_color_manual(breaks = c("Regulators' Exp."), values = c("#bad9c6" ),guide = guide_legend(order = 4,  legend.spacing.x=unit(-1, "cm"), byrow=TRUE))+
  labs(y="", x = "", color = "")+
  
  #scale_x_continuous(breaks = c(seq(-25, 25, by = 5)), limits = c(-30, 25))+ 
  scale_x_continuous(breaks = c(seq(-35, 35, by = 10)), limits = c(-40, 40))+ 
  scale_y_discrete(position= "right", expand=c(0.02,0.15))+
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
    #axis.text.y = element_blank(), 
    axis.ticks.y = element_blank(), 
    #axis.ticks.x = element_blank(), 
    #axis.text.x = element_blank(), 
    #legend.key = element_blank(),
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
      subset(xy_plot$data, !treated_state %in% c("Boulder, CO", "Seattle, WA")), 
    bt_with_power_2 %+% subset(bt_with_power_2$data, !state_id %in% c("San Francisco, CA", "Boulder, CO", "Seattle, WA")) +
      labs(x="", title = "Average treatment effect (%)")+
      scale_x_continuous(limits=c(-20, 20), breaks = c(seq(-15, 15, by =5)))+
      theme (
        plot.title = element_text(hjust=0.6, size = 10, color=ut_colors[4])), 
    heights = c(0.5, 1))

xy_and_power_2

# ggsave(xy_and_power, filename = "xy_and_power.pdf", device = cairo_pdf,
#        path= figure_path,
#        width = 7, height = 7, units = "in")

ggsave(
  xy_and_power_2, filename = "xy_and_power_2.pdf", device = cairo_pdf,
  path= figure_path,
  width = 9, height = 9, units = "in")



# Output in txt
all_effect <- 100*bt_with_power_data %>% filter(state_id=="All States") %>% pluck("actual_treatment_effect") %>% abs
ma_effect <- 100*bt_with_power_data %>% filter(state_id=="MA") %>% pluck("actual_treatment_effect") %>% abs
ca_effect <- 100*bt_with_power_data %>% filter(state_id=="CA") %>% pluck("actual_treatment_effect")
vt_effect <- 100*bt_with_power_data %>% filter(state_id=="VT") %>% pluck("actual_treatment_effect")
ct_effect <- 100*bt_with_power_data %>% filter(state_id=="CT") %>% pluck("actual_treatment_effect")
ri_effect <- 100*bt_with_power_data %>% filter(state_id=="RI") %>% pluck("actual_treatment_effect")
bo_effect <- 100*bt_with_power_data %>% filter(state_id=="Boulder, CO") %>% pluck("actual_treatment_effect")
se_effect <- 100*bt_with_power_data %>% filter(state_id=="Seattle, WA") %>% pluck("actual_treatment_effect")
sf_effect <- 100*bt_with_power_data %>% filter(state_id=="San Francisco, CA") %>% pluck("actual_treatment_effect")



power_ma <- bt_with_power_data %>% filter(state_id=="MA") %>% pluck("power_low") %>% abs
power_vt <- bt_with_power_data %>% filter(state_id=="VT") %>% pluck("power_low") %>% abs
power_ca <- bt_with_power_data %>% filter(state_id=="CA") %>% pluck("power_low") %>% abs
power_vt_upper <- bt_with_power_data %>% filter(state_id=="VT") %>% pluck("power_high") %>% abs
power_ct <- bt_with_power_data %>% filter(state_id=="CT") %>% pluck("power_low") %>% abs
power_ri <- bt_with_power_data %>% filter(state_id=="RI") %>% pluck("power_low") %>% abs
power_ri_upper <- bt_with_power_data %>% filter(state_id=="RI") %>% pluck("power_high") %>% abs


power_all <- bt_with_power_data %>% filter(state_id=="All States") %>% pluck("power_low") %>% abs
#power_all <- (bt_with_power_data %>% filter(state_id=="All States") %>% pluck("power_high") %>% abs +all_effect)
power_all_upper <- bt_with_power_data %>% filter(state_id=="All States") %>% pluck("power_high") %>% abs
reject_all <- power_all_upper %>% round(1)+all_effect %>% round(1)


power_se <- bt_with_power_data %>% filter(state_id=="Seattle, WA") %>% pluck("power_low") %>% abs
power_bo <- bt_with_power_data %>% filter(state_id=="Boulder, CO") %>% pluck("power_low") %>% abs
power_sf <- bt_with_power_data %>% filter(state_id=="San Francisco, CA") %>% pluck("power_low") %>% abs


reject_ca <-  -100*bt_with_power_data %>% filter(state_id=="CA") %>% pluck("actual_treatment_effect")+bt_with_power_data %>% filter(state_id=="CA") %>% pluck("power_high") %>% abs
reject_vt <-  -100*bt_with_power_data %>% filter(state_id=="VT") %>% pluck("actual_treatment_effect")+bt_with_power_data %>% filter(state_id=="VT") %>% pluck("power_high") %>% abs
reject_ri <-  -100*bt_with_power_data %>% filter(state_id=="RI") %>% pluck("actual_treatment_effect")+bt_with_power_data %>% filter(state_id=="RI") %>% pluck("power_high") %>% abs
reject_ct <-  -100*bt_with_power_data %>% filter(state_id=="CT") %>% pluck("actual_treatment_effect")+bt_with_power_data %>% filter(state_id=="CT") %>% pluck("power_high") %>% abs


mae_all <- xy_plot$data %>% filter(treated_state=="All", location == "Actual") %>% select(MAE) %>% mutate(MAE = str_extract(MAE, "\\d+\\.\\d+")) %>% filter(!is.na(MAE)) %>% pluck("MAE") %>% as.numeric
mae_ca <- xy_plot$data %>% filter(treated_state=="CA", location == "Actual") %>% select(MAE) %>% mutate(MAE = str_extract(MAE, "\\d+\\.\\d+")) %>% filter(!is.na(MAE)) %>% pluck("MAE") %>% as.numeric
mae_ct <- xy_plot$data %>% filter(treated_state=="CT", location == "Actual") %>% select(MAE) %>% mutate(MAE = str_extract(MAE, "\\d+\\.\\d+")) %>% filter(!is.na(MAE)) %>% pluck("MAE") %>% as.numeric
mae_ma <- xy_plot$data %>% filter(treated_state=="MA", location == "Actual") %>% select(MAE) %>% mutate(MAE = str_extract(MAE, "\\d+\\.\\d+")) %>% filter(!is.na(MAE)) %>% pluck("MAE") %>% as.numeric
mae_ri <- xy_plot$data %>% filter(treated_state=="RI", location == "Actual") %>% select(MAE) %>% mutate(MAE = str_extract(MAE, "\\d+\\.\\d+")) %>% filter(!is.na(MAE)) %>% pluck("MAE") %>% as.numeric
mae_vt <- xy_plot$data %>% filter(treated_state=="VT", location == "Actual") %>% select(MAE) %>% mutate(MAE = str_extract(MAE, "\\d+\\.\\d+")) %>% filter(!is.na(MAE)) %>% pluck("MAE") %>% as.numeric


figure_path <- "C:/Users/fa24575/Dropbox/Apps/Overleaf/Organic Waste Bans/Figures"

fileConn<-file(paste0(figure_path, "/ma_att.txt"))
writeLines(paste0(format(round(ma_effect,1),big.mark=",",scientific=FALSE),'%'), fileConn)
close(fileConn)

fileConn<-file(paste0(figure_path, "/all_att.txt"))
writeLines(paste0(format(round(all_effect,1),big.mark=",",scientific=FALSE),'%'), fileConn)
close(fileConn)

fileConn<-file(paste0(figure_path, "/ca_att.txt"))
writeLines(paste0(format(round(ca_effect,1),big.mark=",",scientific=FALSE),'%'), fileConn)
close(fileConn)

fileConn<-file(paste0(figure_path, "/ct_att.txt"))
writeLines(paste0(format(round(ct_effect %>% abs,1),big.mark=",",scientific=FALSE),'%'), fileConn)
close(fileConn)

fileConn<-file(paste0(figure_path, "/ri_att.txt"))
writeLines(paste0(format(round(ri_effect,1),big.mark=",",scientific=FALSE),'%'), fileConn)
close(fileConn)

fileConn<-file(paste0(figure_path, "/vt_att.txt"))
writeLines(paste0(format(round(vt_effect,1),big.mark=",",scientific=FALSE),'%'), fileConn)
close(fileConn)

fileConn<-file(paste0(figure_path, "/power_ma.txt"))
writeLines(paste0(format(round(power_ma,1),big.mark=",",scientific=FALSE),'%'), fileConn)
close(fileConn)

fileConn<-file(paste0(figure_path, "/power_vt.txt"))
writeLines(paste0(format(round(power_vt,1),big.mark=",",scientific=FALSE),'%'), fileConn)
close(fileConn)

fileConn<-file(paste0(figure_path, "/power_ca.txt"))
writeLines(paste0(format(round(power_ca,1),big.mark=",",scientific=FALSE),'%'), fileConn)
close(fileConn)

fileConn<-file(paste0(figure_path, "/power_vt_upper.txt"))
writeLines(paste0(format(round(power_vt_upper,1),big.mark=",",scientific=FALSE),'%'), fileConn)
close(fileConn)

fileConn<-file(paste0(figure_path, "/power_ri_upper.txt"))
writeLines(paste0(format(round(power_ri_upper,1),big.mark=",",scientific=FALSE),'%'), fileConn)
close(fileConn)


fileConn<-file(paste0(figure_path, "/power_ct.txt"))
writeLines(paste0(format(round(power_ct,1),big.mark=",",scientific=FALSE),'%'), fileConn)
close(fileConn)

fileConn<-file(paste0(figure_path, "/power_ri.txt"))
writeLines(paste0(format(round(power_ri,1),big.mark=",",scientific=FALSE),'%'), fileConn)
close(fileConn)

fileConn<-file(paste0(figure_path, "/power_bo.txt"))
writeLines(paste0(format(round(power_bo,1),big.mark=",",scientific=FALSE),'%'), fileConn)
close(fileConn)

fileConn<-file(paste0(figure_path, "/power_sf.txt"))
writeLines(paste0(format(round(power_sf,1),big.mark=",",scientific=FALSE),'%'), fileConn)
close(fileConn)

fileConn<-file(paste0(figure_path, "/power_se.txt"))
writeLines(paste0(format(round(power_se,1),big.mark=",",scientific=FALSE),'%'), fileConn)
close(fileConn)

fileConn<-file(paste0(figure_path, "/power_all.txt"))
writeLines(paste0(format(round(power_all,1),big.mark=",",scientific=FALSE),'%'), fileConn)
close(fileConn)

fileConn<-file(paste0(figure_path, "/power_all_upper.txt"))
writeLines(paste0(format(scales::number(power_all_upper, accuracy = 0.1),big.mark=",",scientific=FALSE),'%'), fileConn)
close(fileConn)


fileConn<-file(paste0(figure_path, "/reject_all.txt"))
writeLines(paste0(format(round(reject_all,1),big.mark=",",scientific=FALSE),'%'), fileConn)
close(fileConn)
fileConn<-file(paste0(figure_path, "/reject_ca.txt"))
writeLines(paste0(format(round(reject_ca,1),big.mark=",",scientific=FALSE),'%'), fileConn)
close(fileConn)
fileConn<-file(paste0(figure_path, "/reject_ct.txt"))
writeLines(paste0(format(round(reject_ct,1),big.mark=",",scientific=FALSE),'%'), fileConn)
close(fileConn)
fileConn<-file(paste0(figure_path, "/reject_ri.txt"))
writeLines(paste0(format(round(reject_ri,1),big.mark=",",scientific=FALSE),'%'), fileConn)
close(fileConn)
fileConn<-file(paste0(figure_path, "/reject_vt.txt"))
writeLines(paste0(format(round(reject_vt,1),big.mark=",",scientific=FALSE),'%'), fileConn)
close(fileConn)



fileConn<-file(paste0(figure_path, "/mae_all.txt"))
writeLines(paste0(format(round(mae_all,2),big.mark=",",scientific=FALSE),'%'), fileConn)
close(fileConn)

fileConn<-file(paste0(figure_path, "/mae_ca.txt"))
writeLines(paste0(format(round(mae_ca,2),big.mark=",",scientific=FALSE),'%'), fileConn)
close(fileConn)

fileConn<-file(paste0(figure_path, "/mae_ct.txt"))
writeLines(paste0(format(round(mae_ct,2),big.mark=",",scientific=FALSE),'%'), fileConn)
close(fileConn)

fileConn<-file(paste0(figure_path, "/mae_ma.txt"))
writeLines(paste0(format(round(mae_ma,2),big.mark=",",scientific=FALSE),'%'), fileConn)
close(fileConn)

fileConn<-file(paste0(figure_path, "/mae_ri.txt"))
writeLines(paste0(format(round(mae_ri,2),big.mark=",",scientific=FALSE),'%'), fileConn)
close(fileConn)

fileConn<-file(paste0(figure_path, "/mae_vt.txt"))
writeLines(paste0(format(round(mae_vt,2),big.mark=",",scientific=FALSE),'%'), fileConn)
close(fileConn)


rm(mae_all, mae_ca, mae_ct, mae_ma, mae_ri, mae_vt, reject_all, reject_ca, reject_ct, reject_ri, reject_vt)
rm(power_all, power_all_upper, power_ca, power_ct, power_vt, power_ma, power_ri, power_vt, power_vt_upper, power_se, power_sf, power_bo, power_ri_upper, power_vt_upper)
#rm(ma_effect, ca_effect, vt_effect, all_effect)
##### ONLY STATE LEVEL #####


xy_and_power_2_pres <- 
  ggpubr::ggarrange(
  xy_plot %+% 
    subset(xy_plot$data %>% mutate(MAE= NA), !treated_state %in% c("Boulder, CO", "Seattle, WA") & !location %in% c("Expected", "Regulators' Exp.") )+
    scale_y_continuous(expand = expansion(mult = c(.1, .1)))+
    theme(
      panel.spacing = unit(1.0, "cm"), strip.text = element_blank()
    ), 
  bt_with_power_data %>% filter(!state_id %in% c("San Francisco, CA", "Boulder, CO", "Seattle, WA")) %>% 
    mutate(
      state_id = fct_recode(
        state_id,
        "Vermont" = "VT",
        "Rhode Island" = "RI",
        "Massachusetts" = "MA",
        "Connecticut" = "CT",
        "California" = "CA",
        "All States" = "All States"
      ))%>% 
    ggplot()+
    aes(y= state_id, x= 100*actual_treatment_effect, group = 1)+
    geom_vline(xintercept = 0, lty = "dotted", color = ut_colors[5])+
    geom_point(aes(color = "Estimate"),size=1.5)+
    scale_color_manual(breaks = c("Estimate"), values = c(ut_colors[4]),guide = guide_legend(order = 1,  legend.spacing.x=unit(-1, "cm"), byrow=TRUE))+
    labs(y="", x = "", color = "")+
    ggnewscale::new_scale_color()+      
    geom_errorbar(aes(xmin = power_low, xmax = power_high, color = "Placebo"), width = 0.2, size=0.5)+
    scale_color_manual(breaks = c("Placebo"), values = c(ut_colors[5]), guide = guide_legend(order = 2, legend.spacing.x=unit(-1, "cm"), byrow=TRUE ))+
    geom_vline(xintercept = 0, lty = "dotted", color = ut_colors[5])+
    labs(y="", x = "", color = "")+
    ggnewscale::new_scale_color()+
    
    #scale_x_continuous(breaks = c(seq(-25, 25, by = 5)), limits = c(-30, 25))+ 
    scale_x_continuous(breaks = c(seq(-15, 15, by = 5)), limits = c(-20, 20))+ 
    scale_y_discrete(position= "right", expand=c(0.02,0.15))+
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
      #axis.text.y = element_blank(), 
      axis.ticks.y = element_blank(), 
      #axis.ticks.x = element_blank(), 
      #axis.text.x = element_blank(), 
      #legend.key = element_blank(),
      legend.key = element_rect(colour = NA, fill = NA, size = 5),
      legend.spacing.x = unit(-1, "pt"),
      axis.ticks.x = element_line(size = 0.1), 
      legend.text = element_text(family = "Helvetica",size = 10, color= ut_colors[4])
    )+
    labs(x="", title = "Average treatment effect (%)")+
    theme (
      plot.title = element_text(hjust=0.6, size = 10, color=ut_colors[4]), 
      panel.spacing = unit(1.0, "cm")), 
  heights = c(0.5, 1))

ggsave(xy_and_power_2_pres, filename = "xy_and_power_2_pres.pdf", device = cairo_pdf,
       path= figure_path,
       width = 9, height = 7, units = "in")



#With expected effect, no placebo
xy_and_power_2_pres <- 
  ggpubr::ggarrange(
    xy_plot %+% 
      subset(xy_plot$data %>% mutate(MAE= NA), !treated_state %in% c("Boulder, CO", "Seattle, WA"))+
      scale_y_continuous(expand = expansion(mult = c(.1, .1)))+
      theme(
        panel.spacing = unit(1.0, "cm"), strip.text = element_blank()
      ), 
    bt_with_power_data %>% filter(!state_id %in% c("San Francisco, CA", "Boulder, CO", "Seattle, WA")) %>% 
      mutate(
        state_id = fct_recode(
          state_id,
          "Vermont" = "VT",
          "Rhode Island" = "RI",
          "Massachusetts" = "MA",
          "Connecticut" = "CT",
          "California" = "CA",
          "All States" = "All States"
        ))%>% 
      ggplot()+
      aes(y= state_id, x= 100*actual_treatment_effect, group = 1)+
      #geom_line(color = col)+
      #facet_wrap(vars(rows=vars(factor(state_id, levels = c("All States", "California", "Connecticut", "MA", "RI", "VT", "Seattle, WA", "Boulder, CO", "San Francisco, CA")))), ncol=1)+
      #facet_grid(
      #  rows=vars(factor(state_id, levels = c("All States", "California", "Connecticut", "Massachusetts", "Rhode Island", "Vermont", "Seattle, WA", "Boulder, CO", "San Francisco, CA"))))+
      geom_vline(xintercept = 0, lty = "dotted", color = ut_colors[5])+
      # 
      #geom_text(aes(x=15, y=as.numeric(state_id)+0.2, label=paste0("[",mae_low, " , ", mae_high, "]")), family = "Helvetica", size=3)+
      geom_errorbar(aes(xmin = power_low, xmax = power_high, color = "Placebo"), width = 0.2, size=0.5)+
      scale_color_manual(breaks = c("Placebo"), values = c(ut_colors[5]), guide = guide_legend(order = 2, legend.spacing.x=unit(-1, "cm"), byrow=TRUE ))+
      geom_vline(xintercept = 0, lty = "dotted", color = ut_colors[5])+
      labs(y="", x = "", color = "")+
      ggnewscale::new_scale_color()+
      
      geom_errorbar(aes(xmin=-100*mean_effect, xmax=-100*mean_effect, color = "Expected"), linewidth=1, width=0.2)+
      scale_color_manual(breaks = c("Expected"), values = c("seagreen" ),guide = guide_legend(order = 3,  legend.spacing.x=unit(-1, "cm"), byrow=TRUE))+
      labs(y="", x = "", color = "")+
      ggnewscale::new_scale_color()+
      
      # geom_errorbar(aes(xmin=-100*reg_effect, xmax=-100*reg_effect, color = "Regulators' Exp."), linewidth=1, width=0.2)+
      # scale_color_manual(breaks = c("Regulators' Exp."), values = c("#bad9c6" ),guide = guide_legend(order = 4,  legend.spacing.x=unit(-1, "cm"), byrow=TRUE))+
      # labs(y="", x = "", color = "")+
      
      #scale_x_continuous(breaks = c(seq(-25, 25, by = 5)), limits = c(-30, 25))+ 
      scale_x_continuous(breaks = c(seq(-15, 15, by = 5)), limits = c(-20, 20))+ 
      scale_y_discrete(position= "right", expand=c(0.02,0.15))+
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
        #axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(), 
        #axis.ticks.x = element_blank(), 
        #axis.text.x = element_blank(), 
        #legend.key = element_blank(),
        legend.key = element_rect(colour = NA, fill = NA, size = 5),
        legend.spacing.x = unit(-1, "pt"),
        axis.ticks.x = element_line(size = 0.1), 
        legend.text = element_text(family = "Helvetica",size = 10, color= ut_colors[4]),
        plot.title = element_text(hjust=0.6, size = 10, color=ut_colors[4]), 
        panel.spacing = unit(1.0, "cm")
      ))
    
ggsave(xy_and_power_2_pres, filename = "xy_and_power_2_pres_exp_no_est.pdf", device = cairo_pdf,
       path= figure_path,
       width = 9, height = 7, units = "in")


## With Expected Effect but not regulators'

xy_and_power_2_pres <- 
  ggpubr::ggarrange(
    xy_plot %+% 
      subset(xy_plot$data %>% mutate(MAE= NA) %>% filter(location != "Regulators' Exp."), !treated_state %in% c("Boulder, CO", "Seattle, WA"))+
      scale_y_continuous(expand = expansion(mult = c(.1, .1)))+
      theme(
        panel.spacing = unit(1.0, "cm"), strip.text = element_blank()
      ), 
    bt_with_power_data %>% filter(!state_id %in% c("San Francisco, CA", "Boulder, CO", "Seattle, WA")) %>% 
      mutate(
        state_id = fct_recode(
          state_id,
          "Vermont" = "VT",
          "Rhode Island" = "RI",
          "Massachusetts" = "MA",
          "Connecticut" = "CT",
          "California" = "CA",
          "All States" = "All States"
        ))%>% 
      ggplot()+
      aes(y= state_id, x= 100*actual_treatment_effect, group = 1)+
      #geom_line(color = col)+
      #facet_wrap(vars(rows=vars(factor(state_id, levels = c("All States", "California", "Connecticut", "MA", "RI", "VT", "Seattle, WA", "Boulder, CO", "San Francisco, CA")))), ncol=1)+
      #facet_grid(
      #  rows=vars(factor(state_id, levels = c("All States", "California", "Connecticut", "Massachusetts", "Rhode Island", "Vermont", "Seattle, WA", "Boulder, CO", "San Francisco, CA"))))+
      geom_vline(xintercept = 0, lty = "dotted", color = ut_colors[5])+
      # 
      #geom_text(aes(x=15, y=as.numeric(state_id)+0.2, label=paste0("[",mae_low, " , ", mae_high, "]")), family = "Helvetica", size=3)+
      geom_point(aes(color = "Estimate"),size=1.5)+
      scale_color_manual(breaks = c("Estimate"), values = c(ut_colors[4]),guide = guide_legend(order = 1,  legend.spacing.x=unit(-1, "cm"), byrow=TRUE))+
      labs(y="", x = "", color = "")+
      ggnewscale::new_scale_color()+
      
      geom_errorbar(aes(xmin = power_low, xmax = power_high, color = "Placebo"), width = 0.2, size=0.5)+
      scale_color_manual(breaks = c("Placebo"), values = c(ut_colors[5]), guide = guide_legend(order = 2, legend.spacing.x=unit(-1, "cm"), byrow=TRUE ))+
      geom_vline(xintercept = 0, lty = "dotted", color = ut_colors[5])+
      labs(y="", x = "", color = "")+
      ggnewscale::new_scale_color()+
      
      geom_errorbar(aes(xmin=-100*mean_effect, xmax=-100*mean_effect, color = "Expected"), linewidth=1, width=0.2)+
      scale_color_manual(breaks = c("Expected"), values = c("seagreen" ),guide = guide_legend(order = 3,  legend.spacing.x=unit(-1, "cm"), byrow=TRUE))+
      labs(y="", x = "", color = "")+
      ggnewscale::new_scale_color()+
      
      # geom_errorbar(aes(xmin=-100*reg_effect, xmax=-100*reg_effect, color = "Regulators' Exp."), linewidth=1, width=0.2)+
      # scale_color_manual(breaks = c("Regulators' Exp."), values = c("#bad9c6" ),guide = guide_legend(order = 4,  legend.spacing.x=unit(-1, "cm"), byrow=TRUE))+
      # labs(y="", x = "", color = "")+
      
      #scale_x_continuous(breaks = c(seq(-25, 25, by = 5)), limits = c(-30, 25))+ 
      scale_x_continuous(breaks = c(seq(-15, 15, by = 5)), limits = c(-20, 20))+ 
      scale_y_discrete(position= "right", expand=c(0.02,0.15))+
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
        #axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(), 
        #axis.ticks.x = element_blank(), 
        #axis.text.x = element_blank(), 
        #legend.key = element_blank(),
        legend.key = element_rect(colour = NA, fill = NA, size = 5),
        legend.spacing.x = unit(-1, "pt"),
        axis.ticks.x = element_line(size = 0.1), 
        legend.text = element_text(family = "Helvetica",size = 10, color= ut_colors[4]),
        plot.title = element_text(hjust=0.6, size = 10, color=ut_colors[4]), 
        panel.spacing = unit(1.0, "cm")
      ))

ggsave(xy_and_power_2_pres, filename = "xy_and_power_2_pres_exp_no_reg.pdf", device = cairo_pdf,
       path= figure_path,
       width = 9, height = 7, units = "in")

## With Expected Effect


xy_and_power_2_pres <- 
  ggpubr::ggarrange(
    xy_plot %+% 
      subset(xy_plot$data, !treated_state %in% c("Boulder, CO", "Seattle, WA"))+
      scale_y_continuous(expand = expansion(mult = c(.1, .1)))+
      theme(
        panel.spacing = unit(1.0, "cm")
      ), 
    bt_with_power_2 %+% subset(bt_with_power_2$data, !state_id %in% c("Boulder, CO", "Seattle, WA", "San Francisco, CA")) +
      labs(x="", title = "Average treatment effect (%)")+
      scale_x_continuous(breaks = c(seq(-15, 15, by = 5)), limits = c(-20, 20))+ 
      
      theme (
        plot.title = element_text(hjust=0.6, size = 10, color=ut_colors[4]), 
        panel.spacing = unit(1.0, "cm")), 
    heights = c(0.5, 1))

ggsave(xy_and_power_2_pres, filename = "xy_and_power_2_pres_exp.pdf", device = cairo_pdf,
       path= figure_path,
       width = 9, height = 7, units = "in")



#NO MAE, NO ACTUAL EFFECT
xy_and_power_2_pres <- 
  ggpubr::ggarrange(
    xy_plot %+% 
      subset(xy_plot$data %>% mutate(MAE=NA, label=NA, ban_year=0), !treated_state %in% c("Boulder, CO", "Seattle, WA") & !location %in% c("Expected", "Regulators' Exp.", "Synthetic") )+
      scale_y_continuous(expand = expansion(mult = c(.1, .1)))+
      theme(
        panel.spacing = unit(1.0, "cm"), strip.text = element_blank()
      ), 
    bt_with_power_data %>% filter(!state_id %in% c("San Francisco, CA", "Boulder, CO", "Seattle, WA")) %>% 
      mutate(
        state_id = fct_recode(
          state_id,
          "Vermont" = "VT",
          "Rhode Island" = "RI",
          "Massachusetts" = "MA",
          "Connecticut" = "CT",
          "California" = "CA",
          "All States" = "All States"
        ))%>% 
      ggplot()+
      aes(y= state_id, x= 100*actual_treatment_effect, group = 1)+
      #geom_line(color = col)+
      #facet_wrap(vars(rows=vars(factor(state_id, levels = c("All States", "California", "Connecticut", "MA", "RI", "VT", "Seattle, WA", "Boulder, CO", "San Francisco, CA")))), ncol=1)+
      #facet_grid(
      #  rows=vars(factor(state_id, levels = c("All States", "California", "Connecticut", "Massachusetts", "Rhode Island", "Vermont", "Seattle, WA", "Boulder, CO", "San Francisco, CA"))))+
      geom_errorbar(aes(xmin = power_low, xmax = power_high, color = "Placebo"), width = 0.2, size=0.5)+
      scale_color_manual(breaks = c("Placebo"), values = c(ut_colors[5]), guide = guide_legend(order = 2, legend.spacing.x=unit(-1, "cm"), byrow=TRUE ))+
      geom_vline(xintercept = 0, lty = "dotted", color = ut_colors[5])+
      labs(y="", x = "", color = "")+
      ggnewscale::new_scale_color()+
      #scale_x_continuous(breaks = c(seq(-25, 25, by = 5)), limits = c(-30, 25))+ 
      scale_x_continuous(breaks = c(seq(-15, 15, by = 5)), limits = c(-20, 20))+ 
      scale_y_discrete(position= "right", expand=c(0.02,0.15))+
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
        #axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(), 
        #axis.ticks.x = element_blank(), 
        #axis.text.x = element_blank(), 
        #legend.key = element_blank(),
        legend.key = element_rect(colour = NA, fill = NA, size = 5),
        legend.spacing.x = unit(-1, "pt"),
        axis.ticks.x = element_line(size = 0.1), 
        legend.text = element_text(family = "Helvetica",size = 10, color= ut_colors[4])
      )+
      labs(x="", title = "Average treatment effect (%)")+
      theme (
        plot.title = element_text(hjust=0.6, size = 10, color=ut_colors[4]), 
        panel.spacing = unit(1.0, "cm")), 
    heights = c(0.5, 1))

ggsave(xy_and_power_2_pres, filename = "xy_and_power_2_pres_0.pdf", device = cairo_pdf,
       path= figure_path,
       width = 9, height = 7, units = "in")


# Cities 
xy_and_power_2_cities <- 
  ggpubr::ggarrange(
    ggpubr::ggarrange(
      xy_plot %+% 
        subset(xy_plot$data, treated_state %in% c("Boulder, CO", "Seattle, WA"))+
        scale_y_continuous(expand = expansion(mult = c(.1, .1)))+
        theme(
          panel.spacing = unit(1.0, "cm")
        ), 
      xy_plot_sf, 
      nrow=2, 
      heights = c(2, 1)), 
    bt_with_power_2 %+% subset(bt_with_power_2$data, state_id %in% c("Boulder, CO", "Seattle, WA", "San Francisco, CA")) +
      labs(x="", title = "Average treatment effect (%)")+
      theme (
        plot.title = element_text(hjust=0.6, size = 10, color=ut_colors[4]), 
        panel.spacing = unit(1.0, "cm")), 
    heights = c(0.5, 1))


ggsave(xy_and_power_2_cities, filename = "xy_and_power_2_cities.pdf", device = cairo_pdf,
       path= figure_path,
       width = 11, height = 5, units = "in")



# Only SF


xy_and_power_2_sf <- 
  ggpubr::ggarrange(
      xy_plot_sf+
        labs(title="Disposal (tons per capita)")+
        theme(
          legend.position = "top", 
          plot.title = element_text(hjust=0.6, size = 10, color=ut_colors[4]) 
        ), 
      bt_with_power_2 %+% subset(bt_with_power_2$data, state_id %in% c("San Francisco, CA")) +
      labs(x="", title = "Average treatment effect (%)")+
      theme (
        plot.title = element_text(hjust=0.6, size = 10, color=ut_colors[4]), 
        panel.spacing = unit(1.0, "cm")), 
    heights = c(0.5, 1))


ggsave(xy_and_power_2_sf, filename = "xy_and_power_2_sf.pdf", device = cairo_pdf,
       path= figure_path,
       width = 11, height = 2.5, units = "in")



############################ FOR 5 MIN PRESENTATION #####################################



xy_plot_fun_pres <- function (i)
{
  
  reg_expect <- 
    tibble(
      state_id=c("MA", "CA", "VT"), 
      reg_effect = c(0.098, 0.185, 0.1752)
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
      disposal_effect_size %>% 
        filter(effect_type== "lower_bound") %>%  
        select(year, state_id, effect_size) %>% 
        group_by(year) %>% 
        summarise(effect_size=mean(effect_size, na.rm=TRUE)), 
      by = "year"
    ) %>% 
    mutate(
      y_0_effect = y_0*(1-effect_size),
      ban_year = 2016, 
      treated_state= "All",
      y_0_effect = ifelse(year< ban_year, NA, y_0_effect)
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
      Expected = y_0_effect
    ) %>% 
    left_join(reg_expect, by = c("county_id"="state_id")) %>% 
    mutate(
      `Regulators' Exp.` = ifelse(!is.na(effect_size), Synthetic*(1-reg_effect), NA)
    ) %>% 
    pivot_longer(
      cols = c("Synthetic", "Actual", "Expected", `Regulators' Exp.`), 
      names_to = "location", 
      values_to = "tons_pc") %>% select(year, attempt, location, tons_pc, treated_state, ban_year) %>% 
    rbind(
      all%>% 
        rename(
          Synthetic = y_0, 
          Actual = tons_pc, 
          Expected = y_0_effect
        ) %>% 
        pivot_longer(
          cols = c("Synthetic", "Actual", "Expected"), 
          names_to = "location", 
          values_to = "tons_pc") %>% select(year, attempt, location, tons_pc, treated_state, ban_year)
    ) %>% 
    left_join(
      rbind(xy_plot_data%>% select(year, treated_state, ban_year, attempt, tons_pc, y, y_0, y_0_effect), all) %>% 
        group_by(treated_state) %>% 
        filter(year >= ban_year-3, year < ban_year) %>%
        mutate(
          year=mean(year), # i duplicate so i join and then also keep the xlab_mae
          xlab_mae=mean(year)+0.5,  
          xlab_mae=ifelse(treated_state%in%c("Boulder, CO"), xlab_mae+0.3, xlab_mae),
          ylab_mae=ifelse(treated_state%in%c("All", "CA", "CT"), mean(tons_pc)-0.05, mean(tons_pc)-0.06), 
          ylab_mae=ifelse(treated_state%in%c("Seattle, WA"), ylab_mae-0.01, ylab_mae), 
          ylab_mae=ifelse(treated_state%in%c("CT"), ylab_mae+0.01, ylab_mae), 
          ylab_mae=ifelse(treated_state%in%c("CA"), ylab_mae+0.01, ylab_mae), 
          xlab_mae=ifelse(treated_state%in%c("All"), xlab_mae, xlab_mae), 
          xlab_mae=ifelse(treated_state%in%c("CA"), xlab_mae+0.2, xlab_mae)) %>% 
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
      tibble (xlab= c(2009, 2012, 2016), ylab = 0.85, label = c("Train", "Validation", "Evaluation"), treated_state = "All"), 
      by =c ("treated_state", "year"="xlab")
    ) %>% 
    mutate(
      xlab = ifelse(!is.na(ylab), year, NA),
      xlab = ifelse(!is.na(ylab) & year==2012, 2012.5, xlab), 
      MAE = ifelse(treated_state=="All", paste0("MAPE (%): ", MAE), MAE), 
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
    geom_vline(aes(xintercept = ban_year), size=1, linetype="dotted", color = ut_colors[5])+
    geom_line(size=2)+
    facet_grid(
      rows=vars(factor(treated_state, levels = c("All", "CA", "CT", "MA", "RI", "VT", "Seattle, WA", "Boulder, CO", "San Francisco, CA"))),
      scales="free_y"
    )+
    #geom_text(aes(x=xlab_mae,y=ylab_mae, label = MAE), color ="#857d95", size=4, family="Helvetica")+
    scale_color_manual(breaks= c("Actual", "Synthetic", "Expected", "Regulators' Exp."), values = c(ut_colors[4],ut_colors[5],"seagreen", "#bad9c6"), name = "")+
    scale_linetype_manual(breaks= c("Actual", "Synthetic", "Expected", "Regulators' Exp."), values = c("solid", "solid", "solid", "solid"), name = "")+
    scale_size_manual(breaks= c("Actual", "Synthetic", "Expected", "Regulators' Exp."), values = c(0.5, 0.5, 1.0, 1.0), name = "")+
    scale_x_continuous(breaks=c(seq(2006, 2018, 2)), limits=c(2005, 2019), expand = c(0,0))+
    scale_y_continuous(breaks=c(seq(0.5, 1.2, 0.2)), limits = c(0.45, 1.25))+
    labs(y="", x= "")+
    theme_classic()+
    theme(
      legend.position = "top",
      strip.background = element_rect(color = "white", fill = "white"),
      panel.grid.major.x = element_blank() ,
      panel.grid.major.y = element_blank(), 
      text = element_text(family = "Helvetica",size = 15, color= ut_colors[4]), 
      axis.line.x = element_blank(),
      #axis.ticks.y = element_blank(), 
      #axis.text.y= element_blank(),
      axis.line.y = element_blank(),
      axis.ticks.x = element_line(size = 0.1), 
      legend.text = element_text(family = "Helvetica", color = ut_colors[4],size = 15),
      panel.spacing = unit(1.1, "cm")
    )
  
  
  return(p)
  
}




p <- xy_plot_fun_pres(1) 

p <- 
  p+
  labs(x="", y = "", title = "Disposal (tons per capita)") + 
  theme(
    strip.text = element_blank(),
    plot.title = element_text(family = "Helvetica", color = ut_colors[4],size = 15, hjust=0.5))


xy_pres_5min <-
  p%+% 
  subset(p$data %>% mutate(MAE=NA,tons_pc = ifelse(location %in% c("Synthetic", "Actual")& year>ban_year, NA, tons_pc)), treated_state %in% c("CA") & !location %in% c("Synthetic","Expected", "Regulators' Exp.") )+
  scale_y_continuous(breaks = c(seq(0.6, 1.2, by = 0.1)), limits=c(0.58, 1.22))+
  theme(
    panel.spacing = unit(1.0, "cm"), strip.text = element_blank()
    )

ggsave(xy_pres_5min, filename = "xy_pres_5min_1.pdf", device = cairo_pdf,
       path= figure_path,
       width = 15, height = 6, units = "in")

xy_pres_5min <-
  p%+% 
  subset(p$data %>% mutate(tons_pc = ifelse(location %in% c("Synthetic", "Actual")& year>ban_year, NA, tons_pc)), treated_state %in% c("CA")& !location %in% c("Expected", "Regulators' Exp.") )+
  scale_y_continuous(breaks = c(seq(0.6, 1.2, by = 0.1)), limits=c(0.58, 1.22))+
  theme(
    panel.spacing = unit(1.0, "cm"), strip.text = element_blank()
  )

ggsave(xy_pres_5min, filename = "xy_pres_5min_2.pdf", device = cairo_pdf,
       path= figure_path,
       width = 15, height = 6, units = "in")


xy_pres_5min <-
  p%+% 
  subset(p$data %>% mutate(tons_pc = ifelse(location %in% c("Synthetic", "Actual")& year>ban_year, NA, tons_pc)), treated_state %in% c("CA") & !location %in% c("Regulators' Exp.") )+
  scale_y_continuous(breaks = c(seq(0.6, 1.2, by = 0.1)), limits=c(0.58, 1.22))+
  theme(
    panel.spacing = unit(1.0, "cm"), strip.text = element_blank()
  )

ggsave(xy_pres_5min, filename = "xy_pres_5min_3.pdf", device = cairo_pdf,
       path= figure_path,
       width = 15, height = 6, units = "in")


xy_pres_5min <-
  p%+% 
  subset(p$data %>% mutate(tons_pc = ifelse(location %in% c("Synthetic")& year>ban_year, NA, tons_pc)), treated_state %in% c("CA") & !location %in% c("Regulators' Exp.") )+
  scale_y_continuous(breaks = c(seq(0.6, 1.2, by = 0.1)), limits=c(0.58, 1.22))+
  theme(
    panel.spacing = unit(1.0, "cm"), strip.text = element_blank()
  )

ggsave(xy_pres_5min, filename = "xy_pres_5min_4.pdf", device = cairo_pdf,
       path= figure_path,
       width = 15, height = 6, units = "in")

xy_pres_5min <-
  p%+% 
  subset(p$data, treated_state %in% c("CA") & !location %in% c("Regulators' Exp.") )+
  scale_y_continuous(breaks = c(seq(0.6, 1.2, by = 0.1)), limits=c(0.58, 1.22))+
  theme(
    panel.spacing = unit(1.0, "cm"), strip.text = element_blank()
  )

ggsave(xy_pres_5min, filename = "xy_pres_5min_5.pdf", device = cairo_pdf,
       path= figure_path,
       width = 15, height = 6, units = "in")



xy_pres_5min_CT <- 
  p%+% 
  subset(p$data, treated_state %in% c("CT") & !location %in% c("Regulators' Exp.") )+
  scale_y_continuous(expand = expansion(mult = c(.1, .1)))+
  theme(
    panel.spacing = unit(1.0, "cm"), strip.text = element_blank()
  )

ggsave(xy_pres_5min_CT, filename = "xy_pres_5min_CT.pdf", device = cairo_pdf,
       path= figure_path,
       width = 15, height = 6, units = "in")


xy_pres_5min_RI <- 
  p%+% 
  subset(p$data, treated_state %in% c("RI") & !location %in% c("Regulators' Exp.") )+
  scale_y_continuous(expand = expansion(mult = c(.1, .1)))+
  theme(
    panel.spacing = unit(1.0, "cm"), strip.text = element_blank()
  )

ggsave(xy_pres_5min_RI, filename = "xy_pres_5min_RI.pdf", device = cairo_pdf,
       path= figure_path,
       width = 15, height = 6, units = "in")


xy_pres_5min_VT <- 
  p%+% 
  subset(p$data, treated_state %in% c("VT") & !location %in% c("Regulators' Exp.") )+
  scale_y_continuous(expand = expansion(mult = c(.1, .1)))+
  theme(
    panel.spacing = unit(1.0, "cm"), strip.text = element_blank()
  )

ggsave(xy_pres_5min_VT, filename = "xy_pres_5min_VT.pdf", device = cairo_pdf,
       path= figure_path,
       width = 15, height = 6, units = "in")


xy_pres_5min_MA <- 
  p%+% 
  subset(p$data, treated_state %in% c("MA") & !location %in% c("Regulators' Exp.") )+
  scale_y_continuous(expand = expansion(mult = c(.1, .1)))+
  theme(
    panel.spacing = unit(1.0, "cm"), strip.text = element_blank()
  )

ggsave(xy_pres_5min_MA, filename = "xy_pres_5min_MA.pdf", device = cairo_pdf,
       path= figure_path,
       width = 15, height = 6, units = "in")

xy_pres_5min_All <- 
  p%+% 
  subset(p$data, treated_state %in% c("All") & !location %in% c("Regulators' Exp.") )+
  scale_y_continuous(expand = expansion(mult = c(.1, .1)))+
  theme(
    panel.spacing = unit(1.0, "cm"), strip.text = element_blank()
  )

ggsave(xy_pres_5min_All, filename = "xy_pres_5min_All_States.pdf", device = cairo_pdf,
       path= figure_path,
       width = 15, height = 6, units = "in")



xy_and_power_2_pres <- 
  ggpubr::ggarrange(
    xy_plot %+% 
      subset(xy_plot$data %>% mutate(MAE= NA), !treated_state %in% c("Boulder, CO", "Seattle, WA") & !location %in% c("Regulators' Exp.") )+
      scale_y_continuous(expand = expansion(mult = c(.1, .1)))+
      theme(
        panel.spacing = unit(1.0, "cm"), strip.text = element_blank()
      ), 
    bt_with_power_data %>% filter(!state_id %in% c("San Francisco, CA", "Boulder, CO", "Seattle, WA")) %>% 
      mutate(
        state_id = fct_recode(
          state_id,
          "Vermont" = "VT",
          "Rhode Island" = "RI",
          "Massachusetts" = "MA",
          "Connecticut" = "CT",
          "California" = "CA",
          "All States" = "All States"
        ))%>% 
      ggplot()+
      aes(y= state_id, x= 100*actual_treatment_effect, group = 1)+
      #geom_line(color = col)+
      #facet_wrap(vars(rows=vars(factor(state_id, levels = c("All States", "California", "Connecticut", "MA", "RI", "VT", "Seattle, WA", "Boulder, CO", "San Francisco, CA")))), ncol=1)+
      #facet_grid(
      #  rows=vars(factor(state_id, levels = c("All States", "California", "Connecticut", "Massachusetts", "Rhode Island", "Vermont", "Seattle, WA", "Boulder, CO", "San Francisco, CA"))))+
      geom_vline(xintercept = 0, lty = "dotted", color = ut_colors[5])+
      # 
      #geom_text(aes(x=15, y=as.numeric(state_id)+0.2, label=paste0("[",mae_low, " , ", mae_high, "]")), family = "Helvetica", size=3)+
      geom_errorbar(aes(xmin = power_low, xmax = power_high, color = "Placebo"), width = 0.2, size=0.5)+
      scale_color_manual(breaks = c("Placebo"), values = c(ut_colors[5]), guide = guide_legend(order = 2, legend.spacing.x=unit(-1, "cm"), byrow=TRUE ))+
      geom_vline(xintercept = 0, lty = "dotted", color = ut_colors[5])+
      labs(y="", x = "", color = "")+
      ggnewscale::new_scale_color()+
      
      geom_point(aes(color = "Estimate"),size=1.5)+
      scale_color_manual(breaks = c("Estimate"), values = c(ut_colors[4]),guide = guide_legend(order = 1,  legend.spacing.x=unit(-1, "cm"), byrow=TRUE))+
      labs(y="", x = "", color = "")+
      ggnewscale::new_scale_color()+
      
      geom_errorbar(aes(xmin=-100*mean_effect, xmax=-100*mean_effect, color = "Expected"), linewidth=1, width=0.2)+
      scale_color_manual(breaks = c("Expected"), values = c("seagreen" ),guide = guide_legend(order = 3,  legend.spacing.x=unit(-1, "cm"), byrow=TRUE))+
      labs(y="", x = "", color = "")+
      
      #scale_x_continuous(breaks = c(seq(-25, 25, by = 5)), limits = c(-30, 25))+ 
      scale_x_continuous(breaks = c(seq(-35, 35, by = 10)), limits = c(-40, 40))+ 
      scale_y_discrete(position= "right", expand=c(0.02,0.15))+
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
        #axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(), 
        #axis.ticks.x = element_blank(), 
        #axis.text.x = element_blank(), 
        #legend.key = element_blank(),
        legend.key = element_rect(colour = NA, fill = NA, size = 5),
        legend.spacing.x = unit(-1, "pt"),
        axis.ticks.x = element_line(size = 0.1), 
        legend.text = element_text(family = "Helvetica",size = 10, color= ut_colors[4]),
        plot.title = element_text(hjust=0.6, size = 10, color=ut_colors[4]), 
        panel.spacing = unit(1.0, "cm")
      ), 
    heights = c(0.5, 1))

ggsave(xy_and_power_2_pres, filename = "xy_pres_5min_all.pdf", device = cairo_pdf,
       path= figure_path,
       width = 9, height = 7, units = "in")


######## Synthetic Control Example ####################


syn_example <- 
  dt_state_initial %>% filter(state_id %in% c("CT", "WA", "TX", "SC", "MN", "OH")) %>% 
  select(year, state_id, tons_pc) %>% 
  rbind(
    dt_state_initial %>% filter(state_id %in% c("WA", "TX", "SC", "MN", "OH")) %>% 
      group_by(year) %>% 
      summarise(
        tons_pc = mean(tons_pc)
      ) %>% 
      mutate(
        state_id= "Synthetic"
      )
  ) %>% 
  mutate(
    state_id = factor(state_id, levels = c("CT", "WA", "TX", "SC", "MN", "OH", "Synthetic")),
    
    label_x = ifelse(year== max(year), year+1, NA),
    label_y = ifelse(year== max(year), tons_pc, NA), 
    label_y = ifelse(state_id=="SC", label_y-0.01, label_y), 
    label_y = ifelse(state_id=="Synthetic", label_y+0.01, label_y)
    
  ) %>% 
  ggplot(aes(x=year, y=tons_pc, color = state_id, linetype = state_id, size=state_id))+
  geom_line() +  geom_text(aes(x=label_x, y = label_y, label = state_id), size=5) +
  scale_color_manual(values = c(ut_colors[4], rep(ut_colors[5],6))) +
  scale_linetype_manual(values= c("solid", rep("dotted", 5), "solid"))+ 
  scale_size_manual (values = c(1.5, rep(1, 5), 1.5))+
  scale_x_continuous(breaks= c(seq(2006, 2018, by =2)))+
  geom_vline(aes(xintercept = ban_year), linetype="dotted", color = ut_colors[5])+
  labs(x="Year", y = "Disposal (tons per capita)")+
  theme(
    legend.position = "none",  # Keep the legend at the top
    legend.box.margin = margin(t = 0, r = 0, b = 0, l = 70),
    strip.background = element_rect(color = "white", fill = "white"),
    panel.grid.major.x = element_blank() ,
    panel.grid.major.y = element_blank(),
    panel.background = element_blank(), 
    text = element_text(family = "Helvetica",size = 14, color= ut_colors[4]), 
    strip.text = element_text(angle = 0, hjust = 1), 
    strip.placement = "outside",
    axis.ticks.y = element_blank(), 
    legend.key = element_rect(colour = NA, fill = NA, size = 5),
    legend.spacing.x = unit(-1, "pt"),
    axis.ticks.x = element_line(size = 0.1), 
    legend.text = element_text(family = "Helvetica",size = 14, color= ut_colors[4]), 
    axis.title.x = element_text(margin = margin(t = 20)),
    axis.title.y = element_text(margin = margin(r = 20))
    )

syn_example1 <- 
  syn_example%+% 
  subset(syn_example$data, !state_id %in% c("Synthetic"))+
  scale_linetype_manual(values= c("solid", rep("solid", 5))) 

ggsave(syn_example1, filename = "syn_example1.pdf", device = cairo_pdf,
       path= figure_path,
       width = 15, height = 6, units = "in") 

ggsave(syn_example, filename = "syn_example2.pdf", device = cairo_pdf,
       path= figure_path,
       width = 15, height = 6, units = "in") 

syn_example3 <- 
  syn_example%+% 
  subset(
    syn_example$data%>% 
      mutate(
        tons_pc = ifelse(state_id=="Synthetic", tons_pc - 0.0749, tons_pc), 
        label_x = ifelse(state_id %in% c("CT", "Synthetic") & year== max(year), year+1, NA),
        label_y = ifelse(state_id %in% c("CT", "Synthetic") &year== max(year), tons_pc, NA), 
        label_y = ifelse(state_id=="Synthetic", tons_pc-0.01, label_y), 
        label_y = ifelse(state_id=="CT", tons_pc+0.01, label_y) 
  ))

ggsave(syn_example3, filename = "syn_example3.pdf", device = cairo_pdf,
       path= figure_path,
       width = 15, height = 6, units = "in")


# MAPE during evaluation period

post_mape <- 
  xy_plot$data %>% 
  filter(location %in% c("Synthetic", "Actual"), year >=ban_year) %>% 
  select(year, location, tons_pc, treated_state) %>% 
  pivot_wider(names_from = location, values_from = tons_pc) %>% 
  group_by(treated_state) %>%
  summarise(
    MAPE = mean(abs((Actual-Synthetic)/Actual)), 
    sum(Actual- Synthetic)/sum(Synthetic)) %>% 
  filter(
    treated_state %in% c("CA", "CT", "RI", "VT")
  ) %>% filter(MAPE == max(MAPE)) %>% pluck("MAPE")


# 
# fileConn<-file(paste0(figure_path, "/post_mape.txt"))
# writeLines(paste0(format(scales::number(100*post_mape, accuracy = 0.1),big.mark=",",scientific=FALSE),'%'), fileConn)
# close(fileConn)

#rm(post_mape)
rm(xy_and_power_2_cities, xy_pres_5min, xy_pres_5min_All, xy_pres_5min_CT, xy_pres_5min_MA, xy_pres_5min_RI, xy_pres_5min_VT)




################## ONLY PLACEBO RESULTS ##################



plac_plot <- 
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
    ))%>% 
  filter(!state_id %in% c("San Francisco, CA", "Boulder, CO", "Seattle, WA")) %>% 
  ggplot()+
  aes(y= state_id, x= 100*actual_treatment_effect, group = 1)+
  geom_errorbar(aes(xmin = power_low, xmax = power_high, color = "Placebo"), width = 0.2, size=0.5)+
  scale_color_manual(breaks = c("Placebo"), values = c(ut_colors[5]), guide = guide_legend(order = 2, legend.spacing.x=unit(-1, "cm"), byrow=TRUE ))+
  geom_vline(xintercept = 0, lty = "dotted", color = ut_colors[5])+
  labs(y="", x = "", color = "")+
  # ggnewscale::new_scale_color()+
  # geom_point(aes(color = "Estimate"),size=1.5)+
  # scale_color_manual(breaks = c("Estimate"), values = c(ut_colors[4]),guide = guide_legend(order = 1,  legend.spacing.x=unit(-1, "cm"), byrow=TRUE))+
  # labs(y="", x = "", color = "")+
  # ggnewscale::new_scale_color()+
  # 
  # geom_errorbar(aes(xmin=-100*mean_effect, xmax=-100*mean_effect, color = "Expected"), linewidth=1, width=0.2)+
  # scale_color_manual(breaks = c("Expected"), values = c("seagreen" ),guide = guide_legend(order = 3,  legend.spacing.x=unit(-1, "cm"), byrow=TRUE))+
  # labs(y="", x = "", color = "")+
  # ggnewscale::new_scale_color()+

  scale_x_continuous(breaks = c(seq(-15, 15, by = 5)), limits = c(-20, 20))+ 
  scale_y_discrete(position= "right", expand=c(0.02,0.15))+
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
    legend.text = element_text(family = "Helvetica",size = 10, color= ut_colors[4])
  )

ggsave(plac_plot, filename = "plac_plot_plac.pdf", device = cairo_pdf,
       path= figure_path,
       width = 9, height = 7, units = "in")


plac_plot <- 
  plac_plot+
  ggnewscale::new_scale_color()+
  geom_errorbar(aes(xmin=-100*mean_effect, xmax=-100*mean_effect, color = "Expected"), linewidth=1, width=0.2)+
  scale_color_manual(breaks = c("Expected"), values = c("seagreen" ),guide = guide_legend(order = 3,  legend.spacing.x=unit(-1, "cm"), byrow=TRUE))+
  labs(y="", x = "", color = "")



  ggsave(plac_plot, filename = "plac_plot_exp.pdf", device = cairo_pdf,
         path= figure_path,
         width = 9, height = 7, units = "in")

plac_plot <- 
  plac_plot+
  ggnewscale::new_scale_color()+
  geom_point(aes(color = "Estimate"),size=1.5)+
  scale_color_manual(breaks = c("Estimate"), values = c(ut_colors[4]),guide = guide_legend(order = 1,  legend.spacing.x=unit(-1, "cm"), byrow=TRUE))+
  labs(y="", x = "", color = "")
  
  
  ggsave(plac_plot, filename = "plac_plot_all.pdf", device = cairo_pdf,
         path= figure_path,
         width = 9, height = 7, units = "in")
