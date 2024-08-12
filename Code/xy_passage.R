disposal_effect_size <- read.csv("disposal_effect_size2.csv") %>% as_tibble() %>% rename(effect = effect_size)
year_start = 2006
year_cutoff = 2018
pooled_ban_year = 2015
offset = 3
iterations = 100000
dt_state_initial <- pre_processing_dt_state(power2)
disposal_effect_size2 <- read.csv("disposal_effect_size2.csv") %>% as_tibble() #needed to caclulate the expected effects


#dt_state_initial<- dt_state_initial %>% mutate(tons_pc = ifelse(state_id=="CT", tons_pc+0.15, tons_pc))
treated_counties_id <- unique(dt_state_initial$county_id[dt_state_initial$state_id%in% all_treated])
donors_state <- unique(dt_state_initial$county_id[!(dt_state_initial$state_id%in% all_treated)])
donors <- donors_state

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
      disposal_effect_size2 %>%  select(year, state_id, effect_size), 
      by = c("year", "county_id" = 'state_id')
    ) %>% 
    mutate(
      treated_state = treated_state, 
      #y_0_effect = ifelse(year>=ban_year, y_0*(1-effect), ifelse(year == ban_year-1, y_0, NA))
      y_0_effect = ifelse(year>=ban_year, y_0*(1-effect_size), NA)
    )
}

samp= c(3:10)
all_treated <- c("VT", "MA", "CA", "CT", "RI")# Never changes
bans <- c(2012, 2013, 2014, 2011, 2014)
year_start = 2006
year_cutoff = 2018
vt_exp_effect <- disposal_effect_size2 %>% filter(state_id=="VT", year<2019) %>% summarise(m=mean(effect_size)) %>% pluck("m")
reg_effect <- c(0.098, 0.185, 0.6*0.316/0.214*vt_exp_effect)

xy_plot_fun_passage <- function (i)
{
  
  reg_expect <- 
    tibble(
      state_id=c("MA", "CA", "VT"), 
      reg_effect = reg_effect
    )
  
  all <-  
    xy_plot_data_passage %>% filter(attempt ==i, treated_state %in% c("CA", "CT","MA", "VT", "RI")) %>% 
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
    xy_plot_data_passage %>% 
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
      rbind(xy_plot_data_passage%>% select(year, treated_state, ban_year, attempt, tons_pc, y, y_0, y_0_effect), all) %>% 
        group_by(treated_state) %>% 
        filter(year >= ban_year-3, year < ban_year) %>%
        mutate(
          year=mean(year), # i duplicate so i join and then also keep the xlab_mae
          xlab_mae = 2010,
          xlab_mae = ifelse(treated_state=="CA", 2010, xlab_mae),
          xlab_mae = ifelse(treated_state=="CT", 2009.5, xlab_mae),
          xlab_mae = ifelse(treated_state=="MA", 2011.5, xlab_mae),
          xlab_mae = ifelse(treated_state=="RI", 2012.5, xlab_mae),
          xlab_mae = ifelse(treated_state=="VT", 2010.5, xlab_mae),
          xlab_mae=ifelse(treated_state%in%c("Boulder, CO"), xlab_mae+0.3, xlab_mae),
          ylab_mae=ifelse(treated_state%in%c("All", "CA", "CT"), mean(tons_pc)-0.05, mean(tons_pc)-0.06), 
          ylab_mae=ifelse(treated_state%in%c("Seattle, WA"), ylab_mae-0.01, ylab_mae), 
          #ylab_mae=ifelse(treated_state%in%c("CT"), ylab_mae+0.01, ylab_mae), 
          ylab_mae=ifelse(treated_state%in%c("All"), ylab_mae+0.01,ylab_mae), 
          ylab_mae=ifelse(treated_state%in%c("CA"), ylab_mae-0.05, ylab_mae), 
          ylab_mae=ifelse(treated_state%in%c("RI"), ylab_mae-0.05, ylab_mae) 
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
      MAE = ifelse(treated_state=="CA", paste0("Mean absolute percentage error (%): ", MAE), MAE), 
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
    
    scale_color_manual(breaks= c("Actual", "Synthetic", "Our Exp.", "Regulators' Exp."), values = c(ut_colors[4],ut_colors[5],"seagreen", "#bad9c6"), name = "")+
    scale_linetype_manual(breaks= c("Actual", "Synthetic", "Our Exp.", "Regulators' Exp."), values = c("solid", "solid", "solid", "solid"), name = "")+
    scale_size_manual(breaks= c("Actual", "Synthetic", "Our Exp.", "Regulators' Exp."), values = c(0.5, 0.5, 1.0, 1.0), name = "")+
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



xy_plot_data_passage <-
  rbind(
    xy_plot_data_function("CA",7,1),
    xy_plot_data_function("CT",1,1),
    xy_plot_data_function("MA",4,1),
    xy_plot_data_function("RI",7,1),
    xy_plot_data_function("VT",4,1)
 )

#write.csv(xy_plot_data_passage, "xy_plot_data_passage.csv", row.names=FALSE)

#xy_plot_data_passage <- read.csv("xy_plot_data_passage.csv")

xy_plot <- xy_plot_fun_passage(1)
xy_plot <-
  xy_plot+
  labs(x="", y = "", title = "Disposal (tons per capita)") + 
  theme(
    strip.text = element_blank(),
    plot.title = element_text(family = "Helvetica", color = ut_colors[4],size = 10, hjust=0.5))


passage_spec <- rbind(
  read.csv("power_state_p.csv")) %>% 
  rename(
    tstate_id
  )

chosen_sample_size <-
  passage_spec %>% 
  group_by(treated_state) %>%
  filter(att_min ==max(att_min)) %>% 
  rename(
    chosen_sample_size=sample_size 
  ) %>% 
  select(
    treated_state, chosen_sample_size
  )

bt_with_power_data_passage <- 
  passage_spec %>% 
  rename(
    power_low = att_min, 
    power_high = att_max
  ) %>% 
  select(-att_median)%>%  
  mutate (
    power_low = 100*power_low, 
    power_high=100*power_high) %>% 
  group_by(specification, treated_state, ban_year) %>% 
  right_join(
    chosen_sample_size, by = c("treated_state", "sample_size"="chosen_sample_size")
  ) %>% 
  ungroup %>% 
  left_join(
    xy_plot_data_passage %>% 
      mutate(county_id=ifelse(county_id == "sanfranciscoM3", "san franciscoM3", county_id)) %>% 
      filter(attempt==1) %>% 
      group_by(county_id) %>% 
      filter(year>=ban_year) %>% 
      summarise(actual_treatment_effect = sum(tons_pc - y_0)/sum(y_0)), 
    by = c("treated_state"= "county_id")
  ) %>% 
  mutate(
    treated_state=factor(treated_state, levels=c("VT", "RI", "MA", "CT", "CA"))
  ) %>% 
  rename(state_id=treated_state)

bt_with_power_data <- read.csv("bt_with_power_data.csv")

bt_with_power_data_passage <-
  bt_with_power_data_passage %>% 
  left_join(
    bt_with_power_data %>% 
      select(state_id, mean_effect, reg_effect), 
    by = "state_id") %>% 
  mutate(
    state_id = factor(state_id, levels =c("VT", "RI", "MA", "CT", "CA"))
  ) %>% 
  arrange(state_id)



bt_with_power_passage2 <- 
  bt_with_power_data_passage %>% #filter(state_id!="San Francisco, CA") %>% 
  mutate(
    state_id = fct_recode(
      state_id,
      "Vermont" = "VT",
      "Rhode Island" = "RI",
      "Massachusetts" = "MA",
      "Connecticut" = "CT",
      "California" = "CA"
    )#, 
    #state_id = factor(state_id, levels = c("California","Connecticut", "Massachusetts", "Rhode Island", "Vermont"))
  )%>% 
  ggplot()+
  aes(y= as.factor(state_id), x= 100*actual_treatment_effect, group = 1)+
  geom_errorbar(aes(xmin = power_low, xmax = power_high, color = "Placebo"), width = 0.2, size=0.5)+
  scale_color_manual(breaks = c("Placebo"), values = c(ut_colors[5]), guide = guide_legend(order = 2, legend.spacing.x=unit(-1, "cm"), byrow=TRUE ))+
  geom_vline(xintercept = 0, lty = "dotted", color = ut_colors[5])+
  labs(y="", x = "", color = "")+
  ggnewscale::new_scale_color()+
  geom_point(aes(color = "Estimate"),size=1.5)+
  scale_color_manual(breaks = c("Estimate"), values = c(ut_colors[4]),guide = guide_legend(order = 1,  legend.spacing.x=unit(-1, "cm"), byrow=TRUE))+
  labs(y="", x = "", color = "")+
  ggnewscale::new_scale_color()+
  geom_errorbar(aes(xmin=-100*mean_effect, xmax=-100*mean_effect, color = "Our Exp."), linewidth=1, width=0.2)+
  scale_color_manual(breaks = c("Our Exp."), values = c("seagreen" ),guide = guide_legend(order = 3,  legend.spacing.x=unit(-1, "cm"), byrow=TRUE))+
  labs(y="", x = "", color = "")+
  ggnewscale::new_scale_color()+
  
  geom_errorbar(aes(xmin=-100*reg_effect, xmax=-100*reg_effect, color = "Regulators' Exp."), linewidth=1, width=0.2)+
  scale_color_manual(breaks = c("Regulators' Exp."), values = c("#bad9c6" ),guide = guide_legend(order = 4,  legend.spacing.x=unit(-1, "cm"), byrow=TRUE))+
  labs(y="", x = "", color = "")+
  
  ggnewscale::new_scale_color()+
  scale_x_continuous(limits=c(-20, 20), breaks = c(seq(-15, 15, by =5)))+
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
    legend.key = element_rect(colour = NA, fill = NA, size = 5),
    legend.spacing.x = unit(-1, "pt"),
    axis.ticks.x = element_line(size = 0.1), 
    legend.text = element_text(family = "Helvetica",size = 10, color= ut_colors[4])
  )


xy_and_power_placebo <-
  ggpubr::ggarrange(
    xy_plot %+% 
      subset(xy_plot$data, !treated_state %in% c("All", "Boulder, CO", "Seattle, WA"))+
      geom_point(data = subset(xy_plot$data %>% filter(!treated_state %in%  c("All", "Boulder, CO", "Seattle, WA")), location %in% c("Actual", "Synthetic"))), 
    bt_with_power_passage2+
      labs(x="", title = "Average treatment effect on the treated (%)")+
      scale_x_continuous(limits=c(-20, 20), breaks = c(seq(-15, 15, by =5)))+
      theme (
        plot.title = element_text(hjust=0.6, size = 10, color=ut_colors[4])), 
    heights = c(0.5, 1))

xy_and_power_placebo


ggsave(
  xy_and_power_placebo, filename = "xy_and_power_placebo_passage.pdf", device = cairo_pdf,
  path= figure_path,
  width = 9, height = 9, units = "in")

