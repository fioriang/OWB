pre_processing_dt <- function(power2, year_start, year_cutoff)
{
  year_start <- year_start
  year_cutoff <- year_cutoff
  year_end <- 2011
  
  dt <- power2
  
  dt <- 
    dt %>% 
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
    )
  
  #Exclude missing values and small values
  exc<- 
    as.data.frame(
      dt %>% 
        group_by(county_id) %>% 
        filter(state_id != "CT", state_id != "RI") %>% 
        summarize(
          m = mean(tons_pc), 
          min_year = min(year), 
          min_tons = min(tons_pc), 
          max_tons = max(tons_pc)) %>%
        filter(
          m < 0.3 | min_year > year_start | max_tons > 3.5)
    )
  
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
        filter(state_id != "RI") %>% 
        group_by(county_id) %>% 
        mutate(
          percentage = (tons-dplyr::lag(tons, n = 1, default = NA))/dplyr::lag(tons, n = 1, default = NA),
          percentage_1 = (tons-dplyr::lead(tons, n = 1, default = NA))/dplyr::lead(tons, n = 1, default = NA)
        )
    )
  
  f <- f[!is.na(f$percentage),]
  threshold <- 2.5
  exclude2 <- unique(f$county_id[f$percentage > threshold | f$percentage < -threshold | f$percentage_1 > threshold | f$percentage_1 < -threshold])
  dt <- dt[!dt$county_id %in% exclude2,]
  
  donors <- unique(dt$county_id)
  rm(f, exc,exclude2)
  
  dt_initial <- dt
  return(dt_initial)
}

dt_initial <- pre_processing_dt(power2, 2006, 2019)
# Results

donors <- unique(dt_initial$county_id[!(dt_initial$state_id%in% all_treated)])
treated_counties_id_in <- unique(dt_initial$county_id[dt_initial$state_id%in% all_treated])
treated_counties_names_in <- unique(dt_initial$county_name[dt_initial$state_id%in% all_treated])

# chosen_counties_all <- c(
#   "amador", "butte", "calaveras", "colusa", "del norte", "el dorado", "glenn", "humboldt", "kern", "lassen", "madera", "mariposa", "merced", "modoc",   "monterey", "nevada", "plumas", "san benito", "san bernadino", "san joaquin", "san luis obispo", "shasta", "sierra", "solano", "stanislaus", "tehama", "trinity", "tulare", "tuolumne", "yolo", "yuba", "ventura")
# chosen_counties_all <- paste(chosen_counties_all, "CA", sep = "")
#donors <- donors[donors %in% chosen_counties_all]#needed for matching
#treated_counties_id <- treated_counties_id[treated_counties_id %in% chosen_counties_all]#needed for matching


treated_counties_id <- 
  treated_counties_id_in[
    treated_counties_id_in %in% 
      c(
        dt_initial %>%  
          filter(
            state_id == "CA",
            county_name != "san francisco",
            county_name!="lake",
            year == 2016, 
            pop > 70000
          ) %>% 
          pluck("county_id"))
  ]

treated_counties <- 
  treated_counties_names_in[
    treated_counties_names_in %in% 
      c(
        dt_initial %>%  
          filter(
            state_id == "CA",
            county_name != "san francisco",
            county_name!="lake",
            year == 2016, 
            pop > 70000
          ) %>% 
          pluck("county_name"))
  ]

donors <- 
  dt_initial %>% 
  filter(
    !county_id%in% treated_counties_id, county_name != "san francisco", #county_name!="del norte", county_name!="lake", 
    state_id=="CA"
    #county_id %in% chosen_counties_all #needed for matching
  ) %>% 
  ungroup %>% 
  summarise(county_id= unique(county_id)) %>%
  pluck("county_id")


year_placebo <- read.csv("year_placebo.csv")

mean_effect = 
  disposal_effect_size2 %>% 
  filter(state_id=="CA", year <=2019) %>% 
  summarise(mean_effect=100*mean(effect_size)) %>% pluck("mean_effect")

fileConn<-file(paste0(figure_path, "/ca_expected_year_placebo.txt"))
writeLines(paste0(format(scales::number(mean_effect %>% round(1), accuracy = 0.01),big.mark=",",scientific=FALSE),'%'), fileConn)
close(fileConn)


set.seed(2)
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
  MA  <- (intercept + x[(test_ind_end1+1):test_ind_end2] - y_test ) %>% abs %>%  mean
  
  att <- (y_att-x[(test_ind_end2+1):n]-intercept) %>% sum
  cf <- (x[(test_ind_end2+1):n]+intercept) %>% sum
  intercept2 <-  mean(c(y_train, y_test)-x[1:test_ind_end2])
  att2 <- ((y_att-x[(test_ind_end2+1):n]-intercept2) %>% sum)/(x[(test_ind_end2+1):n]+intercept2) %>% sum
  
  c(r, MA, att, cf,intercept,att2, intercept2, c(samples))
}

xy_plot_data_function_pl_year <- function (placebo_ban_year)
{

  end_year = placebo_ban_year+3
  year_start = placebo_ban_year-10
  dt_initial <- pre_processing_dt(power2, year_start, end_year)
  dt <-  
    dt_initial %>% 
    filter(county_name != "san francisco") %>% 
    mutate(
      tons_pc = tons_pc*.75 + .25*ifelse(is.na(lag(tons_pc, n=1, default = NA)), tons_pc, lag(tons_pc, n=1, default = NA)) 
    ) %>% as.data.frame
  
  dt <- 
    dt_initial %>% as_tibble %>% 
    filter(
      state_id == "CA", 
      year >= year_start, 
      year <=end_year, 
      #county_name !="del norte", #oti na nai noumera
      county_name != "san francisco"
    ) %>% 
    mutate(
      tons_pc = tons_pc*.75 + .25*ifelse(is.na(lag(tons_pc, n=1, default = NA)), tons_pc, lag(tons_pc, n=1, default = NA)),
      state_id = "CA1"
    )
  
  dt <- dt %>%  as.data.frame()    
  
  all_treated <- c("VT", "MA", "CA", "CT", "RI")# Never changes
  bans <- c(2014, 2014, placebo_ban_year, 2014, 2016)
  
  offset = 3
  iterations = 5000
  
  in_sample_R2_xy <- function (k, dt, donors, iterations, offset, samp)
  {
    treated_state <- str_sub(treated_counties_id[k],start=-2)
    ban_year <- bans[which(all_treated == treated_state)]
    year_end <- ban_year-offset
    treated_location <- treated_counties_id[k]
    don_new <- donors[donors!=treated_location]
    n_don <- length(don_new)
    test_ind_end1 <- year_end - year_start+1
    test_ind_end2 <- ban_year-year_end-1 +test_ind_end1
    
    y <- dt[dt$county_id==treated_location, c("tons_pc")] 
    y_train <- y[1:test_ind_end1]
    y_test <-  y[(test_ind_end1+1):test_ind_end2]
    y_att <- y[(test_ind_end2+1):length(y)]
    
    x <- dt[dt$county_id %in% donors,]
    x <- x[x$county_id!=treated_location, c("tons_pc", "county_id")]
    x <- as.matrix(unstack(x, tons_pc ~ county_id)) 
    
    
    res <- tibble(
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
    
    #Initialize x's so it takes less time
    sample_size <- samp
    
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
      arrange(mape) %>% 
      slice(1:50) %>% 
      mutate(
        sample_size = sample_size, 
        county_id = treated_location, 
        iterations = iterations, 
        ban_year = ban_year, 
        att = att/cf
      ) %>%  
      pivot_longer(
        cols = c(paste0(rep("donor", sample_size), paste0("_", c(1:sample_size)))), 
        names_to = "donor_number", 
        values_to = "chosen_donor"
      )%>% 
      mutate(
        chosen_donor = donors[chosen_donor]
      )
    
    attempts <-all %>% filter(donor_number == "donor_1") %>% summarise(n= n()) %>%  pluck("n")
    
    
    effect_size = 
      disposal_effect_size2  %>% 
      select(year, state_id, effect_size) %>% 
      filter(state_id=="CA", year <=2019) %>% 
      summarise(effect_size= mean(effect_size)) %>% pluck("effect_size")
    
    all <- 
      all %>%  
      mutate(
        attempt = rep(1:attempts, each= sample_size)
      )
    
    all <- 
      all %>% 
      left_join(dt %>% select(county_id, year, tons_pc), by = c("county_id")) %>% 
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
      mutate(
        treated_state = treated_state, 
        #y_0_effect = ifelse(year>=ban_year, y_0*(1-effect), ifelse(year == ban_year-1, y_0, NA))
        y_0_effect = ifelse(year>=ban_year, y_0*(1-effect_size), NA)
      )
    
    
    return(all)
    
  }

  all <- lapply(1:length(treated_counties), in_sample_R2_xy, dt,donors, iterations, offset, c(2))
  
  effect_size = 
    disposal_effect_size2 %>% 
    select(year, state_id, effect_size) %>% 
    filter(state_id=="CA", year <=2019) %>% 
    summarise(effect_size= mean(effect_size)) %>% pluck("effect_size")
  
  
  all %>%  bind_rows %>% 
    left_join(
      dt %>%  select(county_id, year, pop), 
      by = c("year", "county_id")
    ) %>% 
    group_by(year, ban_year, attempt) %>% 
    mutate(
      weight = pop/sum(pop)
    ) %>% 
    summarise(
      tons_pc = sum(weight*tons_pc), 
      y_0 = sum(weight*y_0)
    ) %>% 
    mutate(
      y_0_effect = ifelse(year>=ban_year, y_0*(1-effect_size), NA)
    )

}

res <- lapply (2007:2016, xy_plot_data_function_pl_year)

#write.csv(res %>% bind_rows(), "year_placebo.csv", row.names=FALSE)
year_placebo <- read.csv("year_placebo.csv")

#year_placebo <- res %>% bind_rows()
ca_reg_expect =reg_expect %>% filter(state_id=="CA") %>% pluck("reg_effect")

xy_plot_year_data <- 
  year_placebo %>% 
  group_by(ban_year, attempt) %>%
  mutate(
    tons_pc_for_att = ifelse(year >=ban_year, tons_pc, 0),
    tons_0_for_att = ifelse(year >=ban_year, y_0, 0) ,
    
    tons_pc_for_mae = ifelse(year >=ban_year-3 & year < ban_year, tons_pc, 0),
    tons_0_for_mae = ifelse(year >=ban_year-3 & year < ban_year, y_0, 0) ,
    
    att = (sum(tons_pc_for_att)- sum(tons_0_for_att)) / sum(tons_0_for_att), 
    att = ifelse(year == ban_year +3, att, NA), 
    
    mae = ifelse(tons_pc_for_mae!=0, abs((tons_pc_for_mae-tons_0_for_mae)/tons_pc_for_mae) %>% mean(na.rm=TRUE) %>% {.*100}, NA), 
    mae = ifelse(year == ban_year -3, mae, NA), 
    mae_choice = mean(mae, na.rm=TRUE),
    
    reg_expect_tons = ifelse(year >=ban_year, y_0*(1-ca_reg_expect), NA)
  ) %>%  
  group_by(ban_year) %>% 
  filter(mae_choice==min(mae_choice, na.rm=TRUE)) %>% 
  filter(attempt==min(attempt)) %>% 

  ungroup %>% 
  #filter(attempt==3) %>% 
  rename(
    Synthetic = y_0, 
    Actual = tons_pc, 
    Expected = y_0_effect, 
    `Regulators' Exp.`=reg_expect_tons
  ) %>% 
  mutate(
    Expected = ifelse(ban_year==2016, Expected, NA), 
    `Regulators' Exp.` = ifelse(ban_year == 2016, `Regulators' Exp.`, NA)
  ) %>%
  pivot_longer(
    cols = c("Synthetic", "Actual", "Expected", `Regulators' Exp.` ), 
    names_to = "location", 
    values_to = "tons_pc") %>% 
  mutate(
    ban_year_fac = factor(ban_year, levels= c(seq(2016, 2006))), 
    att = ifelse(location == "Actual", att, NA), 
    att = round(att*100, 1), 
    att= scales::number(att, accuracy = 0.1),
    att = ifelse(!is.na(att), paste0(att, "%"), NA),
    att = ifelse(!is.na(att) & ban_year==2016, paste0("Avg. treatment effect on the treated (%): ", att), att), 
    
    mae = ifelse(location == "Actual", mae %>% round(2), NA),
    mae= scales::number(mae, accuracy = 0.01),
    mae = ifelse(year==2013 & ban_year==2016 & location == "Actual", paste0("Mean absolute percentage error (%): ", mae), mae), 
    
  ) %>% 
  select(year, ban_year, attempt, att, mae, location, ban_year_fac, tons_pc) %>%
  group_by(ban_year) %>% 
  mutate(
    xlab = ifelse(year==ban_year-10 & location == "Actual", year, NA ),
    ylab = ifelse(year==ban_year-10 & location == "Actual", tons_pc, NA ), 
    tons_lab = ifelse(year==ban_year-10 & location == "Actual",tons_pc %>% round(2), NA),
    xlab2 = ifelse(year==ban_year+3 & location == "Actual", year, NA ),
    ylab2 = ifelse(year==ban_year+3 & location == "Actual", tons_pc, NA ),
    tons_lab2 = ifelse(year==ban_year+3 & location == "Actual",tons_pc %>% round(2), NA), 
    
    ylab_att = max(tons_pc, na.rm=TRUE), 
    ylab_att = ifelse(ban_year==2006, ylab_att+0.09, ylab_att),
    ylab_att = ifelse(ban_year>=2008 & ban_year <=2015, ylab_att-0.15, ylab_att),
    ylab_att = ifelse(ban_year==2016, ylab_att-0.03, ylab_att),
    
    ylab_mae = min(tons_pc, na.rm=TRUE),
    ylab_mae = ifelse(ban_year>=2010, ylab_mae-0.07, ylab_mae),
    ylab_mae = ifelse(ban_year==2008, ylab_mae+0.15, ylab_mae),
    ylab_mae = ifelse(ban_year==2007, ylab_mae+0.15, ylab_mae),
    
    y_end_low = ifelse(year==ban_year&location=="Actual", tons_pc-0.1, NA), 
    y_end_high = ifelse(year==ban_year&location=="Actual", tons_pc+0.1, NA),
    
    y_end_low_2 = ifelse(year==ban_year-3&location=="Actual", tons_pc-0.1, NA), 
    y_end_high_2 = ifelse(year==ban_year-3&location=="Actual", tons_pc+0.1, NA),
    
    y_end_low = ifelse(ban_year==2016, y_end_low-0.1, y_end_low), 
    y_end_high = ifelse(ban_year==2016, y_end_high+0.1, y_end_high), 
    
    y_end_low_2 = ifelse(ban_year==2016, y_end_low_2-0.1, y_end_low_2), 
    y_end_high_2 = ifelse(ban_year==2016, y_end_high_2+0.1, y_end_high_2)
  ) %>% 
  left_join(
    tibble (year= c(2009, 2012, 2016), xlab_set= c(2010, 2014, 2017.5), ylab_set = 1, label = c("Train", "Validation", "Evaluation"), ban_year = 2016), 
    by =c ("year", "ban_year")
  ) 

xy_plot_year <- 
  xy_plot_year_data %>% 
  ggplot()+
  aes(x=year, y = tons_pc, color = location, size=location)+
  geom_segment(
    aes(x=ban_year, xend=ban_year, y=y_end_low, yend  = y_end_high, color= location), 
    linetype = "dotted", linewidth = 0.1, color =ut_colors[5])+
  geom_segment(
    aes(x=ban_year-3, xend=ban_year-3, y=y_end_low_2, yend  = y_end_high_2, color=location), 
    linetype = "dotted", linewidth = 0.1, color =ut_colors[5])+
  
  geom_line()+
  geom_point(data = subset(xy_plot_year_data, location %in% c("Actual", "Synthetic")), size = .1)+
  
  facet_grid(vars(ban_year_fac), scales="free_y")+
  scale_color_manual(breaks= c("Actual", "Synthetic", "Expected", "Regulators' Exp."), values = c(ut_colors[4],ut_colors[5],"#417c5b", "#bad9c6"), name = "")+
  scale_size_manual(breaks= c("Actual", "Synthetic", "Expected", "Regulators' Exp."), values = c(0.2, 0.2, 0.6, 0.6), name = "")+
  scale_y_continuous(expand = c(0.1, 0.1))+
  scale_x_continuous(breaks = c(seq(1996, 2019, by =2)))+
  ggnewscale::new_scale_color()+
  ggnewscale::new_scale_color()+
  #att
  geom_text (aes(x=ifelse(ban_year==2016, ban_year-1, ban_year+1.5), y = ylab_att, label = att), color = "#417c5b", size=2, family="Helvetica")+
  #mae
  geom_text (aes(x=ifelse(ban_year==2016, ban_year-4, ban_year-1.5), y = ylab_mae, label = mae), color = ut_colors[5], size=2, family="Helvetica")+
  #first tons
  geom_text (aes(x=xlab-0.8, y = ylab, label = scales::number(tons_lab, accuracy = 0.01) ), color = ut_colors[4], size=2, family="Helvetica")+
  #last tons
  geom_text (aes(x=xlab2+0.7, y = ylab2, label =  scales::number(tons_lab2, accuracy = 0.01) ),color = ut_colors[4], size=2, family="Helvetica")+
  #lines
  #geom_vline(aes(xintercept = ban_year), linetype = "dotted", color = ut_colors[5])+
  geom_text(aes(x=xlab_set, y=ylab_set+0.3, label = label), color =ut_colors[5], size=2, family="Helvetica")+
  labs(y="", x= "")+
  theme_classic()+
  theme(
    legend.position = "top",
    strip.background = element_rect(color = "white", fill = "white"),
    panel.grid.major.x = element_blank() ,
    panel.grid.major.y = element_blank(), 
    text = element_text(family = "Helvetica",size = 8, color = ut_colors[4]), 
    axis.line.x = element_blank(),
    axis.line.y = element_blank(), 
    axis.ticks.y = element_blank(), 
    axis.ticks.x = element_line(size = 0.1), 
    axis.text.y= element_blank(),
    panel.spacing.y = unit(0.0, "cm"),
    strip.text = element_blank(), 
    legend.text = element_text(color = ut_colors[4])
  )



# Remove line type from legend


ggsave(xy_plot_year, filename = "xy_plot_year.pdf", device = cairo_pdf,
       path= figure_path,
       width = 5.5, height = 7, units = "in")


## Save numbers

att_pl_year <- 
  year_placebo %>% 
  group_by(ban_year, attempt) %>%
  mutate(
    tons_pc_for_att = ifelse(year >=ban_year, tons_pc, 0),
    tons_0_for_att = ifelse(year >=ban_year, y_0, 0) ,
    
    tons_pc_for_mae = ifelse(year >=ban_year-3 & year < ban_year, tons_pc, 0),
    tons_0_for_mae = ifelse(year >=ban_year-3 & year < ban_year, y_0, 0) ,
    
    att = (sum(tons_pc_for_att)- sum(tons_0_for_att)) / sum(tons_0_for_att), 
    att = ifelse(year == ban_year +3, att, NA), 
    
    mae = ifelse(tons_pc_for_mae!=0, abs((tons_pc_for_mae-tons_0_for_mae)/tons_pc_for_mae) %>% mean(na.rm=TRUE) %>% {.*100}, NA), 
    mae = ifelse(year == ban_year -3, mae, NA), 
    mae_choice = mean(mae, na.rm=TRUE),
    
    reg_expect_tons = ifelse(year >=ban_year, y_0*(1-ca_reg_expect), NA)
  ) %>%  
  group_by(ban_year) %>% 
  filter(mae_choice==min(mae_choice, na.rm=TRUE)) %>% 
  filter(attempt==min(attempt)) %>% 
  ungroup %>%  
  filter(ban_year == 2016, !is.na(att)) %>%
  pluck("att") 
  

att_pl_year_min <- 
  year_placebo %>% 
  group_by(ban_year, attempt) %>%
  mutate(
    tons_pc_for_att = ifelse(year >=ban_year, tons_pc, 0),
    tons_0_for_att = ifelse(year >=ban_year, y_0, 0) ,
    
    tons_pc_for_mae = ifelse(year >=ban_year-3 & year < ban_year, tons_pc, 0),
    tons_0_for_mae = ifelse(year >=ban_year-3 & year < ban_year, y_0, 0) ,
    
    att = (sum(tons_pc_for_att)- sum(tons_0_for_att)) / sum(tons_0_for_att), 
    att = ifelse(year == ban_year +3, att, NA), 
    
    mae = ifelse(tons_pc_for_mae!=0, abs((tons_pc_for_mae-tons_0_for_mae)/tons_pc_for_mae) %>% mean(na.rm=TRUE) %>% {.*100}, NA), 
    mae = ifelse(year == ban_year -3, mae, NA), 
    mae_choice = mean(mae, na.rm=TRUE),
    
    reg_expect_tons = ifelse(year >=ban_year, y_0*(1-ca_reg_expect), NA)
  ) %>%  
  group_by(ban_year) %>% 
  filter(mae_choice==min(mae_choice, na.rm=TRUE)) %>% 
  filter(attempt==min(attempt)) %>% 
  ungroup %>% 
  filter(!is.na(att)) %>% 
  filter(att==min(att)) %>% pluck("att")


att_pl_year_max <- 
  year_placebo %>% 
  group_by(ban_year, attempt) %>%
  mutate(
    tons_pc_for_att = ifelse(year >=ban_year, tons_pc, 0),
    tons_0_for_att = ifelse(year >=ban_year, y_0, 0) ,
    
    tons_pc_for_mae = ifelse(year >=ban_year-3 & year < ban_year, tons_pc, 0),
    tons_0_for_mae = ifelse(year >=ban_year-3 & year < ban_year, y_0, 0) ,
    
    att = (sum(tons_pc_for_att)- sum(tons_0_for_att)) / sum(tons_0_for_att), 
    att = ifelse(year == ban_year +3, att, NA), 
    
    mae = ifelse(tons_pc_for_mae!=0, abs((tons_pc_for_mae-tons_0_for_mae)/tons_pc_for_mae) %>% mean(na.rm=TRUE) %>% {.*100}, NA), 
    mae = ifelse(year == ban_year -3, mae, NA), 
    mae_choice = mean(mae, na.rm=TRUE),
    
    reg_expect_tons = ifelse(year >=ban_year, y_0*(1-ca_reg_expect), NA)
  ) %>%  
  group_by(ban_year) %>% 
  filter(mae_choice==min(mae_choice, na.rm=TRUE)) %>% 
  filter(attempt==min(attempt)) %>% 
  ungroup %>% 
  filter(!is.na(att)) %>% ungroup %>% filter(att==max(att)) %>% pluck("att")

mean_pl_year <-
  year_placebo %>% 
  group_by(ban_year, attempt) %>%
  mutate(
    tons_pc_for_att = ifelse(year >=ban_year, tons_pc, 0),
    tons_0_for_att = ifelse(year >=ban_year, y_0, 0) ,
    
    tons_pc_for_mae = ifelse(year >=ban_year-3 & year < ban_year, tons_pc, 0),
    tons_0_for_mae = ifelse(year >=ban_year-3 & year < ban_year, y_0, 0) ,
    
    att = (sum(tons_pc_for_att)- sum(tons_0_for_att)) / sum(tons_0_for_att), 
    att = ifelse(year == ban_year +3, att, NA), 
    
    mae = ifelse(tons_pc_for_mae!=0, abs((tons_pc_for_mae-tons_0_for_mae)/tons_pc_for_mae) %>% mean(na.rm=TRUE) %>% {.*100}, NA), 
    mae = ifelse(year == ban_year -3, mae, NA), 
    mae_choice = mean(mae, na.rm=TRUE),
    
    reg_expect_tons = ifelse(year >=ban_year, y_0*(1-ca_reg_expect), NA)
  ) %>%  
  group_by(ban_year) %>% 
  filter(mae_choice==min(mae_choice, na.rm=TRUE)) %>% 
  filter(attempt==min(attempt)) %>% 
  ungroup %>% 
  filter(!is.na(att)) %>% filter(!is.na(att)) %>% ungroup %>% filter(ban_year< 2016) %>% summarise(m=mean(att)) %>% pluck("m")


fileConn<-file(paste0(figure_path, "/att_pl_year.txt"))
writeLines(paste0(format(scales::number(att_pl_year*100, accuracy = 0.01),big.mark=",",scientific=FALSE),'%'), fileConn)
close(fileConn)
fileConn<-file(paste0(figure_path, "/att_pl_year_max.txt"))
writeLines(paste0(format(scales::number(att_pl_year_max*100, accuracy = 0.01),big.mark=",",scientific=FALSE),'%'), fileConn)
close(fileConn)
fileConn<-file(paste0(figure_path, "/att_pl_year_min.txt"))
writeLines(paste0(format(scales::number(att_pl_year_min*100, accuracy = 0.01),big.mark=",",scientific=FALSE),'%'), fileConn)
close(fileConn)


reject_year <- -att_pl_year +att_pl_year_max + mean_pl_year %>% abs()

fileConn<-file(paste0(figure_path, "/reject_year.txt"))
writeLines(paste0(format(scales::number(reject_year*100, accuracy = 0.01),big.mark=",",scientific=FALSE),'%'), fileConn)
close(fileConn)



p_value_year <- 
  year_placebo %>% 
  group_by(ban_year, attempt) %>%
  mutate(
    tons_pc_for_att = ifelse(year >=ban_year, tons_pc, 0),
    tons_0_for_att = ifelse(year >=ban_year, y_0, 0) ,
    
    tons_pc_for_mae = ifelse(year >=ban_year-3 & year < ban_year, tons_pc, 0),
    tons_0_for_mae = ifelse(year >=ban_year-3 & year < ban_year, y_0, 0) ,
    
    att = (sum(tons_pc_for_att)- sum(tons_0_for_att)) / sum(tons_0_for_att), 
    att = ifelse(year == ban_year +3, att, NA), 
    
    mae = ifelse(tons_pc_for_mae!=0, abs((tons_pc_for_mae-tons_0_for_mae)/tons_pc_for_mae) %>% mean(na.rm=TRUE) %>% {.*100}, NA), 
    mae = ifelse(year == ban_year -3, mae, NA), 
    mae_choice = mean(mae, na.rm=TRUE),
    
    reg_expect_tons = ifelse(year >=ban_year, y_0*(1-ca_reg_expect), NA)
  ) %>%  
  group_by(ban_year) %>% 
  filter(mae_choice==min(mae_choice, na.rm=TRUE)) %>% 
  filter(attempt==min(attempt)) %>% 
  ungroup %>% 
  filter(!is.na(att)) %>% 
  ungroup %>% filter(ban_year < 2016) %>% 
  mutate(att=abs(att)) %>% 
  summarise(
    p = mean(att > abs(att_pl_year))
  ) %>% pluck("p")

fileConn<-file(paste0(figure_path, "/p_value_year.txt"))
writeLines(paste0(format(round(p_value_year,2),big.mark=",",scientific=FALSE),'%'), fileConn)
close(fileConn)
