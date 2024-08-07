# OWB

In this repository, we provide all the data and necessary information for replication of our paper titled "Of the first five US states with food waste bans, Massachusetts alone has reduced landfilled waste".  
We include all the raw data and software we used to produce all tables and figures in this paper. 
Additionally, for easy replication, we include some outputs generated by our code, such as power analysis results. These are available in the "Data from Code" section.


# Raw data 

1. Waste data: includes all the data we use for our analysis (power2_impexp.csv)   
   Note: in this file "disposal" refers to state-generated MSW for disposal. That is, we have (i) used only the MSW fraction of the total waste and (ii) excluded the imported waste and included waste exports. (For more details, please see sections A.1.2 and A.1.3 of the SM).
3. Waste characterizations data:  (wcs_2.csv)
4. Food waste generators lists: (food_generators_MA.csv & food_generators_VT.csv)
5. Food waste processors lists: (composting_infrastructure_all_states_gov.csv, composting_capacity_all_states.csv)
6. Other data:   
   a. Population of US counties: needed to create the per capita waste dispsosal (population.csv)   
   b. US Cities: coordinates of US cities---needeed to estimate the minimum distance between generators and processors (uscities.csv, town_coordinates_VT.csv)


# Code

## Primary analysis
1. placebo_all.RMD: calculation of the placebo confidence intervals ("power") (right panel of Fig. 2)   
	This script also includes:  
		 a. the statistics of the placebo distributions for state-level bans (Table S6)   
		 b. the calculation of the placebo confidence intervals for city-level specifications (right panel of Fig. S7)  
		 b. the calculation of the placebo confidence intervals for anticipatory rob check (right panel of Fig. S9)  


2. xy.R: main results for the point estimate of the ATT of the bans + the building of the SC for the treated states (left panel of Fig. 2)   
	This script also includes:   
		 a. the calculation of the point estimate of the ATT + the SC for city level bans (left panel of Fig. S7)   

## Data section
1. data_section: producing all the data-related figures and tables (Fig. 1, Fig. S2, Fig. S3, Table S4)   
2. WCS: calculations regarding the expected effect of the bans (Table 1, Fig. S1)   
	This script also includes:   
		a. analysis of the WCSs of treated and non-treated states in our sample (Fig. S4)   
## Mechanism
1. infrastructure_gov.R: includes analyses related to the final section of the paper, the potential drivers of MA's ban's success (Fig. 3, Fig. 4, Fig. S9)

## Robustness checks
1. year_placebo: produces results for California-specific provision (Fig. S8)      
2. composting: all analyses regarding bans's effects on composting (Fig. S10)   
3. ghg: all analyses regarding emissions (Fig. S5, Fig. S6, Table S8)   
4. xy_passage: all analyses regarding bans's anticipation effects (left panel of Fig. S9)    
5. diff_in_diff: all analyses for difference in differences specification (Table S7)   

# Intermediate data
These files include intermediate data that have been produced at midpoints in the code. They are needed in order to replicate parts of the code without running every script (e.g., run xy.R without running placebo_all.RMD)

1. Expected effects    
	a. State-level expected effects for each phase: (disposal_effect_size2.csv)   
	b. (Robustness check) City-level expected effects (municipal_effect.csv)   
	c. (Robustness check) Effects of the bans on composting (composting_effect.csv)   

2. Time series of actual and synthetic disposal    
	a. Detailed results of the time series for state-level and city-level bans (xy_plot_data.csv)   
	a. Summarised results of the ATT estimates for state-level and city-level bans (bt_with_power_data.csv)   
	c. (Robustness check) Detailed results of the time series for state-level bans (xy_plot_data_passage.csv)   
	c. (Robustness check) Detailed results of the time series when using methane emissions as the main outcome (sc_data_ghg.csv)   
	d. (Robustness check) Detailed results of the California provision placebo distribution (year_placebo.csv)   
	e. (Robustness check) Summarised results of the estimates of the ATT when using composting as the main outcome for state-level (tr_res_state_multiple_composting.csv) & for the city-level bans (tr_res_municipal_multiple_composting.csv)   

3. Placebo distributions results 
	a. Detailed results of the state-level placebo distribution (power_state_plac_no_r_sq_filter.csv) & the for the aggregate case (pool_estimates_All.csv)   
	b. Summarised results of the state-level placebo distribution (power_state.csv)   
	c. (Robustness check) Detailed results of the couty-level placebo distribution (plac_for_histogram4.csv, plac_for_histogram6.csv)   
	d. (Robustness check) Summarised results of the county-level placebo distribution (power_county.csv)   
	e. (Robustness check) Detailed results of the SF placebo distribution (plac_sf.csv)   
	f. (Robustness check) Summarised results of the SF placebo distribution (sf_power.csv)   
	d. (Robustness check) Summarised results of the county-level placebo distribution (power_county.csv)   
	g. (Robustness check) Detailed results of the state-level placebo distribution when assuming the passage date as implementation date (power_state_pass_data.csv)   
	h. (Robustness check) Summarised results of the state-level placebo distribution when assuming the passage date as implementation date (power_state_p.csv)   
	i. (Robustness check) Detailed results of the state-level placebo distribution when using methane emissions as the main outcome (power_gas_res.csv)   
	j. (Robustness check) Detailed results of the California provision placebo distribution (year_placebo.csv)   
	k. (Robustness check) Summarised results of the state-level placebo distribution when using composting as our outcome (power_state_composting.csv)   
	l. (Robustness check) Detailed results of the county-level placebo distribution when using composting as our outcome (plac_for_histogram4_composting.csv, plac_for_histogram6_composting.csv)   
	m. (Robustness check) Summarised results of the county-level placebo distribution when using composting as our outcome (power_county_composting.csv)   
