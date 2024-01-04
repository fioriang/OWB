# OWB

In this repository, we provide all the data and necessary information for replication of our paper titled "Organic waste bans have failed to divert waste away from landfills in the United States—except in Massachusetts".

# Data 

## Raw Data 

Includes all the data we used to estimate the treatment effects of organic waste bans: 
1. Waste data: power2_impexp: includes all the data we use for our analysis. These are not the "raw" data received from states. These are the "cleaned" and adjusted data according to the data_section file. Data received from state agencies are available upon request. 
2. Waste characterizations data: all_WCS.csv
3. Data required to estimate the bans' coverage: a. food_generators_MA & food_generators_VT, b. food_processors_list_MA & food_processors_list_VT, c. towns_coordinates_VT
## Data from Code

Here are all the data that are results of the code. 

1. All State Level Results [found in xy.R] (xy_plot_data.csv)

2. Expected effects [found in xy.R] (composting_effect.csv, disposal_effect.csv, disposal_effect_size.csv, municipal_effect.csv)
   
3. Power results [found in placebo_all.RMD] (power_county.csv, power_state.csv, sf_power.csv);
   County-level power results: (plac_for_histogram1--6.csv);
   State-level power results: (power_state_plac.csv, pool_estimates_All.csv);
   Composting power results: (plac_for_histogram_composting.csv);
   

5. MAPE/MAE of placebo [found in placebo_all.RMD] 
   mae_placebo;
   mape_state;
   
6. Alternative methods [found in lasso_disposal.RMD & ridge_disposal.RMD] (all files starting with lasso_ or ridge_)

7. Average Treatment effects results [found in placebo_all.RMD] 
   County-level (treat_county.csv, treat_pool_county.csv);
   State-level (treat_state.csv);
   Composting (tr_res_state_multiple_composting.csv, tr_res_multiple_composting.csv);
   Backdated treatment(treat_state_passage.csv, treat_county_passage.csv);
   City-level (sf_power.csv, plac_sf.csv, tr_res_municipal_multiple.csv)

8. Year Placebo [found in year_placebo.R] 
   year_placebo

## Other data 
1. Population (population.csv)
2. Other controls for determining correlates of waste disposal [cross_sectional_variation_2.RMD]

# Code

## Primary analysis
1. placebo_all.RMD: includes all the code for calculating the placebo confidence interval ("power") and estimating the average ATT ("treated") (Fig. S4, Fig. 2
2. xy.R: main results and plots for Fig. 2, Fig. S5
## Robustness checks
1. year_placebo: produces results for California-specific provision (Fig. S6)
2. composting: all analyses regarding bans's effects on composting (Fig. S8)
3. ghg: all analyses regarding emissions (Section B.1.6, Fig. S9, Table S7, Fig. S10)
4. xy_passage: all analyses regarding bans's anticipation effects (Fig. S7)
5. diff_in_diff: all analyses for difference in differences specification (Table S6)
## Data section
1. data_section: all data manipulations can be found there. Additionally imports/exports (Fig. 1, Table S1, Fig. S1,
2. WCS: contains all information regarding waste composition (Fig. S2, Fig. S3, )
3. cross_sectional_variation: all analyses regarding determinants of waste (Section A.1.1, Table S3)

