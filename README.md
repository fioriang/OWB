# OWB


# Data 

## Raw Data 

Includes all the data we used to estimate the treatment effects of organic waste bans: 
1. Waste data: power2_impexp: includes all the data we use for our analysis. These are not the "raw" data received from states. These are the "cleaned" and adjusted data according to the data_section file. Data received from state agencies are available upon request. 
2. Waste characterizations data: all_WCS.csv
3. Data required to estimate the bans' coverage: a. food_generators_MA & food_generators_VT, b. food_processors_list_MA & food_processors_list_VT, c. towns_coordinates_VT
## Data from Code

Here are all the data that are results of the code. 

1. All State Level Results: (Fig. 2)
   a. xy_plot_data

2. Expected effects (Fig. 2)
   a. composting_effect: \\
   b. disposal_effect: \\
   c. disposal_effect_size:\\
   d. municipal_effect
   
3. Power results (Fig. 2)
   a. composting_spec:
   b. disposal_spec:
   c. plac_for_histogram_composting
   d. plac_sf
   e. pool_estimates_All
   d. pool_estimates_CA
   f. power_county
   g. power_county_passage
   h. power_state
   i. power_state_passage
   j. power_state_plac

4. Alternative methods
   a. lasso_att_res
   b. lasso_att_res_state
   c. lasso_pool_estimates
   d. lasso_pool_estimates_state
   e. ridge_att_res
   f. ridge_att_res_state
   g. ridge_pool_estimates
   h. ridge_pool_estimates_state

5. MAPE/MAE of placebo
   a. mae_placebo
   b. mape_state

6. Treatment effects results 
   a. tr_res_multiple_composting
   b. tr_res_municipal_multiple
   c. tr_res_municipal_multiple_composting
   d. tr_res_state_multiple_composting
   e. treat_ca_all
   f. treat_ca_ca
   g. treat_county
   h. treat_county_composting
   i. treat_county_good
   j. treat_county_passage
   k. treat_pool_county
   l. treat_pool_county_good
   m. treat_pool_county_passage
   n. treat_state
   o. treat_state_good
   p. treat_state_passage

7. Year Placebo
  a. year_placebo

# Code

## Primary analysis
1. placebo_all: includes all the code for calculating the placebo confidence interval ("power") and estimating the average ATT ("treated") (Fig. S4, Fig. 2
2. xy (and ): main results and plots for Fig. 2, Fig. S5
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

