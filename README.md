# OWB


# Data 

## Raw Data 

Includes all the data that we have used to estimate the treatment effects of organic waste bans: 
1. Waste data: power2_impexp: includes all the data we use for our analysis. These are not the "raw" data received from states. These are the "cleaned" and adjusted data according to the data_section file. Data received from state agencies are available upon request. 
2. Waste characterizations data: wcs.csv
## Data from Code

Here are all the data that are results of the code. 

1. Expected effects
   a. composting_effect: what is the expected effect on composting at each of the different phases of the bans for CT and VT \\
   b. disposal_effect: \\
   c. disposal_effect_size:\\
   d. municipal_effect
   
2. Power results
   a. composting_spec: results of the power for each specification and passage year for composting (sample_size, ban_year, specification)
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

3. Alternative methods
   a. lasso_att_res
   b. lasso_att_res_state
   c. lasso_pool_estimates
   d. lasso_pool_estimates_state
   e. ridge_att_res
   f. ridge_att_res_state
   g. ridge_pool_estimates
   h. ridge_pool_estimates_state

4. MAPE/MAE of placebo
   a. mae_placebo
   b. mape_state

5. Treatment effects results 
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

8. All State Level Results:
   a. xy_plot_data
   
