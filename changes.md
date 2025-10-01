<<<<<<< HEAD
28-aug-2025: Updated power2\_impexp.csv to reflect corrections to data discrepancies. Updated code files to correct code bugs. 


Data discrepancies: (1) for NC, we used total rather than municipal solid waste (MSW) disposal in our analyses, because prior to 2011, the state included construction and demolition in its measure of MSW but did not report this inclusion. We correct this measure in the paper (p.6 of SM); (2) for MI, we mistakenly believed exported waste was included in the state-reported MSW, as MI does not report exports. We correct this mistake using the same approach we used for states without export data (p.6 of SM); (3) 
for WI, we assumed exported waste was 0 in 2018, because the state stopped tracking exports after 2017. We correct this mistake with the same approach as in (2); (4) for OH, we double-counted imports/exports for 2006--2009 because the state changed its reporting for imports/exports in 2010 and we mistakenly believed they were excluded from reported disposal; (5) for PA, we misclassified waste imported from DC as PA-in-state waste due to a typo in the raw data files received from PA (the reports list DC as “Disrict of Columbia,” so our filter failed); (6) for NY, we originally included only landfilled waste (instead of landfilled and incinerated waste), as the state initially provided only landfill data. Upon review and correspondence with the state, we correct NY's disposal to include both landfilled and incinerated waste.


Coding errors: (1) Bans are often implemented mid-year, however, disposal data are at the yearly level. For example, MA's 2014 ban was implemented in October, meaning the first 3/4ths of 2014's disposal occurred when no ban was in place. To address this issue, we originally intended to re-center the time-series so that ``2014" in the previous example would be $\frac{1}{4} \cdot  \text{disposal}_{2014} +\frac{3}{4} \cdot \text{disposal}_{2015}$, and so on, correcting every year. However, our original code used incorrect subscripts assigning ``2014" the value of $\frac{1}{4} \cdot  \text{disposal}_{2014} +\frac{3}{4} \cdot \text{disposal}_{2013}$. The updated code reflects the originally-intended re-centering. We also verify this bug was not consequential by including two additional specifications---assuming all bans started (i) in the nearest January, and (ii) in January of their implementation year (p. 13 of SM); (2) for UT and TN we incorrectly treated four NA county-year observations as zeros. We correct this mistake by using linear interpolation between the preceding and subsequent years. 


Typo: In Fig. 2, we reported prediction errors calculated using two time periods instead of the three time-periods used elsewhere in the paper.


Added several comments to the code.
=======
28-aug-2025: Updated power2_impexp.csv to reflect two data updates & to correct 2 minor data discrepancies in the initial sample construction as well as fix three errors in the code. 
Data discrepancies : (1) for NC, we used total waste disposal instead of municipal solid waste disposal; (2) for MI, we did not include exported waste. 
Data updates: (1) updated NY's data to reflect additional information we received in May 2025, and (2) updated WI's waste exports for 2018, which are not reported by WI's environmental agency, by assuming that the ratio of exported waste remains constant. 
Errors in code: (1) for OH, we double-counted imports and exports from 2006 to 2009; (2) for PA, we misclassified waste imported from Washington, DC as in-state waste; and (3) fixed mid-year ban-implementation handling. 
Added several comments to the code to better guide the reader.
>>>>>>> 0816f106a7313d2419ac11625815b8cc8a8ffd6e
