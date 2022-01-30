# Ring_et_al_2022_GLM

GLMM code used in Ring et al. 2022

In Ring et al. 2022, we used a generalized mixed-effect model (GLMM) with a binomial error distribution to determine if larval bloodmeal host was a predictor of nymphal pathogen acquisition. We used larval host bloodmeal (lizard or mouse feeding) as a fixed effect with Bb infections status of the engorged nymphs as the response variable. With multiple nymphal ticks feeding on the same Bb-infected mouse, we included mouse ID as a random effect to account for pseudoreplication and any correlation between infection status and the individual nymphal host in our analyses. Trial number was nested with mouse ID as an additional random effect to account for host inoculum load variation among the experimental trials. The random effects were nested because trial and mouse ID were not independent covariates. Analyses were performed using the glmm package (v.1.4.2 ).

Data utilized in these analyses can be found under Host_Blood_Meal_Data.csv
