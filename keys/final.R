## Step 1: Prepare DEM


## Step 2: GD Flowline
should_plot_flowline  = TRUE
step_2_params = list(should_plot_flowline = should_plot_flowline)

## Step 3: Extract intensity profiles along flowline
max_percent_missing = 0.50
max_percent_na = 0.8

step_3_params = list(max_percent_missing = max_percent_missing, max_percent_na = max_percent_na)


## Step 4: Get candidate termini paths

distPerYear = 50
plot = TRUE #todo integrate this
step_4_params = list(distPerYear = distPerYear)


## Step 5: Termini path clustering

eps_cl = 1500
plot = TRUE #todo integrate this
step_5_params = list(eps_cl = eps_cl)

## Step 6: Analysis


## Params
params = list(step_2 = step_2_params, step_3 = step_3_params, step_4 = step_4_params, step_5 = step_5_params)
