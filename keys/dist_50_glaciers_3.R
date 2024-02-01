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
n_paths = 10
knot_function <- function(ss){
    print("todo")
}

step_4_params = list(distPerYear = distPerYear, knot_function = knot_function, n_paths = n_paths)


eps_cl = 1500
optics_min_pts = 2
optics_eps = 50000 # arbitrarily high value
step_5_plot = TRUE 
step_5_params = list(eps_cl = eps_cl, n_paths = n_paths, optics_min_pts = optics_min_pts, optics_eps = optics_eps, step_5_plot = step_5_plot)

## Step 6: Analysis


## Params
params = list(step_2 = step_2_params, step_3 = step_3_params, step_4 = step_4_params, step_5 = step_5_params)
