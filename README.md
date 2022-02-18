# Marketing Mix Simulation and Optimization Brick

R Shiny brick for simulating/optimizing spend scenarios and comparing their performance, for spend planning. Detailed steps below to use the brick with the sample data provided in folder "Data":

## 1. Configuration

In this section, all configurations required to generate simulation/optimization scenarios are to be defined. There is a sequential ordering of the same that the user needs to follow and is described as tab headers.

### 1.1 Upload Historical Dataset & Model

#### 1.1.1 Load Historical Data

Upload the historical dataset (superset of train dataset) based on which the MMx model was built. Spend patterns will be decided based on this dataset. Spend variables must be present in the wide format, i.e. each spend channel must be specified as individual columns with spend values populated as rows corresponding to each column.

![](www/Images/load_historical_data.png)

#### 1.1.2 Load MMx Model

This will load the MMX model object created in the modeling phase and display the inherent formula used to forecast the spends.This model object will be used to simulate/optimize results for the specified range of spends.

![](www/Images/load_model_object.png)

### 1.2 Configure Variables

#### 1.2.1 Select Date Columns

For the brick to identify the columns in the data representing the Datetime variable and format, the user has to specify the order in which the date elements (year, month, day) are and the function figures out the rest.

#### 1.2.2 Select Spend, Panel and Target Columns

For the brick to identify the columns in the data representing spend channels, panel and target variable, the user has to either define the model object with these variables defined, or has to specify them via the UI of the brick.

![](www/Images/spend_panel_target_column_selection.png)

#### 1.2.3 Configure Adstock Decay Rates

The decay rates used when building the marketing mix model must be specified against each spend channel for calculating spends after applying adstock effects.

![](www/Images/adstock_decay_definition.png)

### 1.3 Define Simulation/Optimization Period and Load Non-Spend variables

#### 1.3.1 Define Simulation/Optimization Period

This will define the simulation/optimization period for which sales will be predicted based on the simulated spends. Trends from the historical period corresponding to simulation/optimization period will be used to generate the simulation/optimization dataset. The simulation/optimization period must have a comparable historical period in the uploaded historical dataset. The time series frequency refers to the level of the date column specified, i.e. is it monthly, daily or weekly data.

![](www/Images/simulation_period_definition.png)

#### 1.3.2 Load Non-Spend Variables

There may be certain variables in the model equation for which data cannot be extrapolated from the historical dataset. For such cases, the user has to load the dataset for the simulation/optimization period with the same column names as specified in the historical dataset. All external variables used in the model creation must have corresponding historical variables in the uploaded historical dataset.

![](www/Images/non_spend_var_load.png)

## 2. Simulation Scenarios

### 2.1 View Historical Performance

![](www/Images/historical_comparison_plot.png)

### 2.2 Define Scenarios

This functionality allows one to create and save multiple feasible scenarios (one at a time) and view them later for comparison or review by the client

<b>Method 1: Percentage change from historical pattern</b>

![](www/Images/method_1.png)


<b>Method 2: Flighting - Spend concentrated in a few months</b>

![](www/Images/method_3.png)

<b>Method 3: Custom Spend Pattern</b>

![](www/Images/method_2.png)

### 2.3 Generate and Run Simulation Data

#### 2.1 Generate Simulation Spend Data

This section creates a simulation dataset based on the variation parameter and spend strategy specified above. A user can additionally, download this simulated data, edit it if required, and upload for running the simulation

![](www/Images/generate_data.png)

#### 2.2 Run Simulation

Running the model on simulation data generated above creates the simulation results dataframe. The simulation results show the Sales corresponding to different combinations of spend values from the data generated in the section above.

![](www/Images/run_simulation.png)

## 3. Simulation Results

Visualizations for each scenario depicting both, consolidated and time series view of spends, sales, contributions and RoI. Slice data by scenario, panel and channel to identify months of peak marketing activity and understand campaign effectiveness

![](www/Images/view_sim_results.png)

## 4. Optimization Scenarios

### 4.1 Select Business Objective

![](www/Images/SelectBusinessObjective.png)

### 4.2 Set Spend Budget

![](www/Images/setSpendBudget.png)

### 4.3 Configure Custom Constraints

This option gives the user the flexibility to choose the current business and channel constraints that must be taken into consideration when calculating optimized spend mix.

#### 4.3.1 Business Constraints

The business constraint option allows user to define current business constraints which then can be reviewed in the table below

![](www/Images/businessConstraints.png)

#### 4.3.2 Channel Bounds

The bounds table is editable. The user can change or alter the generated default bounds by clicking on the cell to alter its contents.

![](www/Images/enterConstraints.png)

### 4.4 Upload Custom Constraints

A user can also add constraints by downloading the sample constraint files provided, editing and uploading it back to this section.

![](www/Images/uploadCustomConstraints.png)


### 4.3 Select Optimzation Technique

This option allows the user to choose from one of the two derivateive-free optimization techniques available (ISRES and COBYLA) and specify the number of optimization iterations to be run 

![](www/Images/optimisationTechniques.png)

## 5. Optimization Results

This helps the user understand and compare the results of the optimization scenario across different metrics and slices of data

![](www/Images/Results.png)

## 6. Compare Scenarios

Compare simulation/optimization scenarios created in previous sections to identify a feasible spend distribution for available budget

![](www/Images/compare_scenario.png)

![](www/Images/compare_scenario_plots.png)