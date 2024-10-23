========== DATA USAGE AND COMPLIANCE ==========
In compliance with Data User Agreements, we are unable to publicly share the original dataset that was used in our study. Therefore, we provide code to simulate synthetic data and run the procedure. The original code/data were hosted on Pandora/Sirius XM servers and were implemented with an in-house version of TensorFlow for estimation. Here, we provide code for creating synthetic data and code using Torch for estimation. We have made an effort to ensure that the synthetic dataset is as representative as possible of the original data, emulating its inherent patterns and trends.

========== Synthetic Data ==========
To generate synthetic data, we first create a few pre-treatment features, which include hours, ad-supported hours, thumb rate, song skip rate, age, gender, and income. This process starts in the first file below, "01.simulateData.R". To then create heterogeneous responses in ad load, we developed a function hosted under "02.simulatePanelData.R", which is called from subsequent files. We have made an effort to make it similar to actual data, where the treatment effects are heterogeneous and the effect of ad load increase on consumption and subscriptions adjusts gradually over time, albeit at different speeds (subscriptions converge faster).

========== FILE DIRECTORY ==========
The folder houses eleven R/Python files:

"01.simulateData.R"
- This code is used to simulate synthetic data (cross-sectional)
- Results are saved under the Data Folder

"02.simulatePanelData.R"
- This code hosts a function to generate synthetic data in panel form
- That is, given a week post/pre experiment, it generates synthetic data
- This file is called from subsequent files

"03.randomizationCheck.R"
- This file creates the randomization check table in the paper to ensure that randomization was done properly and saves it under the Tables folder

"04.agg_patterns.R"
- This file generates the aggregate level patterns for the highest/lowest ad load condition relative to control for different outcomes
- The results are saved under the Figures folder

"05.IV_regs.R"
- This script estimates the IV regressions to measure the average treatment effect of a persistent increase in ad load at different points in time and plots the coefficients
- The results are then saved under the Figures folder

"06.ML_HTE_data.R"
- This script creates the input for the machine learning algorithm
- It exports cross-section data for multiple time periods into the experiment and then saves the results under Data, which is read by subsequent files

"07.HTE_sub.ipynb"
- This script estimates the heterogeneous treatment effect of increasing ad load on subscriptions
- The results are saved under the Data folder

"08.HTE_ads.ipynb"
- This script estimates the heterogeneous treatment effect of increasing ad load on ad revenues (number of ads shown)
- The results are saved under the Data folder

"09.HTE_Figures.R"
- Based on the estimates from the previous two files, this file creates figures related to heterogeneous treatment effects and the lift in the holdout sample
- The results are saved under the Data folder

"10.ParetoFrontier.R"
- This file creates the Pareto frontier, which shows the returns to personalization at different ad load levels
- We then identify the personalized counterpart to control conditions, which is used in the subsequent file to generate the panel figures pertaining to this personalized condition
- Results are saved under the Figures folder

"11.CounterFactualPanel.R"
- This file uses inverse propensity weights to generate the performance of the personalized counterpart over time
- The results are saved under the Figure folder

Running these scripts in the above order should generate the main results.