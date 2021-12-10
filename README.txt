Run the model using main_final.R

Results will be saved to output folders

Each catchment has a folder containing subfolders with the reconstructions results for each of the climate indices that passed convergence checks. In each sub folder you see a plot of the results and a .csv file. The .csv file contains
 
1. Year
2. climate_variable_recon – this is the estimate for the reconstruction
3. lower – this is the lower bound of the 95% uncertainty interval
4. upper – this is the upper bound of the 95% uncertainty interval
5. the remainder of the columns have scaled versions of 2-4. You can ignore these.

In each catchment folder there will be a file called pass.txt that will contain information about whether or not the model passed convergence checks for the various climate variables.