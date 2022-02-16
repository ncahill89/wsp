Download the repo and then click wsp.Rproj to open the project in Rstudio. 

To produce catchment-scale reconstructions open main_catchment.R. This contains the wrapper code to run the model.

To produce reconstructions for IOD, IPO, SOI, NINO open main_queensland.R. This contains the wrapper code to run the model.


If using main_catchment.R then results will be saved to catchment specific output folders.

Each catchment will have a folder containing subfolders with the reconstruction results for each of the climate indices. In each sub folder you will see a plot of the results and a .csv file. The .csv file contains
 
1. Year
2. climate_variable_recon – this is the estimate for the reconstruction
3. lower – this is the lower bound of the 95% uncertainty interval
4. upper – this is the upper bound of the 95% uncertainty interval
5. the remainder of the columns have scaled versions of 2-4. You can ignore these.

In each catchment folder there will be a file called pass.txt that will contain information about whether or not the model passed convergence checks for the various climate variables.

If using main_queensland.R then output will be saved in a folder called queensland. The format is the same as described above. 