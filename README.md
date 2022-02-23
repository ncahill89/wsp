This Repo contains a model for producing catchment-scale hydroclimate reconstruction in Queensland, Australia using multiple palaeoclimate proxy records. We are making use of the [PalaeoWISE database](https://figshare.com/articles/dataset/PalaeoWISE/14593863/3) compiled by [Croke et al., in 2021](https://www.nature.com/articles/s41597-021-01074-8#Sec7) which provides a set of standardised proxy records most relevant to hydroclimate reconstruction
 
 To run the model, download the repo and then click wsp.Rproj to open the project in Rstudio. 

  - To produce catchment-scale reconstructions open __main_catchment.R__. This contains the wrapper code to run the model. This is the code that produces the results in the [preprint paper](https://arxiv.org/abs/2202.09383). If you wish to view the raw catchment-scale data you'll find it in the __data_all__ folder. 

  - To produce reconstructions for IOD, IPO, SOI, NINO open __main_queensland.R__. This contains the wrapper code to run the model. If you wish to view the raw data you'll find it in the __data__ folder. 


 - If using __main_catchment.R__ then results will be saved to catchment specific output folders.

 - Each catchment will have a folder containing subfolders with the reconstruction results for each of the climate indices. In each sub folder you will see a plot of the results and a .csv file. The .csv file contains:
 
  1. Year
  2. climate_variable_recon – this is the estimate for the reconstruction
  3. lower – this is the lower bound of the 95% uncertainty interval
  4. upper – this is the upper bound of the 95% uncertainty interval
  5. the remainder of the columns have scaled versions of 2-4. You can ignore these.

 - In each catchment folder there will be a file called pass.txt that will contain information about whether or not the model passed initial convergence checks for the various climate variables.

 - If using __main_queensland.R__ then all output will be saved in a folder called queensland that will again contain subfolders for the various indices. The format is the same as described above. 
