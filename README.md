# Submission to JOAS - "Hidden Markov Models and Flight Phase Identification" 

### Step 1: Download raw data 

- Go the NASA DASHlink project page: https://c3.ndc.nasa.gov/dashlink/projects/85/ (last check : 2023/11/17)
- Download data "Flight Data For Tail 687" 
- Gather all Matlab files in one folder named "raw_data" (create it) (5,376 flights in total)

### Step 2: Clean raw data with "00_Clean_NASA_raw.R"

For each individual file (Matlab file), a .csv file is created if the flight is valid. 
Create and specify a folder to store all the clean flights "clean_data" (create it) (2,868 flights in total)

### Reproduce results

- **01_Vizu_NASA_sample.R** : vizualize a single NASA flight (Figure 1 in the article).    
  
- **02_HMM_uni_unique.R** : use the RoC to perform the segmentation of the 3 main flight phases for a single flight of your choice (Figure 3).        
  Loop this script to get the results for 2,868 flights. First, you need to create an empty folder "folder_res_1".   
  An example of such loop for the first 500 flights is given in the script "02_HMM_uni_01.R".  
  For each interation (that is to say for each flight), a csv is created in "folder_res_1".  
  
- **03_HMM_uni_unique_smoothed.R** : segmentation of the 3 main flight phases for given flight with smoothing.    
  Loop this script to get the results for several flights. First, you need to create an empty folder "folder_res_2".     
  An example of such loop for the first 500 flights is given in the script "03_HMM_uni_smoothed_01.R".     
  For each interation (that is to say for each flight), a csv is created in "folder_res_2".  
    
- **04_Plot_uni.R** : plot results for the 3 main flight phases (Figure 4). This script reads from "folder_res_1".   

- **04_Plot_uni_smooth.R** : plot results for the 3 main flight phases with smoothing (not in the article). This script reads from "folder_res_2".    

- **04_Plot_uni_smooth_gridsearch.R** : reproduce Figure 9.  
  You can use "grid_search_uni_01.R" to produce grid search results for the first 100 flights. First, you need to create an empty folder "folder_res_3".  
  For each interation (that is to say for each flight), a csv is created in "folder_res_3".    
 
- **05_HMM_uni_unique_missing.R** : use the RoC to perform the segmentation of the 3 main flight phases for a single flight of your choice but draw missing values (Figure 5).  
    
- **06_HMM_multi_unique.R** : segmentation for 6 flight phases for a single flight of your choice (Figure 7).     
  Loop this script to get the results for several flights. First, you need to create an empty folder "folder_res_4".       
  An example of such loop for the first 500 flights is given in the script "06_HMM_multi_01.R".      
  For each interation (that is to say for each flight), a csv is created in "folder_res_4".  
  
- **07_Plot_multi.R** : plot results for 6 flight phases (Figure 8). This script reads from "folder_res_4".     
    
- **08_Plot_multi_smooth_gridsearch.R** : smoothing effect - multivariate HMM (Figure 10 & Figure 11).  
  You can use "Smoothing_multivariate_01.R" to reproduce data for the first 100 flights. First, you need to create an empty folder "folder_res_5".
  For each interation (that is to say for each flight), a csv is created in "folder_res_5".  
  
- **09_Helicopter.R** : download helicopter flight and perform the segmentation (Subsection 6.3). You need to access the Impala Shell (https://opensky-network.org/impala-guide).         
