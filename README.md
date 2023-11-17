# Submission to JOAS - "Hidden Markov Models and Flight Phase Identification" 

### Step 1: Download raw data 

- Go the NASA DASHlink project page: https://c3.ndc.nasa.gov/dashlink/projects/85/ (last check : 2023/11/17)
- Download data "Flight Data For Tail 687" 
- Gather all Matlab files in one folder named "raw_data" (5,376 flights in total)

### Step 2: Clean raw data with "00_Clean_NASA_raw.R"

For each individual file (Matlab file), a .csv file is created if the flight is valid. 
Create and specify a folder to store all the clean flights "clean_data" (2,868 flights in total)

### Reproduce results

- "01_Vizu_NASA_sample.R" : vizualize a single NASA flight (Figure 1 in the article)  
- "02_HMM_uni_unique.R" : use the RoC to perform the segmentation of the 3 main flight phases for a single flight of your choice (Figure 3)  
  Loop this script to get the results for 2,868 flights.   
- "03_HMM_uni_unique_smoothed.R" : segmentation of the 3 main flight phases for given flight with smoothing  
  Loop this script to get the results for several flights.   
- "04_Plot_uni.R" : plot results for the 3 main flight phases looping script "02_HMM_uni_unique.R" (Figure 4)  
- "04_Plot_uni_smooth.R" : plot results for the 3 main flight phases with smoothing (not in the article)  
- "04_Plot_uni_smooth_gridsearch.R" : reproduce Figure 9  
- "05_HMM_uni_unique_missing.R" : use the RoC to perform the segmentation of the 3 main flight phases for a single flight of your choice but draw missing values (Figure 5)  
- "06_HMM_multi_unique.R" : segmentation for 6 flight phases for a single flight of your choice (Figure 7)  
  Loop this script to get the results for several flights.  
- "07_Plot_multi.R" : plot results for 6 flight phases looping script "06_HMM_multi_unique.R" (Figure 8). 
- "08_Plot_multi_smooth_gridsearch.R" : smoothing effect - multivariate HMM (Figure 10 & Figure 11)  
- "09_Helicopter.R" : dowload helicopter flight and perform the segmentation (Subsection 6.3)    
