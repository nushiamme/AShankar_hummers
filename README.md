# AShankar_hummers

README last updated: November 12, 2016

Location: Maquipucuna and Santa Lucia Ecoreserves, Pichincha Ecuador; El Gullan research station, La Paz

Field Data: Collected by Shankar A, Powers DR, Schroeder RJ, Canepa JR, Morales AM, Cordova G, Urgiles G, Ricart JM, Machado PMT, Eberts E, Quezada A, Elting R

Goals:

1. Torpor analyses for multiple species with data from 2013 - from Arizona, and 2014 - from Maquipucuna and Santa Lucia 
2. Construct an Energy budget for Broad-billed hummingbirds in Arizona at two sites, and then later for all sites
3. Compile and compare daily energy expenditures of various temperate and tropical hummingbird species
4. Analyze hummingbird behavioral data to construct better activity budgets for different ecological roles
5. Analyze temperature data for all our study sites to compare with physiological measures
6. At a later stage, analyze energy intake, nectar and floral resource availability

Contents

Folders: 

  Ecuador_2015: Scripts to analyze data from 2015 field season at La Paz, Ecuador
    Files:
    Hummer_behavior.R - analyze behavioral data for activity budgets
    
    Julisa_territorial_script.R - for JM Ricart's territorial behavior data
    
    Nectar2015.R - flower counts and nectar measurements
    
    Temperature_data_2015.R - to analyze data from ambient and operative temperature sensors
    
    TNZ_plots.R - To analyze data from thermoneutral zone experiments
   
  Torpor1415: hasn't been updated recently, contains files for torpor analyses for DLW, torpor, temperature data from 2013/14
    Files:
    There's a bunch, the names are pretty self-explanatory
    
Other files of note:
    Torpor Analyses:
      Torpor_paper_Nov2015.R - This is the most up-to-date version of this file; there are older versions in the Torpor1415 folder- do
      not use those
      
      TemperaturePlots2016.R - Plotting temperatures for torpor and BBLH energy budget papers, script made in 2016 but data are from
      2013-14
      
      BBLH_energy_budget.Rmd - To put together analyses from other scripts into a readable energy budget word/pdf document
      
      EnergyBudget_AShankar.Rmd - Old energy budget used for dissertation proposal - initially drafted by DR Powers; improved when 
      A Shankar and DR Powers met and rebuilt the model in Jan 2014
      
      DLW2016.Rmd - Hummingbird DEE allometry
      
      AnushaHornbill_Mar2015.R - To analyze data from my Master's thesis on Malabar Pied Hornbill nest site distributions
      
Required Packages:

ggplot2 
reshape
stringr
maptools
dplyr
data.table
extrafont
RColorBrewer
pander
