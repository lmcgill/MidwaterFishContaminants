##### This R Script will be used to visualize pilot data 

# install and/or load the necessary R packages
if("pacman" %in% installed.packages() == FALSE){install.packages("pacman")}
pacman::p_load(geojsonR,sf,dplyr, ggplot2, maps, lubridate, tidyr,ggh4x,cowplot,
               marmap,sjPlot,rgeos, tidyverse, plyr) 

##################################################################################################################
################### Read in inital data   ###################
##################################################################################################################

samples = read.csv(here::here("data","CalCOFI_data","myctophid_samples_19Oct2023.csv"), skip=1) %>% 
  dplyr::select(UID, SpeciesName, Line.Station, Date, Pool, Region, Standard.Length..mm., Gape.Width...Upper..mm., 
                Gape.Width...Lower..mm., Gape.Width...Width..mm., Weight.after.tissue...stomach.removal..g.) %>% 
  dplyr::mutate(Date = as.Date(Date, format="%m/%d/%y"))

results = read.csv(here::here("data","contaminant_data","raw","myctophid_samples_19Oct2023.csv")) %>% 
  dplyr::select(Pool, Year, X24DDE:ww.g)


##################################################################################################################
################### Plot  ###################
##################################################################################################################

data = samples %>% 
  dplyr::filter(Pool %in% results$Pool) %>% 
  left_join(., results, by="Pool")

data.summarized = samples %>% 
  dplyr::filter(Pool %in% results$Pool) %>% 
  dplyr::group_by(Pool) %>% 
  dplyr::summarize(Length.mm = mean(Standard.Length..mm.), 
                   Weight.g = mean(Weight.after.tissue...stomach.removal..g.), 
                   Region = unique(Region)) %>% 
  dplyr::ungroup() %>% 
  left_join(., results, by="Pool") %>% 
  tidyr::gather(X24DDE:X44DDMU, key="DDX", value="Value.ng") %>% 
  dplyr::mutate(DDX = factor(DDX, levels=c("X24DDE", "X44DDE", "X24DDD", "X44DDD", 
                                           "X24DDT", "X44DDT", "X44DDMU"))) 

data.summarized %>% 
  #dplyr::filter(!Pool %in% c("7","8","9")) %>% 
  dplyr::filter(!Pool %in% c("9", "7")) %>% 
  ggplot(aes(x=DDX, y=Value.ng/Lipid.g)) + 
  geom_bar(stat="identity")+
  facet_grid(Region~Year) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

data.summarized %>% 
  dplyr::group_by(Pool) %>% 
  dplyr::mutate(totalDDX = sum(Value.ng)) %>% 
  dplyr::ungroup() %>% 
  ggplot(aes(x=Year, y=totalDDX/Lipid.g)) + 
  geom_point(aes(size=Length.mm)) + 
  geom_line()+ 
  facet_grid(~Region) 
  
data.summarized %>% 
  dplyr::group_by(Pool) %>% 
  dplyr::mutate(totalDDX = sum(Value.ng), 
                totalDDE = sum(Value.ng[DDX %in% c("X24DDE","X44DDE")])) %>% 
  dplyr::ungroup() %>% 
  ggplot(aes(x=Year, y=totalDDE/totalDDX)) + 
  geom_point(aes(size=Weight.g)) + 
  geom_line()+ 
  facet_grid(~Region) 


data.summarized %>% 
  dplyr::group_by(Pool) %>% 
  dplyr::mutate(totalDDX = sum(Value.ng), 
                totalDDE = sum(Value.ng[DDX %in% c("X24DDD","X44DDD")])) %>% 
  dplyr::ungroup() %>% 
  ggplot(aes(x=Year, y=totalDDE/totalDDX)) + 
  geom_point(aes(size=Weight.g)) + 
  geom_line()+ 
  facet_grid(~Region) 


test = read.csv(here::here("data","CalCOFI_data","juvenile_with_cov.csv"))
