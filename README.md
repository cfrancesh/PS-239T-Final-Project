# PS239T Final Project: HK LegCo

## Short Description

In this project, I collected and analyzed data to answer these three questions: 
1) How has the effectiveness of governance of Hong Kong changed in the past 17 years? 
2) How has the Hong Kong Legislative Council been discussing key constitutional matters? 
3) Who are the most active legislators pushing for discussion on press freedom, electoral matters, and constitutional development?  

## Dependencies

1. R, version 3.1
2. Python, version 3.5, Anaconda distribution.

## Files

#### Data

##### WGI: 
1. WGI_raw_*.csv: The World Governance Indicators dataset, stored in six different csv files for the different indicators: (Available here: http://info.worldbank.org/governance/wgi/index.aspx#home) 
	- CC: control of corruption
	- GE: governance efficiency
	- PS: political stability
	- RL: rule of law
	- RQ: regulatory quality
	- VA: voice of accountability 
2. WGI_code.pdf: codebook for the WGI dataset. 
3. coureg.xls: dataset with country names and their regions

###### LegCo: 
4. legco_*.csv: 6 different csv files containing data collected via 02-legco-dat.api.py. Each file contains one particular policy issue:
	- chex: corruption issues of the chief executive
	- acct: accountability
	- elema: electoral matter
	- condev: constitutional development
	- prfm: press freedom
	- all: merged dataset including the first five datasets
5. members_clean: contains information of the names of the legislators whose names were mentioned in the event summary of the legco_all dataset and their 

#### Code

1. 01-HK-governance.R: cleans the WGI datasets and produces 3 plots found in the Results Dataset. 
2. 02-legco-data-api.py: collects data of the HK legislative council from the Open LegCo API
2. 03-legco-cleaning.R: cleans the legco events dataset and extracts motion topics from the event summary. Produces one plot and a wordcloud stored in the Results folder. 
4. 04-legislators+parties.R: extracts the names of the legislators from the legco dataset and scrapes the table with party affiliation for each legislator from WIkipedia. Produces a list of most active legislators and their affiliation. Stored in the Results folder. 

#### Results

1. hk_plot.jpeg: Graphs the change of the governance indicators of Hong Kong
2. ea_va_*.jpeg: graphs the voice of accountability for all Asian countries in order:
	- 15: the year of 2015
	- 98: the year of 1998
3. keyissu.jpeg: graphs the change in the number of discussion on constitutional matters (1998-2016)
4. wc-motion.jpeg: a wordcloud of the motion topics 

## More Information

If you are interested in working on projects relating to the Hong Kong legislative council, please contact me at: 
chuyue.huang@berkeley.edu
