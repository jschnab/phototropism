# phototropism
R scripts for data analysis of dataset related to phototropsim

## hypocotyl_time_series.R 
### Description of the script
*hypocotyl_time_series.R* performs data analysis and plotting of hypocotyl curvature time series. The scripts calculates the
mean and standard error of the mean (sem) for each genotype and time point then plots the mean +/- sem agains time.

### Input file format
*hypocotyl_time_series.R* expects you to input a comma-separated values file with four columns:
* genotype : name of genotypes
* hypocotyl curvature : value of the absolute hypocotyl angle for a specific genotype and a specific time point. 180 denotes a
vertical hypocotyl, 90 a horizontal hypocotyl. The direction is irrelevant in this column and must be specified in the *direction*
column.
* time : a number representing the time in minutes spent under unidirectional blue light exposure.
* direction : a letter indicating the direction of hypocotyl bending. "n" (negative) denotes bending away from the light source
while "p" (positive) denotes bending towards the light source.

For a concrete example of how the data should be formatted, see the file names *hypocotyl_phototrop.csv* in this repository. The
data are organized according to the principles of "tidy data": each variable is a column and each observation is a row. For further
explanations of this data format, see [Hadley Wickham.Tidy data. The Journal of Statistical Software, vol. 59, 2014.](http://vita.had.co.nz/papers/tidy-data.html)
