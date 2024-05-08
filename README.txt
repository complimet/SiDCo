SiDCo README
Author: David Stewart

This app calculates the distance correlation coefficients in a data set.

In normal execution, the columns of a sheet are compared pairwise, to create 
a coefficient between every possible combination of columns

When the One To All option is enabled, the script will compare each column to the 
concatenation of every other column in the sheet. One of the advantages of distance
correlation is that it can be calculated between matricies of different dimensions.

The output is two sheets - one with the distance correlation coefficients, and one
with the p-values for these calculations.

-----Parameters-----
inputPath(String): local path where input is stored
outputPath(String): local path where output is saved
startColString(String): index of first column in data sheet containing numerical data
firstRow(int): first row in the sheet to extract and use in calculation
lastRow(int): last row in the sheet to extract and use in calculation
pValTolerance(float): p-value Tolerance for distance correlation calculations
dCorTolerance(float): distance correlation tolerance for distance correlation calculations
numPartners(int): Maximum number of correlation partners to use in building models. If 0, all correlation partners are used.
oneToAll(bool): enables or disables the one to all calculation as described above

-----Main Calls-----
A quick summary of the heirarchy of calls

main()
	normalize()

	if(oneToAll){
		splitArray()
		pingouin.distance_corr()
	}else{
		colsToVectors()
		pearsonCoefficient()
		dcor.distance_correlation()
		pValue()
	}
