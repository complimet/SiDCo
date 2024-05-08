import sys
import dcor
import math
import pingouin
import numpy as np
import pandas as pd
from scipy.stats import t

'''Normalize a single column of a 2D array
Calculate mean and standard deviation, then replace
each value with its corresponding Z score'''
def normalizeCol(data, colNum):
    total=0.
    colLen=len(data)
    smallest=float('inf')

    #find smallest value
    for i in range(1,colLen):
        if(smallest>=data[i][colNum]):
            smallest=data[i][colNum]

    #count and sum values
    for i in range(colLen):
        #if((type(data[i][colNum])!=float and type(data[i][colNum])!=int) or pd.isna(data[i][colNum])):
        #MCC no imputation    data[i][colNum]=smallest/5
        total+=data[i][colNum]

    mean=total/colLen

    total=0.
    for i in range(colLen):
        if(not((type(data[i][colNum])!=float and type(data[i][colNum])!=int) or pd.isna(data[i][colNum]))):
            total+=(data[i][colNum]-mean)**2
    stdev=math.sqrt(total/(colLen-1))

    for i in range(colLen):
        data[i][colNum]=(data[i][colNum]-mean)/stdev

'''Iterate through columns of 
a 2D array and normalize using
normalizeCol() method'''
def normalize(data,startCol):
    for i in range(startCol,len(data[0])):
        normalizeCol(data,i)

'''Take an array, and return an array containing
only the specified column, and another containing
all but the specified column'''
def splitArray(data,colNum,startCol,firstRow,lastRow):
    print(firstRow)
    print(lastRow)
    X, Y=[], []
    for row in range(firstRow,lastRow):
        print(row)
        Y+=[[]]
        for col in range(startCol,len(data[0])):
            if(col==colNum):
                X+=[[data[row][col]]]
            else:
                Y[row-firstRow]+=[data[row][col]]
    return X, Y

'''Get relevant column data, and transpose'''
def colsToVetors(data,col1,col2,firstRow,lastRow):
    X=[]
    Y=[]
    #in hindsight, this could be done with numpy.transpose(),
    #but if it ain't broke...?
    for row in range(firstRow,lastRow):
        X+=[float(data[row][col1]) ]
        Y+=[float(data[row][col2])]
    return np.array(X),np.array(Y)

'''given a string, convert to an integer index as seen in excel sheets
ex. "a"->0, "b"->1, "aa"->26'''
def colStringToInt(text):
    if(str(text).isdigit()):
        return int(text)
    else:
        result=0
        for i in range(len(text)):
            result+=int(((ord(text[i].upper()))-64)*math.pow(26,len(text)-i-1))
        return result-1

'''get ONLY the sign of the pearson coefficient
not necessary to get the actual coefficient
This code calculates the numerator, and returns 
1/-1 if it's positive/negative'''
def pearsonCoefficient(X,Y):
    meanX,meanY=0,0
    for i in range(len(X)):
        meanX+=X[i]
        meanY+=Y[i]
    meanX=meanX/len(X)
    meanY=meanY/len(Y)

    total=0
    for i in range(len(X)):
        total+=(X[i]-meanX)*(Y[i]-meanY)
    if(total<0):
        return -1
    return 1

'''get p value of the distance correlation coefficient'''
def pValue(dc,n):
    return 1 - t.cdf( (dc * 
        math.sqrt(n-2) / 
        (math.sqrt(1-dc**2))), 
        (n-2))

##########################     Main     ##########################

def main(inputPath, outputPath, startColString="A",firstRow=-1,lastRow=-1,pValTolerance=0.01,dCorTolerance=0.6,oneToAll=False):
    
    #### input processing ####
    if(inputPath[inputPath.index('.')+1:]=="xlsx"):
        df = pd.read_excel(inputPath)
    else:
        df = pd.read_csv(inputPath)
    
    startCol=colStringToInt(startColString)
    data=df.to_numpy(copy=True)
    normalize(data,startCol)

    firstRow=int(firstRow)
    lastRow=int(lastRow)
    pValTolerance=float(pValTolerance)
    dCorTolerance=float(dCorTolerance)
    
    if(firstRow>1 and firstRow<=len(data)+1):
        firstRow-=2
    else:
        firstRow=0
    if(lastRow>-1 and lastRow<=len(data)+1):
        lastRow-=1
    else:
        lastRow=len(data)

    oneToAll=(str(oneToAll).upper()=="TRUE")

    #### calculations ####
    #if oneToAll enabled
    if(oneToAll):

        #create empty sheet for results
        result=[df.columns.values[startCol:],[],[]]

        #split sheet into a single column X and a matrix of the rest of the sheet Y
        #calculate correlation between X and Y
        for col in range(startCol, len(data[0])):
            X, Y = splitArray(data,col,startCol,firstRow,lastRow)
            dCorr, pVal = pingouin.distance_corr(X, Y)
            if(dCorr<dCorTolerance or pVal>pValTolerance):
                result[1]+=[0.0]
                result[2]+=[pVal]
            else:
                result[1]+=[dCorr]
                result[2]+=[pVal]

        result = pd.DataFrame(result)
        result.insert(0, "Label", ["", "Distance Correlation", "p-value"], True)
        result.to_excel(outputPath)
    
    #else pairwise comparisons
    else:

        #create empty sheets for results
        result=[[""]*len(df.columns.values) for i in df.columns.values]
        result2=[[""]*len(df.columns.values) for i in df.columns.values]

        #populate column and row names
        for i in range(len(data[0])-startCol):
            result[0][i+1]=df.columns.values[i+startCol]
            result[i+1][0]=df.columns.values[i+startCol]
            result2[0][i+1]=df.columns.values[i+startCol]
            result2[i+1][0]=df.columns.values[i+startCol]

        #iterate through columns and calculate coefficient for each pair
        for col1 in range(startCol, len(data[0])):
            for col2 in range(col1+1, len(data[0])):
                X,Y=colsToVetors(data,col1,col2,firstRow,lastRow)
                pc=pearsonCoefficient(X,Y)
                dc=dcor.distance_correlation(X,Y)
                dCorr=pc*dc
                pVal=pValue(dc,len(X))
                if(dc<dCorTolerance or pVal>pValTolerance):
                    result[col1-startCol+1][col2-startCol+1]=0
                    result2[col1-startCol+1][col2-startCol+1]=pVal
                else:
                    result[col1-startCol+1][col2-startCol+1]=dCorr
                    result2[col1-startCol+1][col2-startCol+1]=pVal

        return (result,result2)


args = sys.argv
(result,result2)=main(args[1],args[2],args[3],args[4],args[5],args[6],args[7],args[8])

result = pd.DataFrame(result)
result2 = pd.DataFrame(result2)

with pd.ExcelWriter(args[2]) as writer:  
    result.to_excel(writer, sheet_name='Distance Correlations')
    result2.to_excel(writer, sheet_name='P Values')