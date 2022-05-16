
function plotcorr(testvalue, ptestvalue, metabolites, treshold,pvalue,i,j,k)
% copyright MCuperlovicCulf, Ottawa, 2021
% plot circo plot for testvalue and pvalues 
% input:
% correlation values; p-value for correlations;
% metabolite names;
% threshold for correlation value and p-value
% i, j, k = optional values in case of multiple plots
if nargin<6
  i=1;
  j=1;
  k=1;
end
corrgraph=(testvalue);
corrgraph(isnan(corrgraph)) = 0;
corrgraph(ptestvalue>pvalue)=0;
corrgraph((corrgraph)<treshold)=0;
corrgraph((corrgraph)~=0)=1;
namestemp=metabolites;
subplot(i,j,k);circularGraph(corrgraph,'Label',namestemp(1,:));
end
