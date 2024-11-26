
function [DC, pDC]=distancecorrelation(mat1)
% copyright MCuperlovicCulf, Canada, 2020
% calculated distance correlation  with p-value for all elements of mat1 by
% features are in columns, samples rows
% input - matrix of data for correlation analysis
% output - DC - distance correlation values; pDC - p-value of distance
% correlation values

[size1, size2]=size(mat1)
for i=1:size2
   for j=1:size2
       x=mat1(:,i);
       y=mat1(:,j);
    DC(i,j)=distcorr(x,y);
    n=size1;
    T=DC(i,j)*sqrt(n-2)/sqrt(1-DC(i,j)^2);
    df = n-2;
    pDC(i,j)=1 - tcdf(T, df);
   end
end
end

