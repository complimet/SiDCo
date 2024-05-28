
function [DC, pDC]=distancecorrelation2(mat1,mat2)
% calculated distance correlation and  p-value for all elements one-by-one between
% two different matrices
% copyright MCuperlovicCulf, Ottawa, 2020
[size1, size2]=size(mat1);
[size12, size22]=size(mat2);

for i=1:size2
   for j=1:size22
       x=mat1(:,i);
       y=mat2(:,j);
    DC(i,j)=distcorr(x,y);
    n=size1;
    T=DC(i,j)*sqrt(n-2)/sqrt(1-DC(i,j)^2);
    df = n-2;
    pDC(i,j)=1 - tcdf(T, df);
   end
end
end


