
function [DC, pDC]=distancecorrelation_one_to_all(mat1,mat2)
% calculated distance correlation and  p-value for all elements  between
% two different matrices
% copyright MCuperlovicCulf, Ottawa, 2020
[size1, size2]=size(mat1);
[size12, size22]=size(mat2);

for i=1:size2
       x=mat1(:,i);
       y=mat2(:,:);
       y(:,i)=[];
    DC(i)=distcorr_one_to_all(x,y);
    n=size1;
    T=DC(i)*sqrt(n-2)/sqrt(1-DC(i)^2);
    df = n-2;
    pDC(i)=1 - tcdf(T, df);   
end
end


