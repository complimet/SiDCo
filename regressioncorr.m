
function [mdl1, wilc]=regressioncorr(corr1, corr2, p1, p2, threshold,pin)
% copyright  MCuperlovicCulf, Ottawa, 2021
% calculated distance correlation and with p-value for all elements of x 
% input is:
% corr1, corr2 - correlation matrices, 
% p1, p2 - p-values for correlation matrices
% threshold for the correlation value and 
% pin threshold for p-value
% output:
% mdl1 - linear regression parameters: 1- slope, 2 p-value; 3 - R2
% wilc - Wilcox rank sum test p-value for corr1 and corr2 statistical
% difference

test1=corr1(:,:);
test1(test1<threshold)=0;
test1(p1>pin)=0;
test2=corr2(:,:);
test2(test2<threshold)=0;
test2(p2>pin)=0;
for i=1:numel(corr2(1,:))
[r,m,b] = regression(test1(:,i),test2(:,i),'one');
mdl=fitlm(test1(:,i),test2(:,i));
mdl1(i,1)=mdl.Coefficients.Estimate(2);
mdl1(i,2)=mdl.Coefficients.pValue(2);
mdl1(i,3)=mdl.Rsquared.Ordinary;
reg(i)=r;
wilc(i)=ranksum(test1(:,i),test2(:,i)); 
% Wilcox test that  null hypothesis that data in x and y are samples from 
% continuous distributions with equal medians p<0.05 accepted null
% hypothesis
end
end
