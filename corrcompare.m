function [p,z]=corrcompare(r1,r2,n1,n2)
% function compares significance of the difference between two correlation
% coefficients for a group
%--------------------------------------------------------------------------
% Inputs: r1: correlation coefficient of the first correlation r2:
% correlation coefficient of the second correlation n1: number of
% samples used to compute the first correlation n2: number of samples
% used to compute the second correlation
% Output: 
% p: p value, the probability that H0 (the correlation
% coefficiets are not different) is correct - p <0.05 means - corr. are 
% different
% z: corrected difference between Fisher z-transformed correlations
%
% copyright M. Cuperlovic-Culf, 2021, Ottawa

zr1 = 0.5*(log(1+r1)-log(1-r1));
zr2 = 0.5*(log(1+r2)-log(1-r2));
z = abs(zr1-zr2)/sqrt((1/(n1-3))+(1/(n2-3)));
mu=0;
sigma=1;
p = (normcdf(normalize(z),mu,sigma,'upper'));  
%Normal cumulative distribution function - using this function provides more information about the low valus of p
end