clc
clear all
beta0  = -4.9;
beta  = [1 1 1 1 1]';
%z = zeros(5,1);
%y = sigmf(x,[1 0]);
W = 20 *[1 0 2 2 2 ; 1 1 0 -1 -0.5; -1 1 -1 0 1];
K = 5;
x0 = 1;
syms x1 x2
x = [x0;x1;x2];
for k = 1:K
    z(k,1)=sigmf((W(:,k)'*x),[1 0]);
end
f = beta0 + beta'*z;
ezplot(f) 
hold on;

beta0 = -3.9;
f1 = beta0 + beta'*z;
ezplot(f1)
hold off;   