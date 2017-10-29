%problem 1
clc
clear all
K=1;
N_tilda = 1000;

PrY_plus1 = 0.5;
muX_givenY_plus1 = [1.6 0]';
muX_givenY_minus1 = [-1.6 0]';

sigma = [1 0 ; 0 1];
R = chol(sigma);




y = binornd(1,0.5,N_tilda,1);

for k = 1:N_tilda
    if y(k,1) == 1
        Y(k,1) = 1;
        X_givenY(k,:) = repmat(muX_givenY_plus1',1,1) + randn(1,2)*R;
    elseif y(k,1) == 0
        Y(k,1) = -1;
        X_givenY(k,:) = repmat(muX_givenY_minus1',1,1) + randn(1,2)*R;
    end
end    
   
D_tilda = [Y X_givenY];

gscatter(X_givenY(:,1),X_givenY(:,2),Y, 'br','+o')
%scatter(X_givenY(:,1),X_givenY(:,2))
%scatter3(x,y,z)