clc; clear all; close all

%%    DEFINE
t = 2000;

%%    BEGIN
NA = 0;    %to overwrite 'NA' strings in .txt file
time_full = linspace(1965, 1997, 33)';    %set up the years spanned
time_short = [1965 1970 1975 1980 1985 1990 1995 1996 1997]';

%%    load lorry data
lorry_full = [
    52.5
    NA
    NA
    NA
    NA
    50.0
    NA
    NA
    NA
    NA
    48.0
    NA
    NA
    NA
    NA
    43.5
    NA
    NA
    NA
    NA
    41.5
    NA
    NA
    NA
    NA
    39.0
    NA
    NA
    NA
    NA
    38.0
    36.5
    35.0
];

lorry_short = lorry_full(lorry_full~=0);   %take out zeros for fuel data

% concatonate column vectors
full_long = horzcat(time_full, lorry_full);
full_short = horzcat(time_short, lorry_short);

% calculate averages
len = length(lorry_short);
Xbar = sum(lorry_short)/len;   %average fuel
tbar = sum(time_short)/len;   %average time

%calculate alpha and beta
A = 0; B = 0;
e = zeros(len,1);

for i = 1:len
    A = (full_short(i,2)-Xbar)*(full_short(i,1)-tbar) + A;
    B = (full_short(i,1)-tbar)^2 + B;
end

% plug all in to find yhat
beta = A/B;
alpha = Xbar-beta*tbar;

yhat = zeros(len,1);
for j = 1:len    
    yhat(j) = alpha + beta*full_short(j,2);
    e(j) = yhat(j)-full_short(j,2);   
end

dof = 7; n = 9;
sigma2 = sum(e.^2)/dof;
sigma = sqrt(sigma2);

var = sigma2*(1/n+((t-tbar)^2/B));

CI_95 = [yhat(end)-2.356*var yhat(end)+2.356*var]

