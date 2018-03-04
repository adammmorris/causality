% model should be: ours, sp, dp, icard, hh
function [ll] = likelihood(params, data, model, norm)

if strcmp(model, 'hh')
    sderror = params(1);
    alpha = params(2);
    beta = params(3);
    base = 0;
    scale = 1;
else
    base = params(1);
    scale = params(2);
    sderror = params(3);
    alpha = 0;
    beta = 0;
end

x = data(:, 2) / 10;
a = data(:, 3) / 10;
rating = data(:, 1);

ours = @(x,a) a.*(1-x) ./ (1-x.*a);
sp = @(x,a) a.*(1-x);
dp = @(x,a) a;
hh = @(x,a) (x > .5) * 0 + (x < a) * alpha + (x > a & x < .5) * beta;
icard = @(x,a) 1 - x.*(1-a);
normed = @(x,a,f,scale) exp(scale * f(x,a)) ./ (exp(scale * f(x,a)) + exp(scale * f(a,x)));

if ~norm
    if strcmp(model, 'ours')
        prediction = ours(x,a);
    elseif strcmp(model, 'sp')
        prediction = sp(x,a);
    elseif strcmp(model, 'dp')
        prediction = dp(x,a);
    elseif strcmp(model, 'icard')
        prediction = icard(x,a);
    elseif strcmp(model, 'hh')
        prediction = hh(x,a);
    end
else
    if strcmp(model, 'ours')
        prediction = normed(x,a,ours,1);
    elseif strcmp(model, 'sp')
        prediction = normed(x,a,sp,1);
    elseif strcmp(model, 'dp')
        prediction = normed(x,a,dp,1);
    elseif strcmp(model, 'icard')
        prediction = normed(x,a,icard,1);
    elseif strcmp(model, 'hh')
        prediction = normed(x,a,hh,1);
    end    
end

prediction(isnan(prediction)) = 1/2;
ll = sum(log(normpdf(rating, base + prediction * scale, sderror)));

if ll == -Inf, ll = -realmax; end