% model should be: ours, sp, dp, icard, hh
function [ll] = likelihood_byitem(params, ratings, model, xind, aind)

if strcmp(model, 'hh')
    sderror = params(1);
    alpha = params(2);
    beta = params(3);
    base = 0;
    scale = 1;
elseif strcmp(model, 'ours_normed')
    base = 0;
    scale = params(1);
    sderror = params(2);
else
    base = 0;
    scale = params(2);
    sderror = params(3);
end

if (strcmp(model, 'ours') || strcmp(model, 'ours_normed')) && xind == 10 && aind == 10
    ll = 0;
else
    x = xind / 10; a = aind / 10;
    prediction = -1;
    if strcmp(model, 'ours')
        prediction = a * (1-x) / (1 - x*a);
    elseif strcmp(model, 'sp')
        prediction = a * (1-x);
    elseif strcmp(model, 'dp')
        prediction = a;
    elseif strcmp(model, 'icard')
        prediction = 1-x*(1-a);
    elseif strcmp(model, 'hh')
        if x > .5
            prediction = 0;
        elseif x < a
            prediction = alpha;
        else
            prediction = beta;
        end
    elseif strcmp(model, 'ours_normed')
        prediction = exp(scale * (a * (1-x) / (1 - x*a))) / ...
            (exp(scale*(a * (1-x) / (1 - x*a))) + exp(scale*(x * (1-a) / (1 - x*a))));
    end
    
    ll = sum(log(normpdf(ratings, base + prediction * scale, sderror)));
end