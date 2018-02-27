% model should be: ours, sp, dp, icard, hh
function [ll] = likelihood_bysubj(params, ratings, model)

if strcmp(model, 'hh')
    sderror = params(1);
    alpha = params(2);
    beta = params(3);
    base = 0;
    scale = 1;
else
    base = 0;
    scale = params(1);
    sderror = params(2);
end

x = ratings(:, 2) / 10;
a = ratings(:, 3) / 10;
rating = ratings(:, 1);


if strcmp(model, 'ours')
    prediction = a .* (1-x) ./ (1 - x.*a);
elseif strcmp(model, 'sp')
    prediction = a .* (1-x);
elseif strcmp(model, 'dp')
    prediction = a;
elseif strcmp(model, 'icard')
    prediction = 1-x.*(1-a);
elseif strcmp(model, 'hh')
    if x > .5
        prediction = 0;
    elseif x < a
        prediction = alpha;
    else
        prediction = beta;
    end
elseif strcmp(model, 'ours_normed')
    prediction = exp(a .* (1-x) ./ (1 - x.*a)) ./ ...
        (exp(a .* (1-x) ./ (1 - x.*a)) + exp(x .* (1-a) ./ (1 - x.*a)));
end

prediction(isnan(prediction)) = 1/2;
ll = sum(log(normpdf(rating, base + prediction * scale, sderror)));

if ll == -Inf, ll = -realmax; end