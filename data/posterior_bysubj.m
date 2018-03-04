function [lp] = posterior_bysubj(params, data, model, priors, norm)

lp = likelihood_bysubj(params, data, model, norm);
for k = 1:length(params)
    lp = lp + log(priors{k}(params(k)));
end

if lp == -Inf, lp = -realmax; end