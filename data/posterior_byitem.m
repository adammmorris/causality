function [ll, ll_full] = posterior_byitem(params, data, model, priors)

ll_full = zeros(10, 10);
for xind = 1:10
    for aind = 1:10
        ratings = data(data(:, 2) == xind & data(:, 3) == aind, 1);
        ll_full(xind, aind) = likelihood_byitem(params, ratings, model, xind, aind);
    end
end

%lp_full = zeros();
%for k = 1:length(params)
%    lp_full(k) = log(priors{k}(params(k)));
%end

ll = sum(sum(ll_full));
if ll == -Inf, ll = -realmax; end