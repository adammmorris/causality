function [ll] = likelihood_bysubj(params, ratings, model, norm)

if strcmp(model, 'hh')
    sderror = params(1);
    alpha = params(2);
    beta = params(3);
    base = 0;
    scale = 1;
    model = @(x,a) (x > .5) * 0 + (x < a) * alpha + (x >= a & x <= .5) * beta;
else
    base = params(1);
    scale = params(2);
    sderror = params(3);
    model = model{1};
end

if strcmp(model, 'ours_normed') || strcmp(model, 'icard_normed')
    norm = 1;
    temp = scale;
    scale = 1;
end

x = ratings(:, 2) / 10;
a = ratings(:, 3) / 10;
rating = ratings(:, 1);
normed = @(x,a,f,temp) exp(temp.*f(x,a)) ./ (exp(temp.*f(x,a)) + exp(temp.*f(a,x)));

if ~norm
    prediction = model(x,a);
else
    prediction = normed(x,a,model,temp);
end

prediction(isnan(prediction)) = 1/2;
ll = sum(log(normpdf(rating, base + prediction * scale, sderror)));