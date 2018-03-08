%% Set up

data = csvread('ratings.csv');
model_names = {'ours', 'ours_normed', 'sp', 'dp', 'icard', 'icard_normed', 'hh'};
models = {@(x,a) a.*(1-x) ./ (1-x.*a), @(x,a) a.*(1-x) ./ (1-x.*a), @(x,a) a.*(1-x), @(x,a) a, @(x,a) 1 - x.*(1-a), @(x,a) 1-x.*(1-a), @(x) 0};

norm = 0;

bounds_full = [0 0.1 0 0 0; 1 10 1 1 1];
numStarts = 10;
numSubjects_total = length(unique(data(:,4)));
numModels = length(model_names);

subjects = 1:numSubjects_total;
numSubjects = length(subjects);

%% Fit
numParams = 3;
lme_bms = zeros(numSubjects, numModels);
opt_params = zeros(numSubjects,numParams,numModels);
opt_params_group = zeros(numParams, 2, numModels); % mean var
results = zeros(numSubjects, 4, numModels); % lp ll bic lme
tol = 1e-3;
maxiter = 100;
options = optimoptions(@fmincon, 'Display', 'off', 'UseParallel', false);

for modelind = 1:numModels

    model = model_names(modelind);
    params = 1:3;
    A = []; b = [];
    if strcmp(model, 'hh')
        params = 3:5;
        A = [0 1 -1];
        b = 0;
    else
        model = models(modelind);
    end
    
    bounds = bounds_full(:,params);
    numParams = length(params);
    
    means = randn(numParams,1);
    vars = ones(numParams,1) * 100;
    
    lme_iter = zeros(maxiter, 1);
    
    % do E-M
    iter = 0;
    while iter < maxiter
        iter = iter + 1;
        
        priors = cell(numParams,1);
        for k = 1:numParams
            priors{k} = @(x) normpdf(x, means(k), vars(k));
        end
         
        % e step
        hessians = cell(numSubjects, 1);
        parfor subjind = 1:numSubjects
            subj = subjects(subjind);
            ratings = data(data(:,4) == subj, 1:3);
            
            starts = zeros(numStarts, numParams);
            for i = 1:numParams
                ub = bounds(2,i);
                lb = bounds(1,i);
                starts(:,i) = rand(numStarts, 1) * (ub-lb) + lb;
            end
                       
            f = @(params) -posterior_bysubj(params, ratings, model, priors, norm);
            logposts_starts = zeros(numStarts, 1);
            params_starts = zeros(numStarts, numParams);
            
            for thisStart = 1:numStarts
                [params_starts(thisStart, :), logposts_starts(thisStart), ~, ~, ~, ~] = ...
                    fmincon(f, starts(thisStart, :), A, b, [], [], ...
                    bounds(1, :), bounds(2, :), [], options);
            end
            
            % get lme's
            [~, bestStart] = min(logposts_starts);
            post = -logposts_starts(bestStart);
            opt_params(subjind,:,modelind) = params_starts(bestStart, :);
            
            ll = likelihood_bysubj(params_starts(bestStart, :), ratings, model, norm);
            bic = (numParams * (log(size(ratings,1)) - log(2*pi)) - 2 * ll);
            hessians{subjind} = NumHessian(f,params_starts(bestStart, :));
            
            lme = post + .5 * (numParams * log(2*pi) - log(det(hessians{subjind})));
            
            if~isreal(lme) || isnan(lme) || isinf(lme)
                lme = -0.5 * bic;
            end
            
            results(subjind,:,modelind) = [post, ll, bic, lme];
            lme_bms(subjind, modelind) = lme;
        end
        
        % m step
        vars = zeros(numParams,1);
        means = nanmean(opt_params(:,:,modelind))';
        numSubjUsed = 0;
        for subj = 1:numSubjects
            if ~any(isnan(hessians{subj}))
                vars = vars + opt_params(subj,:,modelind)' .^ 2 + diag(pinv(hessians{subj}));
                numSubjUsed = numSubjUsed+1;
            end
        end
        vars = max(1e-5, vars / numSubjUsed - means .^ 2);
        vars(isnan(vars)) = nanvar(opt_params(:,isnan(vars),modelind));
        
        lme_iter(iter) = sum(lme_bms(:, modelind)) - numParams * log(size(data,1));
        opt_params_group(:,:,modelind) = [means vars];
        
        if iter > 1 && abs(lme_iter(iter) - lme_iter(iter - 1)) < tol
            break;
        end
    end
end

save('subjfits_hierarchical.mat', 'results', 'lme_bms', 'opt_params', 'opt_params_group');

%% Run BMS
[~, modelprobs, ~, pxp, ~] = bms(lme_bms);
modelprobs
pxp