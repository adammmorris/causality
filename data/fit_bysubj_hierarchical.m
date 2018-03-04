%% Set up

data = csvread('ratings.csv');
exclude = [5   6   7  10  11  17  18  20  23  25  38  46  48  53  56  62  73  76  81  84  86  88 103 106 111 114 121 122 130 131 134 136 141 144 ...
148 153 162 163 180 181 188 189 192 200 206 212 216 219 220 223 224 226 231 238 245 249 254 257 258 259 265 266 268 271 277 278 284 287 ...
290 292 306 308 311 313 314 315 316 317 324 338 347 354 361 362 372 373 382 386 387 389 398 402 413 418 422 428 433 436 448 455 456 459 ...
462 483 488 502 515 519 531 535 543 544 561 563 565 578 580 584 592 593 603 605 617 618 623 633 635 637 642 644 648 649 650 656 659 660 ...
663 664 682 683 687 689 694 695 703 712 715 722 723 724 726 730 737 738 739 741 742 748 761 778 784 786 793 799 809 812 817 819 822 830 ...
834 839 855 860 862 863 864 865 869 886 889 892 894 896 899 902 907 914 926 941 943 947 953 964 967 974 976 977 980 981 984 988 999];

model_names = {'ours', 'ours_normed', 'sp', 'dp', 'icard', 'hh'};
models = {@(x,a) a.*(1-x) ./ (1-x.*a), @(x,a) a.*(1-x) ./ (1-x.*a), @(x,a) a.*(1-x), @(x,a) a, @(x,a) 1 - x.*(1-a), @(x) 0};

norm = 0;

%priors = {@(x) 1/2, @(x) normpdf(x, 0, .25), @(x) 1, @(x) 1};

bounds_full = [0 0 0 0 0; 1 10 1 1 1];
numStarts = 10;
numSubjects_total = length(unique(data(:,4)));
numModels = length(model_names);
%numModels = 1;

subjects = 1:numSubjects_total;
include = true(numSubjects_total, 1);
for i = 1:numSubjects_total
    %if any(exclude == i)
    %    include(i) = false;
    %end
end
subjects = subjects(include);
numSubjects = length(subjects);

%% Fit
lme_bms = zeros(numSubjects, numModels);
opt_params = zeros(numSubjects,numParams,numModels);
opt_params_group = zeros(numParams, 2, numModels); % mean var
results = zeros(numSubjects, 4, numModels); % lp ll bic lme
tol = 1e-3;
maxiter = 40;
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

%% Run BMS
[~, modelprobs, ~, pxp, ~] = bms(lme_bms(:,2:end));
modelprobs
pxp