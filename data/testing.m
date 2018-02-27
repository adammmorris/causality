mat = zeros(100000, 1);
for i = 1:100000
    mat(i) = likelihood_bysubj([4, .25], data, 'ours');
end