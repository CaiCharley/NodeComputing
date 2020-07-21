% function mlbench_princeR(data_filepath, allcomplexes_path, output_path, Nfraction, Nreplicate)

% wrapper for MatLab PrInCE with runtime and RAM benchmarking

tic
s = 10000
for c = 1:s
    for r = 1:s
        H(r,c) = 1/(r+c-1);
    end
end
walltime = toc

fprintf('%.2f seconds\n',walltime)