% function mlbench_princeR(data_filepath, allcomplexes_path, output_path, Nfraction, Nreplicate)

% wrapper for MatLab PrInCE with runtime and RAM benchmarking

tic
prince(data_filepath, 'coreComplexes.txt', output_path, Nfraction, Nreplicate)
walltime = toc

outputfile = strrep(data_filepath, '.csv', '.out')

fileID = fopen(outputfile,'w');
fprintf(fileID, '%.2f-seconds\n', walltime);
fclose(fileID)

exit