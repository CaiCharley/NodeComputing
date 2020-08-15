function mlbench_princeR(data_filepath, allcomplexes_path, output_path, Nfraction, Nreplicate)

% wrapper for MatLab PrInCE with runtime and RAM benchmarking

start = clock;
prince(data_filepath, 'coreComplexes.txt', output_path, Nfraction, Nreplicate)
stop = clock;

walltime = etime(stop, start)

outputfile = strrep(data_filepath, '.csv', '.out')

fileID = fopen(outputfile,'w');
fprintf(fileID, '%.2f-matlabwalltime\n', walltime);
fclose(fileID);

exit;