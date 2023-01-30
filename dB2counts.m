function dB2counts(db)

%conversion from Tim Rowell
%20*log10(23170 counts rms) - 171.9 dB re 1 uPa rms = -84.6 dB re 1 count/uPa

%enter in a positive number for dB
%needed for the vessel detector, to be entered as the "transfer function"


counts= 23170; %current metric of number of counts/ dB rms
db_counts= abs(20*log10(counts)-db);
msg= sprintf('%0.2f dB re 1 uPa RMS= %0.2f dB re 1 count/uPa',db, db_counts);
disp(msg)