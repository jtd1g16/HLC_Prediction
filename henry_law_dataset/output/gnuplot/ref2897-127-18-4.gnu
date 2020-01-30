# load "ref2897-127-18-4.gnu"
# chem = "tetrachloroethene"

set terminal postscript eps color
set title "ref = 2897; chem = tetrachloroethene; casrn = 127-18-4"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.5839399E-03 * exp(  -4214.365    *(1/   298.    -1/T))

set label "" at    274.9500    ,   0.1988334E-02 point
set label "" at    294.7500    ,   0.6375761E-03 point
set label "" at    313.1500    ,   0.2887765E-03 point
set label "" at    323.1500    ,   0.2102754E-03 point
set label "" at    333.1500    ,   0.1432602E-03 point
set label "" at    343.1500    ,   0.8425361E-04 point
set label "" at    298.1500    ,   0.5839399E-03 point ps 2 pt 6

plot [270:350] H(T)
