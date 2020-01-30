# load "ref2290-7487-94-7.gnu"
# chem = "mercury dichloride"

set terminal postscript eps color
set title "ref = 2290; chem = mercury dichloride; casrn = 7487-94-7"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =    13910.19     * exp(  -5256.676    *(1/   298.    -1/T))

set label "" at    298.1500    ,    13910.19     point
set label "" at    283.1500    ,    35397.13     point
set label "" at    298.1500    ,    13910.19     point ps 2 pt 6

plot [280:300] H(T)
