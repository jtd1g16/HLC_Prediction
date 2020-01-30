# load "ref3080-930-55-2.gnu"
# chem = "N-nitrosopyrrolidine"

set terminal postscript eps color
set title "ref = 3080; chem = N-nitrosopyrrolidine; casrn = 930-55-2"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =    151.8617     * exp(  -8547.141    *(1/   298.    -1/T))

set label "" at    293.1500    ,    269.7010     point
set label "" at    303.1500    ,    79.32382     point
set label "" at    313.1500    ,    42.14078     point
set label "" at    298.1500    ,    151.8617     point ps 2 pt 6

plot [290:320] H(T)
