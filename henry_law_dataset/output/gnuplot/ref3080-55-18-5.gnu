# load "ref3080-55-18-5.gnu"
# chem = "N-ethyl-N-nitroso-ethanamine"

set terminal postscript eps color
set title "ref = 3080; chem = N-ethyl-N-nitroso-ethanamine; casrn = 55-18-5"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =    5.595896     * exp(  -6346.512    *(1/   298.    -1/T))

set label "" at    273.1500    ,    35.80115     point
set label "" at    283.1500    ,    18.47801     point
set label "" at    293.1500    ,    8.679067     point
set label "" at    303.1500    ,    4.091560     point
set label "" at    313.1500    ,    1.847801     point
set label "" at    298.1500    ,    5.595896     point ps 2 pt 6

plot [270:320] H(T)
