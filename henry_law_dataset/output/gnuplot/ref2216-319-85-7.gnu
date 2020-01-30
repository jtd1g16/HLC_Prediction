# load "ref2216-319-85-7.gnu"
# chem = "beta-HCH"

set terminal postscript eps color
set title "ref = 2216; chem = beta-HCH; casrn = 319-85-7"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =    27.95382     * exp(  -7828.828    *(1/   298.    -1/T))

set label "" at    278.1500    ,    185.1852     point
set label "" at    283.1500    ,    108.6957     point
set label "" at    293.1500    ,    45.45455     point
set label "" at    303.1500    ,    18.86792     point
set label "" at    308.1500    ,    11.36364     point
set label "" at    298.1500    ,    27.95382     point ps 2 pt 6

plot [270:310] H(T)
