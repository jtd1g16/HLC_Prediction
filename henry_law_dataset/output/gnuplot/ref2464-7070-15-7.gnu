# load "ref2464-7070-15-7.gnu"
# chem = "arbanol"

set terminal postscript eps color
set title "ref = 2464; chem = arbanol; casrn = 7070-15-7"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =    1.020413     * exp(  -4125.333    *(1/   298.    -1/T))

set label "" at    279.1500    ,    2.616838     point
set label "" at    296.6500    ,    1.094361     point
set label "" at    298.1500    ,    1.020413     point ps 2 pt 6

plot [270:300] H(T)
