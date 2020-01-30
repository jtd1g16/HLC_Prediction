# load "ref2431-75-65-0.gnu"
# chem = "2-methyl-2-propanol"

set terminal postscript eps color
set title "ref = 2431; chem = 2-methyl-2-propanol; casrn = 75-65-0"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.8019791     * exp(  -6479.514    *(1/   298.    -1/T))

set label "" at    278.1500    ,    3.826558     point
set label "" at    298.1500    ,   0.8019791     point
set label "" at    298.1500    ,   0.8019791     point ps 2 pt 6

plot [270:300] H(T)
