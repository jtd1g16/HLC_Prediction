# load "ref2808-75-89-8.gnu"
# chem = "2,2,2-trifluoroethanol"

set terminal postscript eps color
set title "ref = 2808; chem = 2,2,2-trifluoroethanol; casrn = 75-89-8"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.4712256     * exp(  -6209.733    *(1/   298.    -1/T))

set label "" at    276.0000    ,    2.516654     point
set label "" at    283.0000    ,    1.411300     point
set label "" at    291.0000    ,   0.8063163     point
set label "" at    299.0000    ,   0.4391809     point
set label "" at    298.1500    ,   0.4712256     point ps 2 pt 6

plot [270:300] H(T)
