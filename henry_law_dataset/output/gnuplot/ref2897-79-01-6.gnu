# load "ref2897-79-01-6.gnu"
# chem = "trichloroethene"

set terminal postscript eps color
set title "ref = 2897; chem = trichloroethene; casrn = 79-01-6"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.1016738E-02 * exp(  -3914.520    *(1/   298.    -1/T))

set label "" at    274.9500    ,   0.3124526E-02 point
set label "" at    294.7500    ,   0.1165854E-02 point
set label "" at    313.1500    ,   0.5190171E-03 point
set label "" at    323.1500    ,   0.3721874E-03 point
set label "" at    333.1500    ,   0.2755845E-03 point
set label "" at    343.1500    ,   0.1743756E-03 point
set label "" at    298.1500    ,   0.1016738E-02 point ps 2 pt 6

plot [270:350] H(T)
