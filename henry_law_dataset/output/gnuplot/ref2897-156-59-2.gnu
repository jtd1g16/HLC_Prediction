# load "ref2897-156-59-2.gnu"
# chem = "($Z$)-1,2-dichloroethene"

set terminal postscript eps color
set title "ref = 2897; chem = ($Z$)-1,2-dichloroethene; casrn = 156-59-2"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.2220897E-02 * exp(  -3124.209    *(1/   298.    -1/T))

set label "" at    274.9500    ,   0.4860373E-02 point
set label "" at    294.7500    ,   0.2914634E-02 point
set label "" at    313.1500    ,   0.1324389E-02 point
set label "" at    323.1500    ,   0.1005912E-02 point
set label "" at    333.1500    ,   0.7521159E-03 point
set label "" at    343.1500    ,   0.5231269E-03 point
set label "" at    298.1500    ,   0.2220897E-02 point ps 2 pt 6

plot [270:350] H(T)
