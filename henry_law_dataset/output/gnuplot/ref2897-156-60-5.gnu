# load "ref2897-156-60-5.gnu"
# chem = "($E$)-1,2-dichloroethene"

set terminal postscript eps color
set title "ref = 2897; chem = ($E$)-1,2-dichloroethene; casrn = 156-60-5"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.1023444E-02 * exp(  -3502.665    *(1/   298.    -1/T))

set label "" at    274.9500    ,   0.2733960E-02 point
set label "" at    294.7500    ,   0.1200143E-02 point
set label "" at    313.1500    ,   0.5566271E-03 point
set label "" at    323.1500    ,   0.4181881E-03 point
set label "" at    333.1500    ,   0.3166804E-03 point
set label "" at    343.1500    ,   0.2098773E-03 point
set label "" at    298.1500    ,   0.1023444E-02 point ps 2 pt 6

plot [270:350] H(T)
