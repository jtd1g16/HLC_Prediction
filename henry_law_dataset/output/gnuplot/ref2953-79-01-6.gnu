# load "ref2953-79-01-6.gnu"
# chem = "trichloroethene"

set terminal postscript eps color
set title "ref = 2953; chem = trichloroethene; casrn = 79-01-6"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.1076730E-02 * exp(  -4213.580    *(1/   298.    -1/T))

set label "" at    275.1500    ,   0.4062413E-02 point
set label "" at    279.1500    ,   0.2868523E-02 point
set label "" at    283.1500    ,   0.2248626E-02 point
set label "" at    291.1500    ,   0.1392765E-02 point
set label "" at    298.1500    ,   0.1005222E-02 point
set label "" at    303.1500    ,   0.7647303E-03 point
set label "" at    313.1500    ,   0.5343992E-03 point
set label "" at    323.1500    ,   0.3615927E-03 point
set label "" at    333.1500    ,   0.2776616E-03 point
set label "" at    298.1500    ,   0.1076730E-02 point ps 2 pt 6

plot [270:340] H(T)
