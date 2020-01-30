# load "ref2931-71-43-2.gnu"
# chem = "benzene"

set terminal postscript eps color
set title "ref = 2931; chem = benzene; casrn = 71-43-2"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.1738649E-02 * exp(  -3992.605    *(1/   298.    -1/T))

set label "" at    293.1500    ,   0.2202532E-02 point
set label "" at    303.1500    ,   0.1378870E-02 point
set label "" at    313.1500    ,   0.9124619E-03 point
set label "" at    323.1500    ,   0.6208064E-03 point
set label "" at    298.1500    ,   0.1738649E-02 point ps 2 pt 6

plot [290:330] H(T)
