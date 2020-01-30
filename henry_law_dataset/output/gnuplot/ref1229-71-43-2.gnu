# load "ref1229-71-43-2.gnu"
# chem = "benzene"

set terminal postscript eps color
set title "ref = 1229; chem = benzene; casrn = 71-43-2"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.1681384E-02 * exp(  -4018.050    *(1/   298.    -1/T))

set label "" at    283.1500    ,   0.3450781E-02 point
set label "" at    288.1500    ,   0.2631795E-02 point
set label "" at    293.1500    ,   0.2173840E-02 point
set label "" at    298.1500    ,   0.1655912E-02 point
set label "" at    303.1500    ,   0.1350100E-02 point
set label "" at    298.1500    ,   0.1681384E-02 point ps 2 pt 6

plot [280:310] H(T)
