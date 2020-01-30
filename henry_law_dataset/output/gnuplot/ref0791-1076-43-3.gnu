# load "ref0791-1076-43-3.gnu"
# chem = "benzene-d6"

set terminal postscript eps color
set title "ref = 791; chem = benzene-d6; casrn = 1076-43-3"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.1648767E-02 * exp(  -4519.423    *(1/   298.    -1/T))

set label "" at    273.1500    ,   0.6604743E-02 point
set label "" at    286.1500    ,   0.3110311E-02 point
set label "" at    296.1500    ,   0.1827539E-02 point
set label "" at    298.1500    ,   0.1648767E-02 point ps 2 pt 6

plot [270:300] H(T)
