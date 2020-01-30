# load "ref2825-110-43-0.gnu"
# chem = "2-heptanone"

set terminal postscript eps color
set title "ref = 2825; chem = 2-heptanone; casrn = 110-43-0"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.6776927E-01 * exp(  -5683.228    *(1/   298.    -1/T))

set label "" at    323.0000    ,   0.1513660E-01 point
set label "" at    333.0000    ,   0.9605805E-02 point
set label "" at    343.0000    ,   0.5710884E-02 point
set label "" at    353.0000    ,   0.3410560E-02 point
set label "" at    298.1500    ,   0.6776927E-01 point ps 2 pt 6

plot [320:360] H(T)
