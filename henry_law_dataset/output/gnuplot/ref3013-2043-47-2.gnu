# load "ref3013-2043-47-2.gnu"
# chem = "4-2FTOH"

set terminal postscript eps color
set title "ref = 3013; chem = 4-2FTOH; casrn = 2043-47-2"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.6639118E-02 * exp(  -4508.697    *(1/   298.    -1/T))

set label "" at    309.6000    ,   0.3548134E-02 point
set label "" at    314.5000    ,   0.3162278E-02 point
set label "" at    319.6000    ,   0.2511886E-02 point
set label "" at    323.0000    ,   0.2089296E-02 point
set label "" at    330.0000    ,   0.1584893E-02 point
set label "" at    334.2000    ,   0.1230269E-02 point
set label "" at    298.1500    ,   0.6639118E-02 point ps 2 pt 6

plot [300:340] H(T)
