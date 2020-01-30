# load "ref0871-7726-95-6.gnu"
# chem = "molecular bromine"

set terminal postscript eps color
set title "ref = 871; chem = molecular bromine; casrn = 7726-95-6"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.7922597E-02 * exp(  -3641.510    *(1/   298.    -1/T))

set label "" at    273.1500    ,   0.2665234E-01 point
set label "" at    283.0900    ,   0.1550794E-01 point
set label "" at    293.6100    ,   0.9189399E-02 point
set label "" at    303.5300    ,   0.6010316E-02 point
set label "" at    313.4600    ,   0.4059715E-02 point
set label "" at    323.4000    ,   0.2862055E-02 point
set label "" at    333.1900    ,   0.2131130E-02 point
set label "" at    343.1300    ,   0.1682008E-02 point
set label "" at    353.3700    ,   0.1294530E-02 point
set label "" at    298.1500    ,   0.7922597E-02 point ps 2 pt 6

plot [270:360] H(T)
