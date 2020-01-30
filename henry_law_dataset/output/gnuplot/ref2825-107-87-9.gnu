# load "ref2825-107-87-9.gnu"
# chem = "2-pentanone"

set terminal postscript eps color
set title "ref = 2825; chem = 2-pentanone; casrn = 107-87-9"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.1041908     * exp(  -4799.505    *(1/   298.    -1/T))

set label "" at    323.0000    ,   0.3027319E-01 point
set label "" at    333.0000    ,   0.1931435E-01 point
set label "" at    343.0000    ,   0.1261325E-01 point
set label "" at    353.0000    ,   0.8582239E-02 point
set label "" at    298.1500    ,   0.1041908     point ps 2 pt 6

plot [320:360] H(T)
