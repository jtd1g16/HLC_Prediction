# load "ref2552-593-70-4.gnu"
# chem = "chlorofluoromethane"

set terminal postscript eps color
set title "ref = 2552; chem = chlorofluoromethane; casrn = 593-70-4"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.1535646E-02 * exp(  -2345.433    *(1/   298.    -1/T))

set label "" at    283.4000    ,   0.2408093E-02 point
set label "" at    307.6000    ,   0.1144831E-02 point
set label "" at    332.4000    ,   0.6572909E-03 point
set label "" at    352.3000    ,   0.4816186E-03 point
set label "" at    298.1500    ,   0.1535646E-02 point ps 2 pt 6

plot [280:360] H(T)
