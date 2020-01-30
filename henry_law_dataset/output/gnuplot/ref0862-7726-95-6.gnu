# load "ref0862-7726-95-6.gnu"
# chem = "bromine (molecular)"

set terminal postscript eps color
set title "ref = 862; chem = bromine (molecular); casrn = 7726-95-6"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.7823041E-02 * exp(  -3830.555    *(1/   298.    -1/T))

set label "" at    273.1500    ,   0.2663913E-01 point
set label "" at    283.1500    ,   0.1545510E-01 point
set label "" at    293.1500    ,   0.9378734E-02 point
set label "" at    303.1500    ,   0.6076363E-02 point
set label "" at    313.1500    ,   0.4138972E-02 point
set label "" at    323.1500    ,   0.2862055E-02 point
set label "" at    333.1500    ,   0.2157549E-02 point
set label "" at    298.1500    ,   0.7823041E-02 point ps 2 pt 6

plot [270:340] H(T)
