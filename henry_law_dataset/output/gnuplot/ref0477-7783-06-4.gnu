# load "ref0477-7783-06-4.gnu"
# chem = "hydrogen sulfide"

set terminal postscript eps color
set title "ref = 477; chem = hydrogen sulfide; casrn = 7783-06-4"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.1014038E-02 * exp(  -1945.621    *(1/   298.    -1/T))

set label "" at    288.1500    ,   0.1275335E-02 point
set label "" at    293.1500    ,   0.1133328E-02 point
set label "" at    298.1500    ,   0.1010437E-02 point
set label "" at    303.1500    ,   0.9066624E-03 point
set label "" at    308.1500    ,   0.8247351E-03 point
set label "" at    298.1500    ,   0.1014038E-02 point ps 2 pt 6

plot [280:310] H(T)
