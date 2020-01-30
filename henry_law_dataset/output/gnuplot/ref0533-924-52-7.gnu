# load "ref0533-924-52-7.gnu"
# chem = "2-butyl nitrate"

set terminal postscript eps color
set title "ref = 533; chem = 2-butyl nitrate; casrn = 924-52-7"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.6328615E-02 * exp(  -5616.276    *(1/   298.    -1/T))

set label "" at    295.9000    ,   0.7303232E-02 point
set label "" at    279.4000    ,   0.2240316E-01 point
set label "" at    298.1500    ,   0.6328615E-02 point ps 2 pt 6

plot [270:300] H(T)
