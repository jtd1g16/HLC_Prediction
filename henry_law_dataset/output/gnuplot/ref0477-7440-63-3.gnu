# load "ref0477-7440-63-3.gnu"
# chem = "xenon"

set terminal postscript eps color
set title "ref = 477; chem = xenon; casrn = 7440-63-3"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.4342597E-04 * exp(  -2338.963    *(1/   298.    -1/T))

set label "" at    288.1500    ,   0.5745290E-04 point
set label "" at    293.1500    ,   0.4943495E-04 point
set label "" at    298.1500    ,   0.4309377E-04 point
set label "" at    303.1500    ,   0.3801974E-04 point
set label "" at    308.1500    ,   0.3392884E-04 point
set label "" at    298.1500    ,   0.4342597E-04 point ps 2 pt 6

plot [280:310] H(T)
