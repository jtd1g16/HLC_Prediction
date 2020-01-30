# load "ref0630-75-07-0.gnu"
# chem = "ethanal"

set terminal postscript eps color
set title "ref = 630; chem = ethanal; casrn = 75-07-0"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.1662674     * exp(  -4970.726    *(1/   298.    -1/T))

set label "" at    283.1500    ,   0.4391809     point
set label "" at    298.1500    ,   0.1470516     point
set label "" at    303.1500    ,   0.1213916     point
set label "" at    308.1500    ,   0.9474463E-01 point
set label "" at    318.1500    ,   0.6415001E-01 point
set label "" at    298.1500    ,   0.1662674     point ps 2 pt 6

plot [280:320] H(T)
