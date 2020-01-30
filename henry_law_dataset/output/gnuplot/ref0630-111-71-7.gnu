# load "ref0630-111-71-7.gnu"
# chem = "heptanal"

set terminal postscript eps color
set title "ref = 630; chem = heptanal; casrn = 111-71-7"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.3261050E-01 * exp(  -7461.409    *(1/   298.    -1/T))

set label "" at    283.1500    ,   0.1223785     point
set label "" at    298.1500    ,   0.3355539E-01 point
set label "" at    308.1500    ,   0.1381693E-01 point
set label "" at    318.1500    ,   0.6908463E-02 point
set label "" at    298.1500    ,   0.3261050E-01 point ps 2 pt 6

plot [280:320] H(T)
