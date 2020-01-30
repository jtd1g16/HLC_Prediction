# load "ref0630-124-13-0.gnu"
# chem = "octanal"

set terminal postscript eps color
set title "ref = 630; chem = octanal; casrn = 124-13-0"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.2119364E-01 * exp(  -7407.840    *(1/   298.    -1/T))

set label "" at    283.1500    ,   0.7698001E-01 point
set label "" at    298.1500    ,   0.2269924E-01 point
set label "" at    308.1500    ,   0.9079694E-02 point
set label "" at    318.1500    ,   0.4441155E-02 point
set label "" at    298.1500    ,   0.2119364E-01 point ps 2 pt 6

plot [280:320] H(T)
