# load "ref0630-123-38-6.gnu"
# chem = "propanal"

set terminal postscript eps color
set title "ref = 630; chem = propanal; casrn = 123-38-6"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.1319336     * exp(  -5665.540    *(1/   298.    -1/T))

set label "" at    283.1500    ,   0.3839132     point
set label "" at    298.1500    ,   0.1204046     point
set label "" at    303.1500    ,   0.9375771E-01 point
set label "" at    308.1500    ,   0.7105848E-01 point
set label "" at    318.1500    ,   0.4243770E-01 point
set label "" at    298.1500    ,   0.1319336     point ps 2 pt 6

plot [280:320] H(T)
