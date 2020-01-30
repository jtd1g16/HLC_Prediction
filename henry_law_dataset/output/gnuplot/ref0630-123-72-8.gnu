# load "ref0630-123-72-8.gnu"
# chem = "butanal"

set terminal postscript eps color
set title "ref = 630; chem = butanal; casrn = 123-72-8"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.9474968E-01 * exp(  -6218.028    *(1/   298.    -1/T))

set label "" at    283.1500    ,   0.3089070     point
set label "" at    298.1500    ,   0.8586232E-01 point
set label "" at    303.1500    ,   0.6316309E-01 point
set label "" at    308.1500    ,   0.4835924E-01 point
set label "" at    318.1500    ,   0.2763385E-01 point
set label "" at    298.1500    ,   0.9474968E-01 point ps 2 pt 6

plot [280:320] H(T)
