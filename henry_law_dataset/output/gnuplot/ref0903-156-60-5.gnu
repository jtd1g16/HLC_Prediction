# load "ref0903-156-60-5.gnu"
# chem = "(E)-1,2-dichloroethene"

set terminal postscript eps color
set title "ref = 903; chem = (E)-1,2-dichloroethene; casrn = 156-60-5"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.9816185E-03 * exp(  -3393.299    *(1/   298.    -1/T))

set label "" at    299.3500    ,   0.9775171E-03 point
set label "" at    308.1500    ,   0.6285355E-03 point
set label "" at    319.2500    ,   0.4791567E-03 point
set label "" at    298.1500    ,   0.9816185E-03 point ps 2 pt 6

plot [290:320] H(T)
