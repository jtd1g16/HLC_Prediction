# load "ref0903-287-92-3.gnu"
# chem = "cyclopentane"

set terminal postscript eps color
set title "ref = 903; chem = cyclopentane; casrn = 287-92-3"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.6493828E-04 * exp(  -3365.880    *(1/   298.    -1/T))

set label "" at    301.0500    ,   0.6017933E-04 point
set label "" at    308.9500    ,   0.4112180E-04 point
set label "" at    318.1500    ,   0.3289690E-04 point
set label "" at    298.1500    ,   0.6493828E-04 point ps 2 pt 6

plot [300:320] H(T)
