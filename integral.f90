program integrale
implicit none
integer::i,n
double precision::inter
real::a,b,som1,som2,h,x
h=0.0
inter=0.0e15
print*,"entrer les bornes de l'integrale"
read*,a,b
print*,"entrer le nombre de division de l'intervale"
read*,n
h=(b-a)/n
som1=0.0
som2=0.0
do i=1,n-1
x=a+i*h
if((i/2)*2==i)then
som2=som2+2*f(x)
else
som1=som1+4*f(x)
end if
end do
inter=(h/3)*(f(a)+f(b)+som1+som2)
print*,'une valeur approchee de cette integrale est:',inter
contains
function f(x)
real::f,x
f=4.0/(1.0+x**2)
end function
end program integrale

