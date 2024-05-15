import math
# import math lo que hace es que me importa una libreria de matematica la cual contiene distintas funciones utiles
# Ejercicio 1
# 1)
def imprimir_hola_mundo(): 
    print ("¡Hola Mundo!")
# 2)
def imprimir_un_verso():
    print ("la la la \nlalalaa laal") 
# 3) 
def raizDe2(): 
    return round (math.sqrt (2),4)
# 4) 
def factorial_de_dos():
    return 2*(2-1) 
# 5) 
def perimetro ():
    return 2*math.pi 
# Ejercicio 2
# 1) 
def imprimir_saludo (nombre:str):
    print (f"Hola {nombre}")
# 2) 
def raiz_cuadrada_de (x:int)->float: 
    return math.sqrt (x)
# 3) 
def fahrenheit_a_celsius (t:float)->float: 
    return (((t)-32)*5)/9 
# 4) 
def imprimir_dos_veces():
    print (2*("la la la \nlalalaa laal\n")) 
# 5) % = resto 
def es_multiplo_de(n:int,m:int)->bool:
    return n/m ==  
# 6) 
#def es_par(n:int) -> bool: 
#    return 



imprimir_hola_mundo()
imprimir_un_verso()
print (raizDe2 ())
print (factorial_de_dos())
print (perimetro())
imprimir_saludo("lucero")
print (raiz_cuadrada_de(4))
print (raiz_cuadrada_de(5))
print (raiz_cuadrada_de(9))
print (fahrenheit_a_celsius(26))
print (fahrenheit_a_celsius(70))
imprimir_dos_veces()
print (es_multiplo_de(4,2))