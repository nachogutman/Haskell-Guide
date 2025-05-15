# Ex 1
print("----------------")
print("Ex 1.1")
def helloWorld ():
    print("Hello World");
    
helloWorld()
    
print("----------------")
print("Ex 1.2")
def printVerse ():
    print("El Domingo ya llego a la cancha yo me voy \nMillonario yo te quiero ver Campeón! \nPor la punta vamo a entrar vas a ver el carnaval, esta fiesta nunca se va a terminar! \nY todo el mundo al compás de los bombos! \nEstán cantando la nueva canción! \nDelira el Monumental! \nYa están acá! Los Borracho del Tablón! \nHay que alentar! De corazon! \nA donde juegue el Campeón! \nY vamo vamo River Plate, hoy no podemos perder!")

printVerse()

import math

print("----------------")
print("Ex 1.3")
def sqrtOf2():
    return round( math.sqrt(2), 4)

print(sqrtOf2())

print("----------------")
print("Ex 1.4")
def factOf2() -> int:
    return 2 * 1

print(factOf2())

print("----------------")
print("Ex 1.5")
def perimeter() -> float:
    return 2 * math.pi

print(perimeter())

# Ex 2
print("----------------")
print("Ex 2.1")
def printHello(name: str):
    print(f"Hola {name}")
    
printHello("Oso")

print("----------------")
print("Ex 2.2")
def sqrtOf(num: int) -> float:
    return math.sqrt(num)

print(sqrtOf(16))

print("----------------")
print("Ex 2.3")
def fahrenheitToCelcius(temp: float) -> float:
    return (temp - 32) * 5/9

print(fahrenheitToCelcius(28))

print("----------------")
print("Ex 2.4")
def printTwice(chorus: str):
    print(2 * chorus)
    
printTwice("El Domingo ya llego a la cancha yo me voy \nMillonario yo te quiero ver Campeón! \nPor la punta vamo a entrar vas a ver el carnaval, esta fiesta nunca se va a terminar! \nY todo el mundo al compás de los bombos! \nEstán cantando la nueva canción! \nDelira el Monumental! \nYa están acá! Los Borracho del Tablón! \nHay que alentar! De corazon! \nA donde juegue el Campeón! \nY vamo vamo River Plate, hoy no podemos perder!")

print("----------------")
print("Ex 2.5")
def isMultipleOf(n: int, m: int) -> bool:
    if (n > m):
        return n % m == 0
    else:
        return m % n == 0

print(isMultipleOf(12,6))

print("----------------")
print("Ex 2.6")
def isEven(num: int) -> bool:
    return isMultipleOf(num, 2)

print(isEven(3))

print("----------------")
print("Ex 2.7")
def amountOfPizzas(people: int, min_slices: int) -> int:
    return int(round(people * min_slices / 8, 0))

print(amountOfPizzas(3, 4))
