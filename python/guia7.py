from typing import List, Dict, Tuple

# Ex 1.1
def belongs1(list: List[int], num: int) -> bool:
    for i in list:
        if (num == i):
            return True

    return False
            

def belongs2(list: List[int], num: int) -> bool:
    i: int = 0
    while (i < len(list)):
        if (num == list[i]):
            return True
        else:
            i = i + 1

    return False

print(belongs1([1,2,3,4], 2))
print(belongs2([1,2,3,4], 5))

# Ex 1.2
def divide_everyone(list: List[int], num: int) -> bool:
    for i in list:
        if (i % num != 0):
            return False
        
    return True

print(divide_everyone([2,4,6,8,11], 2))

# Ex 1.3
def total_sum(list: List[int]) -> int:
    res: int = 0
    for i in list:
        res += i

    return res

print(total_sum([2,4,1]))
    
# Ex 1.4
def max(list: List[int]) -> int:
    res: int = 0
    for i in list:
        if(i >= res):
            res = i

    return res

print(max([4,9,2,11,1]))

# Ex 1.5
def min(list: List[int]) -> int:
    res: int = 999999999999999999
    for i in list:
        if(i <= res):
            res = i

    return res

print(min([4,9,2,11,1]))

# Ex 1.6
def is_ordered(list: List[int]) -> bool:
    aux: int = 0
    for i in list:
        if(i >= aux):
            aux = i
        else:
            return False

    return True

print(is_ordered([1,7,12,8,9,11]))

# Ex 1.7
def pos_max(list: List[int]) -> int:
    for i in range(len(list)):
        if (max(list) == list[i]):
            return i
    
    return -1
        
print(pos_max([4,9,2,11,1]))

# Ex 1.8
def pos_min(list: List[int]) -> int:
    for i in range(len(list)):
        if (min(list) == list[i]):
            return i
    
    return -1

print(pos_min([4,9,2,11,1]))

# Ex 1.9
def word_len_over_7(list: List[str]) -> bool:
    for i in list:
        if (len(i) > 7):
            return True

    return False

print (word_len_over_7(["termo", "gato", "tener", "jirafas", "homoplato"]))

# Ex 1.10
def is_palindrome(text: List[chr]):
    return text == reverse_string(text)

def reverse_string(text: List[chr]):
    aux: str = ""
    for i in text:
        aux = i + aux
    return aux

print(is_palindrome("apopa"))

# Ex 1.11
def consecutive_equals(list: List[int]) -> bool:
    for i in range((len(list)-2)):
        if(list[i] == list[i+1] == list[i+2]):
            return True
    return False

print(consecutive_equals([1,2,3,1,1,1,3,4]))

# Ex 1.12
def different_vowels(text: List[chr]) -> bool:
    aux: List[chr] = []
    for i in text:
        if(i == 'a' or i == 'e' or i == 'i' or i == 'o' or i == 'u'):
            if(not belongs(aux, i)):
                aux.append(i)

    return len(aux) >= 3 

def belongs(list: List, x) -> bool:
    for i in list:
        if (x == i):
            return True

    return False
            
print(different_vowels(['a','e','e','e','e', 'h', 'g']))

# Ex 1.13
#def pos_longer_ordered_sequence()

# Ex 1.14
def cant_odd_digits(list: List[int]) -> int:
    cont: int = 0
    for i in list:
        if (i % 2 != 0):
            cont = cont + 1

    return cont

print(cant_odd_digits([1,5,8,9,6,3,6]))