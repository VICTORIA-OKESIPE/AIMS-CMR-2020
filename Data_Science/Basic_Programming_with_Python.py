#VICTORIA OKESIPE
#Programming in python.
#Week 1

#----------------------------------------------------------------------------------------
#Question 1:
#Write a Python program to get a string from the user and
#outputs a string made of its first 3 and its last 2 characters.
#Sample String : 'I am from Ethiopia'
#Expected Result : 'I aia'


#Answer to Question 1:
st = input("Enter your string")
first_3_characters = st[0:3]
last_2_characters = st[-2:]
print(first_3_characters + last_2_characters) 


#----------------------------------------------------------------------------------------
#Question 2:
#Write a Python program to get a string from the user and change
#its first character to '#'
#Sample String : 'Cameroon'
#Expected Result : '#ameroon'



#Answer to Question 2:
st = input("Enter your string")
st = "#" + st[1:]
print(st)

#----------------------------------------------------------------------------------------
#Question 3:
#Write a Python program to get an integer n and a string s from
#the user and removes the nth index character from the given
#string.



#Answer to Question 3:
n = int(input("Enter an integer"))
s = input("Enter your string")
s = s[0:n] + s[n+1:]
print(s)


#----------------------------------------------------------------------------------------
#Question 4:
#Write a Python program to change a given string to a new string
#where the first and last chars have been exchanged.
#Sample String : 'I am a smart student'
#Expected Result : 't am a smart studenI'



#Answer to Question 4:
st = input("Enter your string")
last_char = st[-1]
first_char = st[0]
st = last_char + st[1:len(st)-1] + first_char
print(st)


#----------------------------------------------------------------------------------------
#Question 5:
#Write a Python program to get a string from the user and
#outputs a string made of 4 copies of the last two characters of a
#specified string (length must be at least 2).
#Sample String : 'Python'
#Expected Result : 'onononon'



#Answer to Question 5:
st = input("Enter your string")
print(4*st[-2:])


#----------------------------------------------------------------------------------------
#Question 6:
#Write a Python program to format a number n in [0,1] with a
#percentage.
#Sample input: 0.1
#Expected Result : 10%



#Answer to Question 6:
x = float(input("Enter your number"))          #finding percentage of number between 0 and 1
if x < 0 or x > 1:
	print("syntax error")
else:
	print("{}%".format(100*x))



#----------------------------------------------------------------------------------------
#Question 7:
#Write a Python program to compute the area of a rectangle and
#outputs the result with the unit.
#Sample output: The area of the rectangle is 56.86cm2



#Answer to Question 7:
length = float(input("Enter the size of the length of the rectangle"))
breadth = float(input("Enter the size of the breadth of the rectangle"))
print("The area of the rectangle is {}cm2".format(length*breadth))


#----------------------------------------------------------------------------------------
#Question 8:
#Write a Python program to compute the volume of a cylinder
#and outputs the result with the unit.
#Sample output: The volume of the cylinder is 89.25cm3


#Answer to Question 8:
height = float(input("Enter the size of the height of the cylinder"))
radius = float(input("Enter the size of the radius of the cylinder"))

import math as m
pi = m.pi
print("The volume of the cylinder is {}cm3".format(pi*(radius**2)*height))


#----------------------------------------------------------------------------------------
#Question 9:
#Write a Python program to print the following pattern:
#*
#**
#***
#****
#*****



#Answer to Question 9:
print("*\n**\n***\n****\n*****")



#----------------------------------------------------------------------------------------
#Question 10:
#Write a Python program to get two integers n and m from the
#user and outputs n raised to power m.



#Answer to Question 10:
n = int(input("Enter your first integer"))
m = int(input("Enter your second integer"))
print(n**m)



#----------------------------------------------------------------------------------------
#Question 11:
#Let x=246897531 Use the modulo and integer division operators
#to write a single expression to produce the hundreds digit of x
#(and which will continue working if you change the value of x).



#Answer to Question 11:
x = float(input("Enter your number"))
x_modulo = x%1000
x_modulo_quotient = x_modulo//100
print(x_modulo_quotient)


#getting the thousands digit of x
x = 246897531
x_1 = x%10000
x_2 = x_1//1000
print(x_2)


#getting the hundreds digit of x
x = 246897531
x_1 = x%1000
x_2 = x_1//100
print(x_2)

#getting the tens digit of x
x = 246897531
x_1 = x%100
x_2 = x_1//10
print(x_2)

#getting the unit digit of x
x = 246897531
x_1 = x%100
x_2 = x_1%10
print(x_2)
#----------------------------------------------------------------------------------------
#Question 12:
#Write a program to get a length given in meters from the user
#and then compute and outputs the corresponding length measured in
#inches. Note: one inch is 2.54 cm



#Answer to Question 12:
number = float(input("Enter your number"))
number *= 100/2.54
print(number)


#----------------------------------------------------------------------------------------
#Question 13:
#Let p be a bank’s interest rate in percent per year. An
#initial amount A has then grown to (equation of compound interest is here)
#after n years. Make a program for computing how much money 1000
#XAF have grown to after four years with 15 percent interest rate.


#Answer to Question 13:
A = 1000
p = 15
n = 4

compound_interest = A*(1+(p/100))**n
print(compound_interest)



#----------------------------------------------------------------------------------------
#Question 14:Write function, a program for evaluating the bell-shaped Gaussian with parameters m = 0, s = 2 and x = 1.
#Verify your program’s result by comparing with hand calculations on a calculator.



#Answer to Question 14:
import math 
pi = math.pi
exp = math.exp
sqrt = math.sqrt

m = 0 
s = 2
x = 1

f_x = (1/(sqrt(2*pi)*s))*(exp((-1/2)*((x-m)/s)**2))
print(f_x)

#verifying my result by comparing with hand calculations on a calculator made me conclude
#that "python's precision is better than calculator's.


#verifying my result by comparing with hand calculations on a calculator made me conclude
#that "python's precision is better than calculator's.

#---------------------------------------------------------------------------------------








