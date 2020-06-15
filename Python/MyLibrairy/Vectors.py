# -*- coding: utf-8 -*-
"""
Created on Mon Mar 18 19:48:21 2019

@author: pierrehb
"""

from random import random

#==============================================================================
#Vector 3D

class Vector3D():
    def __init__(self, x, y, z, calculLenght = False):
        self.x = x
        self.y = y
        self.z = z
        self.calculLenght = calculLenght
        if calculLenght:
            self.lenght = (x**2 + y**2 + z**2)**0.5
        else:
            self.lenght = None

    def init_lenght(self):
        if not self.calculLenght:
            self.calculLenght = True
            self.lenght = (self.x**2 + self.y**2 + self.z**2)**0.5
        return self.lenght

    def dot(self, coef):
        self.x*=coef
        self.y*=coef
        self.z*=coef
        self.calculLenght = False

    def getX(self):
        return self.x
    def getY(self):
        return self.y
    def getZ(self):
        return self.z
    def get_lenght(self):
        return self.init_lenght()
    def get_coord(self):
        return self.x, self.y, self.z

    def setX(self, x):
        if self.x != x:
            self.x = x
            self.calculLenght = False
    def setY(self, y):
        if self.y != y:
            self.y = y
            self.calculLenght = False
    def setZ(self, z):
        if self.z != z:
            self.z = z
            self.calculLenght = False
    def addX(self, x):
        self.x += x
        self.calculLenght = False
    def addY(self, y):
        self.y += y
        self.calculLenght = False
    def addZ(self, z):
        self.z += z
        self.calculLenght = False

    def normalize(self):
        if self.get_lenght() != 1:
            if self.get_lenght() == 0:
                pass
#                self.x = 0.5773502691896257
#                self.y = 0.5773502691896257
#                self.z = 0.5773502691896257
            else:
                self.x /= self.lenght
                self.y /= self.lenght
                self.z /= self.lenght
                self.calculLenght = False

#==============================================================================
#Vector 2D

class Vector2D():
    def __init__(self, x, y, calculLenght = False):
        self.x = x
        self.y = y
        self.calculLenght = calculLenght
        if calculLenght:
            self.lenght = (x**2 + y**2)**0.5
        else:
            self.lenght = None

    def init_lenght(self):
        if not self.calculLenght:
            self.calculLenght = True
            self.lenght = (self.x**2 + self.y**2)**0.5
        return self.lenght

    def dot(self, coef):
        self.x*=coef
        self.y*=coef
        self.calculLenght = False

    def getX(self):
        return self.x
    def getY(self):
        return self.y
    def get_lenght(self):
        return self.init_lenght()
    def get_coord(self):
        return self.x, self.y

    def setX(self, x):
        if self.x != x:
            self.x = x
            self.calculLenght = False
    def setY(self, y):
        if self.y != y:
            self.y = y
            self.calculLenght = False
    def addX(self, x):
        self.x += x
        self.calculLenght = False
    def addY(self, y):
        self.y += y
        self.calculLenght = False

    def normalize(self):
        if self.get_lenght() != 1:
            self.x /= self.lenght
            self.y /= self.lenght
            self.calculLenght = False

#==============================================================================
#Vecto operations

def scalar(vect1, vect2):
    c1, c2 = vect1.get_coord(), vect2.get_coord()
    return sum(c1[i]*c2[i] for i in range(len(c1)))

def cross3D(vect1, vect2):
    x1, y1, z1 = vect1.get_coord()
    x2, y2, z2 = vect2.get_coord()
    x = y1*z2 - z1*y2
    y = z1*x2 - x1*z2
    z = x1*y2 - y1*x2
    return Vector3D(x, y, z)

def distance(vect1, vect2):
    c1 = vect1.get_coord()
    c2 = vect2.get_coord()
    d = sum((c1[i]-c2[i])**2 for i in range(len(c1)))
    return d**0.5

def diference3D(vect1, vect2):
    x = vect1.getX() - vect2.getX()
    y = vect1.getY() - vect2.getY()
    z = vect1.getZ() - vect2.getZ()
    return Vector3D(x, y, z)

def somme3D(vect1, vect2):
    x = vect1.getX() + vect2.getX()
    y = vect1.getY() + vect2.getY()
    z = vect1.getZ() + vect2.getZ()
    return Vector3D(x, y, z)

def dot3D(vect, coef):
    x = vect.getX()
    y = vect.getY()
    z = vect.getZ()
    return Vector3D(x*coef, y*coef, z*coef)

def dot2D(vect, coef):
    x = vect.getX()
    y = vect.getY()
    return Vector2D(x*coef, y*coef)

#==============================================================================
#Perlin Noise

UNIT = 1/2**0.5
V_UNIT = [(UNIT, UNIT), (-UNIT, UNIT), (UNIT, -UNIT), (-UNIT, -UNIT), (0,1), (1, 0), (0, -1), (-1, 0)]

def melange(n):
    numbers = {}
    for v in range(n):
        key = random()
        while key in numbers.keys(): key = random()
        numbers[key] = v
    keys = sorted(numbers.keys())
    return [numbers[key] for key in keys]

def scalaire_(xi, yi, x, y, table):
    dx = x - xi
    dy = y - yi
    vector = V_UNIT[(xi + table[yi])%8]
    return dx * vector[0] + dy * vector[1]

def generate_noise_(resolution):
    table = melange(256)
    def noise(x, y):
        x/=resolution
        y/=resolution

        x0 = int(x)
        y0 = int(y)
        x1 = x0 + 1
        y1 = y0 + 1

        i1 = x0 & 255
        j1 = y0 & 255
        i2 = x1 & 255
        j2 = y1 & 255

        s0 = scalaire_(i1, j1, x, y, table)
        s1 = scalaire_(i1, j2, x, y, table)
        s2 = scalaire_(i2, j1, x, y, table)
        s3 = scalaire_(i2, j2, x, y, table)

        dx = 3*(x - x0)**2 - 2*(x - x0)**3
        dy = 3*(y - y0)**2 - 2*(y - y0)**3
#        print(s1, s2, s3, s0)

        l1 = scale(dx, s0, s1)
        l2 = scale(dx, s2, s3)
        return scale(dy, l1, l2)
    return noise

def create_noise_(layers, offset = 0, emplitude = 1):
    perlins=[]
#    depth=len(layers)
    n = 0
    for coef, strenght, resolution in layers:
        n+= coef
        perlins.append((coef*strenght, generate_noise_(resolution)))
    def noise(x, y):
        result=0
        for strenght, perlin in perlins:
            result+= strenght * perlin(x, y)
        return offset + emplitude*result/n
    return noise



def scale(value, inf, sup):
    return (sup - inf) * value + inf

def scalaire(xi, yi, x, y, gradients):
    dx = x - xi
    dy = y - yi
    vector = gradients[xi][yi]
    return dx * vector[0] + dy * vector[1]

def perlin(x, y, gradients):
    x0 = int(x)
    x1 = x0 + 1
    y0 = int(y)
    y1 = y0 + 1

    dx = (x - x0)
    dy = (y - y0)
    dx = 3*(x - x0)**2 - 2*(x - x0)**3
    dy = 3*(y - y0)**2 - 2*(y - y0)**3

    n0 = scalaire(x0, y0, x, y, gradients)
    n1 = scalaire(x1, y0, x, y, gradients)
    n2 = scalaire(x0, y1, x, y, gradients)
    n3 = scalaire(x1, y1, x, y, gradients)
    l1 = scale(dx, n0, n1)
    l2 = scale(dx, n2, n3)
    return scale(dy, l1, l2)

def generate_gradient(n, strenght=1):
    gradients = []
    for x in range(n):
        gradients.append([])
        for y in range(n):
            x = random()
            y = (1- x**2)**0.5
            gradients[-1].append((x*strenght, y*strenght))

#            gradients[-1].append((random()*strenght, random()*strenght))
    return gradients

def generate_perlin(n, strenght=1):
    gradients = generate_gradient(n+1, strenght)
    def noise(x, y):
        return perlin(x, y, gradients)
    return noise

def generate_noise(layers):
    perlins=[]
    depth=len(layers)
    for n, s in layers:
        perlins.append((max(1, n-1), generate_perlin(n, s)))
    def noise(x, y):
        result=0
        for n, perlin in perlins:
            result+=perlin(x*n, y*n)
        return result/depth
    return noise

#==============================================================================
#Mesh

def rangint(inf, sup, n):
    step = (sup-inf)/(max(1, n-1))
    liste = []
    for i in range(n):
        liste.append(inf+i*step)
    return liste

def generate_mesh(n, vector3D=False, noise=False):
    faces = []
    dots = []
    repartition = rangint(0, 1, n+1)
    for x in repartition:
        dots.append([])
        for y in repartition:
            if vector3D:
                if noise:
                    dots[-1].append(Vector3D(x, y, noise(x,y), False))
                else:
                    dots[-1].append(Vector3D(x, y, 0, False))
            else:
                dots[-1].append(Vector2D(x, y, False))
    for i in range(n):
        for j in range(n):
            faces.append(((i, j), (i+1, j), (i+1, j+1)))
            faces.append(((i, j), (i+1, j+1), (i, j+1)))
    return dots, faces

def flatten(liste):
    if type(liste) == list or type(liste) == tuple:
        return [v for temp in liste for v in flatten(temp)]
    return [liste]


def linaerize_mesh(mesh):
    dots, face = mesh
    size = len(face[0][0])
    dim = []
    current = dots
    for i in range(1, size):
        dim.append(len(current))
        if i != size - 1: current = current[0]
    dim.append(1)
    for i in range(len(face)):
        newFace=[]
        for j in range(3):
            rank = 0
            for k in range(size):
                rank+=face[i][j][k]*dim[k]
            newFace.append(rank)
        face[i] = tuple(newFace)
    return flatten(dots), face

def move_mesh(position, mesh):
    dots, faces = mesh
    x, y, z = position.get_coord()
    for dot in dots:
        dot.addX(x)
        dot.addY(y)
        dot.addZ(z)


def generate_cube(n, size):
    dots, faces = linaerize_mesh(generate_mesh(n))
    N = len(dots)
    #UPPER FACE
    finalDots = []
    finalFaces = [f[:] for f in faces]
    for dot in dots:
        x = dot.getX()
        y = dot.getY()
        finalDots.append(Vector3D(x-0.5, y-0.5, 0.5))
    #UPPER FACE DONE
    for dot in dots:
        x = dot.getX()
        y = dot.getY()
        finalDots.append(Vector3D(y-0.5, x-0.5, -0.5))
    for f in faces:
        f_ = (f[0]+N, f[1]+N, f[2]+N)
        finalFaces.append(f_)
    #LOWER FACE DONE
    for dot in dots:
        x = dot.getX()
        y = dot.getY()
        finalDots.append(Vector3D(0.5, x-0.5, y-0.5))
    for f in faces:
        f_ = (f[0]+2*N, f[1]+2*N, f[2]+2*N)
        finalFaces.append(f_)


    for dot in dots:
        x = dot.getX()
        y = dot.getY()
        finalDots.append(Vector3D(-0.5, y-0.5, x-0.5))
    for f in faces:
        f_ = (f[0]+3*N, f[1]+3*N, f[2]+3*N)
        finalFaces.append(f_)

    for dot in dots:
        x = dot.getX()
        y = dot.getY()
        finalDots.append(Vector3D(y-0.5, 0.5, x-0.5))
    for f in faces:
        f_ = (f[0]+4*N, f[1]+4*N, f[2]+4*N)
        finalFaces.append(f_)

    for dot in dots:
        x = dot.getX()
        y = dot.getY()
        finalDots.append(Vector3D(x-0.5, -0.5, y-0.5))
    for f in faces:
        f_ = (f[0]+5*N, f[1]+5*N, f[2]+5*N)
        finalFaces.append(f_)

    for dot in finalDots:
        dot.dot(size)

    return finalDots, finalFaces

def generate_sphere(n, size):
    dots, face = generate_cube(n, 1)
    for dot in dots:
        dot.normalize()
        dot.dot(size)
    return dots, face





