# -*- coding: utf-8 -*-
"""
Created on Mon Apr 15 17:44:14 2019

@author: pierrehb
"""
from math import tan
import MyLibrairy.Vectors as v


def dot(m1, m2):
    n = len(m1)
    q = len(m2)
    r = len(m2[0])
    d=[]
    for l in range(n):
        d.append([])
        for c in range(r):
            coef = 0
            for i in range(q):
                coef += m1[l][i]*m2[i][c]
            d[-1].append(coef)
    return d

def print_matrice(m):
    print(*m, sep='\n')
    print()

def get_passage(v1, v2, v3):
    x1, y1, z1 = v1.get_coord()
    x2, y2, z2 = v2.get_coord()
    x3, y3, z3 = v3.get_coord()
    return [[x1, y1, z1],[x2, y2, z2],[x3, y3, z3]]

def get_point(passage, point, position):
    x, y, z = point.get_coord()
    xp, yp, zp = position.get_coord()
    vecteur = [[x-xp],[y-yp],[z-zp]]
    return v.Vector3D(*dot(passage, vecteur))

def get_vector(passage, vector):
    x, y, z = vector.get_coord()
    return v.Vector3D(*dot(passage, [[x],[y],[z]]))

def get_projection(screenX, screenY, d):

    def projection(point):
        x, y, z = point.get_coord()
        return screenX*x*d/z+screenX/2, screenX*y*d/z+screenY/2
    return projection

def get_d(largeur, ouverture):
    return largeur/(2*tan(ouverture/2))

I = [[1,0,0],[0,1,0],[0,0,1]]
M = [[1,2,3],[4,5,6],[7,8,9]]
E = [[0,0,0],[1,0,0],[0,0,0]]
V1 = [[1],[2],[3]]
V2 = [[10],[20],[30]]
V3 = [[100],[200],[300]]
V = [[1,10,100],[2,20,200],[3,30,300]]

print_matrice(V1)
print_matrice(V2)
print_matrice(V3)
print_matrice(dot(M,V1))
print_matrice(dot(M,V2))
print_matrice(dot(M,V3))
print_matrice(V)
print_matrice(dot(M,V))

