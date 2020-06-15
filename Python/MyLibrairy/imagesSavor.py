# -*- coding: utf-8 -*-
"""
Created on Thu Mar 21 16:07:32 2019

@author: pierrehb
"""

from random import random

imageBuffer = []
backgroundColor = (255, 255, 255)

class Color():
    def __init__(self, r, g, b, a = 1):
        self.r = r
        self.g = g
        self.b = b
        self.a = a

    def add(self, r, g, b, a = 1):
        self.r = (1-a)*self.r + a*r
        self.g = (1-a)*self.g + a*g
        self.b = (1-a)*self.b + a*b
    def add_color(self, color):
        r = color.r
        g = color.g
        b = color.b
        a = color.a
        self.add(r, g, b, a)

    def reset(self, r, g, b, a = 1):
        self.r = r
        self.g = g
        self.b = b
        self.a = a

    def get_color(self):
        return Color(self.r, self.g, self.b, self.a)

    def get_color_int(self):
        return int(self.r), int(self.g), int(self.b)

    def convert_int_to_str(self, value):
        r = hex(int(value))[2:]
        if r[0]=='x':print(value)
        return r if len(r)==2 else '0'+r if len(r)==1 else 'ff'

    def get_color_str(self):
        r = self.convert_int_to_str(self.r)
        g = self.convert_int_to_str(self.g)
        b = self.convert_int_to_str(self.b)
        return '#'+r+g+b
    def get_color_ppm(self):
        r, g, b = self.get_color_int()
        return str(r)+' '+str(g)+' '+str(b)

class Material():
    def __init__(self, colorMin, colorMax, lightPower):
        self.colorMin = colorMin
        self.colorMax = colorMax
        self.lightPower = lightPower
    def get_color(self):
        cm, cM = self.colorMin, self.colorMax
        rm, gm, bm = cm.get_color_int()
        rM, gM, bM = cM.get_color_int()
        am = cm.a
        aM = cM.a
        r = int((rM - rm)*random() +rm)
        g = int((gM - gm)*random() +gm)
        b = int((bM - bm)*random() +bm)
        a = random()*(aM-am) +am
        return Color(r, g, b, a)
    def get_light_power(self):
        return self.lightPower


def ini_buffer(screenY, screenX, background = False):
    global imageBuffer
    global backgroundColor
    if background: backgroundColor = background
    r, g, b = backgroundColor
    imageBuffer = [[Color(r, g, b) for x in range(screenX)] for y in range(screenY)]

def reset_buffer():
    y = len(imageBuffer)
    x = len(imageBuffer[0])
    ini_buffer(y, x, backgroundColor)

def save(file):
    if not (len(file) >=4 and file[-4] == '.'):
        file+='.ppm'
    try:
        fichier = open(file,"x")
    except:
        fichier = open(file,"w")

    X = len(imageBuffer[0])
    Y = len(imageBuffer)
    fichier.write('P3 '+str(X)+' '+str(Y)+' 255')
    for y in range(Y):
        fichier.write('\n')
        for x in range(X):
            fichier.write(imageBuffer[y][x].get_color_ppm() + ' ')
    fichier.close()

def create_pixel(x, y, color):
    imageBuffer[y][x].add_color(color)

def create_line(x1, y1, x2, y2, color = Color(0, 0, 0)):
    d = ((x1 - x2)**2 + (y1 - y2)**2)**0.5
    v = ((x1-x2)/d, (y1-y2)/d)

    p = (x2, y2)
    t = 0
    xi = None
    yi = None
    x = int(x1)
    y = int(y1)
    while (xi != x or yi != y) and t < d+1:
        x0 = int(t*v[0]+p[0])
        y0 = int(t*v[1]+p[1])
        if x0 != xi or y0 != yi:
            create_pixel(x0, y0, color)
        xi = x0
        yi = y0
        t += 1

def create_triangle(x1, y1, x2, y2, x3, y3, fillColor = Color(0, 0, 0), lineColor = Color(0, 0, 0), fill = False, line = True):

    x1_, x2_, x3_ = int(x1), int(x2), int(x3)
    y1_, y2_, y3_ = int(y1), int(y2), int(y3)
    xoffset = min(x1_, x2_, x3_)
    yoffset = min(y1_, y2_, y3_)
    X = max(abs(x1_-x2_), abs(x2_-x3_), abs(x3_-x1_))+1
    Y = max(abs(y1_-y2_), abs(y2_-y3_), abs(y3_-y1_))+1

    xs = [x1, x2, x3, x1_, x2_, x3_]
    ys = [y1, y2, y3, y1_, y2_, y3_]
    buffer = [[0 for x in range(X)] for y in range(Y)]

    for k in range(3):
        i1, i2 = xs[k], xs[(k+1)%3]
        j1, j2 = ys[k], ys[(k+1)%3]

        d = max(1,((i1 - i2)**2 + (j1 - j2)**2)**0.5)*2
        v = ((i1-i2)/d, (j1-j2)/d)
        p = (i2, j2)
        t = 0
        i_, j_, i, j = None, None, xs[(k)%3 + 3], ys[(k)%3 + 3]

        while (i_ != i or j_ != j):

            i_ = int(t*v[0]+p[0])
            j_ = int(t*v[1]+p[1])
            if not(0<= i_-xoffset < X and 0<= j_-yoffset < Y):
                break
            buffer[j_-yoffset][i_-xoffset] = 1 if line else -1
            t += 1

    if fill:
        locations = [(x,0) for x in range(X)]+[(x,Y-1) for x in range(X)]+[(0,y) for y in range(Y)]+[(X-1,y) for y in range(Y)]
        while len(locations) != 0:
            newLocations = []
            for x, y in locations:
                if buffer[y][x] == 0:
                    buffer[y][x] = -2
                    if y > 0: newLocations.append((x, y-1))
                    if y < Y-1:newLocations.append((x, y+1))
                    if x > 0: newLocations.append((x-1, y))
                    if x < X-1:newLocations.append((x+1, y))
            locations = newLocations

    for x in range(X):
        for y in range(Y):
            if 0<=x+xoffset<len(imageBuffer[0]) and 0<=y+yoffset<len(imageBuffer):
                if buffer[y][x] == 1:
                    create_pixel(x+xoffset, y+yoffset, lineColor)
                elif fill and (buffer[y][x] == -1 or buffer[y][x]==0):
                    create_pixel(x+xoffset, y+yoffset, fillColor)
