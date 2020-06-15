# -*- coding: utf-8 -*-
"""
Created on Fri Jan 11 11:14:28 2019

@author: pierrehb
"""

from tkinter import Canvas,Tk,ALL
from datetime import datetime
import MyLibrairy.Vectors as v
import MyLibrairy.imagesSavor as ims
import MyLibrairy.projection3D as p
from random import random



TIMER = 20# le nombres de ms entre chaque boucle du programme "la periode du programme"
screenX=550
screenY=550
callBack = lambda:True
canvas=Canvas
refresh=True

workWithEnvironnement = False
environnements = None
environnementLayers = 1
callBackEnv = lambda x:1
projectionEnv = lambda x:1

recordImages = False
LINE = False
FILL = True
LINECOLOR = ims.Color(100, 100, 100)
visualiseImage = True
animatedImage = True
LOOP = True



def get_current_time():#renvoit la date en ms
    time = datetime.now()
    time = [time.year,time.month,time.day,time.hour,time.minute,time.second,time.microsecond]
    microSeconds = 0
    for i,m in enumerate([31536000000000,2592000000000,86400000000,3600000000,60000000,1000000,1]):
        microSeconds +=m*time[i]
    return microSeconds

def frequencer():#adapte la fréquence de la boucle pour qu'elle soit le plus stable dans le temps
    #(elle vas retrancher le temps d'éxécution au temps d'attente entre deux boucle)
    timer = TIMER-(get_current_time()-TIME)//1000
    if timer<=0:#la boulce est en retard, on rééxécute tout de suite le programme
        timer=1
    windows.after(timer, loop)

def loop():#la fonction qui vas s'executé 60 fois par seconde
    global TIME
    global LOOP
    TIME=get_current_time()
    if LOOP:
        if refresh:canvas.delete(ALL)
        #==================================
        #C'est ici que l'on affiche les objet... que l'on veux afficher
        callBack()
        #==================================
        if workWithEnvironnement:render_environnement()
        LOOP = animatedImage

    frequencer()

def render_environnement():
    global environnements
    size = [screenX, screenY]

    for environnement in environnements:
        if len(environnement)!=0:
            faces = sorted(environnement, key = callBackEnv)
            for face in faces:
                coord = [*projectionEnv(face.p1), *projectionEnv(face.p2), *projectionEnv(face.p3)]
                if any(0 <= coord[i] < size[i%2] for i in range(6)):
                    create_triangle_recorded(*coord, fillColor=face.color, lineColor = LINECOLOR, fill=FILL, line=LINE)
    environnements = [[] for x in range(environnementLayers)]

def load_faces(faces, environnementlayer):
    global environnements
    for face in faces:
        environnements[environnementlayer].append(face)



def ini_environnement(environnementLayers_ = 1, sortedFunction = p.sorted_face, projectionFunction=p.projection_vectorielle):
    global workWithEnvironnement
    global callBackEnv
    global projectionEnv
    global environnementLayers
    global environnements
    environnementLayers = environnementLayers_
    workWithEnvironnement = True
    callBackEnv = sortedFunction
    projectionEnv = projectionFunction
    environnements = [[] for x in range(environnementLayers)]

def create_windows(screenX_=screenX, screenY_=screenY, name='Mon  programme', callbackInteract=[]):#permet de créé une fenetre
    global windows
    global canvas
    global screenX
    global screenY
    screenX = screenX_
    screenY = screenY_

    if visualiseImage: size = [screenX,screenY]
    else: size = [50, 50]
    windows = Tk()#on créé une fenetre

    #On place notre fenetre au centre de notre ecran
    windowsX = (windows.winfo_screenwidth()-size[0])//2
    windowsY = (windows.winfo_screenheight()-size[1])//2
    windows.geometry(str(size[0])+'x'+str(size[1])+'+'+str(windowsX)+'+'+str(windowsY))

    windows.title(name)#on change le titre de la fenetre
    windows.resizable(False, False)
    #on crée un canvas qui vas nous permetre d'aficher tout ce que l'on veux
    canvas = Canvas(width=size[0], height=size[1],background='black')
    canvas.focus_force()#on met le canvas en "fentre selectionnée"
    for callback in callbackInteract:
        canvas.bind(*callback, True)
    canvas.pack()

def run_start(callback, screenX=screenX, screenY=screenY, name='Mon  programme', Timer=TIMER, callbackInteract=[], refresh_=True):
    global TIME
    global callBack
    global TIMER
    global refresh
    refresh=refresh_
    TIMER=Timer
    callBack= callback
    TIME=get_current_time()
    create_windows(screenX, screenY, name, callbackInteract)#on crée une fenetre
    windows.after(Timer, loop)#on lance la boucle infinit aprés "19 miliseconde"
    windows.mainloop()#on affiche la fenetre

def render_start(callback, nbTurn):
    i = 0
    while i< nbTurn+1:
        callback()
        if workWithEnvironnement:render_environnement()
        i+=1

def create_line(*args, **kw):
    canvas.create_line(*args, **kw)

def create_polygon(*args, **kw):
    canvas.create_polygon(*args, **kw)

def create_oval(*args, **kw):
    canvas.create_oval(*args, **kw)

def create_line_recorded(x1, y1, x2, y2, color = ims.Color(0, 0, 0)):
    if recordImages:
        ims.create_line(x1, y1, x2, y2, color)
    if visualiseImage: create_line(x1, y1, x2, y2, fill=color.get_color_str())

def create_triangle_recorded(x1, y1, x2, y2, x3, y3, fillColor=ims.Color(0, 0, 0), lineColor=ims.Color(0, 0, 0), fill=False, line=True):
    if recordImages:
        ims.create_triangle(x1, y1, x2, y2, x3, y3, fillColor, lineColor, fill, line)
    if visualiseImage:
        if line:
            if fill:
                create_polygon(x1, y1, x2, y2, x3, y3, fill = fillColor.get_color_str(), outline=LINECOLOR.get_color_str())
            else:
                create_line(x1, y1, x2, y2, x3, y3, x1, y1, fill = LINECOLOR.get_color_str())
        elif fill:
            create_polygon(x1, y1, x2, y2, x3, y3, fill = fillColor.get_color_str())

def start_record(screenX, screenY, background = False):
    global recordImages
    recordImages = True
    ims.ini_buffer(screenX, screenY, background)

def end_record():
    global recordImages
    recordImages = False

def save_buffer(file):
    ims.save(file)

def clear_buffer():
    ims.reset_buffer()

class Face():
    def __init__(self, pt1, pt2, pt3, color, material = None):
        self.p1 = pt1
        self.p2 = pt2
        self.p3 = pt3
        self.color = color
        self.material = material
        x = pt1.getX()+pt2.getX()+pt3.getX()
        y = pt1.getY()+pt2.getY()+pt3.getY()
        z = pt1.getZ()+pt2.getZ()+pt3.getZ()
        self.middle = v.Vector3D(x/3, y/3, z/3)
        self.preRender = False
        self.pixelInner = []
        self.pixelBorder = []

    def getX(self):
        return (self.p1.getX()+self.p2.getX()+self.p3.getX())/3
    def getY(self):
        return (self.p1.getY()+self.p2.getY()+self.p3.getY())/3
    def getZ(self):
        return (self.p1.getZ()+self.p2.getZ()+self.p3.getZ())/3
    def get_color(self):
        return self.material.get_color()


