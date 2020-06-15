# -*- coding: utf-8 -*-
"""
Created on Mon Mar 18 20:41:45 2019

@author: pierrehb
"""
import MyLibrairy.Vectors as v
import MyLibrairy.basicScreen as bs
import MyLibrairy.imagesSavor as ims
import MyLibrairy.projection3D as p
import MyLibrairy.projection3D_matriciel as pm
from math import cos, sin
screenX = 1920
screenY = 1080
screen = min(screenX, screenY)
dt = 1
time = 0
RECORDED = False
bs.visualiseImage = True
bs.animatedImage = True
fileName ="test"
resoluion = 10
itemResolution = 2
bs.FILL = True
bs.LINE = True
nbImages = 10
MOVEMOUSE = False
p.PROJECTIONSHERIQUE = True
p.FOV = 2

light = v.Vector3D(10,-5,-3)
light.normalize()
lightColor = ims.Color(200, 200, 200, 0.8)

waterColorMin = ims.Color(0, 0, 50)
waterColorMax = ims.Color(0, 0, 50)
wallColorMin = ims.Color(96, 80, 80)
wallColorMax = ims.Color(96, 80, 80)
itemColorMin = ims.Color(107, 103, 0)
itemColorMax = ims.Color(110, 110, 10)

waterMaterial = ims.Material(waterColorMin, waterColorMax, 1)
wallMaterial = ims.Material(wallColorMin, wallColorMax, 1)
itemMaterial = ims.Material(itemColorMin, itemColorMax, 1)



solidWall = False

position = v.Vector3D(0.8, -0.8, 0.55)
direction = v.Vector3D(0.3, 1, -0.35)
direction.normalize()


wallVects = [[v.Vector3D(x, y, z-0.6) for x in range(2) for y in range(2) for z in range(2)]]
wallPoints = [((0,3), (0,6), (0,7)), ((0,3), (0,2), (0,6)), ((0,1), (0,2), (0,3)), ((0,1), (0,0), (0,2))]

wallMesh = (wallVects, wallPoints)



UP = False# Vrai si la touche UP est appuyer
DOWN = False#pareil
RIGHT = False#...
LEFT = False
FORWARD = False
BACKWARD = False
dx=0
dy=0
mouseX = 0
mouseY = 0

def move_mouse(event):
    global dx
    global dy
    global mouseX
    global mouseY
    dx+=(event.x - mouseX)/200
    dy+=(event.y - mouseY)/200


    mouseX = event.x
    mouseY = event.y

def press(event):#detecte quand on appuit sur une touche
    key = event.keysym
    if key == "space":
        global UP
        UP = True
    elif key == "x":
        global DOWN
        DOWN = True
    elif key == "d":
        global RIGHT
        RIGHT = True
    elif key == "q":
        global LEFT
        LEFT = True
    elif key == "z":
        global FORWARD
        FORWARD = True
    elif key == "s":
        global BACKWARD
        BACKWARD = True


def release(event):#detecte quand on relache une touche
   key = event.keysym
   if key == "space":
        global UP
        UP = False
   elif key == "x":
        global DOWN
        DOWN = False
   elif key == "d":
        global RIGHT
        RIGHT = False
   elif key == "q":
        global LEFT
        LEFT = False
   elif key == "z":
        global FORWARD
        FORWARD = False
   elif key == "s":
        global BACKWARD
        BACKWARD = False


def move():
    step = 0.05
    global position
    if UP: position.addZ(step)
    if DOWN: position.addZ(-step)
    if LEFT: position.addX(-step)
    if RIGHT: position.addX(step)
    if FORWARD: position.addY(step)
    if BACKWARD: position.addY(-step)
    if MOVEMOUSE :
        xd = sin(dy)*cos(dx)
        yd = sin(dy)*sin(dx)
        zd = cos(dy)
        direction.setX(xd)
        direction.setY(yd)
        direction.setZ(zd)
        direction.normalize()
        p.reload_screen()

#========================
def update_mesh_3D(mesh):
    cinetique, mesh_ = mesh
    dots, faces = mesh_
    r=resoluion+1
    newDots = []
    n = len(dots)
    for i in range(n):
        vz, az = cinetique[i]
        vector = dots[i]
        d = 0
        xi, yi, zi = vector.get_coord()
        if i%r != r - 1: d+= dots[i+1].getZ() - zi
        elif solidWall: d+= dots[i-1].getZ() - zi
        if i%r != 0: d+= dots[i-1].getZ() - zi
        elif solidWall: d+= dots[i+1].getZ() - zi
        if i//r != r - 1: d+= dots[i+r].getZ() - zi
        elif solidWall: d+= dots[i-r].getZ() - zi
        if i//r != 0: d+= dots[i-r].getZ() - zi
        elif solidWall: d+= dots[i+r].getZ() - zi
        az=d/8
        az/=2
        vz+=az*dt
        zi+=vz*dt
        cinetique[i] = (vz, az)
        newDots.append( v.Vector3D(xi, yi, zi, False))
    return cinetique, (newDots, faces)




def main():
    global mesh
    global time
    global i
    move()
    print(direction.get_coord(), dx, dy)

    time+=dt
    cinetique, mesh_ = mesh

    if bs.FILL:
        faces = p.generate_faces_colored(mesh_, waterMaterial)
        wall = p.generate_faces_colored(wallMesh, wallMaterial)
        shere = p.generate_faces_colored(shereMesh, itemMaterial)
    else:
        faces = p.generate_faces(mesh_)
        wall = p.generate_faces(wallMesh)
        cube = p.generate_faces(cubeMesh)
        shere = p.generate_faces(shereMesh)
    bs.load_faces(faces,1)

    bs.load_faces(wall,0)



    bs.load_faces(shere, 1)


    bs.render_environnement()
    if RECORDED:
        bs.save_buffer(fileName+str(i))
        i+=1
        bs.clear_buffer()
    mesh = update_mesh_3D(mesh)


def cinetyse_mesh(mesh):
    dots, faces = mesh
    newMesh = []
    for dot in dots:
        newMesh.append((0,0))
    return newMesh, mesh

noise_ = v.create_noise_([(1,0.1,0.645678),(1,0.1,0.5678), (1,1,1), (0.5,0.02,0.0345678)], 0, 0.8)
mesh = v.generate_mesh(resoluion, True, noise_)

mesh = v.generate_mesh(resoluion, True, v.generate_noise([[2,2], [3,0.5], [5,0.5],[7,0.25], [2, 5]]))
mesh = v.linaerize_mesh(mesh)
mesh = cinetyse_mesh(mesh)

wallMesh = v.linaerize_mesh(wallMesh)


cubeMesh = v.generate_cube(itemResolution, 10)

shereMesh = v.generate_sphere(itemResolution, 0.2)
v.move_mesh(v.Vector3D(0.7, 0.5, 0), shereMesh)


i = 0


bs.ini_environnement(2)
p.ini_scene(light, position, direction, lightColor, screenX, screenY)
if RECORDED:bs.start_record(screenY, screenX, background = (0, 0, 0))
if bs.visualiseImage: bs.run_start(main, screenX, screenY,"Wave", 10, [("<KeyPress>", press), ("<KeyRelease>", release), ("<Motion>", move_mouse)])
else :bs.render_start(main, nbImages)

