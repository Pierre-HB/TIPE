# -*- coding: utf-8 -*-
"""
Created on Sat Mar 23 10:15:26 2019

@author: pierrehb
"""

import MyLibrairy.Vectors as v
import MyLibrairy.imagesSavor as ims
import MyLibrairy.basicScreen as bs


light = v.Vector3D(0, 0, 0)
position = v.Vector3D(0, 0, 0)
direction = v.Vector3D(0, 0, 0)
Dx = v.Vector3D(0, 0, 0)
Dz = v.Vector3D(0, 0, 0)
lightColor = ims.Color(0, 0, 0)
screenX = 0
screenY = 0
screen = 0
PROJECTIONSHERIQUE = False
FOV = 3

def get_virtual_screen(direction, screenX, screenY):
    if direction.getX() == 0:
        Dx = v.Vector3D(1, 0, 0)
    else:
        x = direction.getX()
        y = direction.getY()
        rap = (y / x)**2 + 1
        yi = 1 / ((rap)**0.5)
        if x<0: yi*=-1
        xi = (1 - (1 / rap))**0.5

        Dx = v.Vector3D(xi, yi, 0)
    Dx.normalize()
    Dz = v.cross3D(direction, Dx)
    Dz.normalize()
#    rapport = screenX/screenX
#    Dz.dot(rapport)
    return Dx, Dz

def ini_scene(light_, position_, direction_, lightColor_, screenX_, screenY_):
    global light
    global position
    global direction
    global lightColor
    global Dx
    global Dz
    global screenX
    global screenY
    global screen
    light = light_
    position = position_
    direction = direction_
    lightColor = lightColor_
    Dx, Dz = get_virtual_screen(direction, screenX_, screenY_)
    screenX = screenX_
    screenY = screenY_
    screen = min(screenX, screenY)

def reload_screen():
    global Dx
    global Dz
    Dx, Dz = get_virtual_screen(direction, screenX, screenY)

def apply_light(color, ratio, power):
    c = color.get_color()
    r = power*ratio*lightColor.a
    r = 0 if r < 0 else r if r <=1 else 1
    c.add(lightColor.r, lightColor.g, lightColor.b, r)
    return c

def get_vectors_face(face, dots):
    vect1 = dots[face[0]]
    vect2 = dots[face[1]]
    vect3 = dots[face[2]]
#    vect1 = dots[face[0][0]][face[0][1]]
#    vect2 = dots[face[1][0]][face[1][1]]
#    vect3 = dots[face[2][0]][face[2][1]]
    return vect1, vect2, vect3

def generate_faces_colored(mesh, material):
    dots, faces = mesh
    faces_ = []
    for face in faces:
        vect1 = v.diference3D(dots[face[0]], dots[face[1]])
        vect2 = v.diference3D(dots[face[0]], dots[face[2]])
        vect = v.cross3D(vect1, vect2)
        vect.normalize()
        c = apply_light(material.get_color(), (1+v.scalar(vect, light))/2, material.get_light_power())
        faces_.append(bs.Face(*get_vectors_face(face, dots), c, material))
    return faces_

def generate_faces(mesh):
    dots, faces = mesh
    faces_ = []
    for face in faces:
        faces_.append(bs.Face(*get_vectors_face(face, dots), ims.Color(0, 0, 0)))
    return faces_

def pre_generate_face_colored(mesh, meshColor):
    dots, faces = mesh
    face = generate_faces_colored(mesh, meshColor)
    face = sorted(face, key = sorted_face)
    for face in faces:
        coord = [*projection_vectorielle(face.p1), *projection_vectorielle(face.p2), *projection_vectorielle(face.p3)]
        if any(0 <= coord[i] < size[i%2] for i in range(6)):
            create_triangle_recorded(*coord, fillColor=face.color, lineColor = LINECOLOR, fill=FILL, line=LINE)

def sorted_face(face):
    return -v.distance(face.middle, position)

def projection_vectorielle(pt):
    vector = v.diference3D(pt, position)
    if PROJECTIONSHERIQUE:
        vector.normalize()
        vector.dot(FOV)
        z = v.scalar(vector, direction)

    else:
#        z = abs(v.scalar(vector, direction))
        z = v.scalar(vector, direction)

        vector.dot(FOV/z)
    x = v.scalar(vector, Dx)
    y = v.scalar(vector, Dz)
#    x = x*(screen/2) + screenX/2
#    y = y*(screen/2) + screenY/2
    x = x*(screen/2) + screenX/2
    y = y*(screen/2) + screenY/2
#    if x<0 or y<0 or x>screen or y>screen:
#        print(x, y)
    if z<0:
        x -= screenX/2
        y -= screenY/2
        return x*screenX, y*screenY
    return x, y