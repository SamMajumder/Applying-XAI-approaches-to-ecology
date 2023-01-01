# -*- coding: utf-8 -*-
"""
Created on Tue Dec  6 21:48:30 2022

@author: samba
"""

import os
import glob 
import cv2 
from plantcv import plantcv as pcv

####### FLOWER IMAGES ########

path = "D:/Images/HeliantOME/flowertop_Sam_Curated_11_18_2022/*.*"

img_number = 1  


for file in glob.glob(path):
    print(file)
    Image = cv2.imread(file,1)
    mask,masked_image = pcv.threshold.custom_range(img=Image,lower_thresh = [15,150,20],upper_thresh=[35,255,255],channel='HSV')
    cv2.imwrite("D:/Images/HeliantOME/flowertop_mask/"+str(img_number)+".png",mask)
    cv2.imwrite("D:/Images/HeliantOME/flowertop_processed/"+str(img_number)+".png",masked_image)
    img_number +=1 
    print("Done",file)



###########################
### LEAF IMAGES ####
######################## 

path = "D:/Images/HeliantOME/leaftop_Sam_curated/*.*"

img_number = 1  

for file in glob.glob(path):
    print(file)
    Image = cv2.imread(file,1)
    mask,masked_image = pcv.threshold.custom_range(img=Image,lower_thresh = [25,52,72],upper_thresh=[102,255,255],channel='HSV')
    cv2.imwrite("D:/Images/HeliantOME/leaftop_mask/"+str(img_number)+".png",mask)
    cv2.imwrite("D:/Images/HeliantOME/leaftop_processed/"+str(img_number)+".png",masked_image)
    img_number +=1 
    print("Done",file)






