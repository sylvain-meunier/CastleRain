import matplotlib.pyplot as plt
import numpy as np
import os

for name in os.listdir("./Sprite") :
    if name[-4:] == ".png" :
        im = plt.imread("./Sprite/" + name)
        x, y, z = np.shape(im)

        im = im * 255

        with open("./Sprite/" + name[:-4] + ".img", 'w') as f :
            f.write(str(x) + " " + str(y) + "\n") # en-tete : dimensions de la matrice
            for i in im :
                for j in i :
                    if j[0] == j[1] and j[1] == j[2] and j[2] == 255 :
                        f.write("-1\n")
                    else :
                        color = int(j[0]*256**2 + j[1]*256 + j[2])
                        f.write(str(color) + "\n")