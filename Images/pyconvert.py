import matplotlib.pyplot as plt
import numpy as np
import os

path = "./Launcher/" # Chemin où chercher des fichiers en .png

for name in os.listdir(path) :
    if name[-4:] == ".png" and name[:4] == "bout" : # filtres afin de ne pas convertir tous les fichiers à chaque exécution
        im = plt.imread(path + name)
        x, y, z = np.shape(im)

        im = im * 255 # On passe de [0; 1] à [0; 255]

        with open(path + name[:-4] + ".img", 'w') as f :
            f.write(str(x) + " " + str(y) + "\n") # en-tete : dimensions de la matrice
            for i in im :
                for j in i :
                    if j[0] >= 250 and j[1] >= 250 and j[2] >= 250 : # les pixels de blancs pur sont convertis en pixel transparent pour Ggraphics
                        f.write("-1\n")
                    else :
                        color = int(j[0]*256**2 + j[1]*256 + j[2]) # les autres pixels sont convertis en entiers compréhensibles par Graphics
                        f.write(str(color) + "\n")