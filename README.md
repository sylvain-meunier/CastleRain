# CastleRain
L'incroyable histoire d'AlfredAl, architecte honoraire

# Lancement
Sous WSL2, les commandes suivantes sont nécessaires :
setxkbmap
export DISPLAY=$nameserver$:0.0
export LIBGL_ALWAYS_INDIRECT=1

où $nameserver$ est lu dans le fichier /etc/resolv.conf

Le reste de l'initialisation est contenue dans le module Init du fichier castleRain.ml (voir dans la liste des modules)

Remarque : Cette initialisation est effectuée automatiquement au lancement du jeu.