## CastleRain
L'incroyable histoire d'AlfredAl, architecte honoraire

## Lancement
Sous WSL2, les commandes suivantes sont nécessaires :
* export DISPLAY='nameserver':0.0
* export LIBGL_ALWAYS_INDIRECT=1
* (setxkbmap, seulement pour réinitialiser les touches en cas de problèmes)

où 'nameserver' est lu dans le fichier /etc/resolv.conf

Le reste de l'initialisation est contenue dans le module Init du fichier castleRain.ml (voir dans la liste des modules)

Remarque : Cette initialisation est effectuée automatiquement au lancement du jeu.

Remarque : Ce projet est loin d'être achevé, cependant les idées générales sont explicités ci-après et les fonctions principales ont été codées.

Remarque : Les fichiers de code contiennent peu (ou pas) de commentaire car cela risquait de rendre le code moins lisible (d'un point de vue subjectif)

[]Voir la partie Lancement et Test](#Lancement et Test)


# Fonctionnement Principal

## Programme Principal (Client)
Le programme principale divise les tâches entre plusieurs processus, communiquant entre eux par des fichiers avec l'extension ts (pour transfert).
Au lancement du programme, le launcher est lancé dans un nouveau processus.

## Launcher
Le launcher est censé permettre d'initialiser la connexion au serveur, ainsi que de changer les paramètres de jeu (taille de police par exemple).
* Lorsqu'on appuie sur le bouton 'Lancer une partie', il est demandé de sélectionner une partie, ou bien d'en créer une nouvelle, puis d'entrer le nombre de joueurs devant rejoindre la partie, ainsi que notre pseudo pour cette partie (si la partie a déjà été lancée, alors ces informations sont lues dans le fichier de sauvegarde et ne sont pas demandées). Le script envoie ensuite dans son fichier de sortie une chaîne de caratères sous la forme ""LAUNCH NB", où NB est le nombre de joueurs qui doivent rejoindre la partie.
* Lorsqu'on appuie sur le bouton 'Rejoindre une partie', il est demandé d'entrer l'identifiant de la partie, puis le pseudo le cas échéant (de la même façon que précédemment).
* Le bouton quitter le jeu permet de... fermer la fenêtre du launcher, qui envoie alors cette information dans son fichier de sortie.

## Programme Principal (Client)
La sortie du launcher est lue, et transmise au serveur le cas échéant, ou tue tous les processus créés avant de s'arrêter.

## Serveur (port principal : 2400)
Le serveur reçoit la commande envoyée par le client, et va alors chercher un port disponible pour créer un nouveau serveur.
Une fois ce port trouvé, il l'envoie au client.
Dans le cas contraire, il renvoie un message d'erreur. 

## Client
La réponse du serveur est lu et s'il ne s'agit pas d'un message d'erreur, le client répond en envoyant au port indiqué par le serveur le pseudo du joueur, ainsi que son emplacement dans le jeu.
Il en profite pour tuer le processus du launcher et lancer le processus du jeu (qui charge alors notamment toutes les images nécessaires, et active le chat)

## Serveur (port secondaire : 24xx)
Le serveur stocke ces informations dans des listes, contenant également les in_channel et out_channel permettant d'échanger avec le client, puis attend d'autres connexion, selon le nombre de joueurs précisé.

## Launcher
Un joueur peut alors utiliser le bouton 'Rejoindre une partie' et entrer le code pour se joindre au joueur précédent.

## Programme Principal
Dès qu'un joueur rejoint ou quitte la partie, le client devrait être prévenu afin d'afficher une animation (par exemple un sprite représentant un joueur et son pseudo rejoint ou quitte l'écran)

## Serveur (port secondaire : 24xx)
Dès que le nombre de joueur requis est atteint, le serveur l'annonce aux clients et attend de recevoir des informations, qu'il transmet (notamment les messages du chat).

Les messages commençant par "PLAYER". consituent un cas particulier Ceux-ci indiquent des informations de placement d'un personnage, et ne sont donc transmise qu'aux clients qui pourraient l'afficher, c'est-à-dire ceux qui se trouvent dans le même pièce.

Remarque : Le serveur est donc relativement peu sollicité. A terme, il devrait toutefois envoyer des messages de lui-même pour annoncer des évènements de jeu aux clients.

## Jeu
Une fois le chargement passé, le jeu peut alors commencer (ce chargement pourrait également se faire au lancement du programme principal)
Cette partie est composée de 2 scripts :
* Le chat (dont le fonctionnement est explicité dans le fichier Director/chat.txt)
* La fenêtre de jeu principale. Elle est censée :
    * Charger les textures lorsque le joueur change d'endroit, ainsi que permettre les interactions avec les PNG et les objets.
    Remarque : Ces interactions utiliseront les fonctions codés dans le programme du chat
    * Envoyer au serveur les informations de placement du joueur (par l'intermédiaire du Client) lorsque celui-ci se déplace.
    * Afficher les autres joueurs en fonction des informations reçues sur l'entrée.
    * Enregistrer les modifications de la map (car il s'agit d'un jeu de gestion / construction) et les transmettre au serveur

## Remarques générales
* Dans l'idéal, il s'agirait de lire les fichiers ainsi que les informations envoyées / reçues par le serveur en bytes, afin de pouvoir transmettre également des fichiers par exemple, et de minimiser les erreurs liées à la fonction input_line (qui semble mal fonctionner).

* Il a été possible de détecter l'appui sur les flèches de la façon suivante (ces évènements sont manifestement explicitement ignorés par Graphics, pour une raison inconnue) ->
    - on retient dans un fichier l'état actuel du clavier (ce fichier se nomme touchmem.xmm)
    - on utilise le programme xmodmap afin que l'appui sur les flèches produise un caractère peu utilisé
    - à la fin du programme, on rétablit les paramètres initiaux grâce au fichier de sauvegarde.

* Les modifications faites au niveau de la map par les joueurs ne sont pas enregistrées par le serveur, seulement par les clients (afin d'éviter d'avoir à les transmettre d'un client à l'autre, et de surcharger le serveur).

* Les fonctions nécessaires au bon fonctionnement de l'inventaire, des boutiques et des interactions en général seront des adaptations des fonctions déjà écrites pour le chat (il s'agira seulement d'afficher une image en fond)

* Seule une fonction permettant de charger des images depuis un fichier manque dans le module token, bien qu'elle ne se déduise des fonctions existantes.

# Arborescence

## Fichiers
* README.md -> ce fichier
* alias.txt -> explicite les extensions utilisées dans cette application ainsi que le contenu des fichiers concernés
* main.ml -> pas le fichier principal

## Dossiers
* Code contient les fichiers de code

    * Game -> contient les fichiers de code
    * Modules -> contient les Modules (voir plus bas)

* Images contient les fichiers relatifs aux images, notamment les fichiers en .img (qui code une matrice représentant une image)

    * backup -> contient des fichiers de sauvegarde au format png.
    * launcher -> contient des fichiers images utilisables par le launcher (extension .img)
    * Sprite -> contient des fichiers images  utilisables par la fenêtre de jeu principale (extension .img)

* Director contient les fichiers "directeurs" qui explicite le fonctionnement d'un programme (cependant, certains fichiers contiennent simplement des notes sur la façon de coder le programme en question)

## Entrées et Sorties
Les différentes parties du programme utilisent des fichiers avec des extensions en tsin/tsout, correspondant respectivement à leur entrée / sortie, afin de communiquer avec le programme principal, ou entre elles.

## Modules
Les modules sont des fichiers situés dans le dossier Code/Modules et contenant des fonctions et variables souvent utilisées.
Les modules sont :
- castlerain.ml -> contient des fonctions générales d'initialisation et de fermeture de l'application
- client.ml -> devait permettre de se connecter au serveur, jusqu'à ce que la fonction input_line ne fasse des siennes.
- player.ml -> permet d'afficher et de déplacer le joueur (à terme, permettra également les interactions avec des objets et l'inventaire, ainsi que les PNG)
- pyliste.ml -> code des tableaux dynamiques semblables à des listes Python
- server.ml -> module du serveur, contient toutes les fonctions utiles.
- sprite.ml -> permet d'afficher des images selon un ordre de priorité et de les animer.

# Lancement et Test

Les fichiers du dossier Code/Game :
* launcher.ml
* game.ml

sont disponibles à l'exécution et présentent respectivement le launcher du jeu, et la fenêtre principale ainsi que le chat.

Le fichier
* serveur.ml
contient le code du serveur (mais celui-ci n'a pas pu être testé en raison de nombreux problèmes avec la fonction input_line...)

------------

Codé par 
* Sylvain Meunier
* Gaëlle Saint-Paul

(MP2I)