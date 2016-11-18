#######################################################
		Simulateur de Net-List
#######################################################

Compilation (on se place dans le bon répertoire):
on build le document principal par la commande :
	make
Puis on lance la simulation par :
	./netlist_simul.byte
Puis on demande de rentrer le nom du fichier à simuler
Ex: 	test/fulladder.net
On demande ensuite le nombre d'iteration à effectuer
	(taper 0 pour ne faire que le schedule)
On demande ensuite les mémoires (à rentrer sous la forme d'une chaine de 0 et de 1)
	(taper 0 si on n'utilise pas de mémoire)
Le programe écrit dans un ficier de type filename_sch.net, la net-list triée.
On demande ensuite de saisir les Inputs

/!\ /!\ /!\ /!\ /!\ /!\ /!\ /!\ /!\ /!\ /!\ /!\ 
Le simulateur ne peut pas reconnaitre si l'input est valide ou non (de même pour les ROM/RAM).
Il va donc s'éxecuter et échouer.




Une fois les tests termiés, on supprme les fichiers crées par la commande 
	make clean
