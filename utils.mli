(** [nth_bit n byte] renvoie le [n]-ième bit de poids fort d'un [byte].

    @param n l'indice du bit voulu, on s'attend à ce que [n] soit compris entre [0] et [1].
    @param byte le byte duquel on veut extraire le bit, on s'attend à ce que [byte] s'écrive sur 8 bits.
    @return la valeur du [n]-ième bit du [byte], sa valeur est soit [0] soit [1].
    @raise Invalid_argument si [n] n'est pas compris entre [0] et [1], ou si [byte] n'est pas compris entre [0] et [255]
*)
val nth_bit : int -> int -> int

(** [lecture bin_file] affiche le contenu du fichier binaire de nom [bin_file].

    @param bin_file le nom du fichier binaire à lire.
    @raise Invalid_argument si aucun fichier dans le repertoire courant ne porte le nom de [bin_file].
*)
val lecture : string -> unit


(** [ecriture input_file output_file] encode le fichier texte [input_file] en un fichier binaire [output_file].

    @param input_file le nom du fichier texte d'extension '.txt' que l'on veut encoder.
    @param output_file le nom du fichier resultant de l'encodage, d'extension '.bin'.
    @raise Invalid_argument si aucun fichier dans le repertoire courant ne porte le nom de [input_file].
*)
val ecriture : string -> string -> unit