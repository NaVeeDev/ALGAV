(** [compression input_file output_file] compresse le fichier [input_file] avec l'algorithme de huffman dynamique,
    et place le résultat dans le fichier source [output_file]

    @param input_file le nom du fichier à compresser
    @param output_file le nom du fichier source dans lequel la compression sera enregistrée
*)
val compression : string -> string -> unit