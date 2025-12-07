(** [decompression input_file output_file] decompresse le fichier [input_file] avec l'algorithme de huffman dynamique,
    et place le résultat dans le fichier source [output_file]

    @param input_file le nom du fichier à decompresser
    @param output_file le nom du fichier source dans lequel la decompression sera enregistrée
*)
val decompression : string -> string -> bool -> unit