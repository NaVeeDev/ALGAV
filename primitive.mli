(** [is_sorted l] vérifie que la liste [l] est triée par ordre croissant.

    @param l la liste à laquelle on s'intéresse.
    @return true si les valeurs de [l] sont triées par ordre croissant, 
    false sinon.
*)
val is_sorted : int list -> bool

(** [btree] est la structure pour représenter un arbre binaire.*)
type btree = Empty | Node of btree * (char * int) * btree

(** [is_lte t v] vérifie si toutes les valeurs de [t] sont inférieures ou égales
    à v.
    
    @param t l'arbre auquel on s'intéresse.
    @param v la valeur pour laquelle on veut vérifier la propriété.
    @return true si toutes les valeurs de [t] sont inférieures ou égales à [v],
    false sinon. *)
val is_lte : btree -> int -> bool

(** [is_gte t v] vérifie si toutes les valeurs de [t] sont supérieures ou égales
    à v.
    
    @param t l'arbre auquel on s'intéresse.
    @param v la valeur pour laquelle on veut vérifier la propriété.
    @return true si toutes les valeurs de [t] sont supérieures ou égales à [v],
    false sinon. *)
val is_gte : btree -> int -> bool 

(** [max t] renvoie la valeur maximale de l'arbre [t].
    @param t l'arbre auquel on s'intéresse.
    @return la valeur maximale de l'arbre.
*)
val max_t : btree -> int

(** [vals_per_depth t] renvoie la liste de toutes les valeurs de [t] niveau par niveau.

    @param t l'arbre auquel on s'intéresse.
    @return la liste de de toutes les valeurs de [t] niveau par niveau, de gauche
    à droite. Ainsi res.nth i correspond aux valeurs de [t] à la i-ème profondeur.*)
val vals_per_depth : btree -> int -> (int list) list

(** [is_gdbh t] indique si l'arbre donné [t] est bien un arbre gdbh.

    @param t l'arbre auquel on s'intéresse.
    @return true si l'arbre est gdbh, false sinon.
*)
val is_gdbh : btree -> bool

(** [is_aha t] indique si l'arbre donné [t] est bien un AHA (Arbre de Huffman
    Adaptatif).
    
    @param t l'arbre auquel on s'intéresse.
    @return true si l'arbre est binaire, false sinon.
    *)
val is_aha : btree -> bool

(** [insert t node] insert dans l'arbre [t] le noeud [node].

    @param t l'arbre gdbh dans lequel on veut insérer un nouveau noeud.
    @param node le nouveau noeud.
    @return l'arbre gdbh avec son nouveau noeud. Si un noeud de [t] contenait déja
    un noeud dont la clé est la même que celle de [node], alors [insert] met
    seulement à jour la valeur du noeud avec celle de [node].
*)
val insert : btree -> (char * int) -> btree

(** [switch t c1 c2] échange les places des noeuds de clés [c1] et [c2] dans
    l'arbre [t].
    
    @param t l'arbre dans lequel on veut échanger les noeuds.
    @param c1 la clé d'un des noeuds qu'on souhaite échanger.
    @param c2 la clé d'un autre des noeuds qu'on souhaite échanger.
    @return l'arbre avec les noeuds échangés. L'arbre obtenun'est pas forcément
    un arbre gdbh. 
*)
val switch : btree -> char -> char -> btree