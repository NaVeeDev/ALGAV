(*##### FONCTIONS POUR LES LISTES ####*)

(** [is_sorted l] vérifie que la liste [l] est triée par ordre croissant.

    @param l la liste à laquelle on s'intéresse.
    @return true si les valeurs de [l] sont triées par ordre croissant, 
    false sinon.
*)
val is_sorted : int list -> bool


(*##### FONCTIONS POUR LES ARBRES BINAIRES ####*)

(** [btree] est la structure pour représenter un arbre binaire.
    Seules les feuilles contiennent un char.
    Les noeuds internes sont représentés par un None.
*)
type btree = Empty | Node of btree * (char option* int) * btree

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

(** [is_gdbh t] indique si l'arbre donné [t] est bien un arbre qui suit la hierarchie gdbh.

    @param t l'arbre auquel on s'intéresse.
    @return true si l'arbre est gdbh, false sinon.
*)
val is_gdbh : btree -> bool

(** [is_adding_up t] vérifie si l'arbre [t] respecte la propriété qui dit que chaque noeud
    parent a pour valeur la somme des valeurs de ses deux enfants.
    @param t l'arbre auquel on s'intéresse.
    @return true si l'arbre respecte la propriété, false sinon.
*)
val is_adding_up : btree -> bool

(** [insert t c] insert dans l'arbre [t] le noeud dont la clé est [c] et la valeur 1.
    Si [c] est déjà présente dans l'arbre, la valeur du noeud correspondant est incrémentée de 1.

    @param t l'arbre gdbh dans lequel on veut insérer un nouveau noeud.
    @param c la clé du nouveau noeud.
    @return l'arbre gdbh mis à jour.
*)
val insert : btree -> char -> btree

(** [switch t c1 c2] échange les places des noeuds de clés [c1] et [c2] dans
    l'arbre [t].
    
    @param t l'arbre dans lequel on veut échanger les noeuds.
    @param c1 la clé d'un des noeuds qu'on souhaite échanger.
    @param c2 la clé d'un autre des noeuds qu'on souhaite échanger.
    @return l'arbre avec les noeuds échangés. L'arbre obtenun'est pas forcément
    un arbre gdbh. 
*)
val switch : btree -> char -> char -> btree

(** [print_btree t] affiche l'arbre [t] de manière lisible.

    @param t l'arbre à afficher.
*)
val print_btree : btree -> unit

(** [mem t c] vérifie si la clé [c] est présente dans l'arbre [t].

    @param t l'arbre dans lequel on cherche la clé.
    @param c la clé que l'on cherche.
    @return true si la clé est présente, false sinon.
*)
val mem : btree -> char -> bool