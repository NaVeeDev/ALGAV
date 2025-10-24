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
*)
type chara = EmptyChar | Char of char
type btree_ = Leaf of chara * int | Node of btree * int * btree
and btree = {mutable content : btree_}

(** [btreeTable] est la structure pour représenter la map qui associe des chara à un btree.
*)
module CharaKey : sig
  type t = chara
  val compare : t -> t -> int
end
module CharaMap : Map.S with type key = CharaKey.t
type btreeTable = btree CharaMap.t

(** [is_lte t v] vérifie si toutes les valeurs de [t] sont inférieures ou égales
    à [v].
    
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
val vals_per_depth : btree -> (int list) list

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

    @param bTab la map contenant les références aux btree..
    @param c la clé du nouveau noeud.
    @return la table mise à jour.
*)
val insert : btreeTable -> char -> btreeTable

(** [print_btree t] affiche l'arbre [t] de manière lisible.

    @param t l'arbre à afficher.
*)
val print_btree : btree -> unit

(** [mem c m] vérifie si la clé [c] est présente dans l'arbre.

    @param c la clé que l'on cherche.
    @param m le dictionnaire correspondant à l'arbre
    @return true si la clé est présente, false sinon.
*)
val mem : chara -> btreeTable -> bool

(** [update_weights t] met à jour les poids des noeuds afin que [adding_up] renvoie true.

    @param t l'arbre qu'on veut mettre à jour.
*)
val update_weights : btree -> unit 

(** [switch t1 t2] échange l'emplacement des noeuds [t1] et [t2].

    @param t1 le premier noeud à échanger.
    @param t2 le deuxième noeud à échanger.
    @return l'arbre [t] avec les noeuds [t1] et [t2] échangés.
*)
val switch : btree -> btree -> unit

(** [finBloc h m] étant donné un nœud [m] numéroté xm dans un AHA [h], renvoie le nœud [b] tel que W (xm) = W (xm+1), ... = W (xb) et W (xb) < W (xb+1).

    @param h l'arbre principal.
    @param m le noeud numéroté xm.
    @return le nœud [b] tel que W (xm) = W (xm+1), ... = W (xb) et W (xb) < W (xb+1)
*)
val finBloc : btree -> btree -> btree 
