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
type chara = EmptyChar | Char of Uchar.t
type btree_ = Leaf of chara * int | Node of btree * int * btree
and btree = {mutable content : btree_; mutable parent : btree option}

(** [btreeTable] est la structure pour représenter la map qui associe des chara à un btree.
*)
module CharaKey : sig
  type t = chara
  val compare : t -> t -> int
end
module CharaMap : Map.S with type key = CharaKey.t
type btreeTable = btree CharaMap.t

(** [uchar_to_string u] renvoie [u] sous forme de string
    
    @param u le Uchar que l'on veut traduire.
    @return [u] en type String *)
val uchar_to_string : Uchar.t -> string

(** [equal_btree t1 t2] vérifie si [t1] et [t2] sont les même btree
    
    @param t1 le premier arbre auquel on s'intéresse.
    @param t2 le deuxième arbre auquel on s'intéresse.
    @return true si [t1] est [t2], false sinon. *)
val equal_btree : btree ->  btree -> bool

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

(** [node_per_depth t] renvoie la liste de toutes les nodes de [t] niveau par niveau.

    @param t l'arbre auquel on s'intéresse.
    @return la liste de de toutes les nodes de [t] niveau par niveau, de gauche
    à droite. Ainsi res.nth i correspond aux nodes de [t] à la (taille de [t] - i)ème profondeur.*)
val node_per_depth : btree -> btree list list

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

    @param bTab la map contenant les références aux btree.
    @param c la clé du nouveau noeud.
    @return la table mise à jour.
*)
val insert : btreeTable -> Uchar.t -> btreeTable

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

(** [switch table t1 t2] échange l'emplacement des noeuds [t1] et [t2], et met à jour [table].

    @parem table le tableau des feuilles
    @param t1 le premier noeud à échanger.
    @param t2 le deuxième noeud à échanger.
    @return l'arbre [t] avec les noeuds [t1] et [t2] échangés.
*)
val switch : btreeTable -> btree -> btree -> btreeTable

(** [finBloc h m] étant donné un nœud [m] numéroté xm dans un AHA [h], renvoie le nœud [b] tel que W (xm) = W (xm+1), ... = W (xb) et W (xb) < W (xb+1).

    @param h l'arbre principal.
    @param m le noeud numéroté xm.
    @return le nœud [b] tel que W (xm) = W (xm+1), ... = W (xb) et W (xb) < W (xb+1)
*)
val finBloc : btree -> btree -> btree 

(** [parent t t_child] renvoie le nœud parent de [t].

    @param t le nœud dont on cherche le parent.
    @return le nœud parent de [t_child].
*)
val parent : btree -> btree

(** [chemin t_end] renvoie le chemin de [t_end] jusqu'à la racine de l'arbre.

    @param t_end le nœud de fin du chemin.
    @return la liste des nœuds du chemin de [t_end] à la racine.
*)
val chemin : btree -> btree list

(** [is_incrementable t l] vérifie si la liste de nœuds [l] est incrémentable dans l'arbre [t].

    @param t l'arbre dans lequel on vérifie l'incrémentabilité.
    @param l la liste de nœuds à vérifier.
    @return (true, None) si la liste est incrémentable, (false, m) sinon, m étant le premier element du chemin [l] tel que pds(m) == pds(m+1).
*)
val is_incrementable : btree -> btree list -> bool * btree option

(** [modification _H _table s] modifie l'arbre [_H] en insérant le caractère [s] dans la table [_table].

    @param _table le dictionnaire correspondant à l'arbre.
    @param s le caractère à insérer.
    @return la table mise à jour.
*)
val modification : btree -> btreeTable -> Uchar.t -> btreeTable

(** [traitement _H _Q _table] traite les nœuds [_H] et [_Q] dans la table [_table].

    @param _H le premier nœud à traiter.
    @param _Q le deuxième nœud à traiter.
    @param _table le dictionnaire correspondant aux nœuds.
    @return la table mise à jour.
*)
val traitement : btree -> btree -> btreeTable -> btreeTable

(** [code chara table] renvoie le code de [chara] dans l'arbre représenté par [table]

    @param chara le chara à traiter.
    @param table le dictionnaire correspondant aux nœuds.
    @return le code associé à [chara]
*)
val code : chara -> btreeTable -> int list

(*##### FONCTION POUR LE CODE INITIAL ####*)
(** [initial_code s] renvoie le code utf8 de [s] en format liste, chaque element de la
    liste correspond à un bit du code.

    @param s le caractère à traduire
    @return la liste correspondant au code de [s]
*)
val initial_code : Uchar.t -> int list