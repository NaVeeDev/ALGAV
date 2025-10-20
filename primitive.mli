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

(** [is_binary t] indique si l'arbre donné [t] est bien un arbre binaire.

    @param t l'arbre auquel on s'intéresse.
    @return true si l'arbre est binaire, false sinon.
*)
val is_binary : btree -> bool

(** [is_aha t] indique si l'arbre donné [t] est bien un AHA (Arbre de Huffman
    Adaptatif).
    
    @param t l'arbre auquel on s'intéresse.
    @return true si l'arbre est binaire, false sinon.
    *)
val is_aha : btree -> bool

(** [insert t node] insert dans l'arbre [t] le noeud [node].

    @param t l'arbre dans lequel on veut insérer un nouveau noeud.
    @param node le nouveau noeud.
    @return l'arbre avec son nouveau noeud. Si un noeud de [t] contenait déja
    un noeud dont la clé est la même que celle de [node], alors [insert] met
    seulement à jour la valeur du noeud avec celle de [node].
    @raise Invalid_argument si [t] n'est pas binaire
*)
val insert : btree -> (char * int) -> btree

(** [switch t c1 c2] échange les places des noeuds de clés [c1] et [c2] dans
    l'arbre [t].
    
    @param t l'arbre dans lequel on veut échanger les noeuds.
    @param c1 la clé d'un des noeuds qu'on souhaite échanger.
    @param c2 la clé d'un autre des noeuds qu'on souhaite échanger.
    @return l'arbre avec les noeuds échangés. L'arbre obtenun'est pas forcément
    un arbre binaire. 
*)
val switch : btree -> char -> char -> btree