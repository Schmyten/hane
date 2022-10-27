Inductive tree (A : Set) : Set :=
    | Node : forall (x : A) (ts : forest A), tree A
    with forest (A : Set) :=
    | Nil : forest A
    | Cons : forall (t : tree A) (ts : forest A), forest A.

Definition map : forall (A : Set) (B : Set) (f : forall x : A, B) (t : tree A), tree B :=
    fix map_tree (A : Set) (B : Set) (f : forall x : A, B) (t : tree A) : tree B :=
        match t as _ in tree _ return tree B with
        | Node _ x ts => Node B (f x) (map_forest A B f ts)
        end
    with map_forest (A : Set) (B : Set) (f : forall x : A, B) (ts : forest A) : forest B :=
        match ts as _ in forest _ return forest B with
        | Nil _ => Nil B
        | Cons _ t ts => Cons B (map_tree A B f t) (map_forest A B f ts)
        end
    for map_tree.
