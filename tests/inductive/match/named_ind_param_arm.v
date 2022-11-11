Inductive prod (A : Set) (B : Set) : Set :=
    pair : forall (x : A) (y : B), prod A B.

Definition fst (A : Set) (B : Set) (p : prod A B) : A :=
    match p as _ in prod _ _ return A with
    | pair A B x y => x
    end.