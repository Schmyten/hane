Inductive prod (A : Set) (B : Set) : Set :=
    pair : forall (x : A) (y : B), prod A B.

Definition fst (A : Set) (B : Set) (p : prod A B) : A :=
    match p as p in prod _ _ return A with
    | pair _ _ x y => x
    end.

Definition snd (A : Set) (B : Set) (p : prod A B) : B :=
    match p as p in prod _ _ return B with
    | pair _ _ x y => y
    end.

Inductive sum (A : Set) (B : Set) : Set :=
    | inl : forall x : A, sum A B
    | inr : forall x : B, sum A B.

Inductive sig (A : Set) (P : forall x : A, Prop) : Set :=
    exist : forall (x : A) (p : P x), sig A P.

Definition proj1_sig (A : Set) (P : forall x : A, Prop) (x : sig A P) : A :=
    match x as _ in sig _ _ return A with
    | exist _ _ x _ => x
    end.

Definition proj2_sig (A : Set) (P : forall x : A, Prop) (x : sig A P) : P (proj1_sig A P x) :=
    match x as x in sig _ _ return P (proj1_sig A P x) with
    | exist _ _ _ p => p
    end.
