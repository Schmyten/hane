Inductive False : Prop :=.
Inductive True: Prop :=
    I: True.
Inductive and (P : Prop) (Q : Prop) : Prop :=
    conj : forall (p : P) (q : Q), and P Q.
Inductive or (P : Prop) (Q : Prop) : Prop :=
    | or_introl : forall p : P, or P Q
    | or_intror : forall q : Q, or P Q.

Inductive ex (T : Set) (P : forall x : T, Prop) : Prop :=
    ex_intro : forall (x : T) (p : P x), ex T P.

Inductive eq (T : Set) (x : T) : forall y : T, Prop :=
    eq_refl : eq T x x.

Definition ex_falso : forall (P : Prop) (f : False), P :=
    fun (P : Prop) (f : False) =>
        match f as f in False return P with end.

Definition and_elem : forall (P : Prop) (Q : Prop) (R : Prop) (H : forall (p : P) (q : Q), R) (p : and P Q), R :=
    fun (P : Prop) (Q : Prop) (R : Prop) (H : forall (p : P) (q : Q), R) (p : and P Q) =>
        match p as p in and _ _ return R with
        | conj _ _ p q => H p q
        end.

Definition or_elem : forall (P : Prop) (Q : Prop) (R : Prop) (HP : forall p : P, R) (HQ : forall q : Q, R) (o : or P Q), R :=
    fun (P : Prop) (Q : Prop) (R : Prop) (HP : forall p : P, R) (HQ : forall q : Q, R) (o : or P Q) =>
        match o as o in or _ _ return R with
        | or_introl _ _ p => HP p
        | or_intror _ _ q => HQ q
        end.

Definition ex_elem : forall (T : Set) (P : forall x : T, Prop) (R : Prop) (H : forall (x : T) (p : P x), R) (e : ex T P), R :=
    fun (T : Set) (P : forall x : T, Prop) (R : Prop) (H : forall (x : T) (p : P x), R) (e : ex T P) =>
        match e as e in ex _ _ return R with
        | ex_intro _ _ x p => H x p
        end.

Definition eq_elem : forall (T : Set) (x : T) (R : forall x : T, Prop) (r : R x) (y : T) (e : eq T x y), R y :=
    fun (T : Set) (x : T) (R : forall x : T, Prop) (r : R x) (y : T) (e : eq T x y) =>
        match e as e in eq _ _ z return R z with
        | eq_refl _ _ => r
        end.

Definition eq_sym : forall (T : Set) (x : T) (y : T) (e : eq T x y), eq T y x :=
    fun (T : Set) (x : T) (y : T) (e : eq T x y) =>
        match e as e in eq _ _ z return eq T z x with
        | eq_refl _ _ => eq_refl T x
        end.

Definition eq_trans : forall (T : Set) (x : T) (y : T) (z : T) (e1 : eq T x y) (e2 : eq T y z), eq T x z :=
    fun (T : Set) (x : T) (y : T) (z : T) (e1 : eq T x y) (e2 : eq T y z) =>
        match e2 as e2 in eq _ _ z return eq T x z with
        | eq_refl _ _ => e1
        end.

Definition f_equal : forall (S : Set) (T : Set) (f : forall x : S, T) (x : S) (y : S) (e : eq S x y), eq T (f x) (f y) :=
    fun (S : Set) (T : Set) (f : forall x : S, T) (x : S) (y : S) (e : eq S x y) =>
        match e as e in eq _ _ z return eq T (f x) (f z) with
        | eq_refl _ _ => eq_refl T (f x)
        end.
