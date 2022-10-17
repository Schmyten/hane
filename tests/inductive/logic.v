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
