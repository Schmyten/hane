Inductive nat : Set :=
    | O : nat
    | S : forall n : nat, nat.

Definition zero : nat := O.
Definition one : nat := S O.
Definition two : nat := S (S O).

Inductive even : forall n : nat, Prop :=
    | even_O : even O
    | even_S : forall (n : nat) (H : odd n), even (S n)
    with odd : forall n : nat, Prop :=
    | odd_S : forall (n : nat) (H : even n), odd (S n).