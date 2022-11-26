Inductive nat : Set :=
    | O : nat
    | S : forall n : nat, nat.

Definition add : forall (m : nat) (n : nat), nat :=
    fix add (m : nat) (n : nat) {struct m} : nat :=
        match m as _ in nat return nat with
        | O => n
        | S m => S (add m n)
        end.
