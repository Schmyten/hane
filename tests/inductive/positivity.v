Inductive False : Prop :=.
Inductive I : Prop := not_I_I : forall f : (forall i : I, False), I.
