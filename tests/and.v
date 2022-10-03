let imply : forall P : Prop, forall Q : Prop, Prop := fun P : Prop => fun Q : Prop => forall p : P, Q in
fun P : Prop => fun Q : Prop => forall R : Prop, imply (imply P (imply Q R)) R