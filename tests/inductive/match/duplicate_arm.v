Inductive True : Prop := I : True.
Definition test : True :=
    match I as _ in True return True with
    | I => I
    | I => I
    end.
