(* AutoBound Big-Step Operational Semantics — Full Version
   Aalborg University, Software 4th Semester

   Notation guide (closest ASCII match to the report's math notation):
     E |-  e  ↠ v      arithmetic evaluation  (report: E ⊢ e ↠ v)
     E ||- b  ↠ bv     boolean evaluation     (report: E ⊢ b ↠ bv)
     E ,M |-  s ↠ E'   statement execution    (report: E ⊢ s ↠ E')
                       (M is the mutability context, part of the state)

   Rules covered:
     E-Num, E-Var, E-Add, E-Sub, E-Mul, E-Div, E-Mod, E-Neg
     B-True, B-False, B-Eq, B-Neq, B-Lt, B-Le, B-Gt, B-Ge,
     B-And, B-Or, B-Not
     S-Skip, S-Assign, S-Assign-Immut, S-Seq,
     S-IfTrue, S-IfFalse, S-WhileFalse, S-WhileTrue
     B-Decl, B-Write, B-Write-OOB, B-Read, B-Read-OOB            *)

From Stdlib Require Import String List Bool ZArith Lia.
Import ListNotations.
Open Scope string_scope.

(* ================================================================== *)
(*  Runtime Values and Environments                                     *)
(* ================================================================== *)

Definition var := string.

(* Runtime value: integer (Z handles negation), buffer, or Error.
   Z is used instead of nat so that unary minus is modelled correctly. *)
Inductive value : Type :=
  | VNum   : Z -> value
  | VBuf   : nat -> list Z -> value   (* capacity n, current contents xs *)
  | VError : value.

(* Value environment: maps variables to their runtime values. *)
Definition env := var -> value.

(* Mutability environment: true = mutable, false = immutable.
   Declared at variable introduction and never changes. *)
Definition menv := var -> bool.

Definition empty_env : env  := fun _ => VNum 0%Z.
Definition all_mut   : menv := fun _ => true.

Definition update (E : env) (x : var) (v : value) : env :=
  fun y => if String.eqb x y then v else E y.

(* ================================================================== *)
(*  Expressions                                                         *)
(* ================================================================== *)

Inductive aexp : Type :=
  | ANum   : Z    -> aexp
  | AVar   : var  -> aexp
  | APlus  : aexp -> aexp -> aexp
  | AMinus : aexp -> aexp -> aexp
  | AMult  : aexp -> aexp -> aexp
  | ADiv   : aexp -> aexp -> aexp
  | AMod   : aexp -> aexp -> aexp
  | ANeg   : aexp -> aexp.             (* unary minus *)

Inductive bexp : Type :=
  | BTrue  | BFalse
  | BEq    : aexp -> aexp -> bexp
  | BNeq   : aexp -> aexp -> bexp
  | BLt    : aexp -> aexp -> bexp
  | BLe    : aexp -> aexp -> bexp
  | BGt    : aexp -> aexp -> bexp
  | BGe    : aexp -> aexp -> bexp
  | BAnd   : bexp -> bexp -> bexp
  | BOr    : bexp -> bexp -> bexp
  | BNot   : bexp -> bexp.

(* ================================================================== *)
(*  Arithmetic Semantics   E |- a ↠ n                                  *)
(*  Report rule name prefix: E-                                         *)
(* ================================================================== *)

Reserved Notation "E '|-' a '↠' n" (at level 50, a at next level).

Inductive aeval : env -> aexp -> Z -> Prop :=

  (* ── E-Num ───────────────────────────────────────────────────────── *)
  | E_ANum    : forall E (n : Z),
      E |- ANum n ↠ n

  (* ── E-Var ───────────────────────────────────────────────────────── *)
  | E_AVar    : forall E x (n : Z),
      E x = VNum n ->
      E |- AVar x ↠ n

  (* ── E-Add ───────────────────────────────────────────────────────── *)
  | E_APlus   : forall E a1 a2 n1 n2,
      E |- a1 ↠ n1 -> E |- a2 ↠ n2 ->
      E |- APlus a1 a2 ↠ (n1 + n2)%Z

  (* ── E-Sub ───────────────────────────────────────────────────────── *)
  | E_AMinus  : forall E a1 a2 n1 n2,
      E |- a1 ↠ n1 -> E |- a2 ↠ n2 ->
      E |- AMinus a1 a2 ↠ (n1 - n2)%Z

  (* ── E-Mul ───────────────────────────────────────────────────────── *)
  | E_AMult   : forall E a1 a2 n1 n2,
      E |- a1 ↠ n1 -> E |- a2 ↠ n2 ->
      E |- AMult a1 a2 ↠ (n1 * n2)%Z

  (* ── E-Div (defined when divisor ≠ 0) ───────────────────────────── *)
  | E_ADiv    : forall E a1 a2 n1 n2,
      E |- a1 ↠ n1 -> E |- a2 ↠ n2 -> n2 <> 0%Z ->
      E |- ADiv a1 a2 ↠ (n1 / n2)%Z

  (* ── E-Mod (defined when divisor ≠ 0) ───────────────────────────── *)
  | E_AMod    : forall E a1 a2 n1 n2,
      E |- a1 ↠ n1 -> E |- a2 ↠ n2 -> n2 <> 0%Z ->
      E |- AMod a1 a2 ↠ (n1 mod n2)%Z

  (* ── E-Neg ───────────────────────────────────────────────────────── *)
  | E_ANeg    : forall E a n,
      E |- a ↠ n ->
      E |- ANeg a ↠ (- n)%Z

where "E '|-' a '↠' n" := (aeval E a n).

(* ================================================================== *)
(*  Boolean Semantics   E ||- b ↠ bv                                   *)
(*  Report rule name prefix: B-  (not to be confused with buffer B-)   *)
(* ================================================================== *)

Reserved Notation "E '||-' b '↠' bv" (at level 50, b at next level).

Inductive beval : env -> bexp -> bool -> Prop :=

  (* ── B-True / B-False ────────────────────────────────────────────── *)
  | E_BTrue       : forall E,   E ||- BTrue  ↠ true
  | E_BFalse      : forall E,   E ||- BFalse ↠ false

  (* ── B-Eq ────────────────────────────────────────────────────────── *)
  | E_BEqTrue     : forall E a1 a2 n,
      E |- a1 ↠ n -> E |- a2 ↠ n ->
      E ||- BEq a1 a2 ↠ true
  | E_BEqFalse    : forall E a1 a2 n1 n2,
      E |- a1 ↠ n1 -> E |- a2 ↠ n2 -> n1 <> n2 ->
      E ||- BEq a1 a2 ↠ false

  (* ── B-Neq ───────────────────────────────────────────────────────── *)
  | E_BNeqTrue    : forall E a1 a2 n1 n2,
      E |- a1 ↠ n1 -> E |- a2 ↠ n2 -> n1 <> n2 ->
      E ||- BNeq a1 a2 ↠ true
  | E_BNeqFalse   : forall E a1 a2 n,
      E |- a1 ↠ n -> E |- a2 ↠ n ->
      E ||- BNeq a1 a2 ↠ false

  (* ── B-Lt ────────────────────────────────────────────────────────── *)
  | E_BLtTrue     : forall E a1 a2 n1 n2,
      E |- a1 ↠ n1 -> E |- a2 ↠ n2 -> (n1 < n2)%Z ->
      E ||- BLt a1 a2 ↠ true
  | E_BLtFalse    : forall E a1 a2 n1 n2,
      E |- a1 ↠ n1 -> E |- a2 ↠ n2 -> (n2 <= n1)%Z ->
      E ||- BLt a1 a2 ↠ false

  (* ── B-Le ────────────────────────────────────────────────────────── *)
  | E_BLeTrue     : forall E a1 a2 n1 n2,
      E |- a1 ↠ n1 -> E |- a2 ↠ n2 -> (n1 <= n2)%Z ->
      E ||- BLe a1 a2 ↠ true
  | E_BLeFalse    : forall E a1 a2 n1 n2,
      E |- a1 ↠ n1 -> E |- a2 ↠ n2 -> (n2 < n1)%Z ->
      E ||- BLe a1 a2 ↠ false

  (* ── B-Gt ────────────────────────────────────────────────────────── *)
  | E_BGtTrue     : forall E a1 a2 n1 n2,
      E |- a1 ↠ n1 -> E |- a2 ↠ n2 -> (n2 < n1)%Z ->
      E ||- BGt a1 a2 ↠ true
  | E_BGtFalse    : forall E a1 a2 n1 n2,
      E |- a1 ↠ n1 -> E |- a2 ↠ n2 -> (n1 <= n2)%Z ->
      E ||- BGt a1 a2 ↠ false

  (* ── B-Ge ────────────────────────────────────────────────────────── *)
  | E_BGeTrue     : forall E a1 a2 n1 n2,
      E |- a1 ↠ n1 -> E |- a2 ↠ n2 -> (n2 <= n1)%Z ->
      E ||- BGe a1 a2 ↠ true
  | E_BGeFalse    : forall E a1 a2 n1 n2,
      E |- a1 ↠ n1 -> E |- a2 ↠ n2 -> (n1 < n2)%Z ->
      E ||- BGe a1 a2 ↠ false

  (* ── B-And ───────────────────────────────────────────────────────── *)
  | E_BAndTrue    : forall E b1 b2,
      E ||- b1 ↠ true -> E ||- b2 ↠ true ->
      E ||- BAnd b1 b2 ↠ true
  | E_BAndFalseL  : forall E b1 b2,
      E ||- b1 ↠ false ->
      E ||- BAnd b1 b2 ↠ false
  | E_BAndFalseR  : forall E b1 b2,
      E ||- b1 ↠ true -> E ||- b2 ↠ false ->
      E ||- BAnd b1 b2 ↠ false

  (* ── B-Or ────────────────────────────────────────────────────────── *)
  | E_BOrTrue1    : forall E b1 b2,
      E ||- b1 ↠ true ->
      E ||- BOr b1 b2 ↠ true
  | E_BOrTrue2    : forall E b1 b2,
      E ||- b1 ↠ false -> E ||- b2 ↠ true ->
      E ||- BOr b1 b2 ↠ true
  | E_BOrFalse    : forall E b1 b2,
      E ||- b1 ↠ false -> E ||- b2 ↠ false ->
      E ||- BOr b1 b2 ↠ false

  (* ── B-Not ───────────────────────────────────────────────────────── *)
  | E_BNotTrue    : forall E b,
      E ||- b ↠ false -> E ||- BNot b ↠ true
  | E_BNotFalse   : forall E b,
      E ||- b ↠ true  -> E ||- BNot b ↠ false

where "E '||-' b '↠' bv" := (beval E b bv).

(* ================================================================== *)
(*  Statements                                                          *)
(* ================================================================== *)

Inductive stmt : Type :=
  | Skip
  | Assign   : var -> aexp -> stmt
  | Seq      : stmt -> stmt -> stmt
  | If       : bexp -> stmt -> stmt -> stmt
  | While    : bexp -> stmt -> stmt
  | BufDecl  : var -> nat -> list aexp -> stmt   (* buffer x <n> = [a1,..,ak] *)
  | BufWrite : var -> aexp -> aexp -> stmt       (* x[i] = e                  *)
  | BufRead  : var -> aexp -> var -> stmt.       (* z = x[i]                  *)

(* ── Helper: evaluate a list of arithmetic expressions ─────────────── *)
Inductive aeval_list : env -> list aexp -> list Z -> Prop :=
  | E_AListNil  : forall E,
      aeval_list E [] []
  | E_AListCons : forall E a rest v vs,
      E |- a ↠ v ->
      aeval_list E rest vs ->
      aeval_list E (a :: rest) (v :: vs).

(* ── Helper: replace the i-th element of a list ────────────────────── *)
Fixpoint list_update (xs : list Z) (i : nat) (v : Z) : list Z :=
  match xs, i with
  | [],     _    => []
  | _ :: t, 0   => v :: t
  | h :: t, S i' => h :: list_update t i' v
  end.

(* ================================================================== *)
(*  Statement Semantics   E , M |- s ↠ E'                             *)
(*  M is the mutability context (report's E already carries this)      *)
(*  Report rule name prefixes: S- (general), B- (buffer)               *)
(* ================================================================== *)

Reserved Notation "E ',' M '|-' s '↠' E'" (at level 50, s at next level).

Inductive exec : env -> menv -> stmt -> env -> Prop :=

  (* ── S-Skip ──────────────────────────────────────────────────────── *)
  | Ex_Skip : forall E M,
      E , M |- Skip ↠ E

  (* ── S-Assign: mutable variable ─────────────────────────────────── *)
  (*  E ⊢ e ↠ v    M(x) = true                                        *)
  (*  ──────────────────────────────                                    *)
  (*  E ⊢ x = e ↠ E[x ↦ v]                                            *)
  | Ex_Assign : forall E M x a n,
      E |- a ↠ n ->
      M x = true ->
      E , M |- Assign x a ↠ update E x (VNum n)

  (* ── S-Assign-Immut: immutable variable — assignment is an error ── *)
  (*  M(x) = false                                                      *)
  (*  ──────────────────────────────                                    *)
  (*  E ⊢ x = e ↠ E[x ↦ VError]                                       *)
  | Ex_AssignImmut : forall E M x a,
      M x = false ->
      E , M |- Assign x a ↠ update E x VError

  (* ── S-Seq ───────────────────────────────────────────────────────── *)
  | Ex_Seq : forall E E1 E2 M s1 s2,
      E  , M |- s1 ↠ E1 ->
      E1 , M |- s2 ↠ E2 ->
      E  , M |- Seq s1 s2 ↠ E2

  (* ── S-IfTrue ────────────────────────────────────────────────────── *)
  | Ex_IfTrue : forall E E' M b s1 s2,
      E ||- b ↠ true ->
      E , M |- s1 ↠ E' ->
      E , M |- If b s1 s2 ↠ E'

  (* ── S-IfFalse ───────────────────────────────────────────────────── *)
  | Ex_IfFalse : forall E E' M b s1 s2,
      E ||- b ↠ false ->
      E , M |- s2 ↠ E' ->
      E , M |- If b s1 s2 ↠ E'

  (* ── S-WhileFalse ────────────────────────────────────────────────── *)
  | Ex_WhileFalse : forall E M b s,
      E ||- b ↠ false ->
      E , M |- While b s ↠ E

  (* ── S-WhileTrue ─────────────────────────────────────────────────── *)
  | Ex_WhileTrue : forall E E1 E2 M b s,
      E ||- b ↠ true ->
      E  , M |- s           ↠ E1 ->
      E1 , M |- While b s   ↠ E2 ->
      E  , M |- While b s   ↠ E2

  (* ── B-Decl ──────────────────────────────────────────────────────── *)
  (*  n > 0    |as| ≤ n    ∀ aᵢ ∈ as: E ⊢ aᵢ ↠ vᵢ                   *)
  (*  ─────────────────────────────────────────────────────────────── *)
  (*  E ⊢ buffer x⟨n⟩ = [as]  ↠  E ∪ {x ↦ VBuf n [v0,..,vk]}        *)
  | Ex_BufDecl : forall E M x n aes vs,
      n > 0 ->
      length aes <= n ->
      aeval_list E aes vs ->
      E , M |- BufDecl x n aes ↠ update E x (VBuf n vs)

  (* ── B-Write (in-bounds) ─────────────────────────────────────────── *)
  (*  E(x) = VBuf n xs    E ⊢ i ↠ v    0 ≤ v < n    E ⊢ e ↠ val      *)
  (*  ─────────────────────────────────────────────────────────────── *)
  (*  E ⊢ x[i] = e  ↠  E[x ↦ VBuf n (xs[v ← val])]                  *)
  | Ex_BufWrite : forall E M x n xs a_i a_e (v : nat) val,
      E x = VBuf n xs ->
      E |- a_i ↠ Z.of_nat v ->
      v < n ->
      E |- a_e ↠ val ->
      E , M |- BufWrite x a_i a_e ↠ update E x (VBuf n (list_update xs v val))

  (* ── B-Write-OOB ─────────────────────────────────────────────────── *)
  (*  E(x) = VBuf n xs    E ⊢ i ↠ v    v ≥ n                          *)
  (*  ─────────────────────────────────────────────────────────────── *)
  (*  E ⊢ x[i] = e  ↠  E[x ↦ VError]                                 *)
  | Ex_BufWriteOOB : forall E M x n xs a_i a_e (v : nat),
      E x = VBuf n xs ->
      E |- a_i ↠ Z.of_nat v ->
      n <= v ->
      E , M |- BufWrite x a_i a_e ↠ update E x VError

  (* ── B-Read (in-bounds) ──────────────────────────────────────────── *)
  (*  E(x) = VBuf n xs    E ⊢ i ↠ v    0 ≤ v < n                      *)
  (*  ─────────────────────────────────────────────────────────────── *)
  (*  E ⊢ z = x[i]  ↠  E[z ↦ xs[v]]                                  *)
  | Ex_BufRead : forall E M x n xs a_i (v : nat) z r,
      E x = VBuf n xs ->
      E |- a_i ↠ Z.of_nat v ->
      v < n ->
      nth_error xs v = Some r ->
      E , M |- BufRead x a_i z ↠ update E z (VNum r)

  (* ── B-Read-OOB ──────────────────────────────────────────────────── *)
  (*  E(x) = VBuf n xs    E ⊢ i ↠ v    v ≥ n                          *)
  (*  ─────────────────────────────────────────────────────────────── *)
  (*  E ⊢ z = x[i]  ↠  E[z ↦ VError]                                 *)
  | Ex_BufReadOOB : forall E M x n xs a_i (v : nat) z,
      E x = VBuf n xs ->
      E |- a_i ↠ Z.of_nat v ->
      n <= v ->
      E , M |- BufRead x a_i z ↠ update E z VError

where "E ',' M '|-' s '↠' E'" := (exec E M s E').

(* ================================================================== *)
(*  Example Proofs                                                      *)
(* ================================================================== *)

(* ── Proof 1: Scalar assignment chain ────────────────────────────────
   x = 5;   y = x + 1;   ⟹   {x ↦ 5, y ↦ 6}                         *)
Example AssignVariable :
  empty_env , all_mut |-
    Seq (Assign "x" (ANum 5%Z))
        (Assign "y" (APlus (AVar "x") (ANum 1%Z)))
  ↠ update (update empty_env "x" (VNum 5%Z)) "y" (VNum 6%Z).
Proof.
  apply Ex_Seq with (E1 := update empty_env "x" (VNum 5%Z)).
  - apply Ex_Assign. apply E_ANum. reflexivity.
  - apply Ex_Assign.
    + replace 6%Z with (5 + 1)%Z by reflexivity.
      apply E_APlus.
      * apply E_AVar. reflexivity.
      * apply E_ANum.
    + reflexivity.
Qed.

(* ── Proof 2: S-Assign-Immut — immutable variable cannot be updated ──
   Given: x is immutable (M x = false)
   x = 99;   ⟹   x ↦ VError                                           *)
Example AssignImmutable :
  let E0 := update empty_env "x" (VNum 5%Z) in
  let M0 : menv := fun v => if String.eqb v "x" then false else true in
  E0 , M0 |-
    Assign "x" (ANum 99%Z)
  ↠ update E0 "x" VError.
Proof.
  apply Ex_AssignImmut. reflexivity.
Qed.

(* ── Proof 3: B-Decl — buffer my_buf <2> = [42, 99] ─────────────────
   ⟹   my_buf ↦ VBuf 2 [42, 99]                                        *)
Example BufDecl_WithInit :
  empty_env , all_mut |-
    BufDecl "my_buf" 2 [ANum 42%Z; ANum 99%Z]
  ↠ update empty_env "my_buf" (VBuf 2 [42%Z; 99%Z]).
Proof.
  apply Ex_BufDecl with (vs := [42%Z; 99%Z]).
  - lia.
  - simpl. lia.
  - apply E_AListCons. apply E_ANum.
    apply E_AListCons. apply E_ANum.
    apply E_AListNil.
Qed.

(* ── Proof 4: B-Write (in-bounds) ────────────────────────────────────
   Given: my_buf ↦ VBuf 2 [42, 99]
   my_buf[0] = 100;   ⟹   my_buf ↦ VBuf 2 [100, 99]                  *)
Example BufWrite_InBounds :
  let E0 := update empty_env "my_buf" (VBuf 2 [42%Z; 99%Z]) in
  E0 , all_mut |-
    BufWrite "my_buf" (ANum 0%Z) (ANum 100%Z)
  ↠ update E0 "my_buf" (VBuf 2 [100%Z; 99%Z]).
Proof.
  apply Ex_BufWrite with (n := 2) (xs := [42%Z; 99%Z]) (v := 0) (val := 100%Z).
  - reflexivity.
  - apply E_ANum.
  - lia.
  - apply E_ANum.
Qed.

(* ── Proof 5: B-Write-OOB ────────────────────────────────────────────
   Given: my_buf ↦ VBuf 2 [42, 99]
   my_buf[5] = 7;   index 5 ≥ capacity 2   ⟹   my_buf ↦ VError
   (see test/e2e/invalid/bufwrite_overflow2.mylang)                     *)
Example BufWrite_OOB :
  let E0 := update empty_env "my_buf" (VBuf 2 [42%Z; 99%Z]) in
  E0 , all_mut |-
    BufWrite "my_buf" (ANum 5%Z) (ANum 7%Z)
  ↠ update E0 "my_buf" VError.
Proof.
  apply Ex_BufWriteOOB with (n := 2) (xs := [42%Z; 99%Z]) (v := 5).
  - reflexivity.
  - apply E_ANum.
  - lia.
Qed.

(* ── Proof 6: B-Read (in-bounds) ─────────────────────────────────────
   Given: my_buf ↦ VBuf 2 [42, 99]
   z = my_buf[1];   ⟹   z ↦ VNum 99                                   *)
Example BufRead_InBounds :
  let E0 := update empty_env "my_buf" (VBuf 2 [42%Z; 99%Z]) in
  E0 , all_mut |-
    BufRead "my_buf" (ANum 1%Z) "z"
  ↠ update E0 "z" (VNum 99%Z).
Proof.
  apply Ex_BufRead with (n := 2) (xs := [42%Z; 99%Z]) (v := 1) (r := 99%Z).
  - reflexivity.
  - apply E_ANum.
  - lia.
  - reflexivity.
Qed.

(* ── Proof 7: B-Read-OOB ─────────────────────────────────────────────
   Given: my_buf ↦ VBuf 2 [42, 99]
   z = my_buf[3];   index 3 ≥ capacity 2   ⟹   z ↦ VError
   (see test/e2e/invalid/bufread_out_of_bounds.mylang)                  *)
Example BufRead_OOB :
  let E0 := update empty_env "my_buf" (VBuf 2 [42%Z; 99%Z]) in
  E0 , all_mut |-
    BufRead "my_buf" (ANum 3%Z) "z"
  ↠ update E0 "z" VError.
Proof.
  apply Ex_BufReadOOB with (n := 2) (xs := [42%Z; 99%Z]) (v := 3).
  - reflexivity.
  - apply E_ANum.
  - lia.
Qed.

(* ── Proof 8: Negation — unary minus ─────────────────────────────────
   z = -(5);   ⟹   z ↦ VNum (-5)                                       *)
Example Negation_Example :
  empty_env , all_mut |-
    Assign "z" (ANeg (ANum 5%Z))
  ↠ update empty_env "z" (VNum (-5)%Z).
Proof.
  apply Ex_Assign.
  - apply E_ANeg. apply E_ANum.
  - reflexivity.
Qed.
