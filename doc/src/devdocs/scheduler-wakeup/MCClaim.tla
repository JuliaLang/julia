----------------------------- MODULE MCClaim -----------------------------
(* TLC instance for ClaimWake with the implementation's mailbox policy.    *)
EXTENDS TLC
ClaimPolicy == "mailbox"
VARIABLES ost, opc, wloc
INSTANCE ClaimWake
=============================================================================
