{-
Vesting Example
Datum:
    1) The Beneficiary
    2) The deadline
defining it:
data VestingDatum = VestingDatum
    { beneficiary :: PubKeyHash
    , deadline    :: Slot
    } deriving Show

Don't need any information on a redeemer

We need to check only two conditions
1)That only the correct beneficiary can unlock a utxo sitting at this address
this can validate by checking that the beneficiary's signature is included in the transaction
2) That this transaction is only exectued after the deadline is reached.

mkValidator :: VestingDatum -> () -> ScriptContext -> Bool
mkvalidator dat () ctx = 
    traceIfFalse "The bene's signature is missing" checksig &&
    traceIfFalse "deadline not reached "           checkDeadline
where
    info :: TxInfo
    info = scriptContext ctx

    checkSig :: Bool
    checkSig = beneficiary dat `elem` txInfoSignatories info

    checkDeadline :: Bool
    checkDeadline = from (deadline dat) `contains` txInfoValidRange info

data Vesting
instance Scripts.ScriptType Vesting where
   type instance DatumType Vesting = VestingDatum
   type instance RedeemerType Vesting = ()

inst :: Scripts.ScriptInstance Vesting
inst = Scripts.validator @Vesting
   $$(PlutusTx.compile [|| mkValidator ||])
   $$(PlutusTx.compile [|| wrap ||])
where
   wrap = Scripts.wrapValidator @VestingDatum @()

data GiveParams = GiveParams
   { gpBeneficiary :: !PubKeyHash
   , gpDeadline    :: !Slot
   , gpAmount      :: !Integer
   } deriving (Generic, ToJSON, FromJSON, ToSchema)

type VestingSchema =
   BlockchainActions
      .\/ Endpoint "give" GiveParams
      .\/ Endpoint "grab" ()

give :: (HasBlockchainActions s, AsContractError e) => GiveParams -> Contract w s e ()
give gp = do
   let dat = VestingDatum
               { beneficiary = gpBeneficiary gp
               , deadline    = gpDeadline gp
               }
      tx  = mustPayToTheScript dat $ Ada.lovelaceValueOf $ gpAmount gp
   ledgerTx <- submitTxConstraints inst tx
   void $ awaitTxConfirmed $ txId ledgerTx
   logInfo @String $ printf "made a gift of %d lovelace to %s with deadline %s"
      (gpAmount gp)
      (show $ gpBeneficiary gp)
      (show $ gpDeadline gp)

grab :: forall w s e. (HasBlockchainActions s, AsContractError e) => Contract w s e ()
grab = do
   now   <- currentSlot
   pkh   <- pubKeyHash <$> ownPubKey
   utxos <- Map.filter (isSuitable pkh now) <$> utxoAt scrAddress
   if Map.null utxos
      then logInfo @String $ "no gifts available"
      else do
            let orefs   = fst <$> Map.toList utxos
               lookups = Constraints.unspentOutputs utxos  <>
                        Constraints.otherScript validator
               tx :: TxConstraints Void Void
               tx      = mconcat [mustSpendScriptOutput oref $ Redeemer $ PlutusTx.toData () | oref <- orefs] <>
                        mustValidateIn (from now)
            ledgerTx <- submitTxConstraintsWith @Void lookups tx
            void $ awaitTxConfirmed $ txId ledgerTx
            logInfo @String $ "collected gifts"
where
   isSuitable :: PubKeyHash -> Slot -> TxOutTx -> Bool
   isSuitable pkh now o = case txOutDatumHash $ txOutTxOut o of
      Nothing -> False
      Just h  -> case Map.lookup h $ txData $ txOutTxTx o of
            Nothing        -> False
            Just (Datum e) -> case PlutusTx.fromData e of
               Nothing -> False
               Just d  -> beneficiary d == pkh && deadline d <= now
-}